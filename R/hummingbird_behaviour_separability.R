# Can hovering (and hover-feeding) be told apart from other flight at wild
# burst resolution? Random forest on window features from the captive trials
# with BORIS labels (aligned by R/hummingbird_captive_validation.R; run that
# first, this reads its .rds).
#
# Labels: BORIS behaviours are hierarchical and Feed is split by its modifier,
# so each sample has one exclusive state (hb_boris_state()): within Flight
# "Hover-feeding" (Feed with modifier Hovering, inside Hovering), "Hovering"
# (no feeding) or "Other flight"; feeding on a perch is a perched state and
# never mixed with flight.
#
# Windows: the continuous recordings are cut into non-overlapping windows of
# the wild burst lengths (0.3 s = 210 Hz mode, 1.5 s = 105 Hz mode). A flight
# window takes a state when it covers > min_label_frac of the window; "Other
# flight" windows must be pure. Contrasts (positive vs Other flight):
#   any hovering        Hovering + Hover-feeding (main contrast, all feature sets)
#   hovering only       Hovering without feeding
#   hover-feeding       Hover-feeding (S. cyanifrons trials only)
# and hover-feeding vs hovering. Only burst firmware applies: the 0.45 Hz mode
# has one sample per 2.2 s and none of these features.
#
# Features (burst_features() in R/flea_functions.R), in groups:
#   amplitude     dynamic SD, ODBA, wingbeat amplitude per axis, jerk,
#                 cycle-to-cycle variability of VeDBA, kurtosis
#   posture       static (mean) acceleration per axis, its magnitude (~1 g
#                 when weight is exactly supported, as in steady hovering),
#                 pitch / roll, deviation from the bird's own median flight
#                 posture, drift of the static vector between window halves,
#                 variability of the one-wingbeat running-mean static vector
#   spectral      wingbeat frequency, 2nd-harmonic ratios, peak sharpness,
#                 spectral entropy
#   coordination  correlation, coherence and phase between axes at the
#                 wingbeat frequency, linearity of the 3-D oscillation (PCA)
#                 and the angle between the main oscillation axis and gravity
#                 (stroke-plane orientation, independent of tag mounting)
# Features marked orientation-invariant do not depend on how the tag sits on
# the bird.
#
# Models: ranger random forest (class-balanced case weights, permutation
# importance) and, for comparison, logistic regression on the previous
# feature set. Leave-one-bird-out cross-validation (each trial is a different
# bird or deployment); AUC is computed within each held-out bird and averaged,
# because pooling predictions across birds mixes between-bird differences
# into the score.
#
# Outputs (CSV + PNG) go to the captive validation folder, outside the repo.

library(tidyverse)
library(data.table)
library(ranger)
library(patchwork)
source("./R/flea_functions.R")  # read_flea_export(), burst_features()

# ---- paths & parameters -----------------------------------------------------
out_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/Results/captive_validation"
fig_dir <- file.path(out_dir, "figures")
v <- readRDS(file.path(out_dir, "captive_validation.rds"))
window_s <- c(0.3, 1.5)
min_label_frac <- 0.8
wbf_band <- c(15, 50)
n_trees <- 500
seed <- 1
set.seed(seed)

contrasts <- tribble(
  ~contrast,                          ~pos,                           ~neg,           ~pos_birds_only,
  "Any hovering vs other flight",     c("Hovering", "Hover-feeding"), "Other flight", FALSE,
  "Hovering (no feeding) vs other",   "Hovering",                     "Other flight", FALSE,
  "Hover-feeding vs other flight",    "Hover-feeding",                "Other flight", TRUE,
  "Hover-feeding vs hovering",        "Hover-feeding",                "Hovering",     TRUE)
main_contrast <- contrasts$contrast[1]

# ---- window features --------------------------------------------------------
signed_angle_deg <- function(u, w) acos(pmax(-1, pmin(1, sum(u * w) / sqrt(sum(u^2) * sum(w^2))))) * 180 / pi

feature_groups <- tribble(
  ~feature, ~group,
  "log_dyn", "amplitude", "odba", "amplitude", "amp_x", "amplitude", "amp_y", "amplitude", "amp_z", "amplitude",
  "share_x", "amplitude", "share_y", "amplitude", "share_z", "amplitude", "jerk", "amplitude",
  "vedba_cv", "amplitude", "kurt_vedba", "amplitude",
  "mean_x", "posture", "mean_y", "posture", "mean_z", "posture", "static_g", "posture", "pitch_deg", "posture",
  "roll_deg", "posture", "pose_dev_deg", "posture", "drift_deg", "posture", "static_sd", "posture", "static_mag_sd", "posture",
  "wbf_hz", "spectral", "harm_ratio", "spectral", "harm_x", "spectral", "harm_z", "spectral", "peak_frac", "spectral",
  "spec_entropy", "spectral",
  "corr_xy", "coordination", "corr_xz", "coordination", "corr_yz", "coordination",
  "coh_xy", "coordination", "coh_xz", "coordination", "coh_yz", "coordination",
  "cos_ph_xy", "coordination", "cos_ph_xz", "coordination", "cos_ph_yz", "coordination",
  "sin_ph_xy", "coordination", "sin_ph_xz", "coordination", "sin_ph_yz", "coordination",
  "pc1_frac", "coordination", "pc2_frac", "coordination", "stroke_grav_deg", "coordination") %>%
  mutate(orientation_invariant = feature %in% c("log_dyn", "odba", "jerk", "vedba_cv", "kurt_vedba", "static_g",
                                                "pose_dev_deg", "drift_deg", "static_sd", "static_mag_sd", "wbf_hz",
                                                "harm_ratio", "peak_frac", "spec_entropy", "pc1_frac", "pc2_frac",
                                                "stroke_grav_deg"))
# the feature set of the earlier logistic regression
previous_features <- c("log_dyn", "share_x", "share_y", "share_z", "mean_x", "mean_y", "mean_z", "static_g",
                       "wbf_hz", "harm_ratio")

# ---- build windows ----------------------------------------------------------
# cached: feature extraction takes ~10 min and only depends on the validation
# output; delete the cache (or rerun the validation) to rebuild it
win_cache <- file.path(out_dir, "hover_windows.rds")
cap_rds <- file.path(out_dir, "captive_validation.rds")
ok_obs <- v$align %>% filter(align_ok) %>% pull(obs)
win <- if (file.exists(win_cache) && file.mtime(win_cache) > file.mtime(cap_rds)) readRDS(win_cache) else map_dfr(ok_obs, function(o) {
  message("windows: ", o)
  tr <- filter(v$trials, obs == o); al <- filter(v$align, obs == o)
  d <- read_flea_export(tr$path)$data
  t <- al$a + al$k * (seq_len(nrow(d)) - 1) / al$sr_nominal
  lab <- hb_boris_state(t, filter(v$boris, obs == o))
  acc <- cbind(d$x, d$y, d$z)
  map_dfr(window_s, function(ws) {
    n <- round(ws * al$sr_true)
    starts <- seq(1, nrow(d) - n, by = n)
    map_dfr(starts, function(s) {
      idx <- s:(s + n - 1); st <- lab$state[idx]
      if (any(is.na(st)) || any(lab$top[idx] != "Flight")) return(NULL)
      frac <- table(st) / length(st)
      cls <- names(frac)[which.max(frac)]
      if (cls == "Other flight" && max(frac) < 1) return(NULL)   # other flight must be pure
      if (max(frac) <= min_label_frac) return(NULL)
      bind_cols(tibble(obs = o, species = al$species, window_s = ws, t_video = t[s], class = cls),
                burst_features(acc[idx, ], al$sr_true, band = wbf_band))
    })
  })
}) %>%
  # posture relative to the bird's own median flight posture (no labels used)
  group_by(obs, window_s) %>%
  mutate(pose_dev_deg = pmap_dbl(list(mean_x, mean_y, mean_z), function(x, y, z)
    signed_angle_deg(c(x, y, z), c(median(mean_x), median(mean_y), median(mean_z))))) %>%
  ungroup()
saveRDS(win, win_cache)

# individuals: the same bird was sometimes tested with two tags (SC26_1 and
# SC26_2 = band G00193; SC27_1 and SC27_2 = G00184), so cross-validation
# folds, per-bird AUC and within-bird checks group by individual (band
# number; the trial when there is no band), not by trial. Sex from the same sheet.
test_xlsx <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Accelerometer test trials WIN 2025.xlsx"
birds <- readxl::read_excel(test_xlsx, sheet = "Successful trials", skip = 1, .name_repair = "unique_quiet") %>%
  transmute(trial = as.integer(`Trial ID`), sex = Sex, band = na_if(as.character(`Band #`), "NA")) %>%
  filter(!is.na(trial))
win <- win %>%
  left_join(select(v$trials, obs, trial), by = "obs") %>%
  left_join(birds, by = "trial") %>%
  mutate(bird = coalesce(band, obs))
print(distinct(win, obs, trial, species, sex, band, bird), n = Inf)

# windows of one contrast, y = hover (positive class) / other
# (hover-feeding only occurs in the S. cyanifrons trials, so those contrasts
# are restricted to birds with positive windows to avoid a species contrast)
contrast_data <- function(cn, ws) {
  ct <- filter(contrasts, contrast == cn)
  d <- win %>% filter(window_s == ws, class %in% c(ct$pos[[1]], ct$neg[[1]])) %>%
    mutate(y = factor(class %in% ct$pos[[1]], levels = c(FALSE, TRUE), labels = c("other", "hover")))
  if (ct$pos_birds_only) d <- d %>% group_by(bird) %>% filter(any(y == "hover")) %>% ungroup()
  d
}

# features that exist in (almost) all windows of d: the harmonic ratios are NA
# in the ~110 Hz recordings, where the 2nd harmonic is above Nyquist
usable <- function(d, fs, max_na = 0.05) fs[colMeans(is.na(d[, fs])) <= max_na]

all_features <- feature_groups$feature
feature_sets <- list(
  `previous (logistic set)` = previous_features,
  `+ amplitude` = union(previous_features, feature_groups$feature[feature_groups$group == "amplitude"]),
  `+ posture` = union(previous_features, feature_groups$feature[feature_groups$group == "posture"]),
  `+ coordination` = union(previous_features, feature_groups$feature[feature_groups$group == "coordination"]),
  `all features` = all_features,
  `orientation-invariant only` = feature_groups$feature[feature_groups$orientation_invariant])

# ---- leave-one-bird-out cross-validation ------------------------------------
auc <- function(y, p) {  # Mann-Whitney; y logical
  if (n_distinct(y) < 2) return(NA_real_)
  r <- rank(p); n1 <- sum(y); n0 <- sum(!y)
  (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
balanced_w <- function(y) ifelse(y == "hover", 0.5 / mean(y == "hover"), 0.5 / mean(y == "other"))

cv_model <- function(d, fs, method = c("rf", "logistic"), keep_importance = FALSE) {
  method <- match.arg(method)
  fs <- usable(d, fs)
  d <- d %>% drop_na(all_of(fs))
  pred <- rep(NA_real_, nrow(d)); imp <- list()
  for (o in unique(d$bird)) {
    tr <- d$bird != o
    if (n_distinct(d$y[tr]) < 2) next
    w <- balanced_w(d$y[tr])
    if (method == "rf") {
      m <- ranger(x = as.data.frame(d[tr, fs]), y = d$y[tr], num.trees = n_trees, probability = TRUE,
                  case.weights = w, importance = if (keep_importance) "permutation" else "none", seed = seed)
      pred[!tr] <- predict(m, as.data.frame(d[!tr, fs]))$predictions[, "hover"]
      if (keep_importance) imp[[o]] <- tibble(fold = o, feature = names(m$variable.importance), importance = m$variable.importance)
    } else {
      m <- suppressWarnings(glm(reformulate(fs, "y"), data = mutate(d[tr, ], y = y == "hover"), family = binomial, weights = w))
      pred[!tr] <- predict(m, d[!tr, ], type = "response")
    }
  }
  list(pred = mutate(d, p_hover = pred) %>% filter(!is.na(p_hover)), importance = bind_rows(imp))
}

score <- function(pr) {
  y <- pr$y == "hover"; p <- pr$p_hover; hat <- p > 0.5
  per_bird <- pr %>% group_by(bird) %>% summarise(auc = auc(y == "hover", p_hover), n_hover = sum(y == "hover"), .groups = "drop")
  tibble(n_hover = sum(y), n_other = sum(!y), n_birds_hover = n_distinct(pr$bird[y]),
         auc_within_bird = mean(per_bird$auc, na.rm = TRUE),
         auc_within_bird_sd = sd(per_bird$auc, na.rm = TRUE),
         n_birds_auc = sum(!is.na(per_bird$auc)),
         auc_pooled = auc(y, p), sensitivity = mean(hat[y]), specificity = mean(!hat[!y]),
         balanced_accuracy = (mean(hat[y]) + mean(!hat[!y])) / 2)
}

min_pos_windows <- 15   # fewer positive windows: contrast not tested
results <- list(); preds <- list(); per_bird <- list()
for (cn in contrasts$contrast) {
  for (ws in window_s) {
    d <- contrast_data(cn, ws)
    if (sum(d$y == "hover") < min_pos_windows || n_distinct(d$bird[d$y == "hover"]) < 2) {
      results[[length(results) + 1]] <- tibble(contrast = cn, window_s = ws, model = "not tested",
                                               n_hover = sum(d$y == "hover"), n_other = sum(d$y == "other"),
                                               n_birds_hover = n_distinct(d$bird[d$y == "hover"]))
      next
    }
    # all feature sets for the main contrast; full and orientation-invariant sets otherwise
    sets <- if (cn == main_contrast) names(feature_sets) else c("all features", "orientation-invariant only")
    for (fs_name in sets) {
      message("CV: ", cn, ", ", ws, " s, ", fs_name)
      cv <- cv_model(d, feature_sets[[fs_name]], "rf")
      results[[length(results) + 1]] <- bind_cols(tibble(contrast = cn, window_s = ws, model = "random forest", features = fs_name), score(cv$pred))
      preds[[length(preds) + 1]] <- cv$pred %>% select(bird, obs, y, p_hover) %>%
        mutate(contrast = cn, window_s = ws, model = "random forest", features = fs_name)
      per_bird[[length(per_bird) + 1]] <- cv$pred %>% group_by(bird) %>%
        summarise(auc = auc(y == "hover", p_hover), .groups = "drop") %>%
        mutate(contrast = cn, window_s = ws, features = fs_name, model = "random forest")
    }
    if (cn == main_contrast) {
      cv <- cv_model(d, previous_features, "logistic")
      results[[length(results) + 1]] <- bind_cols(tibble(contrast = cn, window_s = ws, model = "logistic", features = "previous (logistic set)"), score(cv$pred))
      per_bird[[length(per_bird) + 1]] <- cv$pred %>% group_by(bird) %>%
        summarise(auc = auc(y == "hover", p_hover), .groups = "drop") %>%
        mutate(contrast = cn, window_s = ws, features = "previous (logistic set)", model = "logistic")
    }
  }
}
rf_results <- bind_rows(results)
rf_preds <- bind_rows(preds)
rf_per_bird <- bind_rows(per_bird) %>% filter(!is.na(auc))

# importance across folds and final models on all data (main contrast, all features)
imp_cv <- map_dfr(window_s, function(ws) {
  cv_model(contrast_data(main_contrast, ws), all_features, "rf", keep_importance = TRUE)$importance %>% mutate(window_s = ws)
}) %>% left_join(feature_groups, by = "feature")
final <- map(set_names(window_s), function(ws) {
  d <- contrast_data(main_contrast, ws); fs <- usable(d, all_features); d <- drop_na(d, all_of(fs))
  list(d = d, fs = fs, m = ranger(x = as.data.frame(d[, fs]), y = d$y, num.trees = n_trees, probability = TRUE,
                         case.weights = balanced_w(d$y), importance = "permutation", seed = seed))
})

# within-bird check: train and test on different time blocks of the same bird.
# If this also fails, the limit is the labels / kinematics, not differences
# between birds.
n_blocks <- 5
within_bird <- map_dfr(c(main_contrast, "Hover-feeding vs other flight"), function(cn) {
  d0 <- contrast_data(cn, 0.3); fs_w <- usable(d0, all_features)
  d0 <- d0 %>% drop_na(all_of(fs_w)) %>%
    group_by(bird) %>% filter(sum(y == "hover") >= min_pos_windows) %>% ungroup()
  if (!nrow(d0)) return(tibble(contrast = cn, note = str_glue("no bird with >= {min_pos_windows} positive windows")))
  d0 %>% group_by(bird) %>%
  group_modify(function(d, key) {
    d <- arrange(d, obs, t_video)   # an individual can span two trials
    blk <- cut(seq_len(nrow(d)), n_blocks, labels = FALSE)
    p <- rep(NA_real_, nrow(d))
    for (b in seq_len(n_blocks)) {
      tr <- blk != b
      if (n_distinct(d$y[tr]) < 2) next
      m <- ranger(x = as.data.frame(d[tr, fs_w]), y = d$y[tr], num.trees = n_trees, probability = TRUE,
                  case.weights = balanced_w(d$y[tr]), seed = seed)
      p[!tr] <- predict(m, as.data.frame(d[!tr, fs_w]))$predictions[, "hover"]
    }
    ok <- !is.na(p)
    tibble(n_hover = sum(d$y == "hover"), n_other = sum(d$y == "other"),
           auc_blocked_rf = auc(d$y[ok] == "hover", p[ok]),
           auc_static_g_only = auc(d$y == "hover", -abs(d$static_g - 1)))  # hovering: weight exactly supported
  }) %>% ungroup() %>% mutate(contrast = cn, .before = 1)
})

write_csv(within_bird, file.path(out_dir, "hover_rf_within_bird.csv"))
print(within_bird, n = Inf)
write_csv(win, file.path(out_dir, "hover_windows.csv"))
write_csv(rf_results, file.path(out_dir, "hover_rf_results.csv"))
write_csv(rf_per_bird, file.path(out_dir, "hover_rf_per_bird_auc.csv"))
write_csv(imp_cv, file.path(out_dir, "hover_rf_importance_cv.csv"))
print(count(win, window_s, class), n = Inf)
print(mutate(rf_results, across(where(is.double), ~ round(.x, 3))) %>%
        select(any_of(c("contrast", "window_s", "model", "features", "n_hover", "n_other", "n_birds_hover", "n_birds_auc",
                        "auc_within_bird", "auc_within_bird_sd", "auc_pooled", "sensitivity", "specificity",
                        "balanced_accuracy"))), n = Inf, width = Inf)

# ---- figures ----------------------------------------------------------------
ink <- "grey20"; ink_muted <- "grey45"
theme_set(theme_minimal(base_size = 10) +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
                  axis.text = element_text(colour = ink_muted), strip.text = element_text(face = "bold", hjust = 0),
                  plot.title = element_text(face = "bold", colour = ink), plot.subtitle = element_text(colour = ink_muted),
                  plot.title.position = "plot", legend.position = "bottom"))
group_cols <- c(amplitude = "#2a78d6", posture = "#eb6834", spectral = "#1baf7a", coordination = "#eda100")
win_lab <- function(ws) paste0(ws, " s windows (", if_else(ws < 1, "210 Hz", "105 Hz"), " burst length)")

# (a) AUC by feature set
model_levels <- rev(c("logistic - previous (logistic set)", paste("random forest -", names(feature_sets))))
add_label <- function(d) mutate(d, label = factor(paste(model, "-", features), levels = model_levels), window = win_lab(window_s))
p_auc <- ggplot(add_label(filter(rf_per_bird, contrast == main_contrast)), aes(auc, label)) +
  geom_vline(xintercept = 0.5, linetype = 2, colour = ink_muted) +
  geom_point(alpha = 0.35, size = 1.4, colour = ink_muted, position = position_jitter(height = 0.15, seed = 1)) +
  geom_point(data = add_label(filter(rf_results, contrast == main_contrast, model != "not tested")), aes(x = auc_within_bird),
             colour = "#2a78d6", size = 3.2, shape = 18) +
  facet_wrap(~ window) + coord_cartesian(xlim = c(0, 1)) +
  labs(x = "AUC within held-out bird (grey = each bird, blue = mean)", y = NULL,
       title = "Any hovering (incl. hover-feeding) vs other flight: leave-one-bird-out performance by feature set")

# (b) variable importance (all features)
imp_sum <- imp_cv %>% group_by(window_s, feature, group) %>%
  summarise(mean = mean(importance), sd = sd(importance), .groups = "drop") %>%
  group_by(window_s) %>% slice_max(mean, n = 20) %>% ungroup()
p_imp <- imp_sum %>%
  mutate(window = win_lab(window_s), feature = reorder(paste(feature, window_s, sep = "___"), mean)) %>%
  ggplot(aes(mean, feature, colour = group)) +
  geom_vline(xintercept = 0, colour = "grey80") +
  geom_errorbarh(aes(xmin = mean - sd, xmax = mean + sd), height = 0, linewidth = 0.5) +
  geom_point(size = 2.2) +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_colour_manual(values = group_cols, name = "Feature group") +
  facet_wrap(~ window, scales = "free_y") +
  labs(x = "Permutation importance (mean ± SD across leave-one-bird-out folds)", y = NULL,
       title = "Random forest variable importance, all features (top 20)")

# (c) ROC and precision-recall curves, pooled cross-validated predictions
roc_df <- function(y, p) {
  o <- order(-p); y <- y[o]
  tibble(fpr = c(0, cumsum(!y) / sum(!y)), tpr = c(0, cumsum(y) / sum(y)),
         precision = c(1, cumsum(y) / seq_along(y)), recall = c(0, cumsum(y) / sum(y)))
}
curves <- rf_preds %>% filter(contrast == main_contrast, features %in% c("previous (logistic set)", "all features", "orientation-invariant only")) %>%
  group_by(window_s, features) %>% reframe(roc_df(y == "hover", p_hover)) %>% mutate(window = win_lab(window_s))
base_rate <- rf_preds %>% filter(contrast == main_contrast, features == "all features") %>% group_by(window_s) %>%
  summarise(r = mean(y == "hover")) %>% mutate(window = win_lab(window_s))
set_cols <- c(`previous (logistic set)` = "grey55", `all features` = "#2a78d6", `orientation-invariant only` = "#eb6834")
p_roc <- ggplot(curves, aes(fpr, tpr, colour = features)) +
  geom_abline(linetype = 2, colour = ink_muted) + geom_path(linewidth = 0.6) +
  facet_wrap(~ window) + coord_equal() + scale_colour_manual(values = set_cols, name = "Random forest features") +
  labs(x = "False positive rate", y = "True positive rate", title = "ROC (pooled cross-validated predictions)")
p_pr <- ggplot(curves, aes(recall, precision, colour = features)) +
  geom_hline(data = base_rate, aes(yintercept = r), linetype = 2, colour = ink_muted) + geom_path(linewidth = 0.6) +
  facet_wrap(~ window) + coord_cartesian(ylim = c(0, 1)) + scale_colour_manual(values = set_cols, guide = "none") +
  labs(x = "Recall (hovering found)", y = "Precision", title = "Precision-recall (dashed = hovering base rate)")

# (d) cross-validated probability by true class
p_prob <- rf_preds %>% filter(contrast == main_contrast, features == "all features") %>%
  mutate(window = win_lab(window_s), truth = if_else(y == "hover", "Hovering (video)", "Other flight (video)")) %>%
  ggplot(aes(p_hover, fill = truth)) +
  geom_density(alpha = 0.6, colour = NA, adjust = 0.8) +
  geom_vline(xintercept = 0.5, linetype = 2, colour = ink_muted) +
  scale_fill_manual(values = c(`Hovering (video)` = "#1baf7a", `Other flight (video)` = "#2a78d6"), name = NULL) +
  facet_wrap(~ window) +
  labs(x = "Cross-validated P(hovering), all features", y = "Density", title = "Predicted probability by video label")

# (e) partial dependence of the top features (final model, 0.3 s windows)
pdp <- function(fit, d, f, fs, grid_n = 25) {
  g <- quantile(d[[f]], seq(0.05, 0.95, length.out = grid_n), na.rm = TRUE)
  map_dfr(unique(g), function(val) {
    dd <- as.data.frame(d[, fs]); dd[[f]] <- val
    tibble(feature = f, value = val, p_hover = mean(predict(fit, dd)$predictions[, "hover"]))
  })
}
top_f <- imp_sum %>% filter(window_s == 0.3) %>% slice_max(mean, n = 6) %>% pull(feature)
pd <- map_dfr(top_f, ~ pdp(final[["0.3"]]$m, final[["0.3"]]$d, .x, final[["0.3"]]$fs)) %>% mutate(feature = factor(feature, top_f))
rugs <- final[["0.3"]]$d %>% select(all_of(top_f), class) %>% pivot_longer(-class, names_to = "feature") %>%
  mutate(feature = factor(feature, top_f)) %>% group_by(feature) %>%
  filter(value >= quantile(value, 0.05, na.rm = TRUE), value <= quantile(value, 0.95, na.rm = TRUE)) %>% ungroup()
p_pdp <- ggplot(pd, aes(value, p_hover)) +
  geom_line(linewidth = 0.7, colour = ink) +
  geom_rug(data = filter(rugs, class != "Other flight"), aes(x = value), inherit.aes = FALSE, colour = "#1baf7a", alpha = 0.3, sides = "b") +
  facet_wrap(~ feature, scales = "free_x", nrow = 1) +
  labs(x = NULL, y = "Mean predicted P(hovering)",
       title = "Partial dependence of the six most important features (0.3 s windows, model on all birds)",
       subtitle = "Rug = hovering and hover-feeding windows")

# (f) the most important features by class, within-bird centred
p_feat <- win %>% filter(window_s == 0.3) %>%
  select(obs, class, all_of(top_f)) %>% pivot_longer(all_of(top_f), names_to = "feature") %>%
  group_by(obs, feature) %>% mutate(value_c = (value - median(value, na.rm = TRUE)) / IQR(value, na.rm = TRUE)) %>% ungroup() %>%
  mutate(feature = factor(feature, top_f)) %>%
  ggplot(aes(value_c, class, fill = class)) +
  geom_boxplot(outlier.shape = NA, width = 0.55, linewidth = 0.3) +
  scale_fill_manual(values = c(Hovering = "#1baf7a", `Hover-feeding` = "#008300", `Other flight` = "#2a78d6"), guide = "none") +
  facet_wrap(~ feature, nrow = 1, scales = "free_x") + coord_cartesian(xlim = c(-3, 3)) +
  labs(x = "Value centred and scaled within bird (median, IQR)", y = NULL,
       title = "Top features by video label (0.3 s windows)")

# (g) every contrast, all features
p_contrast <- rf_per_bird %>% filter(features == "all features") %>%
  mutate(window = win_lab(window_s), contrast = factor(contrast, rev(contrasts$contrast))) %>%
  ggplot(aes(auc, contrast)) +
  geom_vline(xintercept = 0.5, linetype = 2, colour = ink_muted) +
  geom_point(alpha = 0.4, size = 1.6, colour = ink_muted, position = position_jitter(height = 0.12, seed = 1)) +
  geom_point(data = rf_results %>% filter(features == "all features") %>%
               mutate(window = win_lab(window_s), contrast = factor(contrast, rev(contrasts$contrast))),
             aes(x = auc_within_bird), colour = "#2a78d6", size = 3.2, shape = 18) +
  facet_wrap(~ window) + coord_cartesian(xlim = c(0, 1)) +
  labs(x = "AUC within held-out bird (grey = each bird, blue = mean)", y = NULL,
       title = "Flight sub-behaviours from BORIS states (feeding split by modifier), random forest with all features",
       subtitle = "Hover-feeding occurs only in the S. cyanifrons (105 Hz) trials")

ggsave(file.path(fig_dir, "rf_contrasts.png"), p_contrast, width = 12, height = 3.8, dpi = 150, bg = "white")
ggsave(file.path(fig_dir, "rf_auc_by_featureset.png"), p_auc, width = 12, height = 4.5, dpi = 150, bg = "white")
ggsave(file.path(fig_dir, "rf_importance.png"), p_imp, width = 12, height = 6, dpi = 150, bg = "white")
ggsave(file.path(fig_dir, "rf_roc_pr.png"), (p_roc / p_pr) + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
       width = 10, height = 8.5, dpi = 150, bg = "white")
ggsave(file.path(fig_dir, "rf_probability.png"), p_prob, width = 10, height = 3.5, dpi = 150, bg = "white")
ggsave(file.path(fig_dir, "rf_partial_dependence.png"), p_pdp / p_feat, width = 14, height = 6.5, dpi = 150, bg = "white")
message("outputs written to ", out_dir)
