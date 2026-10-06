# Colombia 2025 captive hummingbird trials (Feb 2025, flight cage at the
# finca): validate the FleaTag flight classifiers used for the wild
# deployments (R/hummingbird_activity_budget.R) against BORIS video labels.
#
# Data: 21 continuous FleaTag recordings (210 Hz or 105 Hz "_CONT" modes,
# 42935 samples = memory full; trial 23 is 0.45 Hz without video) and a BORIS
# export (observations.csv, 119.88 fps video) with STATE events Flight / Pearch
# (alternating) plus Hovering, Feed (inside Flight) and Grooming (inside
# Pearch). Trials 21-22 (SC27_1, SC27_2) have no BORIS export yet.
#
# Alignment: video time v = a + k * t_tag, where t_tag = sample index /
# nominal rate. a and k are fitted per trial by cross-correlating ACC flight
# (flight_segment()) with BORIS Flight (+1) / Pearch (-1) over a grid of k.
# k gives the true sampling rate (nominal / k). The prior for a is the phone
# clock in the video: a = tag_start_delay_s + t_rel, t_rel = video time of tag
# activation = camera frame + (ACC start time - phone time). When the trial is
# (almost) all flight, k is not identifiable; then k is fixed at the median of
# the same tag's other trials (or all trials) and only a is fitted.
#
# Validation (within BORIS-annotated time only):
#   sample level   flight_segment() vs BORIS Flight (sensitivity, specificity)
#   bursts         wild burst schedules simulated by sliding 0.3 s and 1.5 s
#                  windows (every 0.5 s): classify_bursts() flight fraction vs
#                  BORIS, and within_burst() mean bout duration vs BORIS
#   0.45 Hz        the continuous data decimated to one sample per 2.22 s
#                  (several phases): classify_continuous() flight fraction and
#                  bout count vs BORIS. The real 0.45 Hz mode may filter
#                  differently from decimated 210 Hz data.
#   wingbeats      wingbeat frequency per species at the nominal and the
#                  fitted (true) sampling rate
#
# Outputs (CSV + PDF) go to out_dir, outside the repo.

library(tidyverse)
library(data.table)
library(readxl)
source("./R/flea_functions.R")  # read_flea_export(), flight_segment(), classify_bursts(), ...

# ---- paths & parameters -----------------------------------------------------
hb_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird"
trial_dir <- file.path(hb_dir, "202502_Fleatag_Hummingbirds_Trials")
meta_xlsx <- file.path(hb_dir, "HUMMINGBIRD VIDEOS.xlsx")
boris_csv <- file.path(hb_dir, "observations.csv")
out_dir <- file.path(hb_dir, "Results/captive_validation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# classifier settings: keep equal to R/hummingbird_activity_budget.R
burst_flight_g <- 1
burst_flight_g_lo <- 0.5
wbf_band <- c(15, 50)
cont_flight_g <- 0.5
seg_args <- list(win_s = 0.1, on_g = 1, off_g = 0.5, peak_g = 2, min_run_s = 0.1)

tag_start_delay_s <- 30            # tag starts logging 30 s after activation
grid_dt <- 0.05                    # s, resolution of the alignment cross-correlation
k_grid <- seq(0.85, 1.05, by = 0.0025)
lag_range <- c(-60, 120)           # s, allowed video time of the first ACC sample
min_transitions <- 3               # fewer BORIS flight/perch transitions: k not identifiable
min_accuracy_no_prior <- 0.95      # alignment check for trials without a phone-clock prior
max_prior_dev_s <- 10              # flag alignments further from the phone-clock prior
burst_lengths_s <- c(0.3, 1.5)     # wild 210 Hz and 105 Hz burst lengths
burst_step_s <- 0.5                # spacing of simulated bursts
cont_period_s <- 1 / 0.45          # 0.45 Hz mode sample interval
n_cont_phases <- 10
wbf_window_s <- 2

# alignment found by hand in R/align_boris_flea_shiny.R (acc_aligned.pdf):
# offset, sampling rate and t_rel per observation, last screenshot per trial
manual_align <- tribble(
  ~obs,      ~man_offset, ~man_sr, ~man_t_rel,
  "CB19_2",   30, 220,  1,
  "AN19_1",   30, 230,  1.5,
  "AN20_1",   30, 230,  3,
  "PG20_1",   30, 230,  3,
  "CCY20_1",  30, 230,  0.5,
  "CCO20_1",  30, 232,  0.5,
  "CB20_1",   30, 233,  1,
  "CB22_1",   30, 226,  3,
  "CB22_2",   30, 226,  1,
  "CCY22_1",  30, 226,  3,
  "CCY24_1",  30, 230,  0,
  "CB25_1",   30, 208,  2,
  "AN25_1",   30, 230,  0,
  "CB25_4",   30, 226,  0,
  "CB25_2",    0, 235,  0,
  "CB25_3",   18, 224,  0,
  "SC26_1",   17, 112,  0,
  "SC26_2",   30, 110,  0,
  "SC27_1",   30, 109, -1,
  "SC27_2",   30, 117, -1)

# ---- metadata & BORIS -------------------------------------------------------
# "hh:mm:ss.sss" or "hh:mm:ss:cc" (phone times and some camera frames) to s
clock_sec <- function(x) {
  map_dbl(x, function(s) {
    p <- strsplit(s %||% NA_character_, ":")[[1]]
    if (length(p) < 3 || any(is.na(suppressWarnings(as.numeric(p))))) return(NA_real_)
    v <- as.numeric(p[1]) * 3600 + as.numeric(p[2]) * 60 + as.numeric(p[3])
    if (length(p) == 4) v <- v + as.numeric(paste0("0.", p[4]))
    v
  })
}

meta <- read_excel(meta_xlsx, sheet = 1, .name_repair = "unique_quiet") %>%
  transmute(trial = as.integer(TRIAL), obs = `OBSERVATION ID`, species = str_replace(SP, "nigricolis", "nigricollis"),
            sheet_tag = toupper(as.character(`TAG ID`)) %>% str_remove("[.]0$"),
            t_rel = clock_sec(`CAMERA FRAME...16`) + clock_sec(`ACC START TIME`) - clock_sec(`PHONE TIME`),
            a_prior = tag_start_delay_s + t_rel)

boris <- fread(boris_csv) %>%
  as_tibble() %>%
  transmute(obs = `Observation id`, behavior = recode(Behavior, Pearch = "Perch"),
            start = `Start (s)`, stop = `Stop (s)`)

# BORIS state at video times v: top = Flight / Perch / NA (not annotated),
# sub = Hovering / Feed / Grooming / NA
boris_state <- function(v, bb) {
  top <- rep(NA_character_, length(v)); sub <- rep(NA_character_, length(v))
  for (b in c("Flight", "Perch")) {
    e <- filter(bb, behavior == b)
    for (i in seq_len(nrow(e))) top[v >= e$start[i] & v < e$stop[i]] <- b
  }
  for (b in c("Grooming", "Feed", "Hovering")) {
    e <- filter(bb, behavior == b)
    for (i in seq_len(nrow(e))) sub[v >= e$start[i] & v < e$stop[i]] <- b
  }
  list(top = top, sub = sub)
}

# ---- alignment --------------------------------------------------------------
# best (a, k) maximising sum(acc * boris) on a grid_dt grid; acc and boris
# coded +1 flight / -1 perched, boris 0 where not annotated
fit_alignment <- function(fly, sr, bb, ks = k_grid) {
  t_tag <- (seq_along(fly) - 1) / sr
  fa <- ifelse(is.na(fly), 0, ifelse(fly, 1, -1))
  vgrid <- seq(0, max(bb$stop) + lag_range[2], by = grid_dt)
  st <- boris_state(vgrid, bb)$top
  fb <- case_when(st == "Flight" ~ 1, st == "Perch" ~ -1, TRUE ~ 0)
  best <- list(score = -Inf)
  for (k in ks) {
    tg <- seq(0, max(t_tag) * k, by = grid_dt)
    fa_g <- approx(t_tag * k, fa, tg, method = "constant", rule = 2)$y
    nn <- 2^ceiling(log2(length(fb) + length(fa_g)))
    cc <- Re(fft(fft(c(fb, rep(0, nn - length(fb)))) * Conj(fft(c(fa_g, rep(0, nn - length(fa_g))))),
                 inverse = TRUE)) / nn
    lags <- c(0:(nn / 2), -((nn / 2 - 1):1)) * grid_dt
    ok <- which(lags > lag_range[1] & lags < lag_range[2])
    j <- ok[which.max(cc[ok])]
    if (cc[j] > best$score) best <- list(score = cc[j], a = lags[j], k = k)
  }
  best
}

files <- list.files(trial_dir, pattern = "Trial0[0-9]{2}[.]txt$", full.names = TRUE, recursive = TRUE)
trials <- tibble(path = files, trial = as.integer(str_match(basename(files), "Trial0*([0-9]+)[.]txt$")[, 2])) %>%
  left_join(meta, by = "trial") %>%
  mutate(has_boris = obs %in% boris$obs)

acc <- list()   # per trial: data, rate, segmentation
fits <- list()
for (i in which(trials$has_boris)) {
  tr <- trials[i, ]
  message("aligning trial ", tr$trial, " (", tr$obs, ")")
  r <- read_flea_export(tr$path)
  seg <- do.call(flight_segment, c(list(r$data$x, r$data$y, r$data$z, r$hz), seg_args))
  bb <- filter(boris, obs == tr$obs)
  f <- fit_alignment(seg$fly, r$hz, bb)
  acc[[tr$obs]] <- list(d = r$data, sr = r$hz, tag = r$tag, dyn = seg$dyn, fly = seg$fly, bb = bb)
  v <- f$a + f$k * (seq_along(seg$fly) - 1) / r$hz
  top <- boris_state(v, bb)$top
  fits[[tr$obs]] <- tibble(trial = tr$trial, obs = tr$obs, species = tr$species, tag = r$tag, sr_nominal = r$hz,
                           a_free = f$a, k_free = f$k, perch_frac = mean(top == "Perch", na.rm = TRUE),
                           n_transitions = sum(diff(top[!is.na(top)] == "Flight") != 0),
                           a_prior = tr$a_prior)
}
align <- bind_rows(fits) %>%
  mutate(k_identifiable = n_transitions >= min_transitions & k_free > min(k_grid) & k_free < max(k_grid))

# trials without enough perching: k from the same tag's identifiable trials
k_tag <- align %>% filter(k_identifiable) %>% group_by(tag, sr_nominal) %>% summarise(k_tag = median(k_free), .groups = "drop")
k_all <- align %>% filter(k_identifiable) %>% group_by(sr_nominal) %>% summarise(k_all = median(k_free), .groups = "drop")
align <- align %>%
  left_join(k_tag, by = c("tag", "sr_nominal")) %>% left_join(k_all, by = "sr_nominal") %>%
  mutate(k_source = case_when(k_identifiable ~ "fitted", !is.na(k_tag) ~ "same tag, other trials", TRUE ~ "all trials"),
         k = case_when(k_identifiable ~ k_free, !is.na(k_tag) ~ k_tag, TRUE ~ k_all))
for (j in which(!align$k_identifiable)) {
  o <- align$obs[j]
  align$a_free[j] <- fit_alignment(acc[[o]]$fly, acc[[o]]$sr, acc[[o]]$bb, ks = align$k[j])$a
}
align <- align %>%
  rename(a = a_free) %>%
  mutate(sr_true = sr_nominal / k, rate_error_pct = 100 * (sr_true / sr_nominal - 1),
         prior_dev_s = a - a_prior,
         prior_ok = !is.na(prior_dev_s) & abs(prior_dev_s) <= max_prior_dev_s) %>%
  left_join(manual_align, by = "obs") %>%
  mutate(man_a = man_offset + man_t_rel)

# ---- sample-level validation ------------------------------------------------
samples <- map_dfr(align$obs, function(o) {
  x <- acc[[o]]; al <- filter(align, obs == o)
  v <- al$a + al$k * (seq_along(x$fly) - 1) / x$sr
  st <- boris_state(v, x$bb)
  tibble(obs = o, v = v, dyn = x$dyn, acc_fly = x$fly, top = st$top, sub = st$sub)
}) %>% filter(!is.na(top), !is.na(acc_fly))

sample_val <- samples %>%
  group_by(obs) %>%
  summarise(n_s = n(), boris_flight_pct = 100 * mean(top == "Flight"), acc_flight_pct = 100 * mean(acc_fly),
            sensitivity = mean(acc_fly[top == "Flight"]), specificity = mean(!acc_fly[top == "Perch"]),
            accuracy = mean(acc_fly == (top == "Flight")))

# alignment check: phone-clock prior if available, otherwise agreement with video
align <- align %>%
  left_join(select(sample_val, obs, accuracy), by = "obs") %>%
  mutate(align_ok = if_else(is.na(prior_dev_s), accuracy >= min_accuracy_no_prior, prior_ok)) %>%
  select(-accuracy)

dyn_by_behavior <- samples %>%
  mutate(state = coalesce(sub, top)) %>%
  group_by(state) %>%
  summarise(n_samples = n(), median_dyn_g = median(dyn), q10_dyn = quantile(dyn, 0.1), q90_dyn = quantile(dyn, 0.9),
            pct_classified_flight = 100 * mean(acc_fly))

# BORIS bouts vs. continuous ACC segmentation, same estimator as the wild
# within-burst one: mean bout = 2 * flight time / (take-offs + landings)
mean_bout_est <- function(fly, dt) {
  sw <- sum(abs(diff(fly)))
  if (sw == 0) NA_real_ else 2 * sum(fly) * dt / sw
}
bout_val <- samples %>%
  left_join(select(align, obs, k, sr_nominal), by = "obs") %>%
  group_by(obs) %>%
  summarise(dt = first(k / sr_nominal),
            boris_mean_bout_s = mean_bout_est(top == "Flight", dt),
            acc_mean_bout_s = mean_bout_est(acc_fly, dt),
            boris_n_bouts = sum(diff(top == "Flight") == 1), acc_n_bouts = sum(diff(acc_fly) == 1)) %>%
  select(-dt)

# ---- simulated bursts -------------------------------------------------------
sim_bursts <- map_dfr(align$obs, function(o) {
  x <- acc[[o]]; al <- filter(align, obs == o)
  sr_true <- al$sr_true
  map_dfr(burst_lengths_s, function(bl) {
    n <- round(bl * sr_true)
    starts <- seq(1, length(x$fly) - n, by = round(burst_step_s * sr_true))
    idx <- unlist(map(starts, ~ .x:(.x + n - 1)))
    d <- x$d[idx][, burstCount := rep(seq_along(starts), each = n)]
    cb <- classify_bursts(d, x$sr, burst_flight_g, burst_flight_g_lo, wbf_band)
    wb <- do.call(within_burst, c(list(d, x$sr), seg_args))
    v <- al$a + al$k * (idx - 1) / x$sr
    truth <- tibble(burstCount = d$burstCount, top = boris_state(v, x$bb)$top) %>%
      group_by(burstCount) %>%
      summarise(annotated = all(!is.na(top)), boris_fly_frac = mean(top == "Flight"),
                boris_sw = sum(abs(diff(top == "Flight"))))
    as_tibble(cb) %>% select(burstCount, dyn_sd, wbf_hz, fly, fly_lo) %>%
      left_join(as_tibble(wb), by = "burstCount") %>% left_join(truth, by = "burstCount") %>%
      filter(annotated) %>% mutate(obs = o, burst_s = bl)
  })
})

burst_val <- sim_bursts %>%
  group_by(obs, burst_s) %>%
  summarise(n_bursts = n(), boris_flight_pct = 100 * mean(boris_fly_frac),
            burst_flight_pct = 100 * mean(fly), burst_flight_pct_lo = 100 * mean(fly_lo),
            within_flight_pct = 100 * sum(t_fly_s) / sum(t_obs_s),
            burst_agree = mean(fly == (boris_fly_frac > 0.5)), .groups = "drop")

burst_bout_val <- sim_bursts %>%
  group_by(burst_s) %>%
  summarise(n_bursts = n(), n_transitions = sum(n_on + n_off),
            within_mean_bout_s = 2 * sum(t_fly_s) / sum(n_on + n_off),
            boris_mean_bout_s = 2 * sum(boris_fly_frac * t_obs_s) / sum(boris_sw))

# ---- simulated 0.45 Hz ------------------------------------------------------
sim_cont <- map_dfr(align$obs, function(o) {
  x <- acc[[o]]; al <- filter(align, obs == o)
  step <- cont_period_s * al$sr_true
  map_dfr(seq_len(n_cont_phases) - 1, function(ph) {
    idx <- round(seq(1 + ph * step / n_cont_phases, nrow(x$d), by = step))
    d <- x$d[idx][, `:=`(burstCount = 0L, timeMilliseconds = (seq_along(idx) - 1) * cont_period_s * 1000)]
    cc <- classify_continuous(d, cont_flight_g)
    v <- al$a + al$k * (idx - 1) / x$sr
    tibble(obs = o, phase = ph, fly = cc$fly, top = boris_state(v, x$bb)$top)
  })
}) %>% filter(!is.na(top))

cont_val <- sim_cont %>%
  group_by(obs, phase) %>%
  summarise(boris_flight_pct = 100 * mean(top == "Flight"), cont_flight_pct = 100 * mean(fly),
            sensitivity = mean(fly[top == "Flight"]), specificity = mean(!fly[top == "Perch"]),
            boris_n_bouts = sum(diff(top == "Flight") == 1) + (first(top) == "Flight"),
            cont_n_bouts = sum(diff(fly) == 1) + first(fly), .groups = "drop") %>%
  group_by(obs) %>%
  summarise(across(-phase, mean))

# ---- wingbeat frequency -----------------------------------------------------
wbf <- map_dfr(align$obs, function(o) {
  x <- acc[[o]]; al <- filter(align, obs == o)
  n <- round(wbf_window_s * x$sr)
  starts <- seq(1, nrow(x$d) - n, by = n)
  map_dfr(starts, function(s) {
    idx <- s:(s + n - 1)
    v <- al$a + al$k * (idx - 1) / x$sr
    st <- boris_state(v, x$bb)
    if (!all(st$top == "Flight", na.rm = FALSE) || any(is.na(st$top))) return(NULL)
    a <- cbind(x$d$x[idx], x$d$y[idx], x$d$z[idx])
    tibble(obs = o,
           wbf_nominal = wingbeat_freq(a, x$sr, wbf_band))
  })
}) %>%
  left_join(select(align, obs, species, tag, sr_true, sr_nominal, align_ok), by = "obs") %>%
  mutate(wbf_true = wbf_nominal * sr_true / sr_nominal)

wbf_species <- wbf %>% filter(align_ok) %>%
  group_by(species) %>%
  summarise(n_birds = n_distinct(obs), n_windows = n(), wbf_nominal_hz = median(wbf_nominal),
            wbf_true_hz = median(wbf_true))

# ---- outputs ----------------------------------------------------------------
validation <- align %>%
  select(trial, obs, species, tag, sr_nominal, k_source, sr_true, rate_error_pct, a, a_prior, prior_dev_s,
         align_ok, man_sr, man_a) %>%
  left_join(sample_val, by = "obs") %>%
  left_join(bout_val, by = "obs") %>%
  left_join(rename_with(cont_val, ~ paste0("cont_", .x), -obs), by = "obs") %>%
  arrange(trial)

write_csv(validation, file.path(out_dir, "trial_validation.csv"))
write_csv(burst_val, file.path(out_dir, "burst_validation.csv"))
write_csv(burst_bout_val, file.path(out_dir, "burst_bout_validation.csv"))
write_csv(dyn_by_behavior, file.path(out_dir, "dyn_by_behavior.csv"))
write_csv(wbf_species, file.path(out_dir, "wingbeat_frequency_species.csv"))

r2 <- function(x) round(x, 2)
validation %>%
  transmute(trial, obs, sp = word(species, 1), tag, k_source, sr_true = round(sr_true, 1), man_sr,
            prior_dev_s = round(prior_dev_s, 1), align_ok, boris_fl = round(boris_flight_pct), acc_fl = round(acc_flight_pct),
            sens = r2(sensitivity), spec = r2(specificity), boris_bout = round(boris_mean_bout_s, 1),
            acc_bout = round(acc_mean_bout_s, 1), cont_fl = round(cont_cont_flight_pct), cont_bouts = round(cont_cont_n_bouts, 1),
            boris_bouts = boris_n_bouts) %>%
  print(n = Inf, width = Inf)
print(mutate(dyn_by_behavior, across(where(is.double), r2)))
print(burst_val %>% filter(obs %in% validation$obs[validation$align_ok]) %>% group_by(burst_s) %>%
        summarise(r_flight = cor(boris_flight_pct, burst_flight_pct),
                  mean_abs_err_pct = mean(abs(burst_flight_pct - boris_flight_pct)),
                  mean_err_pct = mean(burst_flight_pct - boris_flight_pct),
                  burst_agree = mean(burst_agree)))
print(mutate(burst_bout_val, across(where(is.double), r2)))
print(mutate(wbf_species, across(where(is.double), ~ round(.x, 1))))
print(align %>% filter(k_identifiable) %>% group_by(sr_nominal) %>%
        summarise(n = n(), sr_true_median = median(sr_true), sr_true_min = min(sr_true), sr_true_max = max(sr_true)))

# ---- figures ----------------------------------------------------------------
col_fly <- "#2a78d6"

p_timeline <- samples %>%
  group_by(obs, t = round(v, 1)) %>%
  summarise(dyn = max(dyn), boris_flight = first(top) == "Flight", .groups = "drop") %>%
  ggplot(aes(t)) +
  geom_rect(data = ~ filter(.x, boris_flight), aes(xmin = t - 0.05, xmax = t + 0.05, ymin = -Inf, ymax = Inf),
            fill = "#cde2fb") +
  geom_line(aes(y = pmin(dyn, 6)), linewidth = 0.2) +
  geom_hline(yintercept = seg_args$on_g, linetype = 2, colour = "grey40") +
  facet_wrap(~ obs, ncol = 3, scales = "free_x") +
  labs(x = "Video time (s)", y = "Dynamic SD, 0.1 s (g, capped at 6)",
       title = "Aligned ACC vs BORIS Flight (shaded)") +
  theme_bw()

p_rate <- align %>% filter(k_identifiable) %>%
  ggplot(aes(sr_true, fct_reorder(paste(obs, tag), sr_true))) +
  geom_vline(aes(xintercept = sr_nominal), linetype = 2) +
  geom_point(colour = col_fly, size = 3) +
  geom_point(aes(x = man_sr), shape = 4, size = 3, na.rm = TRUE) +
  facet_wrap(~ paste(sr_nominal, "Hz nominal"), scales = "free_x") +
  labs(x = "Sampling rate (Hz)", y = NULL,
       title = "Fitted true sampling rate (dot) vs nominal (dashed) and manual alignment (x)") +
  theme_bw()

p_dyn <- samples %>%
  mutate(state = coalesce(sub, top)) %>%
  ggplot(aes(pmax(dyn, 0.01), fill = state)) +
  geom_histogram(bins = 80) +
  geom_vline(xintercept = seg_args$on_g) + geom_vline(xintercept = seg_args$off_g, linetype = 2) +
  scale_x_log10() + facet_wrap(~ state, ncol = 1, scales = "free_y") +
  labs(x = "Dynamic SD, 0.1 s window (g, log)", y = "Samples", title = "ACC by BORIS behaviour") +
  theme_bw() + theme(legend.position = "none")

p_burst <- burst_val %>% filter(obs %in% validation$obs[validation$align_ok]) %>%
  ggplot(aes(boris_flight_pct, burst_flight_pct)) +
  geom_abline(linetype = 2) + geom_point(colour = col_fly, size = 2.5) +
  facet_wrap(~ paste(burst_s, "s bursts")) + coord_equal() +
  labs(x = "BORIS % time in flight", y = "Burst classifier % bursts in flight",
       title = "Simulated wild bursts vs video") + theme_bw()

p_cont <- validation %>% filter(align_ok) %>%
  ggplot(aes(boris_flight_pct, cont_cont_flight_pct)) +
  geom_abline(linetype = 2) + geom_point(colour = col_fly, size = 2.5) + coord_equal() +
  labs(x = "BORIS % time in flight", y = "0.45 Hz classifier % samples in flight",
       title = "Simulated 0.45 Hz sampling vs video") + theme_bw()

pdf(file.path(out_dir, "captive_validation.pdf"), width = 12, height = 9)
print(p_timeline); print(p_rate); print(p_dyn); print(p_burst); print(p_cont)
dev.off()
ggsave(file.path(out_dir, "timeline.png"), p_timeline, width = 13, height = 10, dpi = 100)
ggsave(file.path(out_dir, "sampling_rate.png"), p_rate, width = 10, height = 5, dpi = 110)
ggsave(file.path(out_dir, "dyn_by_behavior.png"), p_dyn, width = 8, height = 8, dpi = 110)
ggsave(file.path(out_dir, "burst_validation.png"), p_burst, width = 9, height = 5, dpi = 110)
ggsave(file.path(out_dir, "cont_validation.png"), p_cont, width = 6, height = 5, dpi = 110)
message("outputs written to ", out_dir)
