# Is wild flight more intense than captive flight? Compares acceleration
# metrics of wild flight bursts (R/hummingbird_activity_budget.R) with
# captive flight cut into windows of the same length and sampling (captive
# birds try to escape, wild birds forage, chase and get chased).
#
# Matching: wild 210 Hz bursts are 62 samples, wild 105 Hz bursts 155
# samples. Captive 210 Hz recordings are cut into 62-sample windows and, after
# keeping every 2nd sample (~105 Hz), into 155-sample windows; captive 105 Hz
# recordings give 155-sample windows directly. Only windows/bursts that are
# flight throughout (flight_segment()) are used, so take-offs and landings do
# not mix in. Captive flight is taken from the ACC (validated against video),
# so trials without BORIS labels are included. Frequencies and jerk use the
# true sampling rate (captive: fitted per tag; wild: nominal x the captive
# median ratio, the wild tags' own rates are unknown); amplitudes do not
# depend on it.
#
# Metrics from burst_features(): dynamic SD, VeDBA, peak |a|, fraction of
# samples at the +-8 g limit, static |a| (differs from 1 g under sustained
# acceleration or turning), drift of the static vector within the burst,
# cycle-to-cycle VeDBA variability, wingbeat frequency.
#
# "High-intensity" wild bursts = above the 99th percentile of captive flight
# (same window type). There are no chase labels; these bursts are candidates.
#
# Outputs (CSV + PNG) go to Hummingbird/Results/flight_intensity on Dropbox.

library(tidyverse)
library(data.table)
library(patchwork)
source("./R/flea_functions.R")  # read_flea_export(), flight_segment(), burst_features()

# ---- paths & parameters -----------------------------------------------------
hb <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird"
cap_rds <- file.path(hb, "Results/captive_validation/captive_validation.rds")
wild_data_dir <- file.path(hb, "WIN 2025/Deployment recap data")
wild_bursts_csv <- file.path(hb, "WIN 2025/Results/activity_budget/burst_classification.csv")
wild_budget_csv <- file.path(hb, "WIN 2025/Results/activity_budget/deployment_budget.csv")
out_dir <- file.path(hb, "Results/flight_intensity")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

window_types <- tibble(window = c("0.3 s (210 Hz bursts)", "1.5 s (105 Hz bursts)"),
                       n_samples = c(62, 155), sr_nominal = c(210, 105))
high_q <- 0.99            # captive quantile defining high-intensity wild bursts
metrics <- c(dyn_sd = "Dynamic SD (g)", vedba = "VeDBA (g)", peak_g = "Peak |a| (g)",
             clip_frac = "Fraction of samples at 8 g limit", static_dev = "|static |a| - 1| (g)",
             drift_deg = "Posture drift within burst (deg)", vedba_cv = "Cycle-to-cycle VeDBA CV",
             wbf_hz = "Wingbeat frequency (Hz)")

cap <- readRDS(cap_rds)
rate_ratio <- cap$align %>% filter(k_source == "fitted") %>% group_by(sr_nominal) %>%
  summarise(ratio = median(sr_true / sr_nominal))

# windows of n samples that are flight throughout
flight_windows <- function(acc, fly, n, sr_true) {
  starts <- seq(1, nrow(acc) - n + 1, by = n)
  starts <- starts[map_lgl(starts, ~ all(fly[.x:(.x + n - 1)] %in% TRUE))]
  map_dfr(starts, ~ burst_features(acc[.x:(.x + n - 1), ], sr_true) %>% mutate(start = .x))
}

# ---- captive flight windows -------------------------------------------------
cap_rates <- cap$align %>% select(obs, tag, sr_true)
cap_trials <- cap$trials %>% filter(!is.na(species)) %>%
  left_join(cap_rates, by = "obs")
captive <- pmap_dfr(cap_trials, function(path, trial, obs, species, ...) {
  r <- read_flea_export(path)
  if (r$hz < 100) return(NULL)                            # 0.45 Hz trial
  sr_true <- coalesce(cap_trials$sr_true[cap_trials$path == path],
                      r$hz * rate_ratio$ratio[rate_ratio$sr_nominal == r$hz])
  message("captive trial ", trial)
  acc <- cbind(r$data$x, r$data$y, r$data$z)
  fly <- flight_segment(acc[, 1], acc[, 2], acc[, 3], r$hz)$fly
  out <- list()
  if (r$hz == 210) {
    out[[1]] <- flight_windows(acc, fly, 62, sr_true) %>% mutate(window = window_types$window[1])
    keep <- seq(1, nrow(acc), by = 2)                      # ~105 Hz
    out[[2]] <- flight_windows(acc[keep, ], fly[keep], 155, sr_true / 2) %>% mutate(window = window_types$window[2])
  } else {
    out[[1]] <- flight_windows(acc, fly, 155, sr_true) %>% mutate(window = window_types$window[2])
  }
  bind_rows(out) %>% mutate(setting = "captive", bird = coalesce(obs, paste0("trial", trial)), species = species)
})

# ---- wild flight bursts -----------------------------------------------------
wb <- read_csv(wild_bursts_csv, show_col_types = FALSE) %>% filter(free, day, fly)
species_wild <- read_csv(wild_budget_csv, show_col_types = FALSE) %>% select(id, species)
wild_files <- list.files(wild_data_dir, pattern = "[.]txt$", full.names = TRUE, recursive = TRUE)
wild <- map_dfr(unique(wb$id), function(i) {
  message("wild ", i)
  r <- read_flea_export(wild_files[str_detect(basename(wild_files), i)])
  sr_true <- r$hz * rate_ratio$ratio[rate_ratio$sr_nominal == r$hz]
  ids <- wb %>% filter(id == i)
  map_dfr(seq_len(nrow(ids)), function(j) {
    d <- r$data[burstCount == ids$burstCount[j]]
    fly <- flight_segment(d$x, d$y, d$z, r$hz)$fly
    if (!all(fly %in% TRUE | is.na(fly)) || !any(fly %in% TRUE)) return(NULL)  # flight throughout (edges NA)
    burst_features(cbind(d$x, d$y, d$z), sr_true) %>%
      mutate(burstCount = ids$burstCount[j], clock = ids$clock[j])
  }) %>% mutate(bird = i, window = window_types$window[match(r$hz, window_types$sr_nominal)])
}) %>% left_join(species_wild, by = c("bird" = "id")) %>% mutate(setting = "wild")

both <- bind_rows(captive, wild) %>% as_tibble() %>%
  mutate(static_dev = abs(static_g - 1),
         species = str_replace(species, "nigricolis", "nigricollis"))
write_csv(both, file.path(out_dir, "flight_windows.csv"))

# ---- comparisons ------------------------------------------------------------
bird_med <- both %>% group_by(setting, window, species, bird) %>%
  summarise(n = n(), across(all_of(names(metrics)), median), .groups = "drop")
write_csv(bird_med, file.path(out_dir, "bird_medians.csv"))

compare <- bird_med %>%
  pivot_longer(all_of(names(metrics)), names_to = "metric") %>%
  group_by(window, metric) %>%
  summarise(captive_median = median(value[setting == "captive"]), wild_median = median(value[setting == "wild"]),
            n_captive_birds = sum(setting == "captive"), n_wild_birds = sum(setting == "wild"),
            p_wilcox = tryCatch(wilcox.test(value[setting == "wild"], value[setting == "captive"])$p.value, error = function(e) NA),
            .groups = "drop") %>%
  mutate(ratio_wild_captive = wild_median / captive_median)
# same species only (A. nigricollis, C. coruscans)
shared_sp <- intersect(unique(captive$species), unique(wild$species))
compare_sp <- bird_med %>% filter(species %in% shared_sp) %>%
  pivot_longer(all_of(names(metrics)), names_to = "metric") %>%
  group_by(window, species, metric) %>%
  summarise(captive_median = median(value[setting == "captive"]), wild_median = median(value[setting == "wild"]),
            n_captive_birds = sum(setting == "captive"), n_wild_birds = sum(setting == "wild"), .groups = "drop")

thresholds <- captive %>% group_by(window) %>%
  summarise(dyn_sd_q = quantile(dyn_sd, high_q), peak_g_q = quantile(peak_g, high_q),
            static_dev_q = quantile(abs(static_g - 1), high_q), .groups = "drop")
high <- wild %>% left_join(thresholds, by = "window") %>%
  mutate(high_dyn = dyn_sd > dyn_sd_q, high_peak = peak_g > peak_g_q,
         high_static = abs(static_g - 1) > static_dev_q, clipped = clip_frac > 0)
high_sum <- high %>% group_by(window) %>%
  summarise(n_bursts = n(), n_birds = n_distinct(bird), pct_above_captive_q_dyn = 100 * mean(high_dyn),
            pct_above_captive_q_peak = 100 * mean(high_peak), pct_above_captive_q_static = 100 * mean(high_static),
            n_birds_high_static = n_distinct(bird[high_static]), pct_any_clipping = 100 * mean(clipped), .groups = "drop") %>%
  left_join(captive %>% group_by(window) %>% summarise(captive_pct_any_clipping = 100 * mean(clip_frac > 0)), by = "window")

write_csv(compare, file.path(out_dir, "wild_vs_captive.csv"))
write_csv(compare_sp, file.path(out_dir, "wild_vs_captive_species.csv"))
write_csv(high_sum, file.path(out_dir, "high_intensity_bursts.csv"))
print(both %>% count(setting, window, species), n = Inf)
print(mutate(compare, across(where(is.double), ~ signif(.x, 3))), n = Inf, width = Inf)
print(mutate(compare_sp, across(where(is.double), ~ signif(.x, 3))) %>% filter(metric %in% c("dyn_sd", "peak_g", "static_dev", "wbf_hz")), n = Inf, width = Inf)
print(mutate(high_sum, across(where(is.double), ~ round(.x, 1))), width = Inf)

# ---- figures ----------------------------------------------------------------
ink_muted <- "grey45"
theme_set(theme_minimal(base_size = 10) +
            theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", hjust = 0),
                  plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = ink_muted),
                  plot.title.position = "plot", legend.position = "bottom"))
set_cols <- c(captive = "grey45", wild = "#2a78d6")

p_dist <- both %>%
  select(setting, window, dyn_sd, peak_g, static_dev, wbf_hz) %>%
  pivot_longer(c(dyn_sd, peak_g, static_dev, wbf_hz), names_to = "metric") %>%
  mutate(metric = factor(metrics[metric], metrics)) %>%
  ggplot(aes(value, colour = setting)) +
  stat_ecdf(linewidth = 0.7) +
  facet_grid(window ~ metric, scales = "free_x") +
  scale_colour_manual(values = set_cols, name = NULL) +
  labs(x = NULL, y = "Cumulative fraction of flight windows",
       title = "Wild vs captive flight: acceleration distributions",
       subtitle = "Flight-only windows matched to wild burst length and sampling")

p_birds <- bird_med %>%
  select(setting, window, species, bird, dyn_sd, peak_g, static_dev, wbf_hz) %>%
  pivot_longer(c(dyn_sd, peak_g, static_dev, wbf_hz), names_to = "metric") %>%
  mutate(metric = factor(metrics[metric], metrics),
         species = str_replace(species, "^(\\w)\\w+ ", "\\1. ")) %>%
  ggplot(aes(value, species, colour = setting)) +
  geom_point(size = 2.2, alpha = 0.85, position = position_dodge(width = 0.5)) +
  facet_grid(window ~ metric, scales = "free_x") +
  scale_colour_manual(values = set_cols, name = NULL) +
  labs(x = "Per-bird median", y = NULL, title = "Per-bird medians by species")

p_high <- high %>%
  ggplot(aes(clock, abs(static_g - 1))) +
  geom_hline(data = thresholds, aes(yintercept = static_dev_q), linetype = 2, colour = ink_muted) +
  geom_point(aes(colour = high_static, shape = species), size = 1.8, alpha = 0.8) +
  scale_colour_manual(values = c(`TRUE` = "#eb6834", `FALSE` = "#2a78d6"),
                      labels = c(`TRUE` = "above captive 99th percentile", `FALSE` = "within captive range"), name = NULL) +
  scale_shape_manual(values = c(16, 2), name = NULL) +
  facet_wrap(~ window) +
  labs(x = "Hour of day (UTC-5, clock-corrected)", y = "|static |a| - 1| (g)", title = "Sustained acceleration in wild flight bursts",
       subtitle = "Mean acceleration over the burst departs from 1 g when the bird speeds up, brakes or turns. Dashed = 99th percentile of captive flight")

ggsave(file.path(out_dir, "intensity_distributions.png"), p_dist, width = 13, height = 6, dpi = 150, bg = "white")
ggsave(file.path(out_dir, "intensity_birds.png"), p_birds, width = 13, height = 5.5, dpi = 150, bg = "white")
ggsave(file.path(out_dir, "intensity_by_hour.png"), p_high, width = 11, height = 4, dpi = 150, bg = "white")
message("outputs written to ", out_dir)
