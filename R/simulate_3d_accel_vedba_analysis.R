
# Simulate continuous 3D acceleration of a flying animal and examine how
# sampling rate affects VeDBA estimation.
#
# Z is the dominant axis of movement.
#
# Output:
#   1) Example raw acceleration time series
#   2) VeDBA time series at selected sampling rates
#   3) Summary metrics vs sampling rate
#
# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

set.seed(42)

# -----------------------------
# Helper functions
# -----------------------------

simulate_flight_accel <- function(duration = 60, fs = 400) {
  t <- seq(0, duration, by = 1 / fs)
  n <- length(t)

  # Smooth low-frequency posture / gravity-like background
  static_x <- 0.08 * sin(2 * pi * 0.08 * t + 0.3)
  static_y <- 0.06 * sin(2 * pi * 0.05 * t + 1.2)
  static_z <- 1.00 + 0.12 * sin(2 * pi * 0.07 * t - 0.6)

  # Wingbeat frequency varies smoothly through time
  wing_freq <- 7 + 1.2 * sin(2 * pi * 0.03 * t) + 0.5 * sin(2 * pi * 0.015 * t + 0.4)

  # Integrate instantaneous frequency -> continuous phase
  phase <- cumsum(2 * pi * wing_freq / fs)

  # Amplitude envelope varies smoothly
  amp_env <- 0.25 + 0.07 * sin(2 * pi * 0.05 * t + 0.7) + 0.03 * sin(2 * pi * 0.11 * t)

  # Dynamic acceleration: Z dominant, X and Y weaker and phase shifted
  dyn_x <- 0.35 * amp_env * sin(phase + 0.2) + 0.03 * sin(2 * phase)
  dyn_y <- 0.22 * amp_env * sin(phase + 1.1) + 0.02 * cos(2 * phase + 0.7)
  dyn_z <- 1.10 * amp_env * sin(phase - 0.4) + 0.10 * sin(2 * phase - 0.8)

  # Smooth correlated sensor / body jitter
  eps_x <- as.numeric(arima.sim(model = list(ar = 0.98), n = n, sd = 0.01))
  eps_y <- as.numeric(arima.sim(model = list(ar = 0.98), n = n, sd = 0.01))
  eps_z <- as.numeric(arima.sim(model = list(ar = 0.98), n = n, sd = 0.015))

  tibble(
    t = t,
    x = static_x + dyn_x + eps_x,
    y = static_y + dyn_y + eps_y,
    z = static_z + dyn_z + eps_z
  )
}

calc_vedba <- function(df, fs, static_window_s = 2) {
  k <- max(3, round(static_window_s * fs))
  if (k %% 2 == 0) k <- k + 1

  # Centered moving average for static acceleration
  filt <- rep(1 / k, k)
  sx <- as.numeric(stats::filter(df$x, filt, sides = 2))
  sy <- as.numeric(stats::filter(df$y, filt, sides = 2))
  sz <- as.numeric(stats::filter(df$z, filt, sides = 2))

  out <- df %>%
    mutate(
      static_x = sx,
      static_y = sy,
      static_z = sz,
      dba_x = x - static_x,
      dba_y = y - static_y,
      dba_z = z - static_z,
      VeDBA = sqrt(dba_x^2 + dba_y^2 + dba_z^2)
    )

  out
}

downsample_df <- function(df, fs_from, fs_to) {
  step <- round(fs_from / fs_to)
  idx <- seq(1, nrow(df), by = step)
  df[idx, , drop = FALSE]
}

compare_rates <- function(full_df, fs_full = 400,
                          rates = c(400, 200, 100, 50, 25, 10, 5),
                          static_window_s = 2) {

  ref <- calc_vedba(full_df, fs = fs_full, static_window_s = static_window_s) %>%
    filter(!is.na(VeDBA))

  res_list <- list()
  ts_list <- list()

  for (r in rates) {
    ds <- if (r == fs_full) full_df else downsample_df(full_df, fs_full, r)
    est <- calc_vedba(ds, fs = r, static_window_s = static_window_s) %>%
      filter(!is.na(VeDBA))

    # Interpolate reference VeDBA onto sampled timestamps
    ref_interp <- approx(x = ref$t, y = ref$VeDBA, xout = est$t, rule = 2)$y

    summ <- tibble(
      rate_hz = r,
      n = nrow(est),
      mean_VeDBA = mean(est$VeDBA, na.rm = TRUE),
      sd_VeDBA = sd(est$VeDBA, na.rm = TRUE),
      bias_mean = mean(est$VeDBA - ref_interp, na.rm = TRUE),
      rel_bias_pct = 100 * (mean(est$VeDBA, na.rm = TRUE) - mean(ref_interp, na.rm = TRUE)) /
        mean(ref_interp, na.rm = TRUE),
      MAE = mean(abs(est$VeDBA - ref_interp), na.rm = TRUE),
      RMSE = sqrt(mean((est$VeDBA - ref_interp)^2, na.rm = TRUE))
    )

    res_list[[as.character(r)]] <- summ
    ts_list[[as.character(r)]] <- est %>% mutate(rate_hz = r, ref_VeDBA = ref_interp)
  }

  list(
    summary = bind_rows(res_list) %>% arrange(desc(rate_hz)),
    series = bind_rows(ts_list)
  )
}

# -----------------------------
# Run analysis
# -----------------------------
fs_full <- 400
duration <- 60
rates <- c(400, 220, 210, 200, 105, 100, 95, 55, 50, 45, 25, 18, 10, 5)
static_window_s <- 2

acc <- simulate_flight_accel(duration = duration, fs = fs_full)
cmp <- compare_rates(acc, fs_full = fs_full, rates = rates, static_window_s = static_window_s)

summary_tbl <- cmp$summary
series_tbl  <- cmp$series

print(summary_tbl)

# -----------------------------
# Plot 1: raw acceleration segment
# -----------------------------
seg <- acc %>%
  filter(t >= 10, t <= 12) %>%
  pivot_longer(cols = c(x, y, z), names_to = "axis", values_to = "acc")

p1 <- ggplot(seg, aes(t, acc)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~axis, ncol = 1, scales = "free_y") +
  labs(
    title = "Simulated continuous 3D acceleration",
    subtitle = "Two-second segment; Z is the dominant movement axis",
    x = "Time (s)",
    y = "Acceleration (g)"
  ) +
  theme_bw()

# -----------------------------
# Plot 2: VeDBA time series at selected rates
# -----------------------------
sel_rates <- c(400, 100, 25, 5)

p2 <- series_tbl %>%
  filter(rate_hz %in% sel_rates, t >= 20, t <= 28) %>%
  ggplot(aes(t, VeDBA)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~rate_hz, ncol = 1, scales = "free_y") +
  labs(
    title = "VeDBA time series under different sampling rates",
    subtitle = "Lower sampling rates progressively smooth and distort the dynamic signal",
    x = "Time (s)",
    y = "VeDBA"
  ) +
  theme_bw()

# -----------------------------
# Plot 3: summary metrics vs sampling rate
# -----------------------------
summary_long <- summary_tbl %>%
  select(rate_hz, mean_VeDBA, sd_VeDBA, MAE, RMSE, rel_bias_pct) %>%
  pivot_longer(-rate_hz, names_to = "metric", values_to = "value")

p3 <- ggplot(summary_long, aes(rate_hz, value)) +
  geom_line() +
  geom_point() +
  scale_x_log10(breaks = rates) +
  facet_wrap(~metric, scales = "free_y", ncol = 2) +
  labs(
    title = "Effect of sampling rate on VeDBA estimation",
    x = "Sampling rate (Hz, log scale)",
    y = "Metric value"
  ) +
  theme_bw()

# Combine and print
combined <- p1 / p2 / p3
print(combined)

# Save outputs
ggsave("simulated_acceleration_segment.png", p1, width = 8, height = 6, dpi = 300)
ggsave("vedba_timeseries_by_rate.png", p2, width = 8, height = 8, dpi = 300)
ggsave("vedba_sampling_rate_summary.png", p3, width = 9, height = 7, dpi = 300)

write.csv(summary_tbl, "vedba_sampling_rate_summary.csv", row.names = FALSE)
