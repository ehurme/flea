# What VeDBA would a NanoFox / TinyFox tag report for a death's-head hawkmoth
# (Acherontia atropos) in flight, at rest and during pre-flight warm-up?
#
# NanoFox and TinyFox carry the same accelerometer as the FleaTag but log only
# a summary. Their VeDBA (the same tags and firmware on bats and moths) is
# described in SigfoxTagPrep/claude/vedba_calculation_guide.md:
#   - a 1-s burst at 28 Hz (28 samples) every 2 min (NanoFox) or every minute
#     (TinyFox V13), +-8 g, 3.9 mg per LSB
#   - per sample: sqrt(ax^2 + ay^2 + az^2) - 1 g, summed over the burst
#   - NanoFox: bursts summed over 36-min windows (18 bursts = 504 samples);
#     the window value is an integer count of 2600 LSB (2600 x 3.9 mg = 10.14
#     g summed over 504 samples = 0.020 g mean per sample), sent as
#     count x 2600 x 3.9 x 0.00980665 = count x 99.44 m/s^2
# The 99.44 m/s^2 step is confirmed by the moth NanoFox data
# (Moths/Data/wildcloud/Moth_nanofox_4_3_2026_records.csv): every value is a
# whole multiple of it, 92% of windows are 0 and the largest is 100 counts.
#
# The guide's per-sample formula is signed (|a| - 1 g), so negative samples
# offset positive ones; it is the main variant. |(|a| - 1 g)| is kept as a
# check in case an absolute value is implied. Not in the guide, so assumed:
#   - the 28 Hz samples are instantaneous points of the signal. A 14 Hz
#     low-pass (sensor filter at ODR/2) is simulated as a check: the
#     hawkmoth wingbeat (26-31 Hz) is at the sampling rate, so the filter
#     would remove it, and flight windows would stay far below the observed
#     maximum
#   - the count is floor()ed (integer division), and a negative window sum
#     is reported as 0 (no negative value occurs in the data)
#
# Steps
#   1. The FleaTag 210 Hz recordings of R/hawkmoth_flight_dynamics.R (same
#      sensor, +-8 g) are resampled to 28 Hz by linear interpolation, starting
#      every 0.25 s: one simulated 1-s burst per start.
#   2. Each burst gets a state from that script: flight (>= 90% of the burst
#      in a flight bout), warm-up (>= 90% in a warm-up episode), rest (dynamic
#      SD of the 210 Hz burst < rest_g) or other movement (handling, walking,
#      transitions).
#   3. Burst sums (guide formula and |(|a| - 1 g)|) by state, in g and m/s^2 (per-burst values are
#      comparable to the per-burst normalisation used for NanoFox / TinyFox
#      in ExploreTinyFox/src/vedba_normalize.R).
#   4. NanoFox 36-min windows: 18 bursts, each in flight with probability
#      (flight minutes / 36), otherwise at rest or warming up, drawn from the
#      simulated bursts; summed and quantised to counts. Repeated n_sim times
#      per flight duration, then compared with the observed moth windows.
#
# Caveats: the FleaTag firmware used here logs at 63 mg resolution, so rest
# values (a few mg on a NanoFox) are dominated by quantisation and are an
# upper bound. Recordings are only ~3.4 min, so "rest" is the moth sitting on
# a hand or perch between flights, not overnight rest.
#
# Outputs (CSV + PNG) go to 2024-Fall-Moth-Barn/Results/nanofox_vedba_sim on Dropbox.

library(tidyverse)
library(data.table)
library(patchwork)

# ---- paths & parameters -----------------------------------------------------
moth_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/2024-Fall-Moth-Barn"
fd <- readRDS(file.path(moth_dir, "Results/flight_dynamics/flight_dynamics.rds"))  # R/hawkmoth_flight_dynamics.R
nf_file <- "C:/Users/ehurme/Dropbox/MPI/Moths/Data/wildcloud/Moth_nanofox_4_3_2026_records.csv"
out_dir <- file.path(moth_dir, "Results/nanofox_vedba_sim")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

nf_hz <- 28             # NanoFox / TinyFox sampling rate (Hz)
burst_n <- 28           # samples per burst
step_s <- 0.25          # spacing of simulated burst starts (s)
state_frac <- 0.9       # share of a burst in flight / warm-up to give it that state
rest_g <- 0.1           # dynamic SD of a rest burst (g, 210 Hz)
win_bursts <- 18        # bursts per NanoFox 36-min window
win_min <- 36
lsb_g <- 0.0039         # g per LSB
count_lsb <- 2600       # LSB per transmitted count
g_ms2 <- 9.80665
count_g <- count_lsb * lsb_g            # 10.14 g summed over a window per count
count_ms2 <- count_g * g_ms2            # 99.44 m/s^2 per count
n_sim <- 2000           # simulated windows per flight duration
set.seed(1)

guide <- "|a| - 1 g (guide)"
check <- "|(|a| - 1 g)| (check)"
win_counts <- function(sum_g) pmax(0, floor(sum_g / count_g))  # window sum (g) -> transmitted count

# ---- simulated bursts -------------------------------------------------------
# burst sums (g) of one burst (n x 3 in g)
burst_metrics <- function(a) {
  m <- sqrt(rowSums(a^2))
  c(signed_mag = sum(m - 1), abs_mag = sum(abs(m - 1)))
}

lp <- signal::butter(4, (nf_hz / 2) / (210 / 2))  # sensor low-pass at ODR / 2

sim_rec <- function(k) {
  s <- fd$samples %>% filter(rec == k)
  sr <- fd$recordings$sr[fd$recordings$rec == k]
  ep <- fd$warmup %>% filter(rec == k)
  warm <- rep(FALSE, nrow(s))
  for (i in seq_len(nrow(ep))) warm[s$t >= ep$t_start[i] & s$t <= ep$t_end[i]] <- TRUE
  raw <- cbind(s$x, s$y, s$z)
  filt <- apply(raw, 2, function(v) signal::filtfilt(lp, v))
  starts <- seq(first(s$t), last(s$t) - burst_n / nf_hz, by = step_s)
  map_dfr(starts, function(t0) {
    tt <- t0 + (seq_len(burst_n) - 1) / nf_hz
    idx <- which(s$t >= t0 & s$t < t0 + burst_n / nf_hz)
    pt <- apply(raw, 2, function(v) approx(s$t, v, tt)$y)
    pf <- apply(filt, 2, function(v) approx(s$t, v, tt)$y)
    st <- case_when(mean(s$fly[idx]) >= state_frac ~ "flight",
                    mean(warm[idx]) >= state_frac ~ "warm-up",
                    sqrt(sum(apply(raw[idx, ], 2, var))) < rest_g ~ "rest",
                    TRUE ~ "other")
    bind_rows(as_tibble_row(burst_metrics(pt)) %>% mutate(sampling = "point"),
              as_tibble_row(burst_metrics(pf)) %>% mutate(sampling = "14 Hz low-pass")) %>%
      mutate(rec = k, t = t0, state = st)
  })
}

bursts <- map_dfr(fd$recordings$rec, function(k) { message(k); sim_rec(k) }) %>%
  pivot_longer(c(signed_mag, abs_mag), names_to = "metric", values_to = "burst_g") %>%
  mutate(burst_ms2 = burst_g * g_ms2,
         state = factor(state, c("rest", "other", "warm-up", "flight")),
         metric = factor(recode(metric, signed_mag = guide, abs_mag = check), c(guide, check)),
         sampling = factor(sampling, c("point", "14 Hz low-pass")))
main <- bursts %>% filter(metric == guide, sampling == "point")

# per state: per-burst sums and the NanoFox window value if the whole window were in that state
by_state <- bursts %>% group_by(metric, sampling, state) %>%
  summarise(n = n(), n_rec = n_distinct(rec),
            burst_ms2_median = median(burst_ms2), burst_ms2_q10 = quantile(burst_ms2, 0.1),
            burst_ms2_q90 = quantile(burst_ms2, 0.9), sample_g_mean = mean(burst_g) / burst_n,
            window_counts = win_counts(win_bursts * mean(burst_g)),
            window_ms2 = window_counts * count_ms2, .groups = "drop")
print(by_state, n = Inf, width = Inf)
# flight per recording (between-moth spread)
flight_rec <- main %>% filter(state == "flight") %>% group_by(rec) %>%
  summarise(n = n(), burst_ms2_median = median(burst_ms2),
            window_counts = win_counts(win_bursts * mean(burst_g)), .groups = "drop")
print(flight_rec)

# ---- NanoFox 36-min windows -------------------------------------------------
# 18 bursts; each in flight with probability flight_min / 36, otherwise from the background state
pool <- split(main$burst_g, main$state)
sim_windows <- expand_grid(flight_min = 0:win_min, background = c("rest", "warm-up")) %>%
  pmap_dfr(function(flight_min, background) {
    n_f <- rbinom(n_sim, win_bursts, flight_min / win_min)
    tot <- map_dbl(n_f, ~ sum(sample(pool$flight, .x, replace = TRUE)) +
                     sum(sample(pool[[background]], win_bursts - .x, replace = TRUE)))
    tibble(flight_min = flight_min, background = background, n_flight_bursts = n_f,
           counts = win_counts(tot))
  }) %>% mutate(window_ms2 = counts * count_ms2)
window_curve <- sim_windows %>% group_by(background, flight_min) %>%
  summarise(counts_median = median(counts), counts_q05 = quantile(counts, 0.05),
            counts_q95 = quantile(counts, 0.95), p_zero = mean(counts == 0), .groups = "drop") %>%
  mutate(across(starts_with("counts"), ~ .x * count_ms2, .names = "{sub('counts', 'ms2', .col)}"))
# flight minutes implied by a window count (median of the simulated windows with that count, rest background)
implied <- sim_windows %>% filter(background == "rest") %>% group_by(counts) %>%
  summarise(flight_min_median = median(flight_min), flight_min_q05 = quantile(flight_min, 0.05),
            flight_min_q95 = quantile(flight_min, 0.95), .groups = "drop") %>%
  mutate(window_ms2 = counts * count_ms2)

# ---- observed moth NanoFox windows ------------------------------------------
obs <- read_delim(nf_file, delim = ";", show_col_types = FALSE, col_types = cols(.default = "c")) %>%
  select(Device, starts_with("VeDBA sum")) %>% pivot_longer(-Device, values_to = "window_ms2") %>%
  mutate(window_ms2 = suppressWarnings(as.numeric(window_ms2))) %>% filter(!is.na(window_ms2)) %>%
  mutate(counts = round(window_ms2 / count_ms2))
obs_summary <- obs %>% summarise(n_windows = n(), n_tags = n_distinct(Device), p_zero = mean(counts == 0),
                                 max_counts = max(counts), q99_counts = quantile(counts, 0.99))
print(obs_summary)
full_flight <- by_state %>% filter(metric == guide, sampling == "point", state == "flight")
message("simulated full-flight window: ", full_flight$window_counts, " counts (",
        round(full_flight$window_ms2), " m/s^2); observed max ", obs_summary$max_counts, " counts")

# ---- outputs ----------------------------------------------------------------
write_csv(bursts, file.path(out_dir, "simulated_bursts.csv"))
write_csv(by_state, file.path(out_dir, "burst_summary_by_state.csv"))
write_csv(flight_rec, file.path(out_dir, "flight_by_recording.csv"))
write_csv(window_curve, file.path(out_dir, "window_vs_flight_minutes.csv"))
write_csv(implied, file.path(out_dir, "flight_minutes_from_counts.csv"))
write_csv(obs_summary, file.path(out_dir, "observed_nanofox_summary.csv"))

# ---- figures ----------------------------------------------------------------
theme_set(theme_bw(base_size = 11))
state_col <- c(rest = "grey50", other = "#8c6d31", `warm-up` = "darkorange", flight = "steelblue")

# 1. what the tag sees: 1 s of 210 Hz |a| with the 28 Hz samples, per state (Moth9)
ex_rec <- "0919-Moth9-1444"
ex <- main %>% filter(rec == ex_rec) %>% group_by(state) %>%
  slice_min(abs(burst_ms2 - median(burst_ms2)), n = 1, with_ties = FALSE) %>% ungroup()
ex_raw <- pmap_dfr(ex, function(t, state, ...) {
  fd$samples %>% filter(rec == ex_rec, t >= !!t, t < !!t + 1) %>%
    transmute(state = state, ts = t - !!t, mag = sqrt(x^2 + y^2 + z^2))
})
ex_pts <- pmap_dfr(ex, function(t, state, ...) {
  s <- fd$samples %>% filter(rec == ex_rec)
  tt <- t + (seq_len(burst_n) - 1) / nf_hz
  tibble(state = state, ts = tt - t,
         mag = sqrt(approx(s$t, s$x, tt)$y^2 + approx(s$t, s$y, tt)$y^2 + approx(s$t, s$z, tt)$y^2))
})
lab <- ex %>% transmute(state, lab = sprintf("burst sum %.0f m/s^2", burst_ms2))
p_ex <- ggplot(ex_raw, aes(ts, mag)) + geom_line(colour = "grey60", linewidth = 0.3) +
  geom_hline(yintercept = 1, linetype = 2) +
  geom_segment(data = ex_pts, aes(x = ts, xend = ts, y = 1, yend = mag, colour = state), linewidth = 0.4) +
  geom_point(data = ex_pts, aes(colour = state), size = 1.2) +
  geom_text(data = lab, aes(x = 0.98, y = Inf, label = lab), hjust = 1, vjust = 1.5, size = 3.2) +
  facet_wrap(~ state, nrow = 1) + scale_colour_manual(values = state_col, guide = "none") +
  labs(x = "Time in burst (s)", y = "|a| (g)",
       title = paste0(ex_rec, ": 1 s at 210 Hz (grey) and the 28 tag samples; segments = |a| - 1 g"))
ggsave(file.path(out_dir, "burst_examples.png"), p_ex, width = 12, height = 4, dpi = 150)

# 2. per-burst sums by state, metric and sampling
# signed sums can be negative: pseudo-log axis (linear around 0, log beyond)
p_b <- ggplot(bursts, aes(state, burst_ms2, fill = state)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  geom_boxplot(outlier.size = 0.3, linewidth = 0.3) + facet_grid(sampling ~ metric) +
  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 5),
                     breaks = c(-100, -10, 0, 10, 100, 1000)) +
  scale_fill_manual(values = state_col, guide = "none") +
  labs(x = NULL, y = "Burst sum (m/s^2)",
       title = "Simulated 1-s bursts (28 samples at 28 Hz); main: |a| - 1 g (guide), point sampling")
p_r <- ggplot(main %>% filter(state == "flight"), aes(reorder(rec, burst_ms2), burst_ms2)) +
  geom_boxplot(fill = state_col["flight"], outlier.size = 0.3) + coord_flip() +
  labs(x = NULL, y = "Flight burst sum (m/s^2), |a| - 1 g")
ggsave(file.path(out_dir, "burst_sums.png"), p_b / p_r + plot_layout(heights = c(2, 1)),
       width = 11, height = 10, dpi = 150)

# 3. NanoFox 36-min window value against minutes of flight, with the observed moth windows
p_w <- ggplot(window_curve, aes(flight_min, ms2_median, colour = background, fill = background)) +
  geom_ribbon(aes(ymin = ms2_q05, ymax = ms2_q95), alpha = 0.2, colour = NA) + geom_line() +
  geom_hline(yintercept = obs_summary$max_counts * count_ms2, linetype = 2) +
  annotate("text", x = 0, y = obs_summary$max_counts * count_ms2, label = "largest observed moth window",
           hjust = 0, vjust = -0.5, size = 3.2) +
  scale_colour_manual(values = state_col, aesthetics = c("colour", "fill")) +
  labs(x = "Minutes of flight in the 36-min window", y = "NanoFox window VeDBA (m/s^2)",
       colour = "Rest of window", fill = "Rest of window",
       title = "Simulated window value (median, 5-95%); steps of 99.44 m/s^2")
p_o <- ggplot(obs %>% filter(counts > 0), aes(window_ms2)) + geom_histogram(binwidth = count_ms2 * 2) +
  labs(x = "Observed moth NanoFox window VeDBA (m/s^2), windows > 0", y = "Windows",
       title = sprintf("%d windows from %d tags; %.0f%% are 0", obs_summary$n_windows, obs_summary$n_tags,
                       100 * obs_summary$p_zero))
ggsave(file.path(out_dir, "windows.png"), p_w / p_o, width = 10, height = 9, dpi = 150)

saveRDS(list(bursts = bursts, by_state = by_state, flight_rec = flight_rec, window_curve = window_curve,
             implied = implied, obs_summary = obs_summary,
             params = list(nf_hz = nf_hz, burst_n = burst_n, step_s = step_s, state_frac = state_frac,
                           rest_g = rest_g, win_bursts = win_bursts, count_g = count_g,
                           count_ms2 = count_ms2, n_sim = n_sim)),
        file.path(out_dir, "nanofox_vedba_sim.rds"))
