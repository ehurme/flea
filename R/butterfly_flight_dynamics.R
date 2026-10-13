# Flight dynamics of butterflies tagged with FleaTags in the Mainau butterfly
# house (tags glued ventrally, butterflies flying freely, no matching video,
# so flight and behaviour come from the ACC alone). Two sessions:
#
#   0827  27 Aug 2024, Morpho peleides, ~3-min deployments, continuous
#         210 Hz +-8 g (Butterfly_Mainau/20240827-Butterfly-Mainau/cropped_data,
#         cropped from release; notes in _deploymentDetails.txt)
#   0808  8-9 Aug 2024 (20240808_Butterfly_Mainau; no deployment notes, so
#         species and mass are not recorded):
#         - data_continuous_105hz_7min: continuous 105 Hz, ~7 min; 3034 and
#           3035 at +-2 g (flapping clips), 303D, 3054, 303F at +-8 g
#         - data_burst: overnight/next-day recordings, either 105 Hz bursts of
#           62 samples (0.6 s) every 2 min (302C, 302E, 3048) or one sample
#           every 2.22 s (0.45 Hz; 301E, 3016, 3019), +-8 g
#   Recordings are named <session>-<tag> because tag IDs were reused (3016).
#
# Steps
#   1. Read: .txt exports via their header (mode, AccHz, AccMaxG), malformed
#      rows dropped (3054 has one corrupt line); cropped .csv files are 210 Hz
#      +-8 g. Sampling rate from the tag clock: (n - 1) / duration, within
#      bursts for burst mode. With no video, frequencies are on the tag clock.
#   2. Continuous high-rate recordings (210 and 105 Hz): flight vs. rest with
#      flight_segment() (running dynamic SD with hysteresis), the window
#      scaled to ~3 wingbeats of a 6-7 Hz butterfly. Bouts shorter than
#      min_bout_s or without a clear wingbeat peak (shakes, handling, impacts)
#      are not flight.
#        Per bout: duration, wingbeat frequency (summed-axis spectrum,
#      wbf_est() with a subharmonic check), dynamic acceleration, amplitude per axis at the
#      fundamental, 2nd-harmonic ratio, posture in the tag frame, clipping,
#      peak |a| at take-off and landing.
#        Per wingbeat: the bout is band-passed around its fundamental and
#      projected on its first PC; each upward zero crossing starts a cycle.
#      Per cycle: frequency, VeDBA and peak-to-peak amplitude in the
#      unfiltered dynamic acceleration. Cycles with VeDBA below glide_frac x
#      the bout median are pauses in flapping (glides).
#        Amplitudes of the +-2 g tags are clipped and left out of amplitude
#      comparisons; their frequencies are kept.
#   3. Calibration of the sparse modes on the +-8 g continuous recordings
#      (flight known from step 2), resampled to 105 Hz:
#        - 62-sample windows -> threshold on dynamic SD (burst_flight_g) with
#          the best balanced accuracy against "window mostly in flight", and
#          the error of the burst wingbeat frequency against its bout.
#        - every 2.22 s -> threshold for classify_continuous() (||a| - 1 g|),
#          over all sampling phases, chosen so the flight share is unbiased
#          (the best balanced accuracy doubles it).
#   4. Long recordings: flight per burst / per sample with those thresholds,
#      flight share in 30-min bins next to the light sensor, and wingbeat
#      frequency of flight bursts.
#   5. 0827 only: check against the flights cropped by hand in
#      extracted_flights (fraction of those samples classified as flight).
#
# Tag orientation is not standardised between individuals, so posture
# (pitch/roll) and per-axis amplitudes compare bouts within an individual.
# The "mass" in the 0827 notes is copied as given (it is not stated whether
# it is butterfly or tag mass).
#
# Outputs (CSV + PNG) go to Butterfly_Mainau/Results/flight_dynamics on Dropbox.

library(tidyverse)
library(data.table)
library(patchwork)
library(signal)
source("./R/flea_functions.R")  # flight_segment(), wingbeat_freq(), classify_continuous()

# ---- paths & parameters -----------------------------------------------------
wb <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat"
aug27 <- file.path(wb, "Butterfly_Mainau/20240827-Butterfly-Mainau")
aug08 <- file.path(wb, "20240808_Butterfly_Mainau")
manual_dir <- file.path(aug27, "extracted_flights")
out_dir <- file.path(wb, "Butterfly_Mainau/Results/flight_dynamics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

band <- c(4, 14)        # wingbeat fundamental search band (Hz): Morpho bouts peak at 5-7.6 Hz,
                        # 0808-3054 at ~10 Hz; take-off transients (< 4 Hz) stay out
sub_frac <- 0.3         # a peak at f0 / 2 with >= this share of the power at f0 is the
                        # fundamental (f0 was its 2nd harmonic, 10-15 Hz in Morpho)
seg_win_s <- 0.45       # flight_segment() window, ~3 wingbeats
on_g <- 0.8             # dynamic SD to enter flight (g)
off_g <- 0.4            # ... and to leave it
peak_g <- 1.5           # a flight run must reach this dynamic SD somewhere
min_run_s <- 0.3        # shorter interior runs are merged into neighbours
min_bout_s <- 1         # shorter bouts are dropped (~6 wingbeats)
min_peak_frac <- 0.15   # share of 1-30 Hz power within +-1 Hz of the fundamental
edge_s <- 0.3           # take-off / landing window for peak |a| (s)
glide_frac <- 0.3       # cycles below this x bout median VeDBA = flapping pause
burst_n <- 62           # samples per burst in burst mode
low_hz <- 0.45          # low-rate mode
bin_h <- 0.5            # activity bins for long recordings (h)

# deployment notes of 0827 (_deploymentDetails.txt); 3043 and 303D had empty memory
deploy <- tribble(
  ~no, ~tag,   ~start,  ~mass_g, ~note,
  1,   "3016", "18:44", NA,      "only 60 s, non-flying (still handling)",
  2,   "302F", "19:06", 1.09,    "full dataset with flight, 200 s",
  4,   "302A", "19:30", 0.41,    "full dataset with flight, 200 s",
  5,   "3052", "19:35", 1.25,    "only 35 s, maybe some flying",
  7,   "3042", "19:51", 0.70,    "only 70 s, maybe some flying",
  8,   "304E", "19:58", NA,      "noted as lost / no data, but a cropped file exists",
  9,   "3014", "20:10", 0.85,    "full dataset with flight, 200 s",
  10,  "3023", "20:19", NA,      "full dataset with flight, 200 s"
) %>% mutate(rec = paste0("0827-", tag), species = "Morpho peleides")

# ---- reading ----------------------------------------------------------------
# returns data (x, y, z in g, t in s, light = R+G+B where logged), tag, mode,
# type (continuous / burst / lowrate), nominal and clock sampling rate, range
read_butterfly <- function(path, session) {
  if (grepl("\\.csv$", path)) {
    d <- fread(path)
    tag <- toupper(sub("_results\\.csv$", "", basename(path)))
    hz <- 210; range_g <- 8; mode <- "210 Hz continuous (cropped)"
  } else {
    l <- readLines(path, warn = FALSE)
    h <- grep("^lineCnt", l)[1]
    e <- grep("^Delete memory", l)[1]
    if (is.na(e)) e <- length(l) + 1
    d <- suppressWarnings(fread(text = l[h:(e - 1)], fill = TRUE))
    key <- function(k) trimws(sub(paste0("^", k, ":"), "", grep(paste0("^", k, ":"), l, value = TRUE)[1]))
    tag <- toupper(key("ID")); hz <- as.numeric(key("AccHz"))
    range_g <- as.numeric(sub("G", "", key("AccMaxG")))
    mode <- sub(".*V\\d+: ", "", grep("^FLEATAG", l, value = TRUE)[1])
  }
  ok <- complete.cases(d[, 1:6]) & abs(d$accX_mg) <= 16500 & abs(d$accY_mg) <= 16500 & abs(d$accZ_mg) <= 16500
  if (sum(!ok)) message(basename(path), ": dropped ", sum(!ok), " malformed rows")
  d <- d[ok]
  d[, `:=`(x = accX_mg / 1000, y = accY_mg / 1000, z = accZ_mg / 1000,
           t = (timeMilliseconds - timeMilliseconds[1]) / 1000,
           light = ColorSensRed_cnt + ColorSensGreen_cnt + ColorSensBlue_cnt)]
  type <- if (hz < 1) "lowrate" else if (grepl("CONT|cropped", mode)) "continuous" else "burst"
  sr <- if (type == "burst") {
    d[, .(n = .N, dur = diff(range(t))), by = burstCount][n == burst_n, median((n - 1) / dur)]
  } else (nrow(d) - 1) / d$t[nrow(d)]
  list(data = d, tag = tag, rec = paste0(session, "-", tag), session = session, mode = mode,
       type = type, hz = hz, sr = sr, range_g = range_g, path = path)
}

# ---- helpers ----------------------------------------------------------------
# wingbeat frequency: wingbeat_freq() in band, moved down to the subharmonic
# when that holds sub_frac of the peak power (the 2nd harmonic of a slow
# wingbeat overlaps the fundamental of a fast one)
wbf_est <- function(a, sr) {
  f0 <- wingbeat_freq(a, sr, band)
  if (f0 / 2 < band[1]) return(f0)
  s <- axis_spec(a, sr, spans = NULL)
  tot <- rowSums(as.matrix(s$spec))
  near <- abs(s$freq - f0 / 2) <= 1
  if (max(tot[near]) >= sub_frac * max(tot[abs(s$freq - f0) <= 0.5])) s$freq[near][which.max(tot[near])] else f0
}

# summed-axis spectrum of a (n x 3), mean removed
axis_spec <- function(a, sr, spans = 3) {
  spec.pgram(ts(sweep(a, 2, colMeans(a)), frequency = sr), spans = spans, taper = 0.1,
             pad = 1, detrend = TRUE, plot = FALSE)
}

# share of 1-30 Hz power within +-1 Hz of f0
peak_share <- function(s, f0) {
  tot <- rowSums(as.matrix(s$spec))
  sum(tot[abs(s$freq - f0) <= 1]) / sum(tot[s$freq >= 1 & s$freq <= 30])
}

bout_features <- function(a, sr, a_all, i0, i1, clip_g) {
  s <- axis_spec(a, sr)
  P <- s$spec; fr <- s$freq
  f0 <- wbf_est(a, sr)
  near <- function(f) abs(fr - f) <= 1
  p_f0 <- colSums(P[near(f0), , drop = FALSE])
  p_2f <- colSums(P[near(2 * f0), , drop = FALSE])
  m <- colMeans(a)
  dyn <- sweep(a, 2, m)
  edge <- round(edge_s * sr)
  mag <- sqrt(rowSums(a_all^2))
  tibble(
    wbf_hz = f0,
    peak_frac = peak_share(s, f0),
    dyn_sd = sqrt(sum(apply(dyn, 2, var))),
    vedba = mean(sqrt(rowSums(dyn^2))),
    amp_x = sqrt(p_f0[1]), amp_y = sqrt(p_f0[2]), amp_z = sqrt(p_f0[3]),
    harm_ratio = log10(sum(p_2f) / sum(p_f0)),
    harm_x = log10(p_2f[1] / p_f0[1]), harm_z = log10(p_2f[3] / p_f0[3]),
    static_g = sqrt(sum(m^2)),
    pitch_deg = atan2(m[1], sqrt(m[2]^2 + m[3]^2)) * 180 / pi,
    roll_deg = atan2(m[2], m[3]) * 180 / pi,
    clip_frac = mean(abs(a) >= clip_g),
    takeoff_peak_g = max(mag[max(1, i0 - edge):min(length(mag), i0 + edge)]),
    landing_peak_g = max(mag[max(1, i1 - edge):min(length(mag), i1 + edge)])
  )
}

# wingbeat cycles of one bout: upward zero crossings of the first PC of the
# signal band-passed around f0
bout_cycles <- function(a, sr, f0) {
  bp <- butter(2, c(0.6, 1.5) * f0 / (sr / 2), type = "pass")
  af <- apply(sweep(a, 2, colMeans(a)), 2, function(v) filtfilt(bp, v))
  pc <- prcomp(af)$x[, 1]
  up <- which(diff(sign(pc)) > 0)
  if (length(up) < 3) return(NULL)
  dyn <- a - apply(a, 2, frollmean, n = round(sr / f0), align = "center", fill = NA)
  dyn[is.na(dyn)] <- 0
  vedba <- sqrt(rowSums(dyn^2))
  tibble(c0 = head(up, -1), c1 = tail(up, -1) - 1) %>%
    mutate(period_s = (c1 - c0 + 1) / sr, freq_hz = 1 / period_s,
           vedba = map2_dbl(c0, c1, ~ mean(vedba[.x:.y])),
           p2p_x = map2_dbl(c0, c1, ~ diff(range(dyn[.x:.y, 1]))),
           p2p_y = map2_dbl(c0, c1, ~ diff(range(dyn[.x:.y, 2]))),
           p2p_z = map2_dbl(c0, c1, ~ diff(range(dyn[.x:.y, 3]))),
           t_in_bout = (c0 - 1) / sr)
}

# sliding-window summed-axis spectrogram, 0-30 Hz
spectrogram <- function(a, sr, win_s = 2, step_s = 0.25, fmax = 30) {
  w <- round(win_s * sr); st <- round(step_s * sr)
  starts <- seq(1, nrow(a) - w + 1, by = st)
  map_dfr(starts, function(i) {
    s <- axis_spec(a[i:(i + w - 1), ], sr, spans = NULL)
    ok <- s$freq <= fmax
    tibble(t = (i - 1 + w / 2) / sr, freq = s$freq[ok], power = rowSums(as.matrix(s$spec))[ok])
  })
}

# one burst / window of high-rate data
burst_stats <- function(a, sr) {
  f0 <- wbf_est(a, sr)
  tibble(dyn_sd = sqrt(sum(apply(a, 2, var))), wbf_hz = f0,
         peak_frac = peak_share(axis_spec(a, sr, spans = NULL), f0))
}

bal_acc <- function(pred, truth) (mean(pred[truth]) + mean(!pred[!truth])) / 2

# continuous high-rate recording -> samples, bouts, cycles, spectrogram
process_continuous <- function(r) {
  d <- r$data; sr <- r$sr
  a <- cbind(d$x, d$y, d$z)
  fs <- flight_segment(d$x, d$y, d$z, sr, win_s = seg_win_s, on_g = on_g, off_g = off_g,
                       peak_g = peak_g, min_run_s = min_run_s)
  fly <- coalesce(fs$fly, FALSE)
  rl <- rle(fly); e <- cumsum(rl$lengths); b <- e - rl$lengths + 1
  cand <- tibble(i0 = b[rl$values], i1 = e[rl$values]) %>%
    filter((i1 - i0 + 1) / sr >= min_bout_s)
  bt <- pmap_dfr(cand, function(i0, i1) {
    bout_features(a[i0:i1, ], sr, a, i0, i1, r$range_g - 0.1) %>% mutate(i0 = i0, i1 = i1)
  })
  fly2 <- rep(FALSE, nrow(d)); cy <- tibble()
  if (nrow(bt)) {
    # bouts without a clear wingbeat stay in the table (wingbeat = FALSE) but
    # are not flight
    bt <- bt %>% mutate(t_start = d$t[i0], t_end = d$t[i1], duration_s = (i1 - i0 + 1) / sr,
                        wingbeat = peak_frac >= min_peak_frac, bout = cumsum(wingbeat) * wingbeat)
    keep <- bt %>% filter(wingbeat)
    for (k in seq_len(nrow(keep))) fly2[keep$i0[k]:keep$i1[k]] <- TRUE
    cy <- keep %>% pmap_dfr(function(i0, i1, wbf_hz, bout, ...) {
      cc <- bout_cycles(a[i0:i1, ], sr, wbf_hz)
      if (is.null(cc)) return(NULL)
      cc %>% mutate(bout = bout, t_to_end = (i1 - i0 + 1) / sr - t_in_bout,
                    glide = vedba < glide_frac * median(vedba))
    })
  }
  bout_id <- rep(0L, nrow(d))
  for (k in seq_len(nrow(bt))) if (bt$wingbeat[k]) bout_id[bt$i0[k]:bt$i1[k]] <- bt$bout[k]
  list(samples = tibble(rec = r$rec, t = d$t, timeMilliseconds = d$timeMilliseconds,
                        dyn = fs$dyn, fly = fly2, bout = bout_id),
       bouts = bt %>% mutate(rec = r$rec), cycles = cy %>% mutate(rec = r$rec),
       spec = spectrogram(a, sr) %>% mutate(rec = r$rec))
}

# ---- read everything --------------------------------------------------------
index <- bind_rows(
  tibble(path = list.files(file.path(aug27, "cropped_data"), "_results\\.csv$", full.names = TRUE), session = "0827"),
  tibble(path = list.files(file.path(aug08, "data_continuous_105hz_7min"), "FleaTagData\\.txt$", full.names = TRUE), session = "0808"),
  tibble(path = list.files(file.path(aug08, "data_burst"), "FleaTagData\\.txt$", full.names = TRUE), session = "0808"))
recs <- pmap(index, read_butterfly) %>% set_names(map_chr(., "rec"))
rec_tab <- map_dfr(recs, ~ tibble(rec = .x$rec, session = .x$session, tag = .x$tag, mode = .x$mode,
                                  type = .x$type, hz = .x$hz, sr = .x$sr, range_g = .x$range_g,
                                  n = nrow(.x$data), duration_h = last(.x$data$t) / 3600))
print(rec_tab, n = Inf)

# ---- continuous high-rate recordings ----------------------------------------
cont <- rec_tab %>% filter(type == "continuous")
res <- map(cont$rec, function(k) { message(k); process_continuous(recs[[k]]) }) %>% set_names(cont$rec)
samples <- map_dfr(res, "samples"); bouts <- map_dfr(res, "bouts")
cycles <- map_dfr(res, "cycles"); specs <- map_dfr(res, "spec")

amp_ok <- rec_tab %>% filter(range_g >= 8) %>% pull(rec)  # unclipped amplitudes
fl <- bouts %>% filter(wingbeat) %>% left_join(rec_tab %>% select(rec, session, range_g), by = "rec")
indiv <- cont %>% select(rec, session, tag, hz, sr, range_g, duration_h) %>%
  mutate(duration_s = duration_h * 3600, .keep = "unused") %>%
  left_join(samples %>% group_by(rec) %>% summarise(flight_s = mean(fly) * (last(t) - first(t)),
                                                     flight_frac = mean(fly)), by = "rec") %>%
  left_join(fl %>% group_by(rec) %>%
              summarise(n_bouts = n(), bout_median_s = median(duration_s),
                        bout_max_s = max(duration_s),
                        wbf_hz = weighted.mean(wbf_hz, duration_s),
                        vedba = weighted.mean(vedba, duration_s),
                        harm_ratio = weighted.mean(harm_ratio, duration_s),
                        clip_frac = weighted.mean(clip_frac, duration_s),
                        takeoff_peak_g = median(takeoff_peak_g),
                        landing_peak_g = median(landing_peak_g)), by = "rec") %>%
  left_join(cycles %>% group_by(rec) %>%
              summarise(n_cycles = n(), cycle_freq_median = median(freq_hz[!glide]),
                        cycle_freq_cv = sd(freq_hz[!glide]) / mean(freq_hz[!glide]),
                        glide_frac = mean(glide)), by = "rec") %>%
  left_join(deploy %>% select(rec, no, start, species, mass_g, note), by = "rec") %>%
  mutate(n_bouts = coalesce(n_bouts, 0L),
         across(c(vedba, takeoff_peak_g, landing_peak_g), ~ if_else(rec %in% amp_ok, .x, NA_real_))) %>%
  arrange(session, no, rec)

# check against the hand-cropped flights (0827)
manual <- map_dfr(list.files(manual_dir, pattern = "\\.csv$", full.names = TRUE), function(f) {
  m <- fread(f)
  rec <- paste0("0827-", toupper(str_match(basename(f), "^\\d+_([0-9A-Za-z]+)_")[, 2]))
  s <- samples %>% filter(rec == !!rec)
  tibble(rec = rec, file = basename(f), manual_s = nrow(m) / recs[[rec]]$sr,
         frac_flight = mean(s$fly[match(m$timeMilliseconds, s$timeMilliseconds)], na.rm = TRUE))
})

# ---- calibration of burst and 0.45 Hz modes ---------------------------------
# +-8 g continuous recordings at ~105 Hz (210 Hz: every 2nd sample)
calib_recs <- cont %>% filter(range_g >= 8) %>% pull(rec)
calib_105 <- map(calib_recs, function(k) {
  r <- recs[[k]]; s <- samples %>% filter(rec == k)
  step <- max(1, round(r$sr / 105)); keep <- seq(1, nrow(r$data), by = step)
  list(rec = k, a = cbind(r$data$x, r$data$y, r$data$z)[keep, ], fly = s$fly[keep], bout = s$bout[keep],
       ms = r$data$timeMilliseconds[keep], sr = r$sr / step)
})

sim_bursts <- map_dfr(calib_105, function(cr) {
  starts <- seq(1, nrow(cr$a) - burst_n + 1, by = burst_n)
  map_dfr(starts, function(i) {
    j <- i:(i + burst_n - 1)
    burst_stats(cr$a[j, ], cr$sr) %>%
      mutate(frac_fly = mean(cr$fly[j]), bout = if (mean(cr$fly[j]) >= 0.5) max(cr$bout[j]) else 0L)
  }) %>% mutate(rec = cr$rec)
}) %>% mutate(truth = frac_fly >= 0.5)
thr_grid <- seq(0.1, 2, by = 0.05)
burst_calib <- tibble(thr = thr_grid,
                      bal_acc = map_dbl(thr_grid, ~ bal_acc(sim_bursts$dyn_sd > .x, sim_bursts$truth)))
burst_flight_g <- burst_calib$thr[which.max(burst_calib$bal_acc)]
sim_bursts <- sim_bursts %>% mutate(pred = dyn_sd > burst_flight_g) %>%
  left_join(fl %>% select(rec, bout, bout_wbf = wbf_hz), by = c("rec", "bout"))
burst_wbf_err <- sim_bursts %>% filter(truth, pred) %>%
  summarise(n = n(), median_abs_err_hz = median(abs(wbf_hz - bout_wbf)),
            within_1hz = mean(abs(wbf_hz - bout_wbf) <= 1))
message("burst threshold ", burst_flight_g, " g, balanced accuracy ", round(max(burst_calib$bal_acc), 3))

low_grid <- seq(0.1, 1.5, by = 0.05)
sim_low <- map_dfr(calib_105, function(cr) {
  step <- round(cr$sr / low_hz)
  map_dfr(round(seq(1, step, length.out = 10)), function(o) {
    j <- seq(o, nrow(cr$a), by = step)
    d0 <- data.table(burstCount = 0L, timeMilliseconds = cr$ms[j],
                     x = cr$a[j, 1], y = cr$a[j, 2], z = cr$a[j, 3], light = NA_real_)
    map_dfr(low_grid, ~ tibble(thr = .x, pred = classify_continuous(d0, flight_g = .x)$fly,
                                truth = cr$fly[j]))
  }) %>% mutate(rec = cr$rec)
})
low_calib <- sim_low %>% group_by(thr) %>%
  summarise(bal_acc = bal_acc(pred, truth), pred_share = mean(pred), true_share = mean(truth))
# the threshold with the best balanced accuracy overestimates flight (rest
# samples are many), so take the one whose flight share is unbiased
low_flight_g <- low_calib$thr[which.min(abs(low_calib$pred_share - low_calib$true_share))]
message("0.45 Hz threshold ", low_flight_g, " g, balanced accuracy ",
        round(low_calib$bal_acc[low_calib$thr == low_flight_g], 3))

# ---- long recordings: bursts and 0.45 Hz ------------------------------------
long_bursts <- rec_tab %>% filter(type == "burst") %>% pull(rec) %>% map_dfr(function(k) {
  r <- recs[[k]]
  r$data[, {
    if (.N == burst_n) burst_stats(cbind(x, y, z), r$sr) %>%
      mutate(t_h = t[1] / 3600, light = light[.N]) else NULL
  }, by = burstCount] %>% as_tibble() %>% mutate(rec = k, fly = dyn_sd > burst_flight_g)
})
long_low <- rec_tab %>% filter(type == "lowrate") %>% pull(rec) %>% map_dfr(function(k) {
  r <- recs[[k]]
  classify_continuous(r$data, flight_g = low_flight_g) %>% as_tibble() %>%
    transmute(rec = k, t_h = t_s / 3600, light, fly)
})
long_light <- bind_rows(long_bursts %>% select(rec, t_h, light),
                        long_low %>% select(rec, t_h, light) %>% filter(!is.na(light)) %>% distinct())
activity <- bind_rows(long_bursts %>% select(rec, t_h, fly), long_low %>% select(rec, t_h, fly)) %>%
  mutate(bin = floor(t_h / bin_h) * bin_h) %>%
  group_by(rec, bin) %>% summarise(flight_pct = 100 * mean(fly), n = n(), .groups = "drop") %>%
  left_join(long_light %>% mutate(bin = floor(t_h / bin_h) * bin_h) %>% group_by(rec, bin) %>%
              summarise(light = median(light, na.rm = TRUE), .groups = "drop"), by = c("rec", "bin"))
long_summary <- rec_tab %>% filter(type %in% c("burst", "lowrate")) %>%
  select(rec, mode, type, sr, duration_h) %>%
  left_join(bind_rows(long_bursts %>% select(rec, fly), long_low %>% select(rec, fly)) %>%
              group_by(rec) %>% summarise(n_obs = n(), flight_pct = 100 * mean(fly)), by = "rec") %>%
  left_join(long_bursts %>% filter(fly, peak_frac >= min_peak_frac) %>% group_by(rec) %>%
              summarise(n_wingbeat_bursts = n(), burst_wbf_median = median(wbf_hz)), by = "rec")

# ---- outputs ----------------------------------------------------------------
write_csv(rec_tab, file.path(out_dir, "recordings.csv"))
write_csv(bouts %>% select(rec, bout, wingbeat, t_start, t_end, duration_s, everything(), -i0, -i1),
          file.path(out_dir, "bouts.csv"))
write_csv(cycles %>% select(rec, bout, t_in_bout, t_to_end, everything(), -c0, -c1),
          file.path(out_dir, "wingbeat_cycles.csv"))
write_csv(indiv, file.path(out_dir, "individual_summary.csv"))
write_csv(manual, file.path(out_dir, "manual_flight_check.csv"))
write_csv(burst_calib %>% mutate(mode = "62-sample bursts") %>%
            bind_rows(low_calib %>% mutate(mode = "0.45 Hz")), file.path(out_dir, "calibration.csv"))
write_csv(long_bursts, file.path(out_dir, "long_bursts.csv"))
write_csv(activity, file.path(out_dir, "activity_bins.csv"))
write_csv(long_summary, file.path(out_dir, "long_summary.csv"))
print(indiv, width = Inf, n = Inf)
print(manual)
print(burst_wbf_err)
print(long_summary, width = Inf)

# ---- figures ----------------------------------------------------------------
theme_set(theme_bw(base_size = 11))
rec_label <- function(k) paste0(k, if_else(k %in% amp_ok, "", " (2 g)"))

# 1. per continuous recording: dynamic SD with flight bouts + spectrogram
for (k in cont$rec) {
  s <- samples %>% filter(rec == k)
  bb <- fl %>% filter(rec == k)
  p1 <- ggplot(s, aes(t, dyn)) +
    geom_rect(data = bb, aes(xmin = t_start, xmax = t_end, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "steelblue", alpha = 0.2) +
    geom_line(linewidth = 0.3, na.rm = TRUE) +
    geom_hline(yintercept = c(off_g, on_g), linetype = 2, colour = "grey50") +
    labs(x = NULL, y = "Dynamic SD (g)",
         title = paste0(rec_label(k), ", ", recs[[k]]$mode, ": flight bouts (shaded)"))
  sp <- specs %>% filter(rec == k)
  lim <- quantile(log10(sp$power[sp$power > 0]), c(0.05, 0.995))  # rest periods are ~0
  p2 <- ggplot(sp, aes(t, freq, fill = log10(pmax(power, 1e-12)))) + geom_raster() +
    scale_fill_viridis_c(name = "log10\npower", limits = lim, oob = scales::squish) +
    geom_point(data = bb, aes(x = (t_start + t_end) / 2, y = wbf_hz), inherit.aes = FALSE,
               colour = "white", shape = 4) +
    labs(x = "Time (s)", y = "Frequency (Hz)")
  ggsave(file.path(out_dir, paste0("overview_", k, ".png")), p1 / p2, width = 11, height = 6, dpi = 150)
}

# 2. bout level
p_wbf <- ggplot(fl, aes(rec, wbf_hz, colour = session)) + geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(size = duration_s), width = 0.15, alpha = 0.6) +
  labs(x = NULL, y = "Wingbeat frequency (Hz)", size = "Bout (s)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_dur <- ggplot(fl, aes(duration_s, wbf_hz, colour = rec)) + geom_point() +
  scale_x_log10() + labs(x = "Bout duration (s)", y = "Wingbeat frequency (Hz)")
p_amp <- ggplot(fl %>% filter(rec %in% amp_ok), aes(wbf_hz, vedba, colour = rec)) + geom_point() +
  labs(x = "Wingbeat frequency (Hz)", y = "Mean VeDBA (g), +-8 g tags")
p_hist <- ggplot(fl, aes(duration_s, fill = session)) + geom_histogram(bins = 20) +
  labs(x = "Bout duration (s)", y = "Bouts")
ggsave(file.path(out_dir, "bouts.png"), (p_wbf | p_dur) / (p_amp | p_hist) + plot_layout(guides = "collect"),
       width = 12, height = 9, dpi = 150)

# 3. within-bout time course (flapping cycles only; VeDBA of +-8 g tags)
cf <- cycles %>% filter(!glide)
tc <- function(df, xvar, xlab, yvar, ylab) {
  ggplot(df, aes({{ xvar }}, {{ yvar }}, colour = rec)) +
    geom_point(alpha = 0.15, size = 0.6) + geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
    labs(x = xlab, y = ylab)
}
on3 <- cf %>% filter(t_in_bout <= 3); off3 <- cf %>% filter(t_to_end <= 3) %>% mutate(tl = -t_to_end)
ggsave(file.path(out_dir, "within_bout.png"),
       (tc(on3, t_in_bout, "Time since take-off (s)", freq_hz, "Cycle frequency (Hz)") |
          tc(off3, tl, "Time to landing (s)", freq_hz, "Cycle frequency (Hz)")) /
         (tc(on3 %>% filter(rec %in% amp_ok), t_in_bout, "Time since take-off (s)", vedba, "Cycle VeDBA (g)") |
            tc(off3 %>% filter(rec %in% amp_ok), tl, "Time to landing (s)", vedba, "Cycle VeDBA (g)")) +
         plot_layout(guides = "collect"),
       width = 12, height = 8, dpi = 150)

# 4. cycle-to-cycle: frequency distribution, frequency vs amplitude, glides, mass
p_cf <- ggplot(cf, aes(freq_hz, colour = rec)) + geom_density() + coord_cartesian(xlim = c(3, 12)) +
  labs(x = "Cycle frequency (Hz)", y = "Density")
p_fa <- ggplot(cf %>% filter(between(freq_hz, 3, 12), rec %in% amp_ok), aes(freq_hz, vedba, colour = rec)) +
  geom_point(alpha = 0.2, size = 0.6) + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(x = "Cycle frequency (Hz)", y = "Cycle VeDBA (g), +-8 g tags")
p_gl <- ggplot(indiv %>% filter(!is.na(glide_frac)), aes(rec_label(rec), glide_frac, fill = session)) + geom_col() +
  labs(x = NULL, y = "Share of cycles that are flapping pauses") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_mass <- ggplot(indiv %>% filter(!is.na(mass_g), !is.na(wbf_hz)), aes(mass_g, wbf_hz, label = rec)) +
  geom_point() + geom_text(nudge_y = 0.08, size = 3) +
  labs(x = "Mass in deployment notes (g), 0827", y = "Wingbeat frequency (Hz)")
ggsave(file.path(out_dir, "cycles.png"), (p_cf | p_fa) / (p_gl | p_mass) + plot_layout(guides = "collect"),
       width = 12, height = 9, dpi = 150)

# 5. calibration of the sparse modes
p_c1 <- ggplot(sim_bursts, aes(dyn_sd, fill = truth)) + geom_histogram(bins = 60, position = "identity", alpha = 0.6) +
  geom_vline(xintercept = burst_flight_g, linetype = 2) + scale_x_log10() +
  labs(x = "Dynamic SD of 62-sample window (g)", y = "Windows", fill = "Mostly flight",
       title = sprintf("Bursts: threshold %.2f g, balanced accuracy %.3f", burst_flight_g, max(burst_calib$bal_acc)))
p_c2 <- ggplot(sim_bursts %>% filter(truth, pred), aes(bout_wbf, wbf_hz)) +
  geom_abline(linetype = 2) + geom_jitter(alpha = 0.3, width = 0.02, height = 0.05) +
  labs(x = "Wingbeat frequency of the bout (Hz)", y = "Wingbeat frequency in 0.6 s window (Hz)",
       title = sprintf("Median error %.2f Hz, %.0f%% within 1 Hz", burst_wbf_err$median_abs_err_hz,
                       100 * burst_wbf_err$within_1hz))
p_c3 <- ggplot(bind_rows(burst_calib %>% mutate(mode = "62-sample bursts"), low_calib %>% mutate(mode = "0.45 Hz")),
               aes(thr, bal_acc, colour = mode)) + geom_line() +
  geom_vline(xintercept = c(burst_flight_g, low_flight_g), linetype = 3) +
  labs(x = "Threshold (g)", y = "Balanced accuracy", colour = NULL)
p_c4 <- ggplot(low_calib, aes(thr)) + geom_line(aes(y = pred_share, linetype = "classified")) +
  geom_line(aes(y = true_share, linetype = "true")) + geom_vline(xintercept = low_flight_g, linetype = 3) +
  labs(x = "0.45 Hz threshold ||a| - 1 g| (g)", y = "Share of samples in flight", linetype = NULL)
ggsave(file.path(out_dir, "calibration.png"), (p_c1 | p_c2) / (p_c3 | p_c4), width = 12, height = 9, dpi = 150)

# 6. long recordings: flight share per bin with light
act_long <- activity %>%
  mutate(light = log10(light + 1)) %>%
  pivot_longer(c(flight_pct, light), names_to = "metric") %>%
  mutate(metric = recode(metric, flight_pct = "Flight (%)", light = "log10(light + 1)"))
p_act <- ggplot(act_long, aes(bin + bin_h / 2, value)) +
  geom_col(data = act_long %>% filter(metric == "Flight (%)"), width = bin_h, fill = "steelblue") +
  geom_line(data = act_long %>% filter(metric != "Flight (%)"), colour = "goldenrod") +
  facet_grid(metric ~ rec, scales = "free") +
  labs(x = "Time since tag start (h)", y = NULL,
       title = sprintf("Long recordings (%.0f-min bins); bursts: dyn SD > %.2f g, 0.45 Hz: ||a| - 1 g| > %.2f g",
                       bin_h * 60, burst_flight_g, low_flight_g))
p_bw <- ggplot(bind_rows(long_bursts %>% filter(fly, peak_frac >= min_peak_frac) %>%
                           transmute(rec, wbf_hz, source = "flight bursts (long recordings)"),
                         fl %>% transmute(rec, wbf_hz, source = "bouts (continuous)")),
               aes(wbf_hz, fill = source)) +
  geom_histogram(binwidth = 0.25, position = "identity", alpha = 0.6) +
  labs(x = "Wingbeat frequency (Hz)", y = "Count", fill = NULL)
ggsave(file.path(out_dir, "long_recordings.png"), p_act / p_bw + plot_layout(heights = c(2, 1)),
       width = 14, height = 9, dpi = 150)

saveRDS(list(recordings = rec_tab, samples = samples, bouts = bouts, cycles = cycles, indiv = indiv,
             manual = manual, sim_bursts = sim_bursts, burst_calib = burst_calib, low_calib = low_calib,
             long_bursts = long_bursts, long_low = long_low, activity = activity, long_summary = long_summary,
             params = list(band = band, seg_win_s = seg_win_s, on_g = on_g, off_g = off_g,
                           peak_g = peak_g, min_run_s = min_run_s, min_bout_s = min_bout_s,
                           min_peak_frac = min_peak_frac, glide_frac = glide_frac,
                           burst_flight_g = burst_flight_g, low_flight_g = low_flight_g)),
        file.path(out_dir, "flight_dynamics.rds"))
