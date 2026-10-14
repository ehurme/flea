# Flight dynamics and pre-flight warm-up of death's-head hawkmoths (Acherontia
# atropos) carrying FleaTags in the barn, 17-20 Sep 2024
# (2024-Fall-Moth-Barn). All recordings are continuous 210 Hz +-8 g
# (BUTTERFLY_210HZ_8G_CONT), ~204 s from tag start until the memory is full.
# The flights were also tracked in 3D (Photos/moth001.png shows a c3d track),
# but the tracks are not in this folder, so flight and warm-up come from the
# ACC alone and frequencies are on the tag clock.
#
# Files (notes are from the file names; tag ID from the header where present):
#   0917  Moth2 (01_niceFlight), Moth3 (02_secondFlight): cropped .csv without
#         header; 01_niceFlight.csv and 02_secondFlight.csv are identical copies
#   0918  Moth5, tag 3023. Moth1_copy_..._155032 holds the same data twice
#         (two dumps identical to the Moth5 file), so this is one recording and
#         it is not known whether it was Moth1 or Moth5
#   0919  Moth7: no header, the first ~3000 lines (~14 s) are missing and a few
#         lines are corrupt (the "complete" and "missing_upto3000" files are
#         identical, "_fixed" only drops the first line); Moth9, tag 3023
#   0920  Moth5 "didn't fly, charging up" (3044), Moth6 (304F), Moth1 "brief
#         flight" (3044), Moth6 "beautiful flight" (304F; the .csv equals the
#         .txt), Moth8 "great flight, lost ball early on" (3044; the export
#         has a second, partial re-read that is ignored), Moth4 "charging up,
#         no flight" (304F)
# Moth numbers are per day, so recordings, not individuals, are the unit
# (0920 Moth6 has two recordings).
#
# Steps
#   1. Read: only well-formed data lines of the first dump; t is the tag clock
#      (s since tag start, so Moth7 starts at ~14 s). Sampling rate from the
#      tag clock and the line counter, which skips over dropped lines.
#   2. Flight vs. not with flight_segment() (running dynamic SD with
#      hysteresis), the window ~4 wingbeats of a ~28 Hz wingbeat. Flapping
#      flight is 2-5 g dynamic SD at 26-31 Hz; warm-up stays below ~1.5 g.
#      Bouts shorter than min_bout_s, without a clear wingbeat peak or above
#      max_wbf_hz (strong bursts within a warm-up, ~39 Hz in Moth9) are not
#      flight.
#        Per bout: duration, wingbeat frequency (summed-axis spectrum in band,
#      wbf_est() with a subharmonic check), dynamic SD, VeDBA, amplitude per
#      axis at the fundamental, 2nd-harmonic ratio, posture in the tag frame,
#      clipping, peak |a| at take-off and landing.
#        Per wingbeat: the bout is band-passed around its fundamental and
#      projected on its first PC; each upward zero crossing (interpolated
#      between samples, a wingbeat is only ~7.5 samples) starts a cycle. Per
#      cycle: frequency, VeDBA and peak-to-peak amplitude in the unfiltered
#      dynamic acceleration.
#   3. Warm-up (pre-flight wing shivering, the "charging up" files): 1-s
#      windows outside flight with dynamic SD between warm_lo and warm_hi and
#      a clear spectral peak at 20-55 Hz. Runs of such windows (gaps up to
#      warm_gap_s bridged) lasting >= warm_min_s are warm-up episodes. Per
#      episode: duration, frequency and amplitude at start and end, slope of
#      frequency over time, and whether it ends in take-off (a flight bout
#      starting within takeoff_s).
#
# Tag orientation is not standardised between individuals, so posture
# (pitch/roll) and per-axis amplitudes compare bouts within a recording.
#
# Outputs (CSV + PNG) go to 2024-Fall-Moth-Barn/Results/flight_dynamics on Dropbox.

library(tidyverse)
library(data.table)
library(patchwork)
library(signal)
source("./R/flea_functions.R")  # flight_segment(), wingbeat_freq()

# ---- paths & parameters -----------------------------------------------------
wb <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat"
moth_dir <- file.path(wb, "2024-Fall-Moth-Barn")
out_dir <- file.path(moth_dir, "Results/flight_dynamics")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

band <- c(18, 40)       # wingbeat fundamental search band in flight (Hz); bouts peak at 26-31 Hz
sub_frac <- 0.3         # a peak at f0 / 2 with >= this share of the power at f0 is the fundamental
seg_win_s <- 0.15       # flight_segment() window, ~4 wingbeats
on_g <- 1.8             # dynamic SD to enter flight (g); warm-up reaches ~1.5 g just before take-off
off_g <- 1              # ... and to leave it
peak_g <- 2.5           # a flight run must reach this dynamic SD somewhere
min_run_s <- 0.3        # shorter interior runs are merged into neighbours
min_bout_s <- 1         # shorter bouts are dropped (~28 wingbeats)
min_peak_frac <- 0.15   # share of 1-80 Hz power within +-1 Hz of the fundamental
max_wbf_hz <- 35        # bouts above this are strong warm-up bursts (flight is 25-31 Hz, warm-up up to ~50 Hz)
edge_s <- 0.3           # take-off / landing window for peak |a| (s)
clip_g <- 7.9           # |a| at or above this is clipped (+-8 g)
warm_band <- c(20, 55)  # warm-up frequency band (Hz); shivering rises to ~50 Hz
warm_win_s <- 1         # warm-up window ...
warm_step_s <- 0.25     # ... and step (s)
warm_lo <- 0.15         # dynamic SD range of warm-up windows (g); rest is < 0.1 g
warm_hi <- 1.8
warm_min_peak <- 0.25   # share of 5-100 Hz power within +-2 Hz of the warm-up peak
warm_gap_s <- 2         # gaps up to this are bridged
warm_min_s <- 3         # shorter runs are not warm-up episodes
takeoff_s <- 3          # an episode ending this close before a bout leads to take-off

hdr <- c("lineCnt", "timeMilliseconds", "burstCount", "accX_mg", "accY_mg", "accZ_mg",
         "ColorSensRed_cnt", "ColorSensGreen_cnt", "ColorSensBlue_cnt", "ColorSensIR_cnt")

# one row per recording; duplicates listed in the header are left out
files <- tribble(
  ~date,   ~moth,   ~file,                                                              ~note,
  "0917",  "Moth2", "20240917/Moth2_20240917_142421_01_niceFlight.csv",                 "nice flight",
  "0917",  "Moth3", "20240917/Moth3_20240917_151055_02_secondFlight.csv",               "second flight",
  "0918",  "Moth5", "20240918/Moth5_20240918_155032_FleaTagData.txt",                   "same data as Moth1_copy (Moth1 or Moth5)",
  "0919",  "Moth7", "20240919/Moth7_complete_20240919_124224_FleaTagData.txt",          "no header, first ~14 s missing",
  "0919",  "Moth9", "20240919/Moth9_20240919_144441_FleaTagData.txt",                   "",
  "0920",  "Moth5", "20240920/Moth5_didntfly_chargingup_20240920_121336_FleaTagData.txt", "didn't fly, charging up",
  "0920",  "Moth6", "20240920/Moth6_20240920_124103_FleaTagData.txt",                   "",
  "0920",  "Moth1", "20240920/Moth1_briefflight_20240920_130152_FleaTagData.txt",       "brief flight",
  "0920",  "Moth6", "20240920/Moth6_beautifulflight_20240920_133313_FleaTagData.txt",   "beautiful flight",
  "0920",  "Moth8", "20240920/Moth8_greatflight_lostballearlyon_20240920_152047_FleaTagData.txt", "great flight, lost ball early on",
  "0920",  "Moth4", "20240920/Moth4_chargingup_noflight_20240920_154955_FleaTagData.txt", "charging up, no flight"
) %>% mutate(time = str_match(file, "_\\d{8}_(\\d{4})\\d{2}_")[, 2],
             rec = paste(date, moth, time, sep = "-"))

# ---- reading ----------------------------------------------------------------
# data (x, y, z in g, t in s since tag start) and header info of one export;
# keeps the well-formed data lines before the first "Delete memory" line
read_moth <- function(path, rec) {
  l <- readLines(path, warn = FALSE)
  e <- grep("^Delete memory", l)[1]
  if (!is.na(e)) l <- l[seq_len(e - 1)]
  key <- function(k) {
    v <- grep(paste0("^", k, ":"), l, value = TRUE)[1]
    if (is.na(v)) NA_character_ else trimws(sub(paste0("^", k, ":"), "", v))
  }
  ok <- grepl("^\\d+,\\d+,\\d+,-?\\d+,-?\\d+,-?\\d+(,\\d*){4}$", l)
  if (sum(!ok & grepl("^\\d", l))) message(rec, ": dropped ", sum(!ok & grepl("^\\d", l)), " malformed lines")
  d <- fread(text = l[ok], header = FALSE, col.names = hdr)
  d <- d[c(TRUE, diff(lineCnt) > 0 & diff(timeMilliseconds) >= 0)]
  d[, `:=`(x = accX_mg / 1000, y = accY_mg / 1000, z = accZ_mg / 1000, t = timeMilliseconds / 1000)]
  sr <- (last(d$lineCnt) - d$lineCnt[1]) / (last(d$t) - d$t[1])
  list(data = d, rec = rec, tag = toupper(key("ID")), mode = key("FLEATAG_SC V11"),
       hz = as.numeric(key("AccHz")), sr = sr, n_gaps = sum(diff(d$lineCnt) > 1),
       n_missing = sum(diff(d$lineCnt) - 1) + d$lineCnt[1])
}

# ---- helpers ----------------------------------------------------------------
# summed-axis spectrum of a (n x 3), mean removed
axis_spec <- function(a, sr, spans = 3) {
  spec.pgram(ts(sweep(a, 2, colMeans(a)), frequency = sr), spans = spans, taper = 0.1,
             pad = 1, detrend = TRUE, plot = FALSE)
}

# share of f_lo-f_hi Hz power within +-w Hz of f0
peak_share <- function(s, f0, w = 1, f_lo = 1, f_hi = 80) {
  tot <- rowSums(as.matrix(s$spec))
  sum(tot[abs(s$freq - f0) <= w]) / sum(tot[s$freq >= f_lo & s$freq <= f_hi])
}

# wingbeat frequency: wingbeat_freq() in band, moved down to the subharmonic
# when that holds sub_frac of the peak power
wbf_est <- function(a, sr, band) {
  f0 <- wingbeat_freq(a, sr, band)
  if (f0 / 2 < band[1]) return(f0)
  s <- axis_spec(a, sr, spans = NULL)
  tot <- rowSums(as.matrix(s$spec))
  near <- abs(s$freq - f0 / 2) <= 1
  if (max(tot[near]) >= sub_frac * max(tot[abs(s$freq - f0) <= 0.5])) s$freq[near][which.max(tot[near])] else f0
}

bout_features <- function(a, sr, a_all, i0, i1) {
  s <- axis_spec(a, sr)
  P <- s$spec; fr <- s$freq
  f0 <- wbf_est(a, sr, band)
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
# signal band-passed around f0, interpolated between samples
bout_cycles <- function(a, sr, f0) {
  bp <- butter(2, c(0.6, 1.5) * f0 / (sr / 2), type = "pass")
  af <- apply(sweep(a, 2, colMeans(a)), 2, function(v) filtfilt(bp, v))
  pc <- prcomp(af)$x[, 1]
  up <- which(pc[-length(pc)] < 0 & pc[-1] >= 0)
  if (length(up) < 3) return(NULL)
  tc <- up + pc[up] / (pc[up] - pc[up + 1])  # fractional sample of each crossing
  dyn <- a - apply(a, 2, frollmean, n = round(sr / f0), align = "center", fill = NA)
  dyn[is.na(dyn)] <- 0
  vedba <- sqrt(rowSums(dyn^2))
  tibble(c0 = head(up, -1) + 1, c1 = tail(up, -1), period_s = diff(tc) / sr) %>%
    mutate(freq_hz = 1 / period_s,
           vedba = map2_dbl(c0, c1, ~ mean(vedba[.x:.y])),
           p2p_x = map2_dbl(c0, c1, ~ diff(range(dyn[.x:.y, 1]))),
           p2p_y = map2_dbl(c0, c1, ~ diff(range(dyn[.x:.y, 2]))),
           p2p_z = map2_dbl(c0, c1, ~ diff(range(dyn[.x:.y, 3]))),
           t_in_bout = head(tc, -1) / sr)
}

# sliding windows over the whole recording: dynamic SD and the warm-up peak;
# also the spectrogram (summed-axis power, 0-fmax Hz)
windows <- function(a, t, sr, fmax = 70) {
  w <- round(warm_win_s * sr); st <- round(warm_step_s * sr)
  starts <- seq(1, nrow(a) - w + 1, by = st)
  res <- map(starts, function(i) {
    b <- a[i:(i + w - 1), ]
    s <- axis_spec(b, sr, spans = NULL)
    tot <- rowSums(as.matrix(s$spec))
    inb <- s$freq >= warm_band[1] & s$freq <= warm_band[2]
    f0 <- s$freq[inb][which.max(tot[inb])]
    # 2nd harmonic of a ~30 Hz shiver -> its fundamental
    sub <- abs(s$freq - f0 / 2) <= 1
    if (f0 / 2 >= warm_band[1] && max(tot[sub]) >= sub_frac * max(tot[abs(s$freq - f0) <= 0.5]))
      f0 <- s$freq[sub][which.max(tot[sub])]
    keep <- s$freq <= fmax
    list(win = tibble(i0 = i, i1 = i + w - 1, t = t[i + w %/% 2],
                      dyn_sd = sqrt(sum(apply(b, 2, var))), warm_hz = f0,
                      warm_peak_frac = peak_share(s, f0, w = 2, f_lo = 5, f_hi = 100)),
         spec = tibble(t = t[i + w %/% 2], freq = s$freq[keep], power = tot[keep]))
  })
  list(win = map_dfr(res, "win"), spec = map_dfr(res, "spec"))
}

# TRUE runs of v separated by gaps of <= gap FALSE values are joined
bridge <- function(v, gap) {
  r <- rle(v)
  fill <- !r$values & r$lengths <= gap & seq_along(r$values) > 1 & seq_along(r$values) < length(r$values)
  r$values[fill] <- TRUE
  inverse.rle(r)
}

# continuous recording -> samples, bouts, cycles, windows, warm-up episodes, spectrogram
process_rec <- function(r) {
  d <- r$data; sr <- r$sr
  a <- cbind(d$x, d$y, d$z)
  fs <- flight_segment(d$x, d$y, d$z, sr, win_s = seg_win_s, on_g = on_g, off_g = off_g,
                       peak_g = peak_g, min_run_s = min_run_s)
  fly <- coalesce(fs$fly, FALSE)
  rl <- rle(fly); e <- cumsum(rl$lengths); b <- e - rl$lengths + 1
  cand <- tibble(i0 = b[rl$values], i1 = e[rl$values]) %>%
    filter((i1 - i0 + 1) / sr >= min_bout_s)
  bt <- pmap_dfr(cand, function(i0, i1) bout_features(a[i0:i1, ], sr, a, i0, i1) %>% mutate(i0 = i0, i1 = i1))
  fly2 <- rep(FALSE, nrow(d)); bout_id <- rep(0L, nrow(d)); cy <- tibble()
  if (nrow(bt)) {
    # bouts without a clear wingbeat stay in the table (wingbeat = FALSE) but are not flight
    bt <- bt %>% mutate(t_start = d$t[i0], t_end = d$t[i1], duration_s = (i1 - i0 + 1) / sr,
                        wingbeat = peak_frac >= min_peak_frac & wbf_hz <= max_wbf_hz, bout = cumsum(wingbeat) * wingbeat)
    keep <- bt %>% filter(wingbeat)
    for (k in seq_len(nrow(keep))) {
      fly2[keep$i0[k]:keep$i1[k]] <- TRUE
      bout_id[keep$i0[k]:keep$i1[k]] <- keep$bout[k]
    }
    cy <- keep %>% pmap_dfr(function(i0, i1, wbf_hz, bout, ...) {
      cc <- bout_cycles(a[i0:i1, ], sr, wbf_hz)
      if (is.null(cc)) return(NULL)
      # crossings from noise in the filter output give cycles outside the pass band
      cc %>% filter(between(freq_hz, 0.6 * wbf_hz, 1.5 * wbf_hz)) %>%
        mutate(bout = bout, t_to_end = (i1 - i0 + 1) / sr - t_in_bout)
    })
  }
  # warm-up windows and episodes
  ws <- windows(a, d$t, sr)
  win <- ws$win %>%
    mutate(fly_frac = map2_dbl(i0, i1, ~ mean(fly2[.x:.y])),
           warm = fly_frac < 0.5 & between(dyn_sd, warm_lo, warm_hi) & warm_peak_frac >= warm_min_peak)
  win$warm_run <- bridge(win$warm, round(warm_gap_s / warm_step_s))
  rw <- rle(win$warm_run); e <- cumsum(rw$lengths); b <- e - rw$lengths + 1
  takeoffs <- if (nrow(bt)) bt %>% filter(wingbeat) %>% pull(t_start) else numeric(0)
  ep <- tibble(w0 = b[rw$values], w1 = e[rw$values]) %>%
    mutate(t_start = win$t[w0] - warm_win_s / 2, t_end = win$t[w1] + warm_win_s / 2,
           duration_s = t_end - t_start) %>%
    filter(duration_s >= warm_min_s)
  if (nrow(ep)) {
    ep <- ep %>% mutate(episode = row_number()) %>%
      pmap_dfr(function(w0, w1, t_start, t_end, duration_s, episode) {
        ww <- win[w0:w1, ] %>% filter(warm)
        h <- min(2, duration_s / 3)  # start / end stretch (s)
        nxt <- takeoffs[takeoffs >= t_end - warm_win_s]
        tibble(episode = episode, t_start = t_start, t_end = t_end, duration_s = duration_s,
               warm_frac = nrow(ww) / (w1 - w0 + 1),
               warm_hz = median(ww$warm_hz),
               hz_start = median(ww$warm_hz[ww$t <= t_start + h + warm_win_s / 2]),
               hz_end = median(ww$warm_hz[ww$t >= t_end - h - warm_win_s / 2]),
               hz_slope = if (nrow(ww) >= 3) unname(coef(lm(warm_hz ~ t, ww))[2]) else NA_real_,
               dyn_start = median(ww$dyn_sd[ww$t <= t_start + h + warm_win_s / 2]),
               dyn_end = median(ww$dyn_sd[ww$t >= t_end - h - warm_win_s / 2]),
               next_takeoff_s = if (length(nxt)) min(nxt) - t_end else NA_real_)
      }) %>% mutate(takeoff = coalesce(next_takeoff_s <= takeoff_s, FALSE))
    for (k in seq_len(nrow(ep))) win$episode[win$t >= ep$t_start[k] & win$t <= ep$t_end[k]] <- ep$episode[k]
  }
  ep <- as_tibble(ep) %>% select(-any_of(c("w0", "w1")))
  if (!"episode" %in% names(win)) win$episode <- NA_integer_
  list(samples = tibble(rec = r$rec, t = d$t, x = d$x, y = d$y, z = d$z, dyn = fs$dyn, fly = fly2, bout = bout_id),
       bouts = bt %>% mutate(rec = r$rec), cycles = cy %>% mutate(rec = r$rec),
       windows = win %>% mutate(rec = r$rec), warmup = ep %>% mutate(rec = r$rec),
       spec = ws$spec %>% mutate(rec = r$rec))
}

# ---- read & process ---------------------------------------------------------
recs <- map2(file.path(moth_dir, files$file), files$rec, read_moth) %>% set_names(files$rec)
rec_tab <- files %>% select(rec, date, moth, note, file) %>%
  left_join(map_dfr(recs, ~ tibble(rec = .x$rec, tag = .x$tag, hz = .x$hz, sr = .x$sr,
                                   n = nrow(.x$data), t_first = first(.x$data$t), t_last = last(.x$data$t),
                                   n_gaps = .x$n_gaps, n_missing = .x$n_missing)), by = "rec")
print(rec_tab %>% select(-file), n = Inf, width = Inf)

res <- map(rec_tab$rec, function(k) { message(k); process_rec(recs[[k]]) }) %>% set_names(rec_tab$rec)
samples <- map_dfr(res, "samples"); bouts <- map_dfr(res, "bouts"); cycles <- map_dfr(res, "cycles")
wins <- map_dfr(res, "windows"); warmup <- map_dfr(res, "warmup"); specs <- map_dfr(res, "spec")
fl <- bouts %>% filter(wingbeat)

# warm-up episodes that end in take-off, with the first flight bout after them
warm_to <- warmup %>% filter(takeoff) %>%
  left_join(fl %>% select(rec, bout, bout_start = t_start, bout_wbf = wbf_hz, bout_s = duration_s), by = "rec",
            relationship = "many-to-many") %>%
  filter(bout_start >= t_end - warm_win_s) %>% group_by(rec, episode) %>% slice_min(bout_start, n = 1) %>%
  ungroup()
# first 1 s of flight after a warm-up, from the cycles
first_s <- cycles %>% filter(t_in_bout <= 1) %>% group_by(rec, bout) %>%
  summarise(hz_first_s = median(freq_hz), .groups = "drop")
warm_to <- warm_to %>% left_join(first_s, by = c("rec", "bout"))

indiv <- rec_tab %>% select(rec, date, moth, tag, note, t_first, t_last) %>%
  mutate(recorded_s = t_last - t_first, .keep = "unused") %>%
  left_join(samples %>% group_by(rec) %>% summarise(flight_s = sum(fly) / recs[[rec[1]]]$sr, flight_frac = mean(fly)),
            by = "rec") %>%
  left_join(fl %>% group_by(rec) %>%
              summarise(n_bouts = n(), bout_median_s = median(duration_s), bout_max_s = max(duration_s),
                        wbf_hz = weighted.mean(wbf_hz, duration_s),
                        dyn_sd = weighted.mean(dyn_sd, duration_s),
                        vedba = weighted.mean(vedba, duration_s),
                        harm_ratio = weighted.mean(harm_ratio, duration_s),
                        clip_frac = weighted.mean(clip_frac, duration_s),
                        takeoff_peak_g = median(takeoff_peak_g), landing_peak_g = median(landing_peak_g)),
            by = "rec") %>%
  left_join(cycles %>% group_by(rec) %>%
              summarise(n_cycles = n(), cycle_freq_median = median(freq_hz),
                        cycle_freq_cv = sd(freq_hz) / mean(freq_hz)), by = "rec") %>%
  left_join(warmup %>% group_by(rec) %>%
              summarise(n_warmup = n(), warmup_s = sum(duration_s), warmup_hz = weighted.mean(warm_hz, duration_s),
                        n_warmup_takeoff = sum(takeoff)), by = "rec") %>%
  mutate(across(c(n_bouts, n_warmup, n_warmup_takeoff), ~ coalesce(as.integer(.x), 0L)),
         warmup_s = coalesce(warmup_s, 0))

# ---- outputs ----------------------------------------------------------------
write_csv(rec_tab, file.path(out_dir, "recordings.csv"))
write_csv(bouts %>% select(rec, bout, wingbeat, t_start, t_end, duration_s, everything(), -i0, -i1),
          file.path(out_dir, "bouts.csv"))
write_csv(cycles %>% select(rec, bout, t_in_bout, t_to_end, everything(), -c0, -c1),
          file.path(out_dir, "wingbeat_cycles.csv"))
write_csv(warmup %>% select(rec, everything()), file.path(out_dir, "warmup_episodes.csv"))
write_csv(warm_to %>% select(rec, episode, t_end, hz_end, dyn_end, bout, bout_start, bout_wbf, hz_first_s, bout_s),
          file.path(out_dir, "warmup_to_takeoff.csv"))
write_csv(wins %>% select(rec, t, dyn_sd, warm_hz, warm_peak_frac, fly_frac, warm, warm_run, episode),
          file.path(out_dir, "windows.csv"))
write_csv(indiv, file.path(out_dir, "recording_summary.csv"))
print(indiv, width = Inf, n = Inf)
print(warmup, width = Inf, n = Inf)

# ---- figures ----------------------------------------------------------------
theme_set(theme_bw(base_size = 11))

# 1. per recording: dynamic SD with flight (blue) and warm-up (orange) + spectrogram
for (k in rec_tab$rec) {
  s <- samples %>% filter(rec == k) %>% slice(seq(1, n(), by = 4))
  bb <- fl %>% filter(rec == k); ww <- warmup %>% filter(rec == k)
  p1 <- ggplot(s, aes(t, dyn)) +
    geom_rect(data = bb, aes(xmin = t_start, xmax = t_end, ymin = 0, ymax = Inf),
              inherit.aes = FALSE, fill = "steelblue", alpha = 0.25) +
    geom_rect(data = ww, aes(xmin = t_start, xmax = t_end, ymin = 0, ymax = Inf),
              inherit.aes = FALSE, fill = "darkorange", alpha = 0.25) +
    geom_line(linewidth = 0.3, na.rm = TRUE) +
    geom_hline(yintercept = c(off_g, on_g), linetype = 2, colour = "grey50") +
    labs(x = NULL, y = "Dynamic SD (g)",
         title = paste0(k, if_else(rec_tab$note[rec_tab$rec == k] == "", "", paste0(" (", rec_tab$note[rec_tab$rec == k], ")")),
                        ": flight (blue), warm-up (orange)"))
  sp <- specs %>% filter(rec == k)
  lim <- quantile(log10(sp$power[sp$power > 0]), c(0.05, 0.995))
  p2 <- ggplot(sp, aes(t, freq, fill = log10(pmax(power, 1e-12)))) + geom_raster() +
    scale_fill_viridis_c(name = "log10\npower", limits = lim, oob = scales::squish) +
    geom_point(data = wins %>% filter(rec == k, warm), aes(t, warm_hz), inherit.aes = FALSE,
               colour = "darkorange", size = 0.5) +
    geom_point(data = bb, aes(x = (t_start + t_end) / 2, y = wbf_hz), inherit.aes = FALSE,
               colour = "white", shape = 4) +
    labs(x = "Time since tag start (s)", y = "Frequency (Hz)")
  ggsave(file.path(out_dir, paste0("overview_", k, ".png")), p1 / p2, width = 11, height = 6, dpi = 150)
}

# 2. bout level
p_wbf <- ggplot(fl, aes(rec, wbf_hz)) + geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(size = duration_s), width = 0.15, alpha = 0.6) +
  labs(x = NULL, y = "Wingbeat frequency (Hz)", size = "Bout (s)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p_dur <- ggplot(fl, aes(duration_s, wbf_hz, colour = rec)) + geom_point() +
  scale_x_log10() + labs(x = "Bout duration (s)", y = "Wingbeat frequency (Hz)")
p_amp <- ggplot(fl, aes(wbf_hz, dyn_sd, colour = rec)) + geom_point(aes(size = duration_s)) +
  labs(x = "Wingbeat frequency (Hz)", y = "Dynamic SD (g)", size = "Bout (s)")
p_hist <- ggplot(fl, aes(duration_s)) + geom_histogram(bins = 20) +
  labs(x = "Bout duration (s)", y = "Bouts")
ggsave(file.path(out_dir, "bouts.png"), (p_wbf | p_dur) / (p_amp | p_hist) + plot_layout(guides = "collect"),
       width = 12, height = 9, dpi = 150)

# 3. within-bout time course of the wingbeat cycles
tc <- function(df, xvar, xlab, yvar, ylab) {
  ggplot(df, aes({{ xvar }}, {{ yvar }}, colour = rec)) +
    geom_point(alpha = 0.1, size = 0.4) + geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
    labs(x = xlab, y = ylab)
}
on5 <- cycles %>% filter(t_in_bout <= 5); off5 <- cycles %>% filter(t_to_end <= 5) %>% mutate(tl = -t_to_end)
ggsave(file.path(out_dir, "within_bout.png"),
       (tc(on5, t_in_bout, "Time since take-off (s)", freq_hz, "Cycle frequency (Hz)") |
          tc(off5, tl, "Time to landing (s)", freq_hz, "Cycle frequency (Hz)")) /
         (tc(on5, t_in_bout, "Time since take-off (s)", vedba, "Cycle VeDBA (g)") |
            tc(off5, tl, "Time to landing (s)", vedba, "Cycle VeDBA (g)")) +
         plot_layout(guides = "collect"),
       width = 12, height = 8, dpi = 150)

# 4. cycle to cycle
p_cf <- ggplot(cycles, aes(freq_hz, colour = rec)) + geom_density() + coord_cartesian(xlim = c(15, 45)) +
  labs(x = "Cycle frequency (Hz)", y = "Density")
p_fa <- ggplot(cycles %>% filter(between(freq_hz, 15, 45)), aes(freq_hz, vedba, colour = rec)) +
  geom_point(alpha = 0.1, size = 0.4) + geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
  labs(x = "Cycle frequency (Hz)", y = "Cycle VeDBA (g)")
ggsave(file.path(out_dir, "cycles.png"), (p_cf | p_fa) + plot_layout(guides = "collect"),
       width = 12, height = 5, dpi = 150)

# 5. warm-up: frequency and amplitude over time, aligned to the end of each episode
ww <- wins %>% filter(warm, !is.na(episode)) %>%
  left_join(warmup %>% select(rec, episode, t_end, takeoff), by = c("rec", "episode")) %>%
  mutate(t_rel = t - t_end, id = paste(rec, episode),
         end = if_else(takeoff, "ends in take-off", "no take-off"))
p_wf <- ggplot(ww, aes(t_rel, warm_hz, colour = rec, group = id)) + geom_point(size = 0.6, alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.75, linewidth = 0.6) +
  facet_wrap(~end) + labs(x = "Time to end of warm-up (s)", y = "Warm-up frequency (Hz)")
p_wa <- ggplot(ww, aes(t_rel, dyn_sd, colour = rec, group = id)) + geom_point(size = 0.6, alpha = 0.5) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, span = 0.75, linewidth = 0.6) +
  facet_wrap(~end) + labs(x = "Time to end of warm-up (s)", y = "Dynamic SD (g)")
p_wt <- ggplot(warm_to, aes(hz_end, coalesce(hz_first_s, bout_wbf), colour = rec)) +
  geom_abline(linetype = 2) + geom_point(aes(size = duration_s)) +
  labs(x = "Warm-up frequency, last 2 s (Hz)", y = "Wingbeat frequency, first 1 s of flight (Hz)",
       size = "Warm-up (s)")
p_wd <- ggplot(warmup, aes(duration_s, hz_slope, colour = rec, shape = takeoff)) + geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Warm-up duration (s)", y = "Frequency change (Hz/s)", shape = "Take-off")
ggsave(file.path(out_dir, "warmup.png"), p_wf / p_wa / (p_wt | p_wd) + plot_layout(guides = "collect"),
       width = 12, height = 12, dpi = 150)

saveRDS(list(recordings = rec_tab, samples = samples, bouts = bouts, cycles = cycles, windows = wins,
             warmup = warmup, warm_to = warm_to, indiv = indiv,
             params = list(band = band, seg_win_s = seg_win_s, on_g = on_g, off_g = off_g, peak_g = peak_g,
                           min_run_s = min_run_s, min_bout_s = min_bout_s, min_peak_frac = min_peak_frac, max_wbf_hz = max_wbf_hz,
                           warm_band = warm_band, warm_win_s = warm_win_s, warm_lo = warm_lo, warm_hi = warm_hi,
                           warm_min_peak = warm_min_peak, warm_gap_s = warm_gap_s, warm_min_s = warm_min_s,
                           takeoff_s = takeoff_s)),
        file.path(out_dir, "flight_dynamics.rds"))
