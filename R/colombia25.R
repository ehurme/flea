# Colombia 2025 (Finca, BiC) flight-cage trials: do bats change
# acceleration-derived flight metrics between trials (i.e. with added tag load)?
# Same analysis as R/flanders25.R, adapted to the Colombia file names and
# metadata sheet.
#
# Design: each bat flew trial 1 with a dummy housing only (no FleaTag, no ACC
# data), then trials 2-4 (sometimes 5) with a FleaTag (0.34 g) + housing of
# varying weight + velcro. Species: Dermanura bogotensis, Carollia
# perspicillata, Eptesicus brasiliensis, Artibeus planirostris, Platyrrhinus
# dorsalis.
#
# Clock drift: as in Flanders, the FleaTag writes timeMilliseconds from the
# NOMINAL 105 Hz (full files are 42935 samples / 408.895 s), so wingbeat
# frequency (Hz) and bout duration (s) are scaled by each tag's unknown clock
# error. Tag ID is a random effect in the models to absorb that bias. VeDBA,
# VeSBA, ODBA, heave amplitude, posture, flight fraction and wingbeats per
# bout are drift-free.
#
# Metadata: Finca_Flights_2025.xlsx, sheet "FlightCage". There, "bat weight*"
# excludes the velcro (total weight = bat + tag + housing + velcro), so
# load = tag + housing + velcro.
#
# Outputs (CSV + PDF) go to out_dir, outside the repo.

library(tidyverse)
library(data.table)
library(readxl)
# signal is used as signal::butter()/filtfilt(), not attached: attaching it
# masks dplyr::filter() (depending on load order), which breaks filter() below
library(lme4)
source("./R/flea_functions.R")  # get_true_groups()
filter <- dplyr::filter  # guard: signal::filter() may mask it if signal is attached in the session

# ---- paths & parameters -----------------------------------------------------
data_root <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Data"
meta_xlsx <- file.path(data_root, "Finca_Flights_2025.xlsx")
out_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Results/acc_trial_comparison"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sr <- 105                # nominal sampling rate (Hz), all files
static_window_s <- 1     # running mean for static acceleration
flight_vedba_g <- 0.8    # 1 s rolling-mean VeDBA threshold (same as Flanders)
min_bout_s <- 2          # keep bouts at least this long (after trimming)
merge_gap_s <- 0.5       # merge bouts separated by shorter gaps
trim_s <- 0.5            # trim take-off / landing from each bout end
wbf_band <- c(5, 25)     # Hz, search band for wingbeat frequency
clip_g <- 7.9            # |acc| at/above this counts as clipped (8 g range)
tag_mass_g <- 0.34       # FleaTag, used when the sheet leaves it blank
min_flight_frac <- 0.10  # exclude trials with less of the recording in flight
                         # (flight_frac is continuous above ~0.05, no natural
                         # break; trials below 0.10 match field notes: tag on
                         # floor, bat refused to fly)

species_lookup <- c("Db" = "Dermanura bogotensis", "Cp" = "Carollia perspicillata",
                    "Eb" = "Eptesicus brasiliensis", "Ap" = "Artibeus planirostris",
                    "Pd" = "Platyrrhinus dorsalis")

# bat IDs that differ between files and the sheet (file ID = sheet ID)
bat_id_fix <- c("221" = 222L)

# ---- file discovery & file-name metadata -----------------------------------
# e.g. 20250213_224550_303D_Db_193_Trial2.txt
# (the folder also holds .csv copies, ppg_*.txt heart-rate files, photos and
#  a 3D-calibration folder, which the pattern skips)
name_re <- "^(\\d{8})_(\\d{6})_([0-9A-Fa-f]{4})_([A-Za-z]{2})_(\\d+)_Trial(\\d+)[.]txt$"
files <- list.files(data_root, pattern = "_Trial\\d+[.]txt$", recursive = TRUE, full.names = TRUE)

file_meta <- tibble(path = files, file = basename(files)) %>%
  mutate(m = str_match(file, name_re),
         file_date = as.Date(m[, 2], "%Y%m%d"),
         night = basename(dirname(path)),       # capture night (folder)
         file_tag = toupper(m[, 4]),
         species_code = m[, 5],
         species = unname(species_lookup[species_code]),
         bat = as.integer(m[, 6]),
         trial = as.integer(m[, 7]),
         trial_label = as.character(trial)) %>%
  select(-m)
if (any(is.na(file_meta$bat))) {
  warning("Unparsed file names:\n", paste(file_meta$file[is.na(file_meta$bat)], collapse = "\n"))
}

# ---- reader for FleaTag exports --------------------------------------------
# Same as flanders25.R: handles serial logs with several download blocks by
# taking the largest block whose ID matches the tag (latest if tied). All
# Colombia files checked so far are single-block, but keep the guard. The ID
# in the file header is the tag actually read, so use it as the tag.
read_flea_block <- function(path, tag) {
  lines <- readLines(path, warn = FALSE)
  id_idx <- grep("^ID:", lines)
  hdr <- grep("^lineCnt", lines)
  stops <- grep("Delete memory by pressing button|Stopped reading", lines)
  blocks <- tibble(start = hdr) %>%
    mutate(end = map_int(start, ~ min(c(stops[stops > .x], length(lines) + 1))) - 1L,
           id = map_chr(start, ~ toupper(trimws(sub("^ID:", "", lines[max(id_idx[id_idx < .x])])))),
           n = end - start)
  hit <- which(blocks$id == toupper(tag))
  cand <- if (length(hit)) blocks[hit, ] else blocks
  b <- cand[max(which(cand$n == max(cand$n))), ]
  # drop malformed rows (wrong field count): fread otherwise stops reading at
  # the first one (e.g. line 35627 of 20250213_224550_303D_Db_193_Trial2.txt)
  blk <- lines[b$start:b$end]
  n_fields <- lengths(regmatches(blk, gregexpr(",", blk))) + 1
  ok <- n_fields == n_fields[1]
  d <- fread(text = blk[ok], select = c("accX_mg", "accY_mg", "accZ_mg"))
  list(acc = as.matrix(d) / 1000,
       n_bad_rows = sum(!ok),
       n_blocks = nrow(blocks),
       block_id = b$id,
       data_hash = rlang::hash(lines[(b$start + 1):b$end]))
}

# ---- per-trial processing ---------------------------------------------------
# dominant frequency of x (Hz, nominal clock) within band
dominant_freq <- function(x, band = wbf_band) {
  s <- spec.pgram(ts(x, frequency = sr), taper = 0.1, pad = 3, detrend = TRUE,
                  spans = 3, plot = FALSE)
  ok <- s$freq >= band[1] & s$freq <= band[2]
  s$freq[ok][which.max(s$spec[ok])]
}

bp <- signal::butter(4, wbf_band / (sr / 2), type = "pass")

process_trial <- function(path, tag) {
  r <- read_flea_block(path, tag)
  a <- r$acc
  w <- round(sr * static_window_s)
  stat <- apply(a, 2, frollmean, n = w, align = "center")
  dyn <- a - stat
  vedba <- sqrt(rowSums(dyn^2))
  vesba <- sqrt(rowSums(stat^2))
  odba <- rowSums(abs(dyn))
  pitch <- atan2(stat[, 2], sqrt(stat[, 1]^2 + stat[, 3]^2)) * 180 / pi
  roll <- atan2(stat[, 1], sqrt(stat[, 2]^2 + stat[, 3]^2)) * 180 / pi
  roll_vedba <- frollmean(vedba, w, align = "center")

  # flight bouts
  fly <- !is.na(roll_vedba) & roll_vedba > flight_vedba_g
  g <- get_true_groups(fly)
  if (nrow(g) > 1) {  # merge short gaps
    gap <- g$start[-1] - g$end[-nrow(g)]
    grp <- cumsum(c(TRUE, gap > merge_gap_s * sr))
    g <- data.frame(start = tapply(g$start, grp, min), end = tapply(g$end, grp, max))
  }
  g$start <- g$start + round(trim_s * sr)
  g$end <- g$end - round(trim_s * sr)
  g <- g[(g$end - g$start + 1) >= min_bout_s * sr, , drop = FALSE]

  bouts <- map_dfr(seq_len(nrow(g)), function(i) {
    idx <- g$start[i]:g$end[i]
    d <- dyn[idx, ]
    # first PC of dynamic acceleration: orientation-independent wingbeat signal
    pc1 <- prcomp(d, center = TRUE)$x[, 1]
    wbf <- dominant_freq(pc1)
    heave <- signal::filtfilt(bp, pc1)
    dur <- length(idx) / sr
    tibble(bout = i, start_sample = g$start[i], n_samples = length(idx),
           duration_s = dur,
           wbf_hz = wbf,
           n_wingbeats = wbf * dur,
           amp_pc1_g = 2 * sqrt(2) * sd(heave),   # peak-to-peak of equivalent sinusoid
           median_vedba = median(vedba[idx]), mean_vedba = mean(vedba[idx]),
           median_vesba = median(vesba[idx]), median_odba = median(odba[idx]),
           median_pitch = median(pitch[idx]), median_roll = median(roll[idx]))
  })

  flight_idx <- unlist(map2(g$start, g$end, seq))
  qc <- tibble(n_samples = nrow(a), n_bad_rows = r$n_bad_rows,
               n_blocks = r$n_blocks, block_id = r$block_id,
               data_hash = r$data_hash,
               clipped_frac_flight = if (length(flight_idx)) mean(abs(a[flight_idx, ]) >= clip_g) else NA,
               rest_vesba = median(vesba[!fly], na.rm = TRUE))
  list(bouts = bouts, qc = qc,
       summary = tibble(
         n_bouts = nrow(g),
         flight_frac = length(flight_idx) / nrow(a),
         flight_time_s = length(flight_idx) / sr,
         median_vedba = median(vedba[flight_idx]),
         median_vesba = median(vesba[flight_idx]),
         median_odba = median(odba[flight_idx]),
         median_pitch = median(pitch[flight_idx]),
         median_roll = median(roll[flight_idx]),
         # bout-level values, weighted by bout length
         wbf_hz = if (nrow(bouts)) weighted.mean(bouts$wbf_hz, bouts$n_samples) else NA,
         amp_pc1_g = if (nrow(bouts)) weighted.mean(bouts$amp_pc1_g, bouts$n_samples) else NA,
         median_wingbeats_per_bout = if (nrow(bouts)) median(bouts$n_wingbeats) else NA,
         median_bout_s = if (nrow(bouts)) median(bouts$duration_s) else NA))
}

results <- map(set_names(file_meta$path, file_meta$file), function(p) {
  message("processing ", basename(p))
  process_trial(p, file_meta$file_tag[file_meta$path == p])
})

# tag = ID read from the file header (file names occasionally have the wrong tag)
trial_summary <- file_meta %>%
  bind_cols(map_dfr(results, "summary"), map_dfr(results, "qc")) %>%
  mutate(tag = block_id)
bout_summary <- map_dfr(results, "bouts", .id = "file") %>%
  left_join(select(trial_summary, file, bat, species, trial, trial_label, tag), by = "file")

# ---- metadata: loads --------------------------------------------------------
num <- function(x) suppressWarnings(as.numeric(x))
flight_meta <- read_excel(meta_xlsx, sheet = "FlightCage") %>%
  transmute(bat = as.integer(bat),
            trial = num(trial),                       # "2.0"; "Calibration"/"HR" -> NA
            bat_mass_g = num(`bat weight*`),
            tag_g = num(`tag weight`),
            housing_g = num(`housing weight`),
            velcro_g = num(`velco weight`),
            sheet_tag = toupper(tagID),
            sheet_comment = comments) %>%
  filter(!is.na(bat), trial >= 2) %>%
  mutate(bat = coalesce(unname(bat_id_fix[as.character(bat)]), bat),
         trial = as.integer(trial),
         tag_g = coalesce(tag_g, tag_mass_g),
         load_g = tag_g + housing_g + velcro_g)

trial_summary <- trial_summary %>%
  left_join(flight_meta, by = c("bat", "trial")) %>%
  mutate(load_pct = 100 * load_g / bat_mass_g,
         tag_mismatch = !is.na(sheet_tag) & sheet_tag != tag,
         file_tag_mismatch = file_tag != tag)

# ---- QC flags ---------------------------------------------------------------
# identical data under two names = same tag memory downloaded twice; cannot
# tell which bat it belongs to, so both copies are excluded
trial_summary <- trial_summary %>%
  group_by(data_hash) %>% mutate(duplicate_data = n() > 1) %>% ungroup() %>%
  mutate(qc_note = str_c(
    if_else(duplicate_data, "identical data in another file; ", "", ""),
    if_else(n_blocks > 1, str_glue("multi-block export ({n_blocks} blocks), used largest {block_id} block; "), "", ""),
    if_else(n_bad_rows > 0, str_glue("{n_bad_rows} malformed row(s) dropped; "), "", ""),
    if_else(file_tag_mismatch, str_glue("file name says tag {file_tag}, header says {tag}; "), "", ""),
    if_else(tag_mismatch, "tag in header differs from sheet; ", "", ""),
    if_else(is.na(load_pct), "load unknown (sheet incomplete); ", "", ""),
    if_else(n_samples < 0.5 * 42935, "short recording (tag lost?); ", "", ""),
    if_else(n_bouts == 0, "no flight detected (tag stationary?); ",
            if_else(n_bouts < 3, "few flight bouts; ", "", ""), ""),
    if_else(n_bouts > 0 & flight_frac < min_flight_frac,
            str_glue("flight < {100 * min_flight_frac}% of recording; "), "", ""),
    if_else(clipped_frac_flight > 0.01, "clipping >1% of flight samples; ", "", "")),
    include = !duplicate_data & n_bouts >= 1 & flight_frac >= min_flight_frac)

write_csv(trial_summary %>% select(-data_hash), file.path(out_dir, "trial_summary.csv"))
write_csv(bout_summary, file.path(out_dir, "bout_summary.csv"))
trial_summary %>% filter(qc_note != "") %>% select(file, qc_note, sheet_comment) %>% print(n = Inf)

# ---- statistics: trial and load effects -------------------------------------
metrics <- c(median_vedba = "Median VeDBA in flight (g)",
             median_vesba = "Median VeSBA in flight (g)",
             median_odba = "Median ODBA in flight (g)",
             amp_pc1_g = "Heave amplitude, PC1 peak-to-peak (g)",
             wbf_hz = "Wingbeat frequency (Hz, tag clock)*",
             log10_wingbeats_per_bout = "log10 wingbeats per bout (median)",
             flight_frac = "Fraction of recording in flight",
             n_bouts = "Number of flight bouts",
             median_pitch = "Median pitch in flight (deg)")

dat <- trial_summary %>% filter(include) %>%
  mutate(log10_wingbeats_per_bout = log10(median_wingbeats_per_bout),
         trial_f = factor(trial), bat_f = factor(bat), tag_f = factor(tag))

# Mixed model, likelihood-ratio test of a fixed effect against the null,
# random intercepts for bat (repeated measures) and tag (clock / sensor bias).
# Several species here, so species is a fixed covariate in both models.
lrt <- function(data, y, term) {
  d <- data %>% filter(!is.na(.data[[y]]), !is.na(.data[[term]]))
  if (n_distinct(d$bat_f) < 3) return(NULL)
  sp <- if (n_distinct(d$species) > 1) " + species" else ""
  f0 <- as.formula(str_glue("{y} ~ 1{sp} + (1|bat_f) + (1|tag_f)"))
  f1 <- update(f0, as.formula(str_glue(". ~ . + {term}")))
  m0 <- suppressMessages(lmer(f0, data = d, REML = FALSE))
  m1 <- suppressMessages(lmer(f1, data = d, REML = FALSE))
  a <- anova(m0, m1)
  fe <- fixef(m1)
  fe <- fe[str_starts(names(fe), term)]
  tibble(metric = y, term = term, n_trials = nrow(d), n_bats = n_distinct(d$bat_f),
         chisq = a$Chisq[2], df = a$Df[2], p = a$`Pr(>Chisq)`[2],
         effect = paste(sprintf("%s=%.3g", names(fe), fe), collapse = "; "),
         singular = isSingular(m1))
}

# Friedman test on bats with all of trials 2-4 (trial 5 ignored): robust check
friedman_trials <- function(data, y) {
  w <- data %>% filter(trial %in% 2:4) %>%
    group_by(bat, trial) %>% summarise(v = mean(.data[[y]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = trial, values_from = v) %>% drop_na()
  if (nrow(w) < 3 || !all(as.character(2:4) %in% names(w))) return(NULL)
  ft <- friedman.test(as.matrix(w[, as.character(2:4)]))
  tibble(metric = y, n_bats_complete = nrow(w), friedman_chisq = unname(ft$statistic),
         friedman_p = ft$p.value)
}

stats_trial <- map_dfr(names(metrics), ~ lrt(dat, .x, "trial_f"))
stats_load <- map_dfr(names(metrics), ~ lrt(filter(dat, !is.na(load_pct)), .x, "load_pct"))
stats_friedman <- map_dfr(names(metrics), ~ friedman_trials(dat, .x))
stats_all <- bind_rows(stats_trial, stats_load) %>%
  group_by(term) %>% mutate(p_holm = p.adjust(p, "holm")) %>% ungroup() %>%
  left_join(mutate(stats_friedman, term = "trial_f"), by = c("metric", "term"))
write_csv(stats_all, file.path(out_dir, "trial_load_stats.csv"))
print(stats_all %>% mutate(across(where(is.numeric), ~ signif(.x, 3))), n = Inf, width = Inf)

# ---- figures ----------------------------------------------------------------
long <- dat %>% select(file, bat, species, trial, trial_label, load_pct, all_of(names(metrics))) %>%
  pivot_longer(all_of(names(metrics)), names_to = "metric") %>%
  mutate(metric = factor(metrics[metric], levels = metrics))

p_trial <- ggplot(long, aes(trial, value, group = bat, colour = species)) +
  geom_line(alpha = 0.5) + geom_point() +
  stat_summary(aes(group = NULL), fun = median, geom = "crossbar", width = 0.4, colour = "black") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(breaks = 2:5) +
  labs(x = "Trial", y = NULL,
       title = "Colombia 2025: flight acceleration metrics by trial (lines = individual bats)",
       caption = "* frequency in Hz relies on the nominal 105 Hz tag clock; tag-specific drift absorbed by tag random effect") +
  theme_bw()

p_load <- ggplot(filter(long, !is.na(load_pct)), aes(load_pct, value, group = bat, colour = species)) +
  geom_line(alpha = 0.5) + geom_point() +
  geom_smooth(aes(group = NULL), method = "lm", colour = "black", se = TRUE, linewidth = 0.6) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Tag + housing + velcro load (% body mass)", y = NULL,
       title = "Flight acceleration metrics vs. load (bats with known load)") +
  theme_bw()

p_bouts <- bout_summary %>%
  filter(file %in% dat$file) %>%
  ggplot(aes(factor(trial), wbf_hz, fill = species)) +
  geom_boxplot(outlier.size = 0.5) + facet_wrap(~ bat, labeller = label_both) +
  labs(x = "Trial", y = "Bout wingbeat frequency (Hz, tag clock)") + theme_bw()

pdf(file.path(out_dir, "trial_comparison.pdf"), width = 13, height = 9)
print(p_trial); print(p_load); print(p_bouts)
dev.off()
ggsave(file.path(out_dir, "metrics_by_trial.png"), p_trial, width = 13, height = 9, dpi = 110)
ggsave(file.path(out_dir, "metrics_by_load.png"), p_load, width = 13, height = 9, dpi = 110)
message("outputs written to ", out_dir)
