# Flanders 2025 flight-tunnel trials: do bats change acceleration-derived
# flight metrics between trials (i.e. with added tag load)?
#
# Design: each bat flew trial 1 with a dummy housing only (no FleaTag, no ACC
# data), then trials 2-4 with a FleaTag (0.34 g) + housing of randomised weight.
# So trials 2-4 differ in load (% body mass) and in order. A ".1" suffix
# (e.g. Trial2.1) is a repeat recording of that trial.
#
# Clock drift: the FleaTag writes timeMilliseconds from the NOMINAL 105 Hz
# (every file is exactly 42935 samples / 408.895 s), so the true time base is
# unknown and differs by tag. Consequences:
#   - nothing here is aligned to video / wall-clock time
#   - drift-FREE metrics: VeDBA, VeSBA, ODBA, heave amplitude, posture,
#     fraction of samples in flight, wingbeats per bout (cycle count)
#   - drift-AFFECTED metrics (scaled by each tag's clock error): wingbeat
#     frequency (Hz) and bout duration (s). Tags were rotated among bats and
#     trials, so tag ID is a random effect in the models to absorb that bias.
#
# Outputs (CSV + PDF) go to out_dir, outside the repo.

library(tidyverse)
library(data.table)
library(readxl)
library(signal)
library(lme4)
source("./R/flea_functions.R")  # get_true_groups()

# ---- paths & parameters -----------------------------------------------------
data_root <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Flanders25/Data/2025_FleaTagging_Flanders"
meta_xlsx <- file.path(data_root, "Flanders_Flights_2025.xlsx")
# August loads are not in the xlsx; fill in the template this script writes to
# out_dir and save it here to add load (% body mass) for August bats
aug_meta_csv <- file.path(data_root, "Flanders_Aug2025_trials.csv")
out_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Flanders25/Results/acc_trial_comparison"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sr <- 105                # nominal sampling rate (Hz), all files
static_window_s <- 1     # running mean for static acceleration
flight_vedba_g <- 0.8    # 1 s rolling-mean VeDBA threshold; rest < 0.5 g, flight > 1.2 g
min_bout_s <- 2          # keep bouts at least this long (after trimming)
merge_gap_s <- 0.5       # merge bouts separated by shorter gaps
trim_s <- 0.5            # trim take-off / landing from each bout end
wbf_band <- c(5, 25)     # Hz, search band for wingbeat frequency
clip_g <- 7.9            # |acc| at/above this counts as clipped (8 g range)

species_lookup <- c("N.leis" = "Nyctalus leisleri", "N.noc" = "Nyctalus noctula",
                    "E.ser" = "Eptesicus serotinus", "V.mur" = "Vespertilio murinus")

# ---- file discovery & file-name metadata -----------------------------------
files <- list.files(data_root, pattern = "[.]txt$", recursive = TRUE, full.names = TRUE)

# e.g. 20250818_Batt11_Trial2.1_3019_N.leis.txt, 20250520_Bat2__Trial3_3029_N.leis.txt,
#      20250817_Bat10_Trail2_304D_N.leis.txt
name_re <- "^(\\d{8})_Bat+(\\d+)_+Tr(?:ia|ai)l(\\d+(?:\\.\\d+)?)_([0-9A-Fa-f]{4})_(.+)[.]txt$"
file_meta <- tibble(path = files, file = basename(files)) %>%
  mutate(m = str_match(file, name_re),
         file_date = as.Date(m[, 2], "%Y%m%d"),
         bat = as.integer(m[, 3]),
         trial_label = m[, 4],
         trial = as.integer(floor(as.numeric(trial_label))),
         is_repeat = str_detect(trial_label, "\\."),
         tag = toupper(m[, 5]),
         species_code = m[, 6],
         species = unname(species_lookup[species_code]),
         season = if_else(str_detect(path, "2025_May"), "May", "August")) %>%
  select(-m)
if (any(is.na(file_meta$bat))) {
  warning("Unparsed file names:\n", paste(file_meta$file[is.na(file_meta$bat)], collapse = "\n"))
}

# ---- reader for FleaTag exports --------------------------------------------
# Some exports are serial logs with several download blocks (different tags);
# read_flea_tag_data() would take the first block. Here take the largest block
# whose ID matches the tag in the file name (latest if tied); short blocks are
# partial re-reads ("Stopped reading (button still pressed)!").
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
  d <- fread(text = lines[b$start:b$end], select = c("accX_mg", "accY_mg", "accZ_mg"))
  list(acc = as.matrix(d) / 1000,
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

bp <- butter(4, wbf_band / (sr / 2), type = "pass")

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
    heave <- filtfilt(bp, pc1)
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
  qc <- tibble(n_samples = nrow(a), n_blocks = r$n_blocks, block_id = r$block_id,
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
  process_trial(p, file_meta$tag[file_meta$path == p])
})

trial_summary <- file_meta %>%
  bind_cols(map_dfr(results, "summary"), map_dfr(results, "qc"))
bout_summary <- map_dfr(results, "bouts", .id = "file") %>%
  left_join(select(file_meta, file, season, bat, species, trial, trial_label, tag), by = "file")

# ---- metadata: loads --------------------------------------------------------
# May: FlightCage sheet (bat labels like "2 blue" / "6.0")
may_meta <- read_excel(meta_xlsx, sheet = "FlightCage") %>%
  transmute(bat = as.integer(str_extract(bat, "^\\d+")),
            trial = as.integer(trial),
            bat_mass_g = initial_weight,
            housing_g = `housing weight`,
            load_g = `total tag weight`,
            sheet_tag = toupper(tagID)) %>%
  filter(trial >= 2)

# August: optional hand-filled CSV (bat, trial, bat_mass_g, housing_g, load_g)
aug_template <- file_meta %>% filter(season == "August") %>%
  distinct(bat, trial, trial_label, tag, species) %>% arrange(bat, trial_label) %>%
  mutate(bat_mass_g = NA_real_, housing_g = NA_real_, load_g = NA_real_)
# bat 17 from clapperboard photos (housing H + FleaTag F = 0.34 g)
aug_template <- aug_template %>%
  mutate(bat_mass_g = case_when(bat == 17 & trial %in% 2:3 ~ 24.56, bat == 17 & trial == 4 ~ 24.64, TRUE ~ bat_mass_g),
         housing_g = case_when(bat == 17 & trial == 2 ~ 0.33, bat == 17 & trial == 3 ~ 2.13,
                               bat == 17 & trial == 4 ~ 1.44, TRUE ~ housing_g),
         load_g = if_else(bat == 17, housing_g + 0.34, load_g))
write_csv(aug_template, file.path(out_dir, "Flanders_Aug2025_trials_TEMPLATE.csv"), na = "")
aug_meta <- if (file.exists(aug_meta_csv)) {
  read_csv(aug_meta_csv, show_col_types = FALSE)
} else {
  aug_template
}
aug_meta <- aug_meta %>% select(bat, trial_label, bat_mass_g, housing_g, load_g) %>%
  mutate(trial_label = as.character(trial_label))

trial_summary <- trial_summary %>%
  left_join(may_meta, by = c("bat", "trial")) %>%
  rows_update(aug_meta %>% filter(bat %in% trial_summary$bat), by = c("bat", "trial_label"),
              unmatched = "ignore") %>%
  mutate(load_pct = 100 * load_g / bat_mass_g,
         tag_mismatch = !is.na(sheet_tag) & sheet_tag != tag)

# ---- QC flags ---------------------------------------------------------------
# identical data under two names = same tag memory downloaded twice; cannot
# tell which bat it belongs to, so both copies are excluded
trial_summary <- trial_summary %>%
  group_by(data_hash) %>% mutate(duplicate_data = n() > 1) %>% ungroup() %>%
  mutate(qc_note = str_c(
    if_else(duplicate_data, "identical data in another file; ", "", ""),
    if_else(n_blocks > 1, str_glue("multi-block export ({n_blocks} blocks), used largest {block_id} block; "), "", ""),
    if_else(tag_mismatch, "tag in file name differs from sheet; ", "", ""),
    if_else(n_bouts == 0, "no flight detected (tag stationary?); ",
            if_else(n_bouts < 3, "few flight bouts; ", "", ""), ""),
    if_else(clipped_frac_flight > 0.01, "clipping >1% of flight samples; ", "", "")),
    include = !duplicate_data & n_bouts >= 1)

write_csv(trial_summary %>% select(-data_hash), file.path(out_dir, "trial_summary.csv"))
write_csv(bout_summary, file.path(out_dir, "bout_summary.csv"))
trial_summary %>% filter(qc_note != "") %>% select(file, qc_note) %>% print(n = Inf)

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
# random intercepts for bat (repeated measures) and tag (clock / sensor bias)
lrt <- function(data, y, term) {
  d <- data %>% filter(!is.na(.data[[y]]), !is.na(.data[[term]]))
  if (n_distinct(d$bat_f) < 3) return(NULL)
  f0 <- as.formula(str_glue("{y} ~ 1 + (1|bat_f) + (1|tag_f)"))
  f1 <- update(f0, as.formula(str_glue(". ~ . + {term}")))
  m0 <- suppressMessages(lmer(f0, data = d, REML = FALSE))
  m1 <- suppressMessages(lmer(f1, data = d, REML = FALSE))
  a <- anova(m0, m1)
  fe <- fixef(m1)[-1]
  tibble(metric = y, term = term, n_trials = nrow(d), n_bats = n_distinct(d$bat_f),
         chisq = a$Chisq[2], df = a$Df[2], p = a$`Pr(>Chisq)`[2],
         effect = paste(sprintf("%s=%.3g", names(fe), fe), collapse = "; "),
         singular = isSingular(m1))
}

# Friedman test on bats with all of trials 2-4 (repeats averaged): robust check
friedman_trials <- function(data, y) {
  w <- data %>% group_by(bat, trial) %>% summarise(v = mean(.data[[y]], na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = trial, values_from = v) %>% drop_na()
  if (nrow(w) < 3) return(NULL)
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
long <- dat %>% select(file, season, bat, species, trial, trial_label, load_pct, all_of(names(metrics))) %>%
  pivot_longer(all_of(names(metrics)), names_to = "metric") %>%
  mutate(metric = factor(metrics[metric], levels = metrics))

p_trial <- ggplot(long, aes(trial, value, group = bat, colour = species)) +
  geom_line(alpha = 0.5) + geom_point() +
  stat_summary(aes(group = NULL), fun = median, geom = "crossbar", width = 0.4, colour = "black") +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(breaks = 2:4) +
  labs(x = "Trial", y = NULL,
       title = "Flanders 2025: flight acceleration metrics by trial (lines = individual bats)",
       caption = "* frequency in Hz relies on the nominal 105 Hz tag clock; tag-specific drift absorbed by tag random effect") +
  theme_bw()

p_load <- ggplot(filter(long, !is.na(load_pct)), aes(load_pct, value, group = bat, colour = species)) +
  geom_line(alpha = 0.5) + geom_point() +
  geom_smooth(aes(group = NULL), method = "lm", colour = "black", se = TRUE, linewidth = 0.6) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(x = "Tag + housing load (% body mass)", y = NULL,
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
