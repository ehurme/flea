# Colombia 2025 (WIN 2025) wild hummingbird FleaTag deployments near Bogota:
# daily activity budgets (flight vs. perching), number of flights, and flight
# bout durations.
#
# Recording modes (from the file header, "FLEATAG_SC V14: ..."):
#   BUTTERFLY_0_45HZ_8G       continuous, one sample every 2.22 s (AD001-AD006)
#   COLIBRIS_210HZ_2M_8G_2F   bursts of 62 samples (0.3 s) every 2 min
#   COLIBRIS_105HZ_5M_8G_5F   bursts of 155 samples (1.5 s) every 5 min
# The tags have no clock: timeMilliseconds starts at 0 at "Tag start" in the
# deployment sheet (UTC-5). Data are kept only between release and recapture
# (some tags kept logging after recapture, e.g. AD011, AD024).
#
# Clock correction from the light sensor: the light reading (one per burst)
# drops to 0 at dusk and comes back at dawn. With the nominal tag clock the
# solar altitude at "light on" is ~5 deg lower than at "light off" (-9 vs -4
# deg), i.e. morning times come out ~10-20 min early, which topography (hills
# east of Bogota) cannot explain. Assuming the sensor switches at the same
# solar altitude in the evening and the morning gives each tag's clock rate
# error: real elapsed time = t * (1 + clock_rate), clock_rate = +1 to +2 %.
# Tags without both anchors get the median rate. Recapture handling in tags
# that kept logging (AD011, AD024) lines up with the sheet only after this
# correction. Day = solar altitude above the altitude at which the light
# sensor switches (estimated, ~-4.5 deg, i.e. ~20 min before sunrise to ~15 min
# after sunset).
#
# Flight classification (thresholds chosen from the wild data):
#   bursts      dynamic SD (sqrt of summed axis variances within the burst)
#               > burst_flight_g. Bursts are bimodal: perched ~0.03 g,
#               flight 3-5 g with a 25-35 Hz spectral peak (wingbeats).
#               Bursts between 0.1 and 1 g (perched movement, partial
#               take-off/landing) count as perching; burst_flight_g_lo gives
#               the sensitivity of the flight fraction to that choice.
#   0.45 Hz     each sample is a random phase of the wingbeat cycle, so in
#               flight |a| and direction jump between samples. A sample is
#               flight-like if ||a| - 1 g| > cont_flight_g, or if it differs by
#               > cont_flight_g from BOTH neighbours (a posture change on the
#               perch differs from one neighbour only). Single-sample gaps
#               inside flight runs are filled; runs of flight samples = bouts.
# Validation: no flight at night (sun below the light threshold) on any tag.
#
# Flight bout durations:
#   0.45 Hz     counted directly (resolution 2.22 s; gaps < ~4 s merged).
#   bursts      take-offs and landings inside bursts. Each sample is flight or
#               perched from a 0.1 s (~3 wingbeat) running dynamic SD with
#               hysteresis (on > within_on_g, off < within_off_g); interior
#               runs shorter than min_run_s are merged into their neighbours.
#               Mean bout duration = flight time / number of bouts
#               = 2 * observed flight time / (take-offs + landings) within
#               the bursts. This ratio is not length-biased (unlike the length
#               of bouts that happen to cover a burst), assumes behaviour is
#               independent of the burst schedule, and resolves continuous
#               flapping, so a perch of > min_run_s ends a bout. Flights per
#               hour for burst tags = flight fraction / mean bout duration.
#               Durations inside bursts use the nominal sampling rate.
#
# Outputs (CSV + PDF) go to out_dir, outside the repo.

library(tidyverse)
library(data.table)
library(readxl)
library(suncalc)
invisible(Sys.setlocale("LC_TIME", "C"))  # English month names in figure labels
source("./R/flea_functions.R")  # read_flea_export(), classify_bursts(), within_burst(), classify_continuous()

# ---- paths & parameters -----------------------------------------------------
win_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025"
data_dir <- file.path(win_dir, "Deployment recap data")
meta_xlsx <- file.path(win_dir, "Accelerometer deployments WIN 2025.xlsx")
out_dir <- file.path(win_dir, "Results/activity_budget")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

tz_local <- "America/Bogota"  # UTC-5, no DST
site_lat <- 4.71              # Bogota; replace with the exact site if needed
site_lon <- -74.07
burst_flight_g <- 1           # burst dynamic SD threshold for flight (g)
burst_flight_g_lo <- 0.5      # sensitivity: lower threshold
wbf_band <- c(15, 50)         # Hz, search band for wingbeat frequency
cont_flight_g <- 0.5          # 0.45 Hz threshold (g)
min_night_h <- 2              # light == 0 for at least this long = night
max_anchor_offset_ok <- 15    # flag tags whose light anchor is further off (min)
within_win_s <- 0.1           # running window for within-burst dynamic SD
within_on_g <- 1              # within-burst hysteresis: flight above ...
within_off_g <- 0.5           # ... perched below
within_peak_g <- 2            # a flight run must reach this (real flight peaks at 4-6 g)
min_run_s <- 0.1              # shorter interior flight/perch runs are merged
min_run_s_sens <- c(0.05, 0.1, 0.25)  # sensitivity of bout duration to min_run_s
n_boot <- 1000                # bootstrap replicates (bursts resampled within birds)
min_short_rec_h <- 3          # flag recordings shorter than this
set.seed(1)

# ---- metadata ---------------------------------------------------------------
# readxl returns the time-of-day columns as 1899-12-31 POSIXct in UTC; combine
# with the date columns as local (UTC-5) clock time
at <- function(date, time) {
  ok <- !is.na(date) & !is.na(time)
  out <- rep(as.POSIXct(NA, tz = tz_local), length(date))
  out[ok] <- as.POSIXct(paste(format(date[ok], "%Y-%m-%d", tz = "UTC"),
                              format(time[ok], "%H:%M:%S", tz = "UTC")), tz = tz_local)
  out
}

meta <- read_excel(meta_xlsx, skip = 1) %>%
  filter(!is.na(`Deploy ID`)) %>%
  transmute(id = `Deploy ID`, species = Species, sex = Sex,
            mass_g = as.numeric(`Weight before release`),
            tag_pct_mass = as.numeric(`Fleatag % bodyweight`),
            rec_start = at(`Date of capture`, `Tag start (UTC -5)`),
            release = at(`Date bird released`, `Time released (UTC -5)`),
            recapture = at(`Date recaptured`, `Time recaptured (UTC -5)`),
            sheet_issue = Issue)

files <- list.files(data_dir, pattern = "[.]txt$", full.names = TRUE, recursive = TRUE)

# ---- per-file classification ------------------------------------------------
units <- list()   # bursts or samples, classified, on the nominal tag clock (t_s)
within <- list()  # within-burst segmentation, one row per burst and min_run_s
for (f in files) {
  id <- regmatches(basename(f), regexpr("AD[0-9]{3}", basename(f)))
  message("processing ", id)
  r <- read_flea_export(f)
  u <- if (r$hz > 1) classify_bursts(r$data, r$hz, burst_flight_g, burst_flight_g_lo, wbf_band) else
    classify_continuous(r$data, cont_flight_g)
  dt_s <- if (r$hz > 1) median(diff(u$t_s)) else 1 / r$hz  # time each unit represents
  u[, `:=`(id = id, hz = r$hz, tag_mode = r$mode, tag = r$tag, dt_s = dt_s)]
  units[[id]] <- u
  if (r$hz > 1) {
    within[[id]] <- rbindlist(lapply(min_run_s_sens, function(mr)
      within_burst(r$data, r$hz, win_s = within_win_s, on_g = within_on_g, off_g = within_off_g,
                   peak_g = within_peak_g, min_run_s = mr)[, min_run_s := mr]))[, id := id]
  }
}
U <- rbindlist(units, fill = TRUE)
U <- merge(U, as.data.table(select(meta, id, rec_start, release, recapture)), by = "id", sort = FALSE)
U[, mode := if_else(hz > 1, sprintf("%g Hz burst", hz), "0.45 Hz continuous")]

# ---- light anchors & clock correction ---------------------------------------
sun_alt <- function(t) getSunlightPosition(date = t, lat = site_lat, lon = site_lon)$altitude * 180 / pi
clock_h <- function(t) as.numeric(format(t, "%H", tz = tz_local)) + as.numeric(format(t, "%M", tz = tz_local)) / 60

# light-off / light-on = midpoints around the longest run of zero light; a
# shorter zero run that lasts to the end of the data still gives a light-off
find_night <- function(t_s, light) {
  r <- rle(light == 0)
  e <- cumsum(r$lengths); s <- e - r$lengths + 1
  dur <- t_s[e] - t_s[s]
  k <- which(r$values & (dur > min_night_h * 3600 | (e == length(t_s) & s > 1)))
  if (!length(k)) return(tibble())
  k <- k[which.max(dur[k])]
  tibble(event = c("light off", "light on"),
         t_s = c(if (s[k] > 1) (t_s[s[k] - 1] + t_s[s[k]]) / 2 else NA,
                 if (e[k] < length(t_s)) (t_s[e[k]] + t_s[e[k] + 1]) / 2 else NA))
}

# one light reading per burst (for 0.45 Hz: the last sample of each burst),
# free-ranging period on the nominal clock (drift is < 20 min, irrelevant here)
anchors <- U[!is.na(light) & rec_start + t_s >= release & (is.na(recapture) | rec_start + t_s < recapture),
             .(t_s = last(t_s), light = last(light)), by = .(id, burstCount)] %>%
  as_tibble() %>%
  group_by(id) %>% group_modify(~ find_night(.x$t_s, .x$light)) %>% ungroup() %>%
  filter(!is.na(t_s)) %>%
  left_join(select(meta, id, rec_start), by = "id")

clock_fit <- anchors %>%
  select(id, rec_start, event, t_s) %>%
  pivot_wider(names_from = event, values_from = t_s) %>%
  filter(!is.na(`light off`), !is.na(`light on`)) %>%
  mutate(clock_rate = pmap_dbl(list(rec_start, `light off`, `light on`), function(st, a, b)
    uniroot(function(d) sun_alt(st + a * (1 + d)) - sun_alt(st + b * (1 + d)), c(-0.1, 0.1))$root))
clock_rate_default <- median(clock_fit$clock_rate)

clock_tab <- meta %>% filter(id %in% U$id) %>% select(id) %>%
  left_join(select(clock_fit, id, clock_rate), by = "id") %>%
  mutate(clock_rate_source = if_else(is.na(clock_rate), "median of other tags", "light anchors"),
         clock_rate = coalesce(clock_rate, clock_rate_default))

anchors <- anchors %>%
  left_join(clock_tab, by = "id") %>%
  mutate(time_nominal = rec_start + t_s, time = rec_start + t_s * (1 + clock_rate),
         clock = format(time, "%H:%M", tz = tz_local),
         sun_alt_nominal = sun_alt(time_nominal), sun_alt = sun_alt(time))
light_alt_deg <- median(anchors$sun_alt)

# how far each anchor is from the time the sun crosses light_alt_deg (min);
# large offsets point to a wrong start time or clock
cross_time <- function(t) {
  f <- function(x) sun_alt(t + x) - light_alt_deg
  uniroot(f, c(-7200, 7200))$root
}
anchors <- anchors %>%
  mutate(anchor_offset_min = map_dbl(time, ~ -cross_time(.x) / 60))
anchor_qc <- anchors %>% group_by(id) %>%
  summarise(max_anchor_offset_min = anchor_offset_min[which.max(abs(anchor_offset_min))])

U <- merge(U, as.data.table(clock_tab), by = "id", sort = FALSE)
U[, time := rec_start + t_s * (1 + clock_rate)]
U[, `:=`(clock = clock_h(time), sun_alt = sun_alt(time),
         free = time >= release & (is.na(recapture) | time < recapture))]
U[, `:=`(day = sun_alt >= light_alt_deg, past_recapture = !is.na(recapture) & max(time) >= recapture), by = id]

# daylight window (sun above the light-sensor threshold) on a mid-deployment date
light_window <- function(date) {
  t <- as.POSIXct(paste(date, "00:00:00"), tz = tz_local) + seq(0, 86340, 60)
  up <- t[sun_alt(t) >= light_alt_deg]
  c(dawn = clock_h(min(up)), dusk = clock_h(max(up)))
}
lw <- light_window("2025-03-15")
day_length_h <- unname(lw["dusk"] - lw["dawn"])
message(sprintf("light threshold %.1f deg: day %s-%s (%.1f h); clock rate %+.2f%% (median)",
                light_alt_deg, sprintf("%02d:%02d", floor(lw["dawn"]), round(60 * lw["dawn"] %% 1)),
                sprintf("%02d:%02d", floor(lw["dusk"]), round(60 * lw["dusk"] %% 1)),
                day_length_h, 100 * clock_rate_default))

# ---- 0.45 Hz flight bouts ---------------------------------------------------
bout_tab <- U[hz < 1 & free == TRUE][order(id, time)][, run := rleid(fly), by = id][
  fly == TRUE, .(start = min(time), n_samples = .N, duration_s = .N * dt_s[1] * (1 + clock_rate[1]),
                 day = day[1]), by = .(id, run)] %>%
  as_tibble() %>% left_join(select(meta, id, species), by = "id")

# ---- burst flight bouts (within-burst take-offs / landings) -----------------
W <- rbindlist(within) %>%
  merge(U[hz > 1, .(id, burstCount, mode, free, day)], by = c("id", "burstCount")) %>%
  as_tibble() %>% filter(free, day)

mean_bout <- function(d) 2 * sum(d$t_fly_s) / sum(d$n_on + d$n_off)
mean_perch <- function(d) 2 * sum(d$t_obs_s - d$t_fly_s) / sum(d$n_on + d$n_off)
boot_ci <- function(d, f) {
  idx <- split(seq_len(nrow(d)), d$id)
  b <- replicate(n_boot, f(d[unlist(lapply(idx, function(i) i[sample.int(length(i), replace = TRUE)])), ]))
  quantile(b[is.finite(b)], c(0.025, 0.975), names = FALSE)
}
burst_bouts <- W %>%
  group_by(mode, min_run_s) %>%
  group_modify(function(d, k) {
    ci <- boot_ci(d, mean_bout)
    tibble(n_birds = n_distinct(d$id), n_bursts = nrow(d), observed_min = sum(d$t_obs_s) / 60,
           flight_s = sum(d$t_fly_s), n_takeoffs = sum(d$n_on), n_landings = sum(d$n_off),
           mean_bout_s = mean_bout(d), mean_bout_lo = ci[1], mean_bout_hi = ci[2],
           mean_perch_s = mean_perch(d))
  }) %>% ungroup()
burst_bout_main <- filter(burst_bouts, min_run_s == !!min_run_s)
# pooled over both burst modes, used for flights per hour of every burst tag
pooled <- filter(W, min_run_s == !!min_run_s)
mean_bout_burst_s <- mean_bout(pooled)
mean_bout_burst_ci <- boot_ci(pooled, mean_bout)

cont_bouts <- bout_tab %>% filter(day) %>%
  summarise(n_birds = n_distinct(id), n_bouts = n(), mean_bout_s = mean(duration_s),
            median_bout_s = median(duration_s), q75_s = quantile(duration_s, 0.75),
            q90_s = quantile(duration_s, 0.9), max_s = max(duration_s),
            pct_single_sample = 100 * mean(n_samples == 1))

# ---- per-deployment summary -------------------------------------------------
fmt_clock <- function(t, f = min) if (length(t)) format(f(t), "%H:%M", tz = tz_local) else NA_character_

budget <- U[free == TRUE, {
  dd <- .SD[day == TRUE]; nn <- .SD[day == FALSE]
  burst <- hz[1] > 1
  fl_day <- mean(dd$fly)
  .(mode = mode[1], tag = tag[1], clock_rate_pct = 100 * clock_rate[1], clock_rate_source = clock_rate_source[1],
    free_from = min(time), free_to = max(time),
    tracked_h = as.numeric(difftime(max(time), min(time), units = "hours")),
    day_h = nrow(dd) * dt_s[1] / 3600, night_h = nrow(nn) * dt_s[1] / 3600,
    n_units_day = nrow(dd),
    pct_flight_day = 100 * fl_day,
    pct_perch_day = 100 * (1 - fl_day),
    pct_flight_day_lo = if (burst) 100 * mean(dd$fly_lo) else NA_real_,
    pct_flight_night = 100 * mean(nn$fly),
    n_flight_units_day = sum(dd$fly),
    first_flight = fmt_clock(time[fly & clock < 12]),
    last_flight = fmt_clock(time[fly & clock >= 12], max),
    median_wbf_hz = if (burst) median(dd$wbf_hz[dd$fly]) else NA_real_)
}, by = id] %>%
  as_tibble() %>%
  left_join(bout_tab %>% filter(day) %>% group_by(id) %>%
              summarise(n_bouts_day = n(), mean_bout_s = mean(duration_s), median_bout_s = median(duration_s),
                        n_bouts_single_sample = sum(n_samples == 1)), by = "id") %>%
  left_join(select(meta, id, species, sex, mass_g, tag_pct_mass, rec_start, release, recapture, sheet_issue), by = "id") %>%
  left_join(anchor_qc, by = "id") %>%
  mutate(continuous = mode == "0.45 Hz continuous",
         flights_per_h_day = if_else(continuous, n_bouts_day / day_h,
                                     (pct_flight_day / 100) * 3600 / mean_bout_burst_s),
         flights_per_h_basis = if_else(continuous, "counted bouts",
                                       "flight fraction / mean within-burst bout duration"),
         flight_min_per_day = pct_flight_day / 100 * day_length_h * 60,
         trimmed_after_recapture = id %in% U[past_recapture == TRUE, unique(id)],
         qc_note = str_c(
           if_else(tracked_h < min_short_rec_h, "short recording; ", "", ""),
           if_else(trimmed_after_recapture, "data after recapture dropped; ", "", ""),
           if_else(abs(max_anchor_offset_min) > max_anchor_offset_ok,
                   str_glue("light anchor {round(max_anchor_offset_min)} min off (start time or clock suspect); "), "", ""),
           if_else(n_flight_units_day == 0, "no flight detected (tag loose or failed?); ", "", ""),
           if_else(day_h < 4, "<4 h of daytime data; ", "", "")),
         include = day_h >= 1 & n_flight_units_day > 0) %>%
  relocate(id, species, sex, mode) %>%
  arrange(mode, id)

# ---- diel profile -----------------------------------------------------------
diel <- U[free == TRUE, .(pct_flight = 100 * mean(fly), n = .N), by = .(id, hour = floor(clock))] %>%
  as_tibble() %>% left_join(select(budget, id, mode, species, include), by = "id")

# ---- outputs ----------------------------------------------------------------
write_csv(budget, file.path(out_dir, "deployment_budget.csv"))
write_csv(bout_tab, file.path(out_dir, "continuous_flight_bouts.csv"))
write_csv(burst_bouts, file.path(out_dir, "burst_bout_duration.csv"))
write_csv(select(anchors, -rec_start), file.path(out_dir, "light_anchors.csv"))
write_csv(as_tibble(U[hz > 1, .(id, mode, burstCount, time, clock, sun_alt, free, day, dyn_sd, wbf_hz, clipped, light, fly, fly_lo)]),
          file.path(out_dir, "burst_classification.csv"))
write_csv(diel, file.path(out_dir, "diel_flight.csv"))

anchors %>% transmute(id, event, clock, sun_alt_nominal = round(sun_alt_nominal, 1), sun_alt = round(sun_alt, 1),
                      offset_min = round(anchor_offset_min), clock_rate_pct = round(100 * clock_rate, 2),
                      clock_rate_source) %>%
  print(n = Inf, width = Inf)

budget %>%
  transmute(id, species = word(species, 1), mode, tracked_h = round(tracked_h, 1), day_h = round(day_h, 1),
            pct_flight_day = round(pct_flight_day, 1), pct_flight_day_lo = round(pct_flight_day_lo, 1),
            pct_flight_night = round(pct_flight_night, 1), n_flight_units_day,
            n_bouts_day, n_bouts_single_sample, mean_bout_s = round(mean_bout_s, 1),
            flights_per_h = round(flights_per_h_day, 1),
            flight_min_per_day = round(flight_min_per_day), first_flight, last_flight,
            wbf = round(median_wbf_hz, 1), qc_note) %>%
  print(n = Inf, width = Inf)

print(mutate(burst_bouts, across(where(is.double), ~ round(.x, 2))), width = Inf)
print(mutate(cont_bouts, across(where(is.double), ~ round(.x, 1))), width = Inf)
message(sprintf("burst tags, pooled: mean flight bout %.2f s (95%% CI %.2f-%.2f)",
                mean_bout_burst_s, mean_bout_burst_ci[1], mean_bout_burst_ci[2]))

# pooled across deployments with >= 4 h of daytime data
group_summary <- budget %>% filter(include, day_h >= 4) %>%
  group_by(mode) %>%
  summarise(n_birds = n(), ids = str_c(id, collapse = ","),
            pct_flight_median = median(pct_flight_day), pct_flight_min = min(pct_flight_day),
            pct_flight_max = max(pct_flight_day),
            flights_per_h_median = median(flights_per_h_day),
            flight_min_per_day_median = median(flight_min_per_day))
write_csv(group_summary, file.path(out_dir, "group_summary.csv"))
print(group_summary, width = Inf)

# ---- figures ----------------------------------------------------------------
col_fly <- "#2a78d6"
col_perch <- "grey80"
species_cols <- c("Anthracothorax nigricollis" = "#2a78d6", "Colibri coruscans" = "#eb6834",
                  "Phaethornis guy" = "#1baf7a")
night_rect <- annotate("rect", xmin = c(0, lw["dusk"]), xmax = c(lw["dawn"], 24),
                       ymin = -Inf, ymax = Inf, fill = "grey92")

p_budget <- budget %>% filter(day_h > 0) %>%
  mutate(label = str_glue("{id} ({word(species, 1)})")) %>%
  ggplot(aes(pct_flight_day, fct_reorder(label, pct_flight_day))) +
  geom_col(fill = col_fly, width = 0.6) +
  geom_point(aes(x = pct_flight_day_lo), shape = 124, size = 4, colour = "grey30", na.rm = TRUE) +
  facet_grid(mode ~ ., scales = "free_y", space = "free_y") +
  labs(x = "% of daytime in flight", y = NULL,
       title = "Daytime flight fraction per deployment",
       subtitle = str_glue("Day = sun above {round(light_alt_deg, 1)} deg (light sensor threshold); ",
                           "bar = burst threshold {burst_flight_g} g, tick = {burst_flight_g_lo} g")) +
  theme_bw()

p_diel <- diel %>% filter(include) %>%
  group_by(hour) %>% summarise(pct_flight = mean(pct_flight), n_birds = n_distinct(id)) %>%
  ggplot(aes(hour + 0.5, pct_flight)) +
  night_rect +
  geom_point(data = filter(diel, include), aes(colour = species), alpha = 0.5, size = 1.5) +
  geom_line(linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = species_cols) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  labs(x = "Hour of day (UTC-5, clock-corrected)", y = "% of samples in flight", colour = "Species (per bird)",
       title = "Diel flight activity (line = mean across birds; shaded = sun below light threshold)") +
  theme_bw() + theme(legend.position = "bottom")

p_timeline <- U[free == TRUE] %>% as_tibble() %>%
  mutate(day_label = format(time, "%d %b", tz = tz_local)) %>%
  ggplot(aes(clock, fct_rev(id))) +
  night_rect +
  geom_point(data = ~ filter(.x, !fly), colour = col_perch, shape = 124, size = 2) +
  geom_point(data = ~ filter(.x, fly), colour = col_fly, shape = 124, size = 3) +
  facet_grid(mode ~ day_label, scales = "free_y", space = "free_y") +
  scale_x_continuous(breaks = seq(0, 24, 6), limits = c(0, 24)) +
  labs(x = "Hour of day (UTC-5, clock-corrected)", y = NULL, title = "Flight (blue) vs perched (grey), free-ranging data only") +
  theme_bw()

p_anchors <- anchors %>%
  select(id, event, `nominal tag clock` = sun_alt_nominal, `corrected clock` = sun_alt) %>%
  pivot_longer(c(`nominal tag clock`, `corrected clock`), names_to = "clock", values_to = "alt") %>%
  mutate(clock = fct_rev(clock)) %>%
  ggplot(aes(alt, fct_rev(id), shape = event)) +
  geom_vline(xintercept = light_alt_deg, linetype = 2) +
  geom_point(size = 3, colour = col_fly) +
  facet_wrap(~ clock) +
  labs(x = "Solar altitude at light on/off (deg)", y = NULL, shape = NULL,
       title = "Light-sensor anchors: same solar altitude at dusk and dawn after clock correction") +
  theme_bw() + theme(legend.position = "bottom")

p_bouts <- bout_tab %>% filter(day) %>%
  ggplot(aes(duration_s)) + geom_histogram(binwidth = 2.22, boundary = 1.11, fill = col_fly) +
  facet_wrap(~ id, scales = "free_y") +
  labs(x = "Bout duration (s; resolution 2.22 s)", y = "Daytime flight bouts",
       title = "0.45 Hz tags: flight bout durations") + theme_bw()

p_threshold <- U[hz > 1 & free == TRUE] %>% as_tibble() %>%
  ggplot(aes(pmax(dyn_sd, 0.005), fill = day)) +
  geom_histogram(bins = 60, colour = "white", linewidth = 0.2) +
  geom_vline(xintercept = c(burst_flight_g_lo, burst_flight_g), linetype = c(2, 1)) +
  scale_x_log10() + scale_fill_manual(values = c(`TRUE` = col_fly, `FALSE` = "grey40"), labels = c("night", "day")) +
  labs(x = "Burst dynamic SD (g, log scale; 0 shown at 0.005)", y = "Bursts", fill = NULL,
       title = "Burst tags: perched vs flight modes and thresholds") + theme_bw()

pdf(file.path(out_dir, "activity_budget.pdf"), width = 12, height = 8)
print(p_budget); print(p_diel); print(p_timeline); print(p_anchors); print(p_bouts); print(p_threshold)
dev.off()
ggsave(file.path(out_dir, "budget.png"), p_budget, width = 9, height = 7, dpi = 110)
ggsave(file.path(out_dir, "diel.png"), p_diel, width = 9, height = 5, dpi = 110)
ggsave(file.path(out_dir, "timeline.png"), p_timeline, width = 12, height = 8, dpi = 110)
ggsave(file.path(out_dir, "anchors.png"), p_anchors, width = 9, height = 5, dpi = 110)
ggsave(file.path(out_dir, "bouts.png"), p_bouts, width = 9, height = 5, dpi = 110)
ggsave(file.path(out_dir, "threshold.png"), p_threshold, width = 8, height = 5, dpi = 110)
message("outputs written to ", out_dir)
