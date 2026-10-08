# Summary figures for the captive hummingbird ACC validation
# (R/hummingbird_captive_validation.R). Reads captive_validation.rds from the
# validation output folder (run that script first) plus the raw trial files
# for the example trace and the wingbeat spectra, and writes PNG/PDF figures
# to the same folder.
#
#   fig1_example     one trial: dynamic acceleration, BORIS ethogram and ACC
#                    classification on a shared time axis, plus raw x/y/z
#                    around a take-off
#   fig2_separation  dynamic acceleration by BORIS behaviour; per-trial
#                    sensitivity and specificity
#   fig3_flight_pct  % time in flight from video vs each wild sampling scheme
#   fig4_rate        fitted true sampling rate per tag vs nominal
#   fig5_wingbeat    3-axis spectra per species (fundamental vs 2nd harmonic)
#                    and wingbeat frequency per species
#   fig6_bouts       flight bout durations, video vs ACC
#   summary          one page with the key panels

library(tidyverse)
library(data.table)
library(patchwork)
source("./R/flea_functions.R")  # read_flea_export(), flight_segment(), wingbeat_freq()

# ---- paths & parameters -----------------------------------------------------
out_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/Results/captive_validation"
fig_dir <- file.path(out_dir, "figures")
dir.create(fig_dir, showWarnings = FALSE)
v <- readRDS(file.path(out_dir, "captive_validation.rds"))

example_obs <- "CB22_2"     # many take-offs and landings, clean alignment
spec_window_s <- 2          # windows for the wingbeat spectra
spec_grid <- seq(5, 60, by = 0.5)

# reference categorical palette, fixed order (dataviz skill, slots 1-6)
species_levels <- c("Anthracothorax nigricollis", "Chalybura buffonii", "Colibri coruscans",
                    "Colibri cyanotus", "Phaethornis guy", "Saucerottia cyanifrons")
species_cols <- setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300"), species_levels)
# BORIS behaviours (ethogram rows) and the exclusive states derived from them;
# feeding is split by the BORIS modifier into feeding in flight and on a perch
behav_levels <- c("Flight", "Hovering", "Feed (hovering)", "Perch", "Feed (perched)", "Grooming")
behav_cols <- c(Flight = "#2a78d6", Hovering = "#1baf7a", `Feed (hovering)` = "#008300", Perch = "grey55",
                `Feed (perched)` = "#eda100", Grooming = "#e87ba4")
state_cols <- c(`Other flight` = "#2a78d6", Hovering = "#1baf7a", `Hover-feeding` = "#008300", `Still perch` = "grey55",
                Grooming = "#e87ba4", `Perched feeding` = "#eda100")
ink <- "grey20"; ink_muted <- "grey45"

theme_set(theme_minimal(base_size = 11) +
            theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey92", linewidth = 0.3),
                  axis.text = element_text(colour = ink_muted), axis.title = element_text(colour = ink),
                  strip.text = element_text(colour = ink, face = "bold", hjust = 0),
                  plot.title = element_text(colour = ink, face = "bold"), plot.subtitle = element_text(colour = ink_muted),
                  plot.title.position = "plot", legend.position = "bottom"))
short_sp <- function(x) str_replace(x, "^(\\w)\\w+ ", "\\1. ")
sp_scale <- scale_colour_manual(values = species_cols, labels = short_sp, name = NULL, drop = TRUE)

validation <- v$validation %>% mutate(species = factor(species, species_levels))
align <- v$align
dt_of <- function(o) with(filter(align, obs == o), k / sr_nominal)  # real seconds per sample

# ---- fig 1: example trial ---------------------------------------------------
ex <- filter(v$samples, obs == example_obs)
ex_b <- filter(v$boris, obs == example_obs) %>% mutate(behavior = factor(behavior, behav_levels))
ex_t <- ex %>% group_by(t = round(v, 1)) %>% summarise(dyn = max(dyn), acc_fly = mean(acc_fly) > 0.5, .groups = "drop")
xr <- range(ex$v)

p1a <- ggplot(ex_t, aes(t, pmin(dyn, 6))) +
  geom_hline(yintercept = c(v$seg_args$off_g, v$seg_args$on_g), linetype = c(3, 2), colour = ink_muted, linewidth = 0.4) +
  geom_line(linewidth = 0.3, colour = ink) +
  annotate("text", x = xr[2], y = v$seg_args$on_g, label = "flight threshold", hjust = 1, vjust = -0.4,
           size = 3, colour = ink_muted) +
  scale_x_continuous(limits = xr, expand = c(0, 0)) +
  labs(x = NULL, y = "Dynamic SD (g)", title = str_glue("Example trial {example_obs}"),
       subtitle = "0.1 s running dynamic acceleration (capped at 6 g); video labels and ACC classification below") +
  theme(axis.text.x = element_blank())

etho <- bind_rows(
  ex_b %>% transmute(row = as.character(behavior), start = pmax(start, xr[1]), stop = pmin(stop, xr[2]), fill = as.character(behavior)),
  ex %>% arrange(v) %>% mutate(run = rleid(acc_fly)) %>% filter(acc_fly) %>%
    group_by(run) %>% summarise(start = min(v), stop = max(v)) %>% transmute(row = "ACC flight", start, stop, fill = "Flight")) %>%
  filter(stop > start) %>%
  mutate(row = factor(row, rev(c("ACC flight", behav_levels))))
p1b <- ggplot(etho) +
  geom_rect(aes(xmin = start, xmax = stop, ymin = as.numeric(row) - 0.38, ymax = as.numeric(row) + 0.38, fill = fill)) +
  geom_hline(yintercept = length(levels(etho$row)) - 0.5, colour = ink_muted, linewidth = 0.3) +
  scale_y_continuous(breaks = seq_along(levels(etho$row)), labels = levels(etho$row), expand = c(0.02, 0.02)) +
  scale_x_continuous(limits = xr, expand = c(0, 0)) +
  scale_fill_manual(values = behav_cols, guide = "none") +
  labs(x = "Video time (s)", y = NULL) +
  theme(panel.grid.major.y = element_blank())

# raw x/y/z around the first take-off
tr <- filter(v$trials, obs == example_obs)
raw <- read_flea_export(tr$path)$data
al <- filter(align, obs == example_obs)
raw[, t := al$a + al$k * (seq_len(.N) - 1) / al$sr_nominal]
takeoff <- ex_b %>% filter(behavior == "Flight", start > xr[1] + 1) %>% slice(1) %>% pull(start)
zoom <- raw[t > takeoff - 0.5 & t < takeoff + 1] %>% as_tibble() %>%
  select(t, x, y, z) %>% pivot_longer(c(x, y, z), names_to = "axis", values_to = "g")
p1c <- ggplot(zoom, aes(t - takeoff, g)) +
  geom_vline(xintercept = 0, linetype = 2, colour = ink_muted, linewidth = 0.4) +
  geom_line(linewidth = 0.35, colour = ink) +
  facet_wrap(~ paste("tag", axis), ncol = 1, strip.position = "left") +
  labs(x = "Time from video take-off (s)", y = "Acceleration (g)", title = "Raw acceleration at take-off",
       subtitle = "Perched (static ~1 g) to flapping at ~25 Hz") +
  theme(strip.placement = "outside")

fig1 <- (p1a / p1b + plot_layout(heights = c(2, 1.3))) | p1c
fig1 <- fig1 + plot_layout(widths = c(2.2, 1))

# ---- fig 2: separation ------------------------------------------------------
sep <- v$samples %>%
  mutate(state = factor(state, hb_state_levels)) %>%
  group_by(state) %>% slice_sample(n = 20000) %>% ungroup()
p2a <- ggplot(sep, aes(pmax(dyn, 0.02), fill = state)) +
  geom_density(colour = NA, alpha = 0.85, adjust = 0.8) +
  geom_vline(xintercept = v$seg_args$on_g, linetype = 2, colour = ink_muted) +
  scale_x_log10(breaks = c(0.03, 0.1, 0.3, 1, 3, 10), labels = c("0.03", "0.1", "0.3", "1", "3", "10")) +
  scale_fill_manual(values = state_cols, guide = "none") +
  facet_wrap(~ state, ncol = 1, strip.position = "left") +
  labs(x = "Dynamic SD, 0.1 s window (g, log scale)", y = NULL,
       title = "Acceleration separates flight from everything else",
       subtitle = "Density per BORIS behaviour (all trials); dashed = 1 g flight threshold") +
  theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank(), strip.text.y.left = element_text(angle = 0))

dyn_tab <- v$samples %>% mutate(state = factor(state, hb_state_levels)) %>%
  group_by(state) %>% summarise(pct = 100 * mean(acc_fly), n = n())
p2a <- p2a + geom_text(data = dyn_tab, aes(x = 12, y = Inf, label = sprintf("%.1f%% flight", pct)),
                       inherit.aes = FALSE, hjust = 1, vjust = 1.5, size = 3, colour = ink)

sens <- validation %>% filter(!is.na(specificity)) %>%
  select(obs, species, sensitivity, specificity) %>%
  pivot_longer(c(sensitivity, specificity), names_to = "metric") %>%
  mutate(metric = recode(metric, sensitivity = "Sensitivity\n(video flight called flight)",
                         specificity = "Specificity\n(video perch called perched)"))
p2b <- ggplot(sens, aes(100 * value, fct_reorder(obs, value, .fun = min))) +
  geom_vline(xintercept = 100, colour = "grey85") +
  geom_point(aes(colour = species), size = 2.5) +
  facet_wrap(~ metric) + sp_scale +
  scale_x_continuous(limits = c(80, 100)) +
  labs(x = "% of samples", y = NULL, title = "Per trial (trials with both flight and perching)") +
  guides(colour = guide_legend(nrow = 2))

fig2 <- p2a | p2b

# ---- fig 3: flight fraction -------------------------------------------------
ff <- bind_rows(
  validation %>% transmute(obs, species, scheme = "Continuous 210/105 Hz\n(0.1 s segmentation)", boris = boris_flight_pct, est = acc_flight_pct),
  v$burst_val %>% left_join(select(validation, obs, species), by = "obs") %>%
    transmute(obs, species, scheme = str_glue("{burst_s} s bursts\n(wild {if_else(burst_s < 1, '210', '105')} Hz mode)"),
              boris = boris_flight_pct, est = burst_flight_pct),
  validation %>% transmute(obs, species, scheme = "0.45 Hz sampling\n(decimated)", boris = boris_flight_pct, est = cont_cont_flight_pct)) %>%
  mutate(scheme = fct_inorder(scheme))
ff_lab <- ff %>% group_by(scheme) %>%
  summarise(lab = sprintf("bias %+.1f pts\nMAE %.1f pts", mean(est - boris), mean(abs(est - boris))))
fig3 <- ggplot(ff, aes(boris, est)) +
  geom_abline(linetype = 2, colour = ink_muted) +
  geom_point(aes(colour = species), size = 2.3, alpha = 0.9) +
  geom_text(data = ff_lab, aes(x = 2, y = 98, label = lab), hjust = 0, vjust = 1, size = 3, colour = ink, lineheight = 0.9) +
  facet_wrap(~ scheme, nrow = 1) + coord_equal(xlim = c(0, 100), ylim = c(0, 100)) + sp_scale +
  labs(x = "% time in flight, video (BORIS)", y = "% in flight, ACC classifier",
       title = "Flight time budgets from every wild sampling scheme match the video",
       subtitle = "One point per trial; dashed = 1:1. Bursts simulated every 0.5 s from the continuous recordings") +
  guides(colour = guide_legend(nrow = 1))

# ---- fig 4: sampling rate ---------------------------------------------------
rate <- align %>%
  mutate(fitted = k_source == "fitted", nominal_lab = paste(sr_nominal, "Hz nominal"))
fig4 <- ggplot(rate, aes(sr_true, fct_rev(tag))) +
  geom_vline(aes(xintercept = sr_nominal), linetype = 2, colour = ink_muted) +
  geom_point(data = ~ filter(.x, !is.na(man_sr)), aes(x = man_sr), shape = 4, size = 2.5, colour = ink_muted) +
  geom_point(aes(shape = fitted), size = 3, colour = "#2a78d6", position = position_jitter(height = 0.12, seed = 1)) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                     labels = c(`TRUE` = "fitted from video", `FALSE` = "borrowed from same tag"), name = NULL) +
  facet_wrap(~ nominal_lab, scales = "free_x") +
  labs(x = "Sampling rate (Hz)", y = "Tag", title = "Sampling rate is tag-specific: -2 to +11% vs nominal",
       subtitle = "True rate fitted per trial by aligning ACC flight with video (dot); dashed = nominal; x = manual alignment")

# ---- fig 5: wingbeat spectra ------------------------------------------------
spec <- map_dfr(align$obs, function(o) {
  tr <- filter(v$trials, obs == o); al <- filter(align, obs == o)
  r <- read_flea_export(tr$path); d <- r$data
  seg <- flight_segment(d$x, d$y, d$z, r$hz)
  n <- round(spec_window_s * r$hz)
  starts <- seq(1, nrow(d) - n, by = n)
  starts <- starts[map_lgl(starts, ~ all(seg$fly[.x:(.x + n - 1)] %in% TRUE))]
  if (!length(starts)) return(NULL)
  sp <- map(starts, function(s) {
    a <- sweep(cbind(d$x, d$y, d$z)[s:(s + n - 1), ], 2, colMeans(cbind(d$x, d$y, d$z)[s:(s + n - 1), ]))
    p <- spec.pgram(ts(a, frequency = r$hz), taper = 0.1, detrend = TRUE, plot = FALSE)
    f <- p$freq * al$sr_true / al$sr_nominal
    sapply(1:3, function(j) approx(f, p$spec[, j], spec_grid)$y)
  })
  m <- Reduce(`+`, sp) / length(sp)
  tibble(obs = o, f = spec_grid, x = m[, 1], y = m[, 2], z = m[, 3])
})
spec_sp <- spec %>% left_join(select(validation, obs, species), by = "obs") %>%
  pivot_longer(c(x, y, z), names_to = "axis", values_to = "power") %>%
  filter(!is.na(power)) %>%
  group_by(obs) %>% mutate(power = power / max(power)) %>%
  group_by(species, f, axis) %>% summarise(power = mean(power), .groups = "drop") %>%
  group_by(species) %>% mutate(power = power / max(power)) %>% ungroup()
wbf_med <- v$wbf %>% filter(align_ok) %>% mutate(species = factor(species, species_levels)) %>%
  group_by(species) %>% summarise(wbf = median(wbf_true), n = n_distinct(obs))
p5a <- ggplot(spec_sp, aes(f, power, linetype = axis)) +
  geom_vline(data = wbf_med, aes(xintercept = wbf), colour = "#2a78d6", linewidth = 0.5) +
  geom_vline(data = wbf_med, aes(xintercept = 2 * wbf), colour = "#2a78d6", linewidth = 0.5, linetype = 3) +
  geom_line(linewidth = 0.45, colour = ink) +
  facet_wrap(~ short_sp(species), ncol = 2) +
  coord_cartesian(xlim = range(spec_grid)) +
  scale_linetype_manual(values = c(x = "solid", y = "dotted", z = "longdash"), name = "Tag axis") +
  labs(x = "Frequency (Hz, true sampling rate)", y = "Relative power",
       title = "Wingbeat spectra: fundamental on z, 2nd harmonic on x",
       subtitle = "Mean flight spectrum per axis; blue = wingbeat frequency estimate, dotted blue = 2x")
p5b <- v$wbf %>% filter(align_ok) %>% mutate(species = factor(species, species_levels)) %>%
  ggplot(aes(wbf_true, fct_rev(species), colour = species)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, linewidth = 0.4) +
  geom_point(position = position_jitter(height = 0.12, seed = 1), size = 0.8, alpha = 0.4) +
  geom_text(data = wbf_med, aes(x = 15, label = str_glue("median {round(wbf, 1)} Hz, {n} bird{if_else(n == 1, '', 's')}")), colour = ink, size = 3,
            hjust = 0, vjust = -1.6) +
  scale_colour_manual(values = species_cols, guide = "none") +
  scale_y_discrete(labels = short_sp) +
  coord_cartesian(xlim = c(15, 50)) +
  labs(x = "Wingbeat frequency (Hz)", y = NULL, title = "Wingbeat frequency in flight",
       subtitle = "2 s flight windows; peak of 3-axis summed spectrum (points near 45-50 Hz: harmonic wins)")
fig5 <- p5a | p5b + plot_layout(widths = c(1.4, 1))

# ---- fig 6: bout durations --------------------------------------------------
# bouts that touch the start or end of the annotated recording are censored
bouts <- v$samples %>% arrange(obs, v) %>% group_by(obs) %>%
  mutate(i = row_number(), dt = dt_of(first(obs))) %>% ungroup()
bout_runs <- bind_rows(
  bouts %>% transmute(obs, i, dt, source = "Video (BORIS)", fly = top == "Flight"),
  bouts %>% transmute(obs, i, dt, source = "ACC (0.1 s segmentation)", fly = acc_fly)) %>%
  group_by(obs, source) %>% mutate(run = rleid(fly), last_i = max(i)) %>%
  group_by(obs, source, run) %>%
  summarise(fly = first(fly), dur = n() * first(dt), censored = min(i) == 1 | max(i) == first(last_i), .groups = "drop") %>%
  filter(fly, !censored)
p6a <- ggplot(bout_runs, aes(dur, colour = source)) +
  stat_ecdf(linewidth = 0.7) +
  coord_cartesian(xlim = c(0.1, 100)) +
  scale_x_log10(breaks = c(0.1, 0.3, 1, 3, 10, 30, 100), labels = c("0.1", "0.3", "1", "3", "10", "30", "100")) +
  scale_colour_manual(values = c(`Video (BORIS)` = "grey40", `ACC (0.1 s segmentation)` = "#2a78d6"), name = NULL) +
  labs(x = "Flight bout duration (s, log scale)", y = "Cumulative fraction of bouts",
       title = "ACC finds more short (< 3 s) flight episodes than the video coder",
       subtitle = str_glue("Complete bouts only (not cut by recording ends); ",
                           "n = {sum(bout_runs$source == 'Video (BORIS)')} video, {sum(bout_runs$source != 'Video (BORIS)')} ACC"))
p6b <- validation %>% filter(!is.na(boris_mean_bout_s), !is.na(acc_mean_bout_s)) %>%
  ggplot(aes(boris_mean_bout_s, acc_mean_bout_s)) +
  geom_abline(linetype = 2, colour = ink_muted) +
  geom_point(aes(colour = species), size = 2.5) +
  scale_x_log10() + scale_y_log10() + coord_equal() + sp_scale +
  labs(x = "Mean flight bout, video (s)", y = "Mean flight bout, ACC (s)", title = "Per trial",
       subtitle = "2 x flight time / (take-offs + landings)") +
  guides(colour = guide_legend(nrow = 3))
fig6 <- p6a | p6b + plot_layout(widths = c(1.4, 1))

# ---- one-page summary -------------------------------------------------------
summary_fig <- (p1a / p1b + plot_layout(heights = c(2, 1.3))) / (p2a | fig4) / fig3 / (p5b | p6a) +
  plot_layout(heights = c(1.1, 0.75, 1.3, 1, 1)) +
  plot_annotation(tag_levels = "a", title = "FleaTag flight classification validated against video, captive hummingbirds",
                  theme = theme(plot.title = element_text(face = "bold", size = 15, colour = ink)))

# ---- save -------------------------------------------------------------------
save_fig <- function(p, name, w, h) {
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 150, bg = "white")
  ggsave(file.path(fig_dir, paste0(name, ".pdf")), p, width = w, height = h)
}
save_fig(fig1, "fig1_example", 13, 6)
save_fig(fig2, "fig2_separation", 12, 6.5)
save_fig(fig3, "fig3_flight_pct", 13, 4.8)
save_fig(fig4, "fig4_rate", 9, 4.5)
save_fig(fig5, "fig5_wingbeat", 13, 7)
save_fig(fig6, "fig6_bouts", 12, 5)
save_fig(summary_fig, "summary", 14, 22)
message("figures written to ", fig_dir)
