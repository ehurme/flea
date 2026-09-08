# flea

Data-processing and analysis code for FleaTag accelerometer loggers, paired
with video-based 3D tracking (TRex) and behavior scoring (BORIS), used for
flight studies in bats and hummingbirds (Flanders 2025, Colombia 2025
deployments).

Most files under `R/` are standalone scripts (not an installable package
despite the `DESCRIPTION`/`NAMESPACE` scaffolding) — source the ones you need
directly.

## Core FleaTag functions (`R/flea_functions.R`)

- `read_flea_tag_data()` — parse a raw FleaTag `.txt`/`.csv` export into
  metadata + accelerometer data frame.
- `flea_preprocess()` — compute static/dynamic acceleration, VeDBA/ODBA/ENMO,
  pitch/roll/yaw, rolling PCA, and flag flying bouts.
- `flea_plot()` — plot preprocessed ACC signals.
- `flea_plot_spectrogram()` — spectrogram of the ACC signal (via seewave).
- `get_true_groups()` / `split_by_true_groups()` — collapse a logical vector
  into contiguous TRUE runs (e.g. flying bouts) and split a data frame by them.
- `get_peak_range()` — density-based peak range finder.
- `to_sec()` — parse `"HH:MM:SS.xxx"` timestamps to seconds.

`R/flea_flight_summary.R` (`flea_flight_summary()`) is a standalone version
of the flight-bout summarizer used by `flanders25.R`.

`R/read_flea_tag_data.R` is an older, simpler duplicate of the reader above —
prefer `flea_functions.R`.

## Shiny apps

- **`shiny_3D.R`** — main 3D trajectory viewer for TRex-tracked flight paths.
  XY/XZ/YZ + interactive 3D views, timeline brushing, per-keypoint/individual
  filtering, movement summary stats, straight/level-flight bout detection
  (adjustable elevation-change, duration, and straightness thresholds, with a
  track viewer), FleaTag ACC upload + offset/sampling-rate alignment against
  track speed (with a rough auto-align), and CSV/PNG/GIF export.
- `shiny_3D_Manual_Frames.R` — earlier variant of the 3D viewer for manual
  frame-range work.
- `shiny_align.R` — align acceleration and trajectory data.
- `align_boris_flea_shiny.R` — align BORIS behavior scoring against FleaTag
  ACC data.
- `shiny_flea.R` / `shiny_fleatag.R` — earlier FleaTag exploration/plotting
  apps (`shiny_fleatag.R` sources `flea_functions.R`).

## BORIS + behavior

- `align_boris_flea.R` — align BORIS event logs with FleaTag ACC data
  (non-Shiny script version of `align_boris_flea_shiny.R`).
- `BehaveAI_explore.R` — filter BehaveAI tracking output and extract wingbeat
  frequency.

## TRex / 3D tracking pipeline

- `transform_trex_pose_data.R` — reshape raw TRex pose export (per-camera
  frame/keypoint/x/y/z) into the joined format used downstream.
- `generate_trex_commands.R` — batch-generate TRex CLI commands for a
  directory of videos.
- `join_tracks_till.R` — package exported TRex tracks into the structure
  expected by MAAP3D.
- `smooth3D_gaps.R` — gap-aware smoothing of a 3D trajectory.
- `plot3d.R` — static/animated 3D track viewing (rgl/gganimate/plotly).
- `hb_trex.R` — hummingbird-specific TRex export handling.

## Audio (bat call detection)

- `Process_audiomoth.R` / `Process_audiomoth_segment.R` — detect Phyllostomid
  echolocation calls from AudioMoth recordings (the `_segment` version cuts
  the wave into chunks before inspecting for calls).

## Hummingbird / PPG

- `hummingbird_wild_deployments.R` — batch FleaTag processing for wild
  hummingbird deployments.
- `ppg_sensor.R` — PPG (photoplethysmography) sensor data exploration.

## Field-season analysis scripts (Flanders 2025)

These are largely one-off scripts with hard-coded local/Dropbox paths, kept
for record-keeping and re-run rather than as reusable functions:
`flanders25.R`, `Flea_Graphs_automated.R`, `Flea_filter_test.R`,
`Flea_weight_comarison.R`, `Frame_segemt_Flea.R`,
`flea_Frame_segment_Google_sheet.R`, `flea_Flight_to_CSV.R`,
`flea_CSV_filter.R`, `flea_boxplot.R`, `assign_weights2.R`.

## Simulation

- `simulate_3d_accel_vedba_analysis.R` — simulate 3D acceleration of a flying
  animal and examine how sampling rate affects VeDBA estimation.

## `explore/`

Scratch/exploratory scripts (PPG heart-rate exploration, AudioMoth reading,
spectral data reading, flight-cage dimension checks, colorspace
visualization, function tests) — not part of the maintained pipeline.
