# AGENTS.md

Guidance for AI coding agents working in this repository.

## Project overview

`flea` is a collection of R code for processing and analyzing data from
**FleaTag accelerometer loggers**, paired with video-based 3D tracking (TRex)
and behavior scoring (BORIS). It supports flight studies on bats and
hummingbirds (Flanders 2025 and Colombia 2025 field deployments).

**Despite the `DESCRIPTION`/`NAMESPACE`/`.Rproj` scaffolding, this is NOT an
installable R package.** The `DESCRIPTION` is still the default
`usethis::create_package()` template (placeholder title, no license chosen)
and `NAMESPACE` is empty. All files under `R/` are standalone scripts that are
`source()`d or run interactively. Do not try to build/install with
`R CMD build`, `devtools::load_all()`, or roxygen2 — there is nothing to
build. Do not add roxygen headers expecting package-style export.

## Repository layout

- `R/` — all maintained analysis scripts (40 files). Key groupings:
  - **Core FleaTag functions** (`R/flea_functions.R`): `read_flea_tag_data()`,
    `flea_preprocess()` (static/dynamic acceleration, VeDBA/ODBA/ENMO,
    pitch/roll/yaw, rolling PCA, flying-bout detection), `flea_plot()`,
    `flea_plot_spectrogram()`, `get_true_groups()`, `get_peak_range()`,
    `to_sec()`, plus the hummingbird flight classifiers (`read_flea_export()`,
    `classify_bursts()`, `flight_segment()`, `within_burst()`,
    `classify_continuous()`, `wingbeat_freq()`). This is the canonical library — other files either
    `source("./R/flea_functions.R")` or copy/trim the functions they need.
  - **Other FleaTag helpers**: `flea_flight_summary.R`
    (`flea_flight_summary()`, per-file spectrogram + flight periods; sources
    `flea_functions.R` itself), `run_functions.R` (minimal example driver),
    `to_sec.R` and `read_flea_tag_data.R` (older duplicates of functions in
    `flea_functions.R`).
  - **Shiny apps**: `shiny_3D.R` (main 3D trajectory viewer for TRex data,
    with FleaTag ACC alignment and straight-flight bout detection),
    `shiny_3D_Manual_Frames.R`, `shiny_align.R`, `align_boris_flea_shiny.R`,
    `shiny_flea.R`, `shiny_fleatag.R`.
  - **BORIS/behavior**: `align_boris_flea.R`, `BehaveAI_explore.R`.
  - **TRex/3D tracking pipeline**: `transform_trex_pose_data.R`,
    `generate_trex_commands.R`, `join_tracks_till.R`, `smooth3D_gaps.R`,
    `plot3d.R`, `hb_trex.R`.
  - **Audio**: `Process_audiomoth.R`, `Process_audiomoth_segment.R`
    (Phyllostomid echolocation call detection from AudioMoth WAVs).
  - **Hummingbird/PPG**: `hummingbird_wild_deployments.R`,
    `hummingbird_activity_budget.R`, `hummingbird_captive_validation.R`,
    `hummingbird_captive_figures.R`, `hummingbird_behaviour_separability.R`,
    `hummingbird_flight_intensity.R`
    (report: `reports/hummingbird_acc_report.qmd`),
    `ppg_sensor.R`.
  - **Field-season one-offs** (hard-coded local/Dropbox paths, kept for the
    record, not reusable): `flanders25.R`, `colombia25.R`, `Flea_Graphs_automated.R`,
    `Flea_filter_test.R`, `Flea_weight_comarison.R`, `Frame_segemt_Flea.R`,
    `flea_Frame_segment_Google_sheet.R`, `flea_Flight_to_CSV.R`,
    `flea_CSV_filter.R`, `flea_boxplot.R`, `assign_weights2.R`.
  - **Simulation**: `simulate_3d_accel_vedba_analysis.R`.
- `explore/` — scratch/exploratory scripts (PPG heart rate, AudioMoth reading,
  spectral data, colorspace, function tests). Not part of the maintained
  pipeline. `explore/*` is in `.gitignore` (existing files are still
  tracked); it is *not* in `.Rbuildignore`, which only lists `flea.Rproj` and
  `.Rproj.user`.
- `trex_commands.txt` — generated batch of TRex CLI commands (Windows paths),
  produced by `generate_trex_commands.R`.
- Root `*.png` files — outputs of `simulate_3d_accel_vedba_analysis.R`.
  `vedba_timeseries_by_rate.png` is gitignored; the other two are committed.
- `README.md` — accurate, detailed map of every script; keep it in sync when
  adding/removing scripts.
- `DESCRIPTION`, `NAMESPACE`, `flea.Rproj` — leftover package scaffolding, see
  above.

## Technology stack

Pure R, run interactively in RStudio. Main libraries used across scripts:
`data.table`, `dplyr`/`tidyverse`, `zoo` (rolling statistics), `seewave` +
`tuneR` (audio/spectrograms), `plotly`/`ggplot2`/`rgl`/`gganimate`
(visualization), `shiny` (apps). There is no lockfile or dependency manifest;
packages are loaded with bare `library()` calls at the top of each script.

External tools referenced (not in repo): the TRex tracking CLI, AudioMoth
recorders, BORIS and BehaveAI software, Google Sheets.

## How to run

- There is no build step, test suite, CI, or deployment process.
- To use a script interactively, run it from the project root so relative
  `source("./R/...")` and `../../../Dropbox/...`-style paths resolve as the
  author intended.
- Shiny apps: open the file in RStudio and click "Run App" (or
  `shiny::runApp("R/shiny_3D.R")`).
- `explore/test_functions.R` is the closest thing to a test — it manually
  sources `flea_functions.R` and eyeballs plots on a known CSV. There are no
  automated tests (`testthat` etc.); verify changes by running the affected
  script on sample data and checking the outputs/plots.

## Code style and conventions

- Plain R scripts, `#`-comments, snake_case function and variable names
  (file names are not always consistent, e.g. typos like
  `Frame_segemt_Flea.R`, `Flea_weight_comarison.R` — do not "fix" them
  without checking for external references).
- Scripts load libraries at the top with `library()`, then define functions,
  then contain an interactive "driver" section with hard-coded file paths
  (often Windows `C:/Users/...` or relative `../../../Dropbox/...` paths)
  that you edit and re-run. It is normal here for multiple sibling `file_path
  <- ...` lines to be left in place with only the active one uncommented.
- Prefer extending `R/flea_functions.R` for reusable logic. Note the
  deliberate duplication pattern: `shiny_3D.R` keeps trimmed copies of
  `get_true_groups()`, `read_flea_tag_data()` and `flea_preprocess()` instead
  of `source()`ing `flea_functions.R`, explicitly to avoid pulling in its
  `seewave`/`tuneR` dependency (comments there mark them as adapted copies).
  When changing `flea_functions.R`, update those copies in parallel. Other
  apps (`align_boris_flea_shiny.R`, `shiny_fleatag.R`) `source()` it and pick
  up changes automatically.
- `R/read_flea_tag_data.R` and `R/to_sec.R` are older duplicates of functions
  in `flea_functions.R` — prefer the latter.

## Data and security considerations

- Raw data (FleaTag exports, videos, WAVs) lives OUTSIDE the repo on local
  drives/Dropbox and is referenced by hard-coded paths; nothing is committed.
- Never commit field data or personally identifying metadata. The repo is
  meant to hold code only (see `.gitignore`; note `explore/*` is ignored but
  existing files there are tracked).
- The FleaTag text format embeds metadata headers (species, trial IDs,
  sampling rate under key `AccHz`) parsed by `read_flea_tag_data()`.

## Version control

Single-branch Git workflow with informal commit messages; no PR process or
CI. Match the existing commit style (short lowercase messages).
