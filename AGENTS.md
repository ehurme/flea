# AGENTS.md

Guidance for AI coding agents working in this repository.

## Project overview

`flea` is a collection of R scripts for processing and analyzing data from **FleaTag accelerometer loggers**, paired with video-based 3D tracking (TRex) and behavior scoring (BORIS). It supports flight studies on bats and hummingbirds, primarily the Flanders 2025 and Colombia 2025 field deployments.

**Despite the `DESCRIPTION`/`NAMESPACE`/`.Rproj` scaffolding, this is NOT an installable R package.** `DESCRIPTION` is still the default `usethis::create_package()` template with placeholder fields, `NAMESPACE` is empty, and `.Rbuildignore` only excludes `flea.Rproj` and `.Rproj.user`. All files under `R/` are standalone scripts that are `source()`d or run interactively. Do not try to build or install with `R CMD build`, `devtools::load_all()`, or roxygen2 — there is nothing to build. Do not add roxygen headers expecting package-style exports.

## Repository layout

- `R/` — 40 maintained analysis scripts. The main groupings are:
  - **Core FleaTag functions** (`R/flea_functions.R`): canonical library containing `read_flea_tag_data()`, `flea_preprocess()` (static/dynamic acceleration, VeDBA/ODBA/ENMO, pitch/roll/yaw, rolling PCA, flying-bout detection), `flea_plot()`, `flea_plot_spectrogram()`, `get_true_groups()`, `split_by_true_groups()`, `get_peak_range()`, `to_sec()`, and the hummingbird flight classifiers (`read_flea_export()`, `classify_bursts()`, `flight_segment()`, `within_burst()`, `classify_continuous()`, `burst_features()`, `wingbeat_freq()`, `dominant_freq()`, `hb_boris_state()`). Other scripts either `source("./R/flea_functions.R")` or copy/trim the functions they need.
  - **Other FleaTag helpers**: `flea_flight_summary.R` (per-file spectrogram + flight-period summarizer; sources `flea_functions.R`), `run_functions.R` (minimal end-to-end example driver), `to_sec.R` and `read_flea_tag_data.R` (older duplicates of functions in `flea_functions.R`; prefer the canonical versions).
  - **Shiny apps**: `shiny_3D.R` (main 3D trajectory viewer for TRex data, with FleaTag ACC alignment and straight-flight bout detection), `shiny_3D_Manual_Frames.R`, `shiny_align.R`, `align_boris_flea_shiny.R`, `shiny_flea.R`, `shiny_fleatag.R`.
  - **BORIS/behavior**: `align_boris_flea.R`, `BehaveAI_explore.R`.
  - **TRex / 3D tracking pipeline**: `transform_trex_pose_data.R`, `generate_trex_commands.R`, `join_tracks_till.R`, `smooth3D_gaps.R`, `plot3d.R`, `hb_trex.R`.
  - **Audio**: `Process_audiomoth.R`, `Process_audiomoth_segment.R` (Phyllostomid echolocation call detection from AudioMoth WAVs).
  - **Hummingbird / PPG**: `hummingbird_wild_deployments.R`, `hummingbird_activity_budget.R`, `hummingbird_captive_validation.R`, `hummingbird_captive_figures.R`, `hummingbird_behaviour_separability.R`, `hummingbird_flight_intensity.R` (report source: `reports/hummingbird_acc_report.qmd`), `ppg_sensor.R`.
  - **Field-season one-offs** (hard-coded local/Dropbox paths, kept for the record, not intended as reusable tools): `flanders25.R`, `colombia25.R`, `Flea_Graphs_automated.R`, `Flea_filter_test.R`, `Flea_weight_comarison.R`, `Frame_segemt_Flea.R`, `flea_Frame_segment_Google_sheet.R`, `flea_Flight_to_CSV.R`, `flea_CSV_filter.R`, `flea_boxplot.R`, `assign_weights2.R`.
  - **Simulation**: `simulate_3d_accel_vedba_analysis.R`.
- `explore/` — 9 scratch/exploratory scripts (PPG heart-rate exploration, AudioMoth reading, spectral data handling, flight-cage dimension checks, colorspace visualization, function tests). Not part of the maintained pipeline. `explore/*` is in `.gitignore` (existing files are still tracked); it is *not* in `.Rbuildignore`.
- `reports/` — `hummingbird_acc_report.qmd` (Quarto/Typst PDF report) and `render_hummingbird_report.R` (render driver). Render outputs are gitignored under `reports/fig/`, `reports/*.pdf`, `reports/*.typ`, and `reports/*_files/`.
- `trex_commands.txt` — generated batch of TRex CLI commands (Windows paths), produced by `R/generate_trex_commands.R`.
- Root `*.png` files — outputs of `R/simulate_3d_accel_vedba_analysis.R`. `vedba_timeseries_by_rate.png` is gitignored; the other two are committed.
- `README.md` — detailed map of every script; keep it in sync when adding or removing scripts.
- `CLAUDE.md` — additional coding details that supplement this file.
- `DESCRIPTION`, `NAMESPACE`, `flea.Rproj` — leftover package scaffolding; see warning above.

## Technology stack

Pure R, run interactively in RStudio/Positron. No Python, JavaScript frameworks, container files, Makefile, CI config, or lockfile exist in the repo.

Libraries used across scripts (loaded with bare `library()` calls):

- Data manipulation: `data.table`, `dplyr`, `tidyverse`, `readxl`
- Rolling statistics: `zoo`
- Audio/spectrograms: `seewave`, `tuneR`
- Visualization: `plotly`, `ggplot2`, `rgl`, `gganimate`, `viridisLite`
- Shiny apps: `shiny`, `janitor`
- Mixed models: `lme4`
- Random forest: `ranger`
- Solar timing: `suncalc`
- Google Sheets: `googlesheets4`
- Reporting: `knitr`, Quarto/Typst

External tools referenced but not in the repo: the TRex tracking CLI, AudioMoth recorders, BORIS and BehaveAI software, Google Sheets, Dropbox.

## Build and test commands

There is no build step, test suite, CI, or deployment process.

The closest thing to a standard command is rendering the hummingbird report:

```r
Rscript reports/render_hummingbird_report.R
```

This script copies generated figures into `reports/fig/` (gitignored), renders `reports/hummingbird_acc_report.qmd` to PDF using the Quarto bundled with Positron, and moves the PDF to `Colombia25/Hummingbird/Results/report` on Dropbox. Run it only after the upstream hummingbird scripts have produced their outputs.

## How to run

- Run scripts from the project root so that relative `source("./R/...")` calls and `../../../Dropbox/...`-style paths resolve as the author intended.
- For interactive exploration, open an R script in RStudio/Positron and run sections line by line.
- Shiny apps: open the file in RStudio and click "Run App", or run `shiny::runApp("R/shiny_3D.R")` from the project root.
- `R/run_functions.R` and `explore/test_functions.R` are the minimal end-to-end examples: they source `flea_functions.R`, load a sample FleaTag export, preprocess it, and produce plots.

## Code organization and main modules

The canonical pipeline is in `R/flea_functions.R`:

```r
read_flea_tag_data(path) ->
  flea_preprocess(data, sampling_rate, window, flying_column, flying_threshold) ->
    flea_plot() / flea_plot_spectrogram() / get_true_groups() / split_by_true_groups()
```

Key behaviors to know before changing or relying on them:

- **Reader.** For `.txt` exports, metadata is the `key: value` lines before the `lineCnt` header; data stops at the line `Delete memory by pressing button for 8`. If the header is missing, fixed column names are assigned (`lineCnt, timeMilliseconds, burstCount, accX_mg..accZ_mg, ColorSens*_cnt`). For `.csv` input, `metadata` is `NA`, so the caller must supply `sampling_rate`.
- **Sampling rate.** Comes from `metadata$AccHz` (a string; wrap in `as.numeric()`). If `NULL`, `flea_preprocess()` infers it from `timeMilliseconds`.
- **Units.** `acc*_mg / 1000` gives g. The `gain` argument is validated (2/4/8) but not used in the conversion.
- **Static/dynamic split.** Uses right-aligned rolling means (`zoo::rollmeanr`) over `window` seconds. VeDBA, VeSBA, ODBA, VM, ENMO, pitch, roll, and yaw are derived from these.
- **Flight detection.** Takes the first PCA component of raw acceleration, computes `rolling_var_PC`, and thresholds it (default 3.5). The resulting flag is shifted earlier by roughly `window_samples/4` via `lead()` to offset the right-aligned window. Changing the window or alignment changes bout boundaries downstream.
- **Spectrogram dependency.** `seewave`/`tuneR` are loaded at the top of `flea_functions.R`; sourcing it pulls in the audio stack even though many callers only need the accelerometry functions.

## Code style and conventions

- Plain R scripts, `#`-comments, snake_case function and variable names. File names are not always consistent (e.g. `Frame_segemt_Flea.R`, `Flea_weight_comarison.R`); do not "fix" them without checking for external references.
- Scripts load libraries at the top with `library()`, then define functions, then contain an interactive driver section with hard-coded file paths. It is normal for multiple sibling `file_path <- ...` lines to be left in place with only the active one uncommented.
- Hard-coded local paths (often `C:/Users/Edward/...`, `C:/Users/ehumre/...`, `C:/Users/ehurme/...`, or relative `../../../Dropbox/...`) are normal and intentional. Don't clean them up unless asked.
- Prefer extending `R/flea_functions.R` for reusable logic.
- **Duplicated code to keep in sync:**
  - `R/shiny_3D.R` deliberately keeps trimmed local copies of `get_true_groups()`, `read_flea_tag_data()`, and `flea_preprocess()` so the accelerometer tab does not pull in the `seewave`/`tuneR` dependency. Mirror relevant changes there.
  - `R/read_flea_tag_data.R` and `R/to_sec.R` are older duplicates of functions in `flea_functions.R`; prefer the canonical versions.
  - `R/align_boris_flea_shiny.R` and `R/shiny_fleatag.R` `source()` `flea_functions.R`, so they pick up changes automatically.
- RStudio project settings in `flea.Rproj`: 2-space indentation, UTF-8, POSIX line endings, strip trailing whitespace, auto-append final newline.

## Testing instructions

There are no automated tests (`testthat` etc.). Verify changes by running the affected script or app on real or sample FleaTag data and checking the outputs/plots.

- `explore/test_functions.R` manually sources `flea_functions.R` and eyeballs plots on a known CSV.
- `R/run_functions.R` is a minimal end-to-end driver that loads a FleaTag file, preprocesses it, and opens the plot.
- For Shiny changes, launch the app and exercise the upload, filtering, brushing, and export paths.
- For the hummingbird pipeline, run the scripts in order (`activity_budget`, `captive_validation`, `captive_figures`, `behaviour_separability`, `flight_intensity`) and confirm the expected CSV/PNG outputs appear before rendering the report.

## Deployment process

There is no deployment process. Scripts write outputs (CSVs, figures, PDFs) to local drives or Dropbox paths referenced by hard-coded paths. Do not commit raw data, rendered reports, or personally identifying metadata.

## Data and security considerations

- Raw data (FleaTag exports, videos, WAVs) lives OUTSIDE the repo on local drives/Dropbox and is referenced by hard-coded paths; nothing is committed.
- Never commit field data or personally identifying metadata. The repo is meant to hold code only.
- The FleaTag text format embeds metadata headers (species, trial IDs, sampling rate under key `AccHz`) parsed by `read_flea_tag_data()`.
- `.gitignore` already excludes `.Rproj.user`, `.Rhistory`, `.RData`, `.Ruserdata`, `explore/*`, `vedba_timeseries_by_rate.png`, `*.pdf`, and report build artifacts under `reports/`.

## Version control

Single-branch Git workflow (`main`) with informal, short lowercase commit messages. No PR process or CI. Match the existing commit style.

Example recent messages:

```
hummingbird project
shiny 3d
hummingbird flights
summarize flight cage acc
flight summary
read me, acc align
```
