# CLAUDE.md

Read `AGENTS.md` first — it is the main guide for this repo (project overview,
layout, conventions, data policy). `README.md` maps every script and must be
kept in sync when scripts are added or removed. This file adds the details
that matter when editing code.

## Essentials

- **Not a package.** R scripts under `R/` are `source()`d or run line by line
  in RStudio. There is no build, no test suite, and no CI. Verify changes by
  running the affected script or app on a real FleaTag export and checking
  the output or plots (`explore/test_functions.R` shows the pattern, and
  `R/run_functions.R` is a minimal end-to-end driver).
- **Run from the project root.** Scripts use `source("./R/flea_functions.R")`
  and relative Dropbox paths such as `../../../Dropbox/MPI/Wingbeat/...`.
  Exception: `R/join_tracks_till.R` sources an absolute
  `C:/Users/Edward/Desktop/...` path.
- **Hard-coded paths are normal.** Drivers stack several `file_path <- ...`
  lines, and the last assignment wins. Don't "clean these up" unless asked.
  Paths mix two machines (`C:/Users/Edward/...` and
  `C:/Users/ehumre/...`/`ehurme`).
- Commits: short lowercase messages, single `main` branch.

## Core pipeline (`R/flea_functions.R`)

`read_flea_tag_data(path)` → `flea_preprocess(data, sampling_rate, window, flying_column, flying_threshold)` → `flea_plot()` / `flea_plot_spectrogram()` → `get_true_groups()` / `split_by_true_groups()` on `is_flying`.

Behaviours to know before changing or relying on them:

- **Reader.** For `.txt` exports, metadata is the `key: value` lines before
  the `lineCnt` header. Data stops at the line `Delete memory by pressing
  button for 8`. If the header is missing, fixed column names are assigned
  (`lineCnt, timeMilliseconds, burstCount, accX_mg..accZ_mg,
  ColorSens*_cnt`). For `.csv` input, `metadata` is `NA`, so
  `metadata$AccHz` isn't available and the caller must supply
  `sampling_rate`. CSV detection is `grepl(".csv", path)` (unescaped regex).
- **Sampling rate** comes from `metadata$AccHz` (a string, so wrap it in
  `as.numeric()`). If `NULL`, `flea_preprocess` infers it from
  `timeMilliseconds`.
- **Units.** `acc*_mg / 1000` gives g. The `gain` argument is validated
  (2/4/8) but not used in the conversion.
- **Static/dynamic split** uses right-aligned rolling means (`zoo::rollmeanr`)
  over `window` seconds. VeDBA, VeSBA, ODBA, VM, ENMO, pitch, roll and yaw
  are derived from these.
- **Flight detection.** It takes the first PCA component of raw
  acceleration, computes `rolling_var_PC`, and thresholds it (default 3.5).
  The resulting flag is shifted earlier by `window_samples/4` via `lead()`
  to offset the right-aligned window. Changing the window or alignment
  changes bout boundaries everywhere downstream.
- `seewave`/`tuneR` are loaded at the top of the file (they are needed only
  for spectrograms), so sourcing it pulls in the audio stack.

## Duplicated code to keep in sync

- `R/shiny_3D.R` has its own trimmed copies of `get_true_groups()`,
  `read_flea_tag_data()` and `flea_preprocess()`, kept deliberately to
  avoid the seewave/tuneR dependency. Mirror relevant changes there.
- `R/read_flea_tag_data.R` (older reader) and `R/to_sec.R` duplicate
  functions in `flea_functions.R`. Prefer the `flea_functions.R` versions.
- `R/align_boris_flea_shiny.R` and `R/shiny_fleatag.R` `source()`
  `flea_functions.R`, so they pick up changes automatically.

## TRex / 3D data notes

- `transform_trex_pose_data()` reads a per-camera TRex export and keeps rows
  with `missing == 0` and pose keypoints `pose_x0..2`/`pose_y0..2`. The
  header comment in `R/transform_trex_pose_data.R` explains the frame
  offset: per-camera `FrameDiffs` are **subtracted** from frame numbers.
- `R/shiny_3D.R` (~935 lines) is the largest and most actively developed
  file. It covers the 3D viewer, straight/level-flight bout detection,
  and ACC upload with offset and sampling-rate alignment to track speed.
