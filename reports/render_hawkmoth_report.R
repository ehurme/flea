# Render reports/hawkmoth_flight_report.qmd to PDF (Quarto + Typst, no LaTeX
# needed) and move the PDF to Dropbox. Run from the project root after
# R/hawkmoth_flight_dynamics.R.
#
# Typst only reads files inside the report folder, so the figures are copied
# to reports/fig/hawkmoth/ (gitignored) first.

quarto <- "C:/Users/ehurme/AppData/Local/Programs/Positron/resources/app/quarto/bin/quarto.exe"  # bundled with Positron
res_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/2024-Fall-Moth-Barn/Results/flight_dynamics"
out_dir <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/2024-Fall-Moth-Barn/Results/report"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("reports/fig/hawkmoth", recursive = TRUE, showWarnings = FALSE)

figs <- c(file.path(res_dir, c("bouts.png", "within_bout.png", "cycles.png", "warmup.png")),
          list.files(res_dir, "^overview_.*\\.png$", full.names = TRUE))
stopifnot(all(file.exists(figs)))
file.copy(figs, "reports/fig/hawkmoth", overwrite = TRUE)

Sys.setenv(QUARTO_R = R.home("bin"))
status <- system2(quarto, c("render", "reports/hawkmoth_flight_report.qmd", "--to", "typst"))
stopifnot(status == 0)
pdf <- "reports/hawkmoth_flight_report.pdf"
file.copy(pdf, file.path(out_dir, basename(pdf)), overwrite = TRUE)
file.remove(pdf)
message("report written to ", file.path(out_dir, basename(pdf)))
