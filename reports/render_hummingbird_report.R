# Render reports/hummingbird_acc_report.qmd to PDF (Quarto + Typst, no LaTeX
# needed) and move the PDF to Dropbox. Run from the project root after
# R/hummingbird_activity_budget.R, R/hummingbird_captive_validation.R,
# R/hummingbird_captive_figures.R, R/hummingbird_behaviour_separability.R and
# R/hummingbird_flight_intensity.R.
#
# Typst only reads files inside the report folder, so the figures are copied
# to reports/fig/ (gitignored) first.

quarto <- "C:/Users/ehurme/AppData/Local/Programs/Positron/resources/app/quarto/bin/quarto.exe"  # bundled with Positron
hb <- "C:/Users/ehurme/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird"
out_dir <- file.path(hb, "Results/report")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("reports/fig", showWarnings = FALSE)

figs <- c(file.path(hb, "Results/captive_validation/figures",
                    c("fig1_example.png", "fig2_separation.png", "fig3_flight_pct.png", "fig4_rate.png",
                      "fig5_wingbeat.png", "fig6_bouts.png", "rf_auc_by_featureset.png", "rf_importance.png",
                      "rf_roc_pr.png", "rf_partial_dependence.png", "rf_contrasts.png")),
          file.path(hb, "WIN 2025/Results/activity_budget", c("timeline.png", "anchors.png")),
          file.path(hb, "Results/flight_intensity", c("intensity_distributions.png", "intensity_by_hour.png")))
stopifnot(all(file.exists(figs)))
file.copy(figs, "reports/fig", overwrite = TRUE)

Sys.setenv(QUARTO_R = R.home("bin"))
status <- system2(quarto, c("render", "reports/hummingbird_acc_report.qmd", "--to", "typst"))
stopifnot(status == 0)
pdf <- "reports/hummingbird_acc_report.pdf"
file.copy(pdf, file.path(out_dir, basename(pdf)), overwrite = TRUE)
file.remove(pdf)
message("report written to ", file.path(out_dir, basename(pdf)))
