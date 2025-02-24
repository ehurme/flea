
library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)

source("./R/flea_functions.R")
sampling_rate = 105

files <- list.files(path = "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/",
           pattern = "Trial", recursive = TRUE, full.names = TRUE)

wb <- list()
for(file_path in files){
  flea_data <- read_flea_tag_data(file_path)
  processed_data <- flea_preprocess(data = flea_data$data,
                                    window = 0.5)

  # p <- flea_plot(processed_data, label_high_variability = TRUE, plot_variance = FALSE)
  # p


  freq <- flea_plot_spectrogram(data = processed_data, #[complete.cases(processed_data$timeMilliseconds),],
                      sampling_rate = 105, fmin = 5, fmax = 40, window = 2,
                      plot_spectro = FALSE, plot_density = TRUE)
  peak <- get_peak_range(density = freq[[2]], smooth = TRUE)
  peak[[3]] <- file_path

}
