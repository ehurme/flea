# Load necessary libraries

library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)

# load functions
source("./R/flea_functions.R")

# load flea tag data
file_path <- "C:/Users/ehumre/ownCloud/2024-Fall-Moth-Barn/20240920/Moth6_beautifulflight_20240920_133313_FleaTagData.txt"
sampling_rate = 210
flea_data <- read_flea_tag_data(file_path)

# add VeDBA to data
processed_data <- flea_preprocess(data = flea_data$data,
                                  window = 0.5)
# summary(processed_data)

# plot data
p <- flea_plot(processed_data, label_high_variability = TRUE, plot_variance = FALSE)
p

s <- flea_plot_spectrogram(data = processed_data, sampling_rate = sampling_rate,
                      plot_spectro = TRUE, fmin = 5, fmax = 45)

peak <- get_peak_range(s[[2]])
