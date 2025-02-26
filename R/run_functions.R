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
file_path <- "C:/Users/Ecophysics lab/Downloads/Fleatag Programming/WildLab Serial Monitor/20250226_163947_FleaTagData_HB_2603_Tag304C.txt"

flea_data <- read_flea_tag_data(file_path)
sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()

# add VeDBA to data
processed_data <- flea_preprocess(data = flea_data$data,
                                  window = 0.5)
# summary(processed_data)

# plot data
processed_data$spectro_freq <- NA
p <- flea_plot(processed_data, plot_spectro = FALSE)
p

s <- flea_plot_spectrogram(data = processed_data, sampling_rate = sampling_rate,
                      plot_spectro = TRUE, fmin = 5, fmax = 45)

peak <- get_peak_range(s[[2]])
