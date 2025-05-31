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
file_path <- "C:/Users/ehumre/Downloads/20250228_102905_Anthracathorax-nigricollis_045Hz_3054_Trial023.txt"
file_path <- "C:/Users/ehumre/Downloads/20250226_163947_Saucerottia-cyanifrons_105HzCont_300B_Trial019.txt"
file_path <- "C:/Users/ehumre/Downloads/20250222_141446_Chalyburra_buffonii_210HzCont_3044_Trial011.txt"
file_path <- "C:/Users/ehumre/Downloads/20250224_162700_Colibri-cyanotus_210HzCont_3054_Trial013.txt"
file_path <- "C:/Users/ehumre/Downloads/20250220_114529_Anthracothorax-nigricollis_210HzCont_300A_Trial005.txt"

flea_data <- read_flea_tag_data(file_path)
sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()

# add VeDBA to data
processed_data <- flea_preprocess(data = flea_data$data,
                                  sampling_rate = sampling_rate,
                                  window = 0.5,
                                  flying_column = "rolling_var_PC",
                                  flying_threshold  = 0.1)
# summary(processed_data)

# plot data
processed_data$spectro_freq <- NA
p <- flea_plot(processed_data)
p

s <- flea_plot_spectrogram(data = processed_data, sampling_rate = sampling_rate,
                      plot_spectro = TRUE, fmin = 5, fmax = 45)

peak <- get_peak_range(s[[2]])
