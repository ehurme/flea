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
file_path <- "C:/Users/Edward/Downloads/2024-Fall-Moth-Barn/2024-Fall-Moth-Barn/20240918/Moth1_20240918_155032_FleaTagData.txt"
file_path <- "C:/Users/Edward/Downloads/2024-Fall-Moth-Barn/2024-Fall-Moth-Barn/20240917/Moth3_20240917_151055_02_secondFlight.csv"
file_path <- "C:/Users/Edward/Downloads/2024-Fall-Moth-Barn/2024-Fall-Moth-Barn/20240919/Moth7_missing_upto3000_20240919_124224_FleaTagData.txt"
file_path <- "C:/Users/Edward/Downloads/2024-Fall-Moth-Barn/2024-Fall-Moth-Barn/20240920/Moth8_greatflight_lostballearlyon_20240920_152047_FleaTagData.txt"
file_path <- "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250213_BiC_Fleatag/20250213_224550_303D_Db_193_Trial2.txt"

flea_data <- read_flea_tag_data(file_path)

sampling_rate = 210
sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()

# add VeDBA to data
processed_data <- {}
processed_data <- flea_preprocess(data = flea_data$data,
                                  sampling_rate = sampling_rate,
                                  window = 0.5,
                                  flying_column = "rolling_var_PC",
                                  flying_threshold  = 3.5)
# summary(processed_data)

flea_data$metadata$ID
# Duration
processed_data$timeSeconds %>% max(na.rm = TRUE)
## Duration flying
sum(processed_data$is_flying, na.rm = TRUE)/sampling_rate

# plot data
processed_data$spectro_freq <- NA
p <- flea_plot(processed_data)
p

s <- flea_plot_spectrogram(data = processed_data, sampling_rate = sampling_rate,
                      plot_spectro = TRUE, fmin = 5, fmax = 45)
s
peak <- get_peak_range(s[[2]])
