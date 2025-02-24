# Load necessary libraries

library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)

source("./R/flea_functions.R")

file_path <- "C:/Users/ehumre/ownCloud/2024-Fall-Moth-Barn/20240920/Moth6_beautifulflight_20240920_133313_FleaTagData.txt"
sampling_rate = 210

# file_path = "C:/Users/ehumre/Dropbox/MPI/Wingbeat/Arizona/flightcage/20230623/Efus_bat57_trial2/[com COM3]  (2023-06-23_231113)  FleaTag.log"
# file_path <- "C:/Users/ehumre/ownCloud/FleaTags in Colombia/bats/20250214/20250214_224124_3015_Trial2_Bat197.txt"
# file_path <- "G:/My Drive/FincaFlights2025/20250219_BiC/20250219_025840_302C_Pd_222_Trial2.txt"
# sampling_rate = 105

flea_data <- read_flea_tag_data(file_path)
processed_data <- flea_preprocess(data = flea_data$data,
                                  window = 0.5)
summary(processed_data)

p <- flea_plot(processed_data, label_high_variability = TRUE, plot_variance = FALSE)
p

# segment data into groups
# split_data <- split_by_true_groups(df = processed_data, flag_column = "is_high_rolling_VeDBA",
#                                    #TODO merge close groups
#                                    buffer = c(start = sampling_rate * 1, end = sampling_rate * 1))

flea_plot_spectrogram(data = processed_data, sampling_rate = sampling_rate,
                      plot_spectro = TRUE, fmin = 5, fmax = 15)
# split_data$`1`$timeSeconds %>% range() #%>% diff()
# split_data$`3`$timeSeconds %>% range() #%>% diff()
# split_data$`6`$timeSeconds %>% range() #%>% diff()
#
# flea_plot_spectrogram(split_data$`1`, sampling_rate = 210)
# flea_plot_spectrogram(split_data$`3`, sampling_rate = 210)
# flea_plot_spectrogram(split_data$`6`, sampling_rate = 210)
# flea_plot_spectrogram(split_data$`7`, sampling_rate = 210)
# flea_plot_spectrogram(split_data$`8`, sampling_rate = 210)
# flea_plot_spectrogram(split_data$`9`, sampling_rate = 210)

