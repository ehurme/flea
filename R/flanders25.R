# flanders flights
library(tidyverse)
source("./R/flea_flight_summary.R")

files <- list.files("../../../Dropbox/MPI/Wingbeat/Flanders25/Data/202505_FleaTagging_Flanders/",
                    pattern = ".txt", recursive = TRUE, full.names = TRUE)
file <- files[20]
flight_summary <- data.frame()

for(file in files){
  bat <- flea_flight_summary(file_path = file, window = 2,
                             flying_column = "rolling_mean_VeDBA",
                             flying_threshold = 1,
                             min_flight_gap = 1)
  # TODO fix shifting flight to get flight to start at the right time
  # flight starts at 196

  flights <- data.frame(file = bat$flight_parameters$file,
                        acc_duration = bat$flight_parameters$acc_duration,
                        flight_duration = bat$flight_parameters$flight_duration,
                        peak_freq = bat$flight_parameters$peak_freq,
                        n_flights = bat$flight_parameters$n_flights,
                        start_time = bat$flight_periods[,1],
                        end_time = bat$flight_periods[,2])
  flight_summary <- rbind(flight_summary, flights)
}
