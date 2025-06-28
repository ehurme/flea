flea_flight_summary <- function(file_path, sampling_rate = NULL) {
  require(data.table)
  require(dplyr)
  require(zoo)
  require(seewave)
  require(tuneR)
  require(plotly)

  source("./R/flea_functions.R")
  # read data
  flea_data <- read_flea_tag_data(file_path)

  if(is.null(sampling_rate)){
    sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()
  }

  # add VeDBA to data
  processed_data <- flea_preprocess(data = flea_data$data,
                                    #sampling_rate = sampling_rate,
                                    window = 0.5,
                                    flying_column = "rolling_var_PC",
                                    flying_threshold  = 1)
  # summary(processed_data)

  acc_duration <- nrow(processed_data)/sampling_rate
  # flying duration
  flight_duration <- sum(as.numeric(processed_data$is_flying), na.rm = TRUE)/sampling_rate

  # get start and stop times of each flight
  flights <- get_true_groups(processed_data$is_flying)
  flights$start_time <- processed_data$timeSeconds[flights$start]
  flights$end_time <- processed_data$timeSeconds[flights$end]
  flights$duration <- flights$end_time - flights$start_time
  flights <- flights %>% filter(duration > 2)

  if(nrow(flights) > 2){
    flights$diff_time <- c(NA,  flights$start_time[2:nrow(flights)] - flights$end_time[1:(nrow(flights)-1)])

    # merge flights that have a gap less than 3 seconds
    idx <- which(flights$diff_time < 3)
    for(i in idx){
      flights$end[i-1] <- flights$end[i]
      flights$end_time[i-1] <- flights$end_time[i]
    }
    flights <- flights[-idx,]
    flights$duration <- flights$end_time - flights$start_time
    flights$diff_time <- c(NA,  flights$start_time[2:nrow(flights)] - flights$end_time[1:(nrow(flights)-1)])
  }

  flight_periods <- flights[,c("start_time", "end_time")]


  s <- flea_plot_spectrogram(data = processed_data, sampling_rate = sampling_rate,
                             plot_spectro = TRUE, fmin = 5, fmax = 45)
  print(s[[1]])
  print(s[[4]])
  peak_freq <- round(s[[4]][which.max(s[[4]][,2]),1] * 1000, 1) %>% as.numeric()

  # add spectro data
  processed_data$spectro_freq <- NA
  idx <- which(!is.na(s[[2]][,2]))
  for(i in idx){
    pidx <- which.min(abs(s[[2]][i,1] - processed_data$timeSeconds))
    processed_data$spectro_freq[pidx] <- s[[2]][i,2]*1000
  }

  # plot data
  p <- flea_plot(processed_data)
  print(p)

  print(acc_duration)
  print(flight_duration)
  print(flight_periods)

  result <- list(
    flight_parameters = data.frame(
      file = file_path,
      acc_duration,
      flight_duration,
      peak_freq,
      n_flights = nrow(flight_periods)
    ),
    flight_periods = flight_periods,
    processed_data,
    p,
    s
  )

  return(result)
}

# test function
moth <- flea_flight_summary(file_path = file_path, sampling_rate = 210)


processed_data <- moth[[3]]
ggplot(processed_data, aes(yaw, rolling_var_PC, col = timeSeconds))+geom_point()

ggplot(processed_data %>% filter(is_flying), aes(timeSeconds, yaw, col = rolling_var_PC))+geom_point()
