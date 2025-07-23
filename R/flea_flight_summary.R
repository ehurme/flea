flea_flight_summary <- function(file_path, sampling_rate = NULL,
                                window = 2,
                                flying_column = "rolling_var_PC",
                                flying_threshold  = 1,
                                min_flight_gap = 3) {
  require(data.table)
  require(dplyr)
  require(zoo)
  require(seewave)
  require(tuneR)
  require(plotly)

  source("./R/flea_functions.R")

  # Read data
  flea_data <- read_flea_tag_data(file_path)

  # Determine sampling rate
  if(is.null(sampling_rate)){
    sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()
  }

  # Preprocess data
  processed_data <- flea_preprocess(
    data = flea_data$data,
    window = window,
    flying_column = flying_column,
    flying_threshold  = flying_threshold
  )

  # Diagnostic plot
  processed_data$rolling_var_PC %>% hist()

  # Calculate durations
  acc_duration <- nrow(processed_data)/sampling_rate
  flight_duration <- sum(as.numeric(processed_data$is_flying), na.rm = TRUE)/sampling_rate

  # Get flight periods
  flights <- get_true_groups(processed_data$is_flying)

  if(nrow(flights) > 0) {
    flights$start_time <- processed_data$timeSeconds[flights$start]
    flights$end_time <- processed_data$timeSeconds[flights$end]
    flights$duration <- flights$end_time - flights$start_time

    # Filter short flights and merge nearby flights
    flights <- flights %>%
      filter(duration > 2) %>%
      arrange(start_time) %>%
      mutate(
        gap = ifelse(row_number() == 1, min_flight_gap + 1, start_time - lag(end_time)),
        group = cumsum(gap >= min_flight_gap)
      ) %>%
      group_by(group) %>%
      summarize(
        start = first(start),
        end = last(end),
        start_time = min(start_time),
        end_time = max(end_time),
        duration = end_time - start_time
      ) %>%
      select(-group) %>%
      as.data.frame()
  } else {
    # Create empty data frame with expected columns if no flights
    flights <- data.frame(
      start = integer(0),
      end = integer(0),
      start_time = numeric(0),
      end_time = numeric(0),
      duration = numeric(0)
    )
  }

  flight_periods <- flights[, c("start_time", "end_time", "duration")]

  # Generate spectrogram
  s <- flea_plot_spectrogram(
    data = processed_data,
    sampling_rate = sampling_rate,
    plot_spectro = TRUE,
    fmin = 5,
    fmax = 45
  )
  print(s[[1]])
  print(s[[4]])

  # Calculate peak frequency
  if(nrow(s[[4]]) > 0) {
    peak_freq <- round(s[[4]][which.max(s[[4]][,2]), 1]) * 1000
  } else {
    peak_freq <- NA_real_
  }

  # Add spectrogram frequency data to processed_data
  processed_data$spectro_freq <- NA
  if(nrow(s[[2]]) > 0) {
    idx <- which(!is.na(s[[2]][,2]))
    for(i in idx) {
      pidx <- which.min(abs(s[[2]][i,1] - processed_data$timeSeconds))
      processed_data$spectro_freq[pidx] <- s[[2]][i,2] * 1000
    }
  }

  # Generate diagnostic plot
  p <- flea_plot(processed_data)
  print(p)

  # Print diagnostics
  print(acc_duration)
  print(flight_duration)
  print(flight_periods)

  # Return results
  list(
    flight_parameters = data.frame(
      file = file_path,
      acc_duration,
      flight_duration,
      peak_freq,
      n_flights = nrow(flight_periods)
    ),
    flight_periods = flight_periods,
    processed_data = processed_data,
    plot = p,
    spectrogram = s
  )
}

# # test function
# moth <- flea_flight_summary(file_path = file_path, sampling_rate = 210)
#
flight_summary <- moth[[1]]
flight_times <- moth[[2]]
# processed_data <- moth[[3]]
moth[[4]]


