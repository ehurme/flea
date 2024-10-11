# Load necessary libraries
library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)

# Function to preprocess the data
flea_preprocess <- function(data, window = 0.5) {
  sampling_rate <- round(1 / mean(diff(data$timeMilliseconds / 1000)))

  data <- data %>%
    mutate(
      timeSeconds = timeMilliseconds / 1000,
      accX_g = accX_mg / 1000,
      accY_g = accY_mg / 1000,
      accZ_g = accZ_mg / 1000,
      rolling_mean_X = rollmean(accX_g, k = sampling_rate * window, fill = NA, align = "right"),
      rolling_mean_Y = rollmean(accY_g, k = sampling_rate * window, fill = NA, align = "right"),
      rolling_mean_Z = rollmean(accZ_g, k = sampling_rate * window, fill = NA, align = "right"),
      accX_dyn = accX_g - mean(accX_g, na.rm = TRUE),
      accY_dyn = accY_g - mean(accY_g, na.rm = TRUE),
      accZ_dyn = accZ_g - mean(accZ_g, na.rm = TRUE),
      ODBA = abs(accX_dyn) + abs(accY_dyn) + abs(accZ_dyn),
      VeDBA = sqrt(accX_dyn^2 + accY_dyn^2 + accZ_dyn^2),
      rolling_VeDBA = rollmean(VeDBA, k = 3 * sampling_rate, fill = NA, align = "right"),
      rolling_var_X = rollapply(accX_g, width = sampling_rate * window, FUN = var, fill = NA, align = "right"),
      rolling_var_Y = rollapply(accY_g, width = sampling_rate * window, FUN = var, fill = NA, align = "right"),
      rolling_var_Z = rollapply(accZ_g, width = sampling_rate * window, FUN = var, fill = NA, align = "right"),
      rolling_var_VeDBA = rollapply(VeDBA, width = sampling_rate * window, FUN = var, fill = NA, align = "right")
    )

  average_rolling_VeDBA <- mean(data$rolling_VeDBA, na.rm = TRUE)

  data <- data %>%
    mutate(is_high_rolling_VeDBA = rolling_VeDBA > average_rolling_VeDBA)

  return(data)
}

# Function to plot data with small points highlighting high variability periods and raw VeDBA variability
flea_plot <- function(data, label_high_variability = TRUE, plot_variance = FALSE) {
  p <- plot_ly(data, x = ~timeSeconds) %>%
    add_lines(y = ~accX_g, name = "X-axis", line = list(color = 'dodgerblue')) %>%
    add_lines(y = ~rolling_mean_X, name = "Smoothed X-axis", line = list(color = 'lightblue', dash = 'dash')) %>%
    add_lines(y = ~accY_g, name = "Y-axis", line = list(color = 'salmon')) %>%
    add_lines(y = ~rolling_mean_Y, name = "Smoothed Y-axis", line = list(color = 'lightcoral', dash = 'dash')) %>%
    add_lines(y = ~accZ_g, name = "Z-axis", line = list(color = 'mediumseagreen')) %>%
    add_lines(y = ~rolling_mean_Z, name = "Smoothed Z-axis", line = list(color = 'lightgreen', dash = 'dash')) %>%
    add_lines(y = ~ODBA, name = "ODBA", line = list(color = 'orchid')) %>%
    add_lines(y = ~VeDBA, name = "VeDBA", line = list(color = 'orange')) %>%
    add_lines(y = ~rolling_var_VeDBA, name = "VeDBA Variability", line = list(color = 'purple', dash = 'dash'))

  # Optionally add smaller points for high variability periods
  if (label_high_variability) {
    high_var_data <- data %>% filter(is_high_rolling_VeDBA == TRUE)

    p <- p %>%
      add_markers(data = high_var_data, y = ~VeDBA, name = "High Variability Points",
                  marker = list(color = 'darkcyan', size = 6))
  }

  # Optionally plot variance data
  if (plot_variance) {
    p <- p %>%
      add_lines(y = ~rolling_var_X, name = "Variance X", line = list(color = 'dodgerblue', dash = 'dot')) %>%
      add_lines(y = ~rolling_var_Y, name = "Variance Y", line = list(color = 'salmon', dash = 'dot')) %>%
      add_lines(y = ~rolling_var_Z, name = "Variance Z", line = list(color = 'mediumseagreen', dash = 'dot'))
  }

  # Final layout
  p <- p %>%
    layout(
      title = "Acceleration Axes, Smoothed Axes, ODBA, and VeDBA Over Time",
      xaxis = list(title = "Time (seconds)"),
      yaxis = list(title = "Acceleration (g) and DBA (g)"),
      legend = list(x = 0.1, y = 0.9),
      plot_bgcolor = 'lightgrey',
      paper_bgcolor = 'lightgrey'
    )

  return(p)
}

flea_plot_spectrogram <- function(data, sampling_rate = NULL, window = 1,
                                  fmin = 5, fmax = 20, n_peaks = 3,
                                  plot_spectro = TRUE, plot_density = TRUE) {
  if(is.null(sampling_rate)){
    sampling_rate = round(1/median(diff(data$timeSeconds)))
  }

  # Extract high variability data
  high_var_data <- processed_data %>%
    mutate(VeDBA = ifelse(rolling_var_VeDBA < var_threshold, 0, VeDBA),
           VeDBA = ifelse(is.na(rolling_var_VeDBA), 0, VeDBA))

  # Check if we have enough data
  if (nrow(high_var_data) < 2) {
    stop("Not enough high variability data to compute the spectrogram.")
  }

  wav <- Wave(left = data$accZ_mg, samp.rate = sampling_rate, bit = 8)
  wav <- bwfilter(wav, from = fmin, to = fmax, f = sampling_rate)

  # Calculate the spectrogram using bioacoustics
  print(sampling_rate*window)

  spec <- spectro(wave = wav, #FFT_size = sampling_rate * window,
                  flim = c(fmin/1000, fmax/1000), f = sampling_rate, plot = FALSE)
  p <- ggspectro(wav, f = sampling_rate, flim = c(fmin/1000, fmax/1000))
  wb <- p + stat_contour(geom="polygon", aes(fill=..level..), bins=30) +
    scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-30,0),
                          na.value="transparent", low="white", high="black") + theme_bw()
  if(plot_spectro == TRUE){
    print(wb)
  }

  density <- meanspec(wav, f = sampling_rate, plot = FALSE)

  peaks <- fpeaks(density, f = sampling_rate, nmax = n_peaks, plot = plot_density)

  # Return the summary
  return(peaks)
}
