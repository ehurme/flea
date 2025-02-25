# Load necessary libraries
library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)

read_flea_tag_data <- function(file_path) {
  # Open the file
  lines <- readLines(file_path)

  # Find the index of the line containing "Delete memory by pressing button for 8"
  stop_idx <- grep("Delete memory by pressing button for 8", lines)[1]

  # Split metadata (everything before the line starting with "lineCnt")
  metadata_end_idx <- (grep("^lineCnt", lines) - 1)[1]
  metadata_lines <- lines[1:metadata_end_idx]

  # Extract the metadata into a list
  metadata <- list()
  for (line in metadata_lines) {
    if (grepl(":", line)) {
      split_line <- strsplit(line, ":")[[1]]
      key <- trimws(split_line[1])
      value <- trimws(split_line[2])
      metadata[[key]] <- value
    }
  }

  # Extract the data part starting from the lineCnt header until the stop point
  data_lines <- lines[metadata_end_idx + 1:(stop_idx - metadata_end_idx - 1)]

  # Read the data into a data frame
  data <- read.csv(text = paste(data_lines, collapse = "\n"))

  # Return both metadata and data
  return(list(metadata = metadata, data = data))
}

# Function to preprocess the data
flea_preprocess <- function(data, window = 0.5) {
  # Check if data is not empty
  if (nrow(data) == 0) {
    stop("Input data is empty.")
  }

  # Calculate sampling rate
  sampling_rate <- round(1 / mean(diff(data$timeMilliseconds / 1000), na.rm = TRUE))

  # Convert acceleration to g and calculate time in seconds
  data <- data %>%
    mutate(
      timeSeconds = timeMilliseconds / 1000,
      accX_g = accX_mg / 1000,
      accY_g = accY_mg / 1000,
      accZ_g = accZ_mg / 1000
    )

  # Calculate dynamic acceleration components
  data <- data %>%
    mutate(
      accX_dyn = accX_g - mean(accX_g, na.rm = TRUE),
      accY_dyn = accY_g - mean(accY_g, na.rm = TRUE),
      accZ_dyn = accZ_g - mean(accZ_g, na.rm = TRUE)
    )

  # Calculate VeDBA and ODBA
  data <- data %>%
    mutate(
      VeDBA = sqrt(accX_dyn^2 + accY_dyn^2 + accZ_dyn^2),
      ODBA = abs(accX_dyn) + abs(accY_dyn) + abs(accZ_dyn)
    )

  # Calculate rolling metrics
  data <- data %>%
    mutate(
      rolling_mean_X = rollmean(accX_g, k = sampling_rate * window, fill = NA, align = "right"),
      rolling_mean_Y = rollmean(accY_g, k = sampling_rate * window, fill = NA, align = "right"),
      rolling_mean_Z = rollmean(accZ_g, k = sampling_rate * window, fill = NA, align = "right"),
      rolling_VeDBA = rollmean(VeDBA, k = sampling_rate * window, fill = NA, align = "right"),
      rolling_var_X = rollapply(accX_g, width = sampling_rate * window, FUN = var, fill = NA, align = "right"),
      rolling_var_Y = rollapply(accY_g, width = sampling_rate * window, FUN = var, fill = NA, align = "right"),
      rolling_var_Z = rollapply(accZ_g, width = sampling_rate * window, FUN = var, fill = NA, align = "right"),
      rolling_var_VeDBA = rollapply(VeDBA, width = sampling_rate * window, FUN = var, fill = NA, align = "right")
    )

  # Calculate first principal component
  pc <- princomp(data %>% select(accX_g, accY_g, accZ_g))
  data$pc <- pc$scores[,1]

  # Calculate rolling metrics for PC
  data <- data %>%
    mutate(
      rolling_mean_PC = rollmean(pc, k = sampling_rate * window, fill = NA, align = "right"),
      rolling_var_PC = rollapply(pc, width = sampling_rate * window, FUN = var, fill = NA, align = "right")
    )

  # Determine if flying based on rolling variance of Z-axis acceleration
  average_rolling_var_Z <- mean(data$rolling_var_Z, na.rm = TRUE)
  data <- data %>%
    mutate(is_flying = rolling_var_Z > 0.2)

  return(data)
}


# Function to plot data with small points highlighting high variability periods and raw VeDBA variability
flea_plot <- function(data, plot_variance = TRUE, plot_vedba = TRUE,
                      plot_pc = TRUE, plot_spectro = TRUE, plot_flying = TRUE) {

  # Prepare high_var_data (empty if label_high_variability is FALSE)
  high_var_data <- data %>% filter(is_flying == TRUE)

  # Add all traces with initial visibility based on parameters
  p <- plot_ly(data, x = ~timeSeconds) %>%
    # Original acceleration signals
    add_lines(y = ~accX_g, name = "X-axis", line = list(color = 'dodgerblue'), visible = TRUE) %>%
    add_lines(y = ~rolling_mean_X, name = "Smoothed X", line = list(color = 'lightblue', dash = 'dash'), visible = TRUE) %>%
    add_lines(y = ~accY_g, name = "Y-axis", line = list(color = 'salmon'), visible = TRUE) %>%
    add_lines(y = ~rolling_mean_Y, name = "Smoothed Y", line = list(color = 'lightcoral', dash = 'dash'), visible = TRUE) %>%
    add_lines(y = ~accZ_g, name = "Z-axis", line = list(color = 'mediumseagreen'), visible = TRUE) %>%
    add_lines(y = ~rolling_mean_Z, name = "Smoothed Z", line = list(color = 'lightgreen', dash = 'dash'), visible = TRUE) %>%
    # Variance traces
    add_lines(y = ~rolling_var_X, name = "Variance X", line = list(color = 'dodgerblue', dash = 'dot'), visible = plot_variance) %>%
    add_lines(y = ~rolling_var_Y, name = "Variance Y", line = list(color = 'salmon', dash = 'dot'), visible = plot_variance) %>%
    add_lines(y = ~rolling_var_Z, name = "Variance Z", line = list(color = 'mediumseagreen', dash = 'dot'), visible = plot_variance) %>%
    # PC1 trace
    add_lines(y = ~pc, name = "PC1", line = list(color = 'darkred'), visible = plot_pc) %>%
    add_lines(y = ~rolling_mean_PC, name = "Smoothed PC1", line = list(color = 'lightblue', dash = 'dash'), visible = plot_pc) %>%
    add_lines(y = ~rolling_var_PC, name = "Variance PC1", line = list(color = 'mediumseagreen', dash = 'dot'), visible = plot_pc) %>%
    # Spectrogram frequency trace
    add_trace(y = ~spectro_freq, name = 'Spectro Freq', type = 'scatter', visible = plot_spectro) %>%
    # VeDBA/ODBA traces
    add_lines(y = ~ODBA, name = "ODBA", line = list(color = 'orchid'), visible = plot_vedba) %>%
    add_lines(y = ~VeDBA, name = "VeDBA", line = list(color = 'orange'), visible = plot_vedba) %>%
    add_lines(y = ~rolling_var_VeDBA, name = "VeDBA Var", line = list(color = 'purple', dash = 'dash'), visible = plot_vedba) %>%
    # High variability markers
    add_markers(data = high_var_data, y = ~VeDBA, name = "Flying",
                marker = list(color = 'darkcyan', size = 6), visible = plot_flying)

  # Define trace groups and their indices (0-based)
  trace_groups <- list(
    "All" = 0:14,
    "Original" = c(0, 2, 4),        # X, Y, Z axes
    "Smoothed" = c(1, 3, 5),        # Smoothed X, Y, Z
    "Variance" = c(6, 7, 8),        # Variance X, Y, Z
    "VeDBA/ODBA" = c(13, 14,15),    # ODBA, VeDBA, VeDBA Var
    "PC1" = c(9,10,11),             # PC1, smoothed PC1, var PC1
    "Spectro" = 12,                 # Spectro Freq
    "HighVar" = 16                  # Flying markers
  )

  # Function to generate visibility list for a group
  create_visible <- function(group_indices, total = 15) {
    visible <- rep(FALSE, total)
    visible[group_indices + 1] <- TRUE  # Convert to 1-based index
    return(visible)
  }

  # Create buttons for each group
  buttons <- list(
    list(
      label = "All",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["All"]])))
    ),
    list(
      label = "Original Signals",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["Original"]])))
    ),
    list(
      label = "Smoothed",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["Smoothed"]])))
    ),
    list(
      label = "Variance",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["Variance"]])))
    ),
    list(
      label = "VeDBA/ODBA",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["VeDBA/ODBA"]])))
    ),
    list(
      label = "PC1",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["PrinComp"]])))
    ),
    list(
      label = "Spectro",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["Spectro"]])))
    ),
    list(
      label = "HighVar",
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[["Flying"]])))
    )
  )

  # Add updatemenus to the plot
  p <- p %>% layout(
    title = "Acceleration Axes, Smoothed Axes, ODBA, and VeDBA Over Time",
    xaxis = list(title = "Time (seconds)"),
    yaxis = list(title = "Acceleration (g) and DBA (g)"),
    legend = list(x = 0.1, y = 0.9),
    plot_bgcolor = 'grey100',
    paper_bgcolor = 'grey100',
    updatemenus = list(
      list(
        active = -1,
        type = "buttons",
        buttons = buttons,
        direction = "down",
        x = 1.05,
        xanchor = "left",
        y = 1,
        yanchor = "top"
      )
    )
  )

  return(p)
}


split_by_true_groups <- function(df, flag_column,
                                 buffer = c(start=0,end=0),
                                 min_duration = 2) {

  # Check if df is a data frame and flag_column exists
  if (!is.data.frame(df))
    stop("Input must be a data frame.")

  if (!(flag_column %in% colnames(df)))
    stop(paste0("Column '", flag_column, "' not found in the data frame."))

  # Remove rows with NA in the specified column
  df <- df[!is.na(df[[flag_column]]), ]

  # Create grouping variable
  df$group <- with(df, {
    lag_flag <- c(NA, get(flag_column)[-nrow(df)])

    group_start <- get(flag_column) & (is.na(lag_flag) | !lag_flag)

    cumsum(group_start) * get(flag_column)
  })

  # Replace zeros with NA (if any), though this shouldn't occur in this corrected logic
  df$group[df$group == 0] <- NA

  # Split into list of data frames
  result_list <- split(df[!is.na(df$group), ], df$group[!is.na(df$group)], drop = FALSE)

  # Apply buffer to each group if specified and filter out small groups
  result_list_buffered_and_filtered = lapply(result_list, function(x){
    n_rows = nrow(x)
    if(n_rows > (sum(buffer)*2)){
      start_idx = max(1 + buffer["start"],1)
      end_idx   = min(n_rows - buffer["end"], n_rows)

      buffered_x = x[start_idx:end_idx ,]

      return(buffered_x)
    }
  })

  result_list_buffered_and_filtered[sapply(result_list_buffered_and_filtered, is.null)] <- NULL

  #TODO remove flights under the minimum duration
  return(result_list_buffered_and_filtered)

}

flea_plot_spectrogram <- function(data, sampling_rate = NULL, window = 1,
                                  var_threshold = 1,
                                  fmin = 5, fmax = 40, n_peaks = 3,
                                  plot_spectro = FALSE, plot_density = TRUE,
                                  overlay_dominant_freq = TRUE, pc = TRUE) {
  if(is.null(sampling_rate)){
    sampling_rate = round(1/median(diff(data$timeSeconds)))
  }

  # Extract high variability data
  high_var_data <- data %>%
    mutate(VeDBA = ifelse(rolling_var_VeDBA < var_threshold, 0, VeDBA),
           VeDBA = ifelse(is.na(rolling_var_VeDBA), 0, VeDBA))

  # Check if we have enough data
  if (nrow(high_var_data) < 2) {
    stop("Not enough high variability data to compute the spectrogram.")
  }

  wav <- Wave(left = data$accZ_mg, samp.rate = sampling_rate, bit = 8)
  if(pc){
    wav <- Wave(left = data$pc, samp.rate = sampling_rate, bit = 8)
  }
  wav <- bwfilter(wav, from = fmin, to = fmax, f = sampling_rate)

  # Calculate the spectrogram using bioacoustics
  # print(sampling_rate*window)

  spec <- spectro(wave = wav, #FFT_size = sampling_rate * window,
                  #flim = c(fmin/1000, fmax/1000),
                  f = sampling_rate,
                  plot = plot_spectro, scale = FALSE)
  if(overlay_dominant_freq){
    par(new=TRUE)
  }
    dom_freq <- dfreq(wav, f=sampling_rate, clip = .3,
                      ovlp=25, threshold=5,
          bandpass = c(fmin, fmax),
          type="l", col="red", lwd=2, plot = overlay_dominant_freq)

  p <- ggspectro(wav, f = sampling_rate, flim = c(fmin/1000, fmax/1000))
  wb <- p + stat_contour(geom="polygon", aes(fill=..level..), bins=30) +
    scale_fill_continuous(name="Amplitude\n(dB)\n", limits=c(-30,0),
                          na.value="transparent", low="white", high="black") + theme_bw()
  if(overlay_dominant_freq){
    wb <- wb+geom_path(data = as.data.frame(dom_freq), aes(x,y,z = NULL), col = 2, lwd = 1)
  }

  if(plot_spectro){
    print(wb)
  }

  density <- meanspec(wav, f = sampling_rate, plot = FALSE)

  peaks <- fpeaks(density, f = sampling_rate, nmax = n_peaks, plot = plot_density)

  # Return the summary
  return(list(wb, dom_freq, density, peaks))
}

# density <- freq[[3]]
# threshold <- 0.8
get_peak_range <- function(density, threshold = 0.8,
                           smooth = TRUE, spar = 0.2, plot_smooth = TRUE){
  idx <- which(density[,2] > threshold)
  peak = density[idx,]
  if(smooth == TRUE){
    sm <- smooth.spline(x = density[,1], y = density[,2], spar = 0.2)
    if(plot_smooth){
      plot(density, type = "l")
      lines(sm, col = 2, lwd = 1)
    }
    idx <- which(sm$y > threshold)
    peak = data.frame(x = sm$x[idx], y = sm$y[idx])
  }
  peak_summary <- summary(peak$x)
  return(list(peak, peak_summary))
}

