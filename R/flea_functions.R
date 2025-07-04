# Load necessary libraries
library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)

read_flea_tag_data <- function(file_path) {

  csv <- grepl(pattern = ".csv", x = file_path)
  metadata <- NA
  if(csv){
    data <- read.csv(file_path)

  }
  if(!csv){
    # Open the file
    lines <- readLines(file_path)

    # Find the index of the line containing "Delete memory by pressing button for 8"
    stop_idx <- grep("Delete memory by pressing button for 8", lines)[1]

    # Split metadata (everything before the line starting with "lineCnt")
    metadata_end_idx <- (grep("^lineCnt", lines) - 1)[1]

    if(!is.na(metadata_end_idx)){
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
    }
    if(is.na(metadata_end_idx)){
      data_lines <- lines[1:(stop_idx - 1)]
    }
    # Read the data into a data frame
    data <- read.csv(text = paste(data_lines, collapse = "\n"))
    if(names(data)[1] != "lineCnt"){
      names(data) <- c("lineCnt","timeMilliseconds","burstCount",
                       "accX_mg","accY_mg","accZ_mg",
                       "ColorSensRed_cnt","ColorSensGreen_cnt",
                       "ColorSensBlue_cnt","ColorSensIR_cnt"  )
    }
  }

  # Return both metadata and data
  return(list(metadata = metadata, data = data))
}

get_true_groups <- function(x) {
  idx <- which(x)
  n <- length(idx)
  if (n == 0) {
    return(data.frame(start = integer(0), end = integer(0)))
  }
  diffs <- diff(idx)
  starts <- idx[c(TRUE, diffs > 1)]
  ends <- idx[c(diffs > 1, TRUE)]
  data.frame(start = starts, end = ends)
}


flea_preprocess <- function(data,
                            sampling_rate = NULL,
                            gain = 8,  # Default to 8G
                            window = 1,
                            flying_column = "rolling_var_PC",
                            flying_threshold = 3.5) {
  # Load required packages
  require(dplyr)
  require(zoo)

  # Check if data is not empty
  if (nrow(data) == 0) {
    stop("Input data is empty.")
  }

  # Validate gain setting
  if (!gain %in% c(2, 4, 8)) {
    stop("Gain must be 2, 4, or 8")
  }

  # Calculate sampling rate if not provided
  if (is.null(sampling_rate)) {
    sampling_rate <- round(1 / mean(diff(data$timeMilliseconds / 1000), na.rm = TRUE))
  }

  # Calculate window size in samples
  window_samples <- round(sampling_rate * window)

  # Convert microgravity units to g based on gain
  # Microgravity units: 1G = 1000 * gain units
  conversion_factor <- 1000
  data <- data %>%
    mutate(
      timeSeconds = timeMilliseconds / 1000,
      accX_g = accX_mg / conversion_factor,
      accY_g = accY_mg / conversion_factor,
      accZ_g = accZ_mg / conversion_factor
    )

  # Initialize new columns
  data$accX_static <- NA
  data$accY_static <- NA
  data$accZ_static <- NA
  data$accX_dynamic <- NA
  data$accY_dynamic <- NA
  data$accZ_dynamic <- NA
  data$VeDBA <- NA
  data$VeSBA <- NA
  data$ODBA <- NA
  data$VM <- NA
  data$ENMO <- NA
  data$pitch <- NA
  data$roll <- NA
  data$yaw <- NA

  # Calculate static acceleration (rolling mean)
  data$accX_static <- rollmeanr(data$accX_g, k = window_samples, fill = NA)
  data$accY_static <- rollmeanr(data$accY_g, k = window_samples, fill = NA)
  data$accZ_static <- rollmeanr(data$accZ_g, k = window_samples, fill = NA)

  # Calculate dynamic acceleration
  data$accX_dynamic <- data$accX_g - data$accX_static
  data$accY_dynamic <- data$accY_g - data$accY_static
  data$accZ_dynamic <- data$accZ_g - data$accZ_static

  # Calculate metrics
  data <- data %>%
    mutate(
      # Vector magnitude (total acceleration)
      VM = sqrt(accX_g^2 + accY_g^2 + accZ_g^2),

      # Overall Dynamic Body Acceleration
      ODBA = abs(accX_dynamic) + abs(accY_dynamic) + abs(accZ_dynamic),

      # Vectorial Dynamic Body Acceleration
      VeDBA = sqrt(accX_dynamic^2 + accY_dynamic^2 + accZ_dynamic^2),

      # Vectorial Static Body Acceleration
      VeSBA = sqrt(accX_static^2 + accY_static^2 + accZ_static^2),

      # Euclidean Norm Minus One
      ENMO = pmax(VM - 1, 0),

      # Orientation angles (in degrees)
      pitch = atan2(accY_static, sqrt(accX_static^2 + accZ_static^2)) * (180/pi),
      roll = atan2(accX_static, sqrt(accY_static^2 + accZ_static^2)) * (180/pi),
      yaw = atan2(accZ_static, sqrt(accX_static^2 + accY_static^2)) * (180/pi)
    )

  # Calculate rolling metrics
  data <- data %>%
    mutate(
      rolling_mean_X = rollmeanr(accX_g, k = window_samples, fill = NA),
      rolling_mean_Y = rollmeanr(accY_g, k = window_samples, fill = NA),
      rolling_mean_Z = rollmeanr(accZ_g, k = window_samples, fill = NA),
      rolling_var_X = rollapplyr(accX_g, width = window_samples, FUN = var, fill = NA),
      rolling_var_Y = rollapplyr(accY_g, width = window_samples, FUN = var, fill = NA),
      rolling_var_Z = rollapplyr(accZ_g, width = window_samples, FUN = var, fill = NA),
      rolling_mean_VeDBA = rollmeanr(VeDBA, k = window_samples, fill = NA),
      rolling_var_VeDBA = rollapplyr(VeDBA, width = window_samples, FUN = var, fill = NA)
    )

  # Calculate principal component
  tryCatch({
    complete_cases <- complete.cases(data[, c("accX_g", "accY_g", "accZ_g")])
    if (sum(complete_cases) > 0) {
      pca <- prcomp(data[complete_cases, c("accX_g", "accY_g", "accZ_g")], scale. = TRUE)
      data$pc[complete_cases] <- pca$x[, 1]

      # Calculate rolling metrics for PC
      data <- data %>%
        mutate(
          rolling_mean_PC = rollmeanr(pc, k = window_samples, fill = NA),
          rolling_var_PC = rollapplyr(pc, width = window_samples, FUN = var, fill = NA)
        )
    }
  }, error = function(e) {
    warning("PCA calculation failed: ", e$message)
    data$pc <- NA
    data$rolling_mean_PC <- NA
    data$rolling_var_PC <- NA
  })

  # Flying detection with shift adjustment
  data$is_flying <- FALSE
  if (!is.null(flying_column)) {
    if (flying_column %in% names(data)) {
      # Create temporary flying indicator
      data$is_flying_temp <- data[[flying_column]] > flying_threshold

      # Replace NAs with FALSE
      data$is_flying_temp[is.na(data$is_flying_temp)] <- FALSE

      # Calculate shift amount (window size in samples)
      shift_amount <- window_samples #- 1

      # Shift flying indicator backward to align with window start
      data$is_flying <- lead(data$is_flying_temp, round(shift_amount/4,0), default = FALSE)

      # Remove temporary column
      data$is_flying_temp <- NULL
    } else {
      warning("Flying column '", flying_column, "' not found. Skipping flying detection.")
    }
  }

  return(data)
}

# Function to plot data with small points highlighting high variability periods and raw VeDBA variability
flea_plot <- function(data,
                      plot_variance = TRUE,
                      plot_vedba = TRUE,
                      plot_pc = TRUE,
                      plot_spectro = TRUE,
                      plot_flying = TRUE,
                      plot_static = TRUE,
                      plot_orientation = TRUE) {

  require(plotly)

  # Prepare flying data points
  high_var_data <- data %>% filter(is_flying == TRUE)

  # Initialize spectro if missing
  if(is.null(data$spectro_freq)) {
    data$spectro_freq <- NA
  }

  # Add all traces with initial visibility
  p <- plot_ly(data, x = ~timeSeconds) %>%
    # Original acceleration signals (0-5)
    add_lines(y = ~accX_g, name = "X-axis", line = list(color = 'dodgerblue'), visible = TRUE) %>%
    add_lines(y = ~rolling_mean_X, name = "Smoothed X", line = list(color = 'lightblue', dash = 'dash'), visible = TRUE) %>%
    add_lines(y = ~accY_g, name = "Y-axis", line = list(color = 'salmon'), visible = TRUE) %>%
    add_lines(y = ~rolling_mean_Y, name = "Smoothed Y", line = list(color = 'lightcoral', dash = 'dash'), visible = TRUE) %>%
    add_lines(y = ~accZ_g, name = "Z-axis", line = list(color = 'mediumseagreen'), visible = TRUE) %>%
    add_lines(y = ~rolling_mean_Z, name = "Smoothed Z", line = list(color = 'lightgreen', dash = 'dash'), visible = TRUE) %>%

    # Static metrics (6-8)
    add_lines(y = ~VeSBA, name = "VeSBA", line = list(color = 'darkblue'), visible = plot_static) %>%
    add_lines(y = ~VM, name = "Vector Magnitude", line = list(color = 'black'), visible = plot_static) %>%
    add_lines(y = ~ENMO, name = "ENMO", line = list(color = 'darkgreen'), visible = plot_static) %>%

    # Variance traces (9-11)
    add_lines(y = ~rolling_var_X, name = "Variance X", line = list(color = 'dodgerblue', dash = 'dot'), visible = plot_variance) %>%
    add_lines(y = ~rolling_var_Y, name = "Variance Y", line = list(color = 'salmon', dash = 'dot'), visible = plot_variance) %>%
    add_lines(y = ~rolling_var_Z, name = "Variance Z", line = list(color = 'mediumseagreen', dash = 'dot'), visible = plot_variance) %>%

    # PC1 trace (12-14)
    add_lines(y = ~pc, name = "PC1", line = list(color = 'lightblue'), visible = plot_pc) %>%
    add_lines(y = ~rolling_mean_PC, name = "Smoothed PC1", line = list(color = 'darkred', dash = 'dash'), visible = plot_pc) %>%
    add_lines(y = ~rolling_var_PC, name = "Variance PC1", line = list(color = 'mediumseagreen', dash = 'dot'), visible = plot_pc) %>%

    # Spectrogram frequency trace (15)
    add_trace(y = ~spectro_freq, name = 'Spectro Freq', type = 'scatter', visible = plot_spectro) %>%

    # VeDBA/ODBA traces (16-19)
    add_lines(y = ~ODBA, name = "ODBA", line = list(color = 'orchid'), visible = plot_vedba) %>%
    add_lines(y = ~VeDBA, name = "VeDBA", line = list(color = 'orange'), visible = plot_vedba) %>%
    add_lines(y = ~rolling_mean_VeDBA, name = "Smoothed VeDBA", line = list(color = 'olive', dash = 'dash'), visible = plot_vedba) %>%
    add_lines(y = ~rolling_var_VeDBA, name = "VeDBA Var", line = list(color = 'purple', dash = 'dot'), visible = plot_vedba) %>%

    # Orientation angles (20-22)
    add_lines(y = ~pitch, name = "Pitch", line = list(color = 'darkorange'), visible = plot_orientation) %>%
    add_lines(y = ~roll, name = "Roll", line = list(color = 'darkred'), visible = plot_orientation) %>%
    add_lines(y = ~yaw, name = "Yaw", line = list(color = 'darkviolet'), visible = plot_orientation) %>%

    # High variability markers (23)
    add_markers(data = high_var_data, y = ~rolling_var_PC, name = "Flying",
                marker = list(color = 'darkcyan', size = 6), visible = plot_flying)

  # Define trace groups and their indices (0-based)
  trace_groups <- list(
    "All" = 0:23,
    "Original" = c(0, 2, 4),        # X, Y, Z axes
    "Smoothed" = c(1, 3, 5),        # Smoothed X, Y, Z
    "Static" = 6:8,                 # VeSBA, VM, ENMO
    "Variance" = 9:11,              # Variance X, Y, Z
    "PC1" = 12:14,                  # PC1, smoothed PC1, var PC1
    "Spectro" = 15,                 # Spectro Freq
    "VeDBA/ODBA" = 16:19,           # ODBA, VeDBA, Smoothed VeDBA, VeDBA Var
    "Orientation" = 20:22,          # Pitch, Roll, Yaw
    "Flying" = 23                   # Flying markers
  )

  # Function to generate visibility list for a group
  create_visible <- function(group_indices, total = 24) {
    visible <- rep(FALSE, total)
    visible[group_indices + 1] <- TRUE  # Convert to 1-based index
    return(visible)
  }

  # Create buttons for each group
  buttons <- lapply(names(trace_groups), function(group_name) {
    list(
      label = group_name,
      method = "restyle",
      args = list(list(visible = create_visible(trace_groups[[group_name]])))
    )
  })

  # Add updatemenus to the plot
  p <- p %>% layout(
    title = "Comprehensive Accelerometer Analysis",
    xaxis = list(title = "Time (seconds)"),
    yaxis = list(title = "Acceleration (g), DBA (g), and Degrees"),
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
                                 min_duration = 2,
                                 sampling_rate = 105) {

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

    # Check if group duration meets the minimum requirement
    if(n_rows >= (min_duration * sampling_rate) + sum(buffer)) {
      start_idx = max(1 + buffer["start"],1)
      end_idx   = min(n_rows - buffer["end"], n_rows)

      buffered_x = x[start_idx:end_idx ,]

      return(buffered_x)
    } else {
      return(NULL)
    }
  })

  result_list_buffered_and_filtered[sapply(result_list_buffered_and_filtered, is.null)] <- NULL

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

