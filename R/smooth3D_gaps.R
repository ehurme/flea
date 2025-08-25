#' Smooth 3D trajectory with gap awareness
#'
#' @param df Data frame containing x, y, z coordinates and frame numbers
#' @param x_col Name of x coordinate column
#' @param y_col Name of y coordinate column
#' @param z_col Name of z coordinate column
#' @param frame_col Name of frame number column
#' @param gap_threshold Maximum allowed gap to interpolate (in frames)
#' @param method Spline method ("fmm", "natural", "periodic", or "monoH.FC")
#' @return Data frame with smoothed coordinates and NA values for gaps
smooth_3d_with_gaps <- function(df, x_col = "x", y_col = "y", z_col = "z",
                                frame_col = "frame", gap_threshold = 60,
                                method = "natural") {

  # Extract data
  x <- df[[x_col]]
  y <- df[[y_col]]
  z <- df[[z_col]]
  frames <- df[[frame_col]]

  # Identify continuous segments
  frame_diff <- diff(frames)
  gap_starts <- which(frame_diff > 1 & frame_diff <= gap_threshold)
  gap_ends <- gap_starts + 1
  segment_breaks <- which(frame_diff > gap_threshold)

  # Create list of continuous segments
  segments <- list()
  start_idx <- 1

  for (break_idx in c(segment_breaks, length(frames))) {
    end_idx <- break_idx
    segments <- c(segments, list(start_idx:end_idx))
    start_idx <- break_idx + 1
  }

  # Initialize output vectors
  out_frames <- min(frames):max(frames)
  out_x <- out_y <- out_z <- rep(NA, length(out_frames))

  # Process each continuous segment
  for (seg in segments) {
    if (length(seg) < 2) next  # Skip single-point segments

    seg_frames <- frames[seg]
    seg_x <- x[seg]
    seg_y <- y[seg]
    seg_z <- z[seg]

    # Create normalized time for the segment (0 to 1)
    t_norm <- (seg_frames - min(seg_frames)) / (max(seg_frames) - min(seg_frames))

    # Fit splines to the segment
    spline_x <- splinefun(t_norm, seg_x, method = method)
    spline_y <- splinefun(t_norm, seg_y, method = method)
    spline_z <- splinefun(t_norm, seg_z, method = method)

    # Generate output points for this segment
    seg_out_start <- which(out_frames == min(seg_frames))
    seg_out_end <- which(out_frames == max(seg_frames))
    t_out <- seq(0, 1, length.out = seg_out_end - seg_out_start + 1)

    # Store smoothed values
    out_x[seg_out_start:seg_out_end] <- spline_x(t_out)
    out_y[seg_out_start:seg_out_end] <- spline_y(t_out)
    out_z[seg_out_start:seg_out_end] <- spline_z(t_out)
  }

  # Create output data frame
  result <- data.frame(
    x = out_x,
    y = out_y,
    z = out_z,
    frame = out_frames
  )

  return(result)
}

#' Plot gap-aware 3D trajectory
#'
#' @param original_df Original data frame with gaps
#' @param smoothed_df Smoothed data frame from smooth_3d_with_gaps()
plot_gap_aware_trajectory <- function(original_df, smoothed_df) {
  if (!requireNamespace("rgl", quietly = TRUE)) {
    stop("Please install the 'rgl' package: install.packages('rgl')")
  }

  rgl::open3d()

  # Plot original points (only where detected)
  detected <- complete.cases(original_df)
  rgl::plot3d(
    original_df$x[detected], original_df$y[detected], original_df$z[detected],
    col = "gray", type = "p", size = 3,
    xlab = "X", ylab = "Y", zlab = "Z",
    main = "Gap-Aware 3D Trajectory Smoothing"
  )

  # Plot smoothed path (only continuous segments)
  continuous <- complete.cases(smoothed_df)
  rgl::lines3d(
    smoothed_df$x[continuous], smoothed_df$y[continuous], smoothed_df$z[continuous],
    col = "red", lwd = 3
  )

  # Add legend
  rgl::legend3d("topright",
                legend = c("Detected Points", "Smoothed Path"),
                col = c("gray", "red"),
                pch = c(16, NA),
                lty = c(NA, 1))
}

# Example with gaps in tracking
set.seed(42)
t <- seq(0, 10, length.out = 1000)
true_x <- t * cos(t)
true_y <- t * sin(t)
true_z <- t

# Create tracking data with gaps
noisy_df <- data.frame(
  x = true_x + rnorm(100, sd = 1.5),
  y = true_y + rnorm(100, sd = 1.5),
  z = true_z + rnorm(100, sd = 1),
  frame = 1:100
)

# Simulate object disappearing between frames 30-50 and 70-90
noisy_df[30:50, c("x", "y", "z")] <- NA
noisy_df[70:90, c("x", "y", "z")] <- NA

# Remove NA rows (when object wasn't detected)
noisy_df <- noisy_df[complete.cases(noisy_df), ]

# Smooth with gap awareness (won't bridge large gaps)
smoothed_gap <- smooth_3d_with_gaps(noisy_df, gap_threshold = 10)

# Plot results
plot_gap_aware_trajectory(noisy_df, smoothed_gap)


smoothed_spline <- smooth_3d_spline(df %>% filter(x > -3000 | x < 3000,
                                                  y > -3000 | y < 3000,
                                                  z > -3000 | z < 3000,
                                                  Frame < 600),
                                    n_out = 100, method = "natural")
plot_3d_spline_trajectory(df %>% filter(x > -3000 | x < 3000,
                                  y > -3000 | y < 3000,
                                  z > -3000 | z < 3000,
                                  Keypoint == "pose0", Frame < 500),
                          smoothed_spline)


plot(smoothed_spline$frame, smoothed_spline$x, type = "l", col = "red", lwd = 2,
     xlab = "Frame", ylab = "X Coordinate", main = "Smoothed X Coordinate Over Frames")
points(df$Frame, df$x, col = "gray", pch = 16, cex = 0.5)
