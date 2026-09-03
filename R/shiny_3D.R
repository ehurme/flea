library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

# Collapses a logical vector into start/end row indices of its TRUE runs.
# (Same utility as get_true_groups() in R/flea_functions.R; kept local here
# so the straight-flight summary below doesn't pull in that file's
# seewave/tuneR dependencies just for this one helper.)
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

# read_flea_tag_data() and flea_preprocess() below are adapted from
# R/flea_functions.R (used by R/align_boris_flea_shiny.R for FleaTag ACC
# import and VeDBA/flying-detection preprocessing) -- copied and trimmed to
# just what this tab needs (VeDBA + PC-based flying detection) rather than
# `source()`d, so the accelerometer tab doesn't pull in that file's
# seewave/tuneR dependency (only used there for spectrograms).

read_flea_tag_data <- function(file_path) {
  csv <- grepl(pattern = ".csv", x = file_path)
  metadata <- NA
  if (csv) {
    data <- read.csv(file_path)
  }
  if (!csv) {
    lines <- readLines(file_path)
    stop_idx <- grep("Delete memory by pressing button for 8", lines)[1]
    metadata_end_idx <- (grep("^lineCnt", lines) - 1)[1]

    if (!is.na(metadata_end_idx)) {
      metadata_lines <- lines[1:metadata_end_idx]
      metadata <- list()
      for (line in metadata_lines) {
        if (grepl(":", line)) {
          split_line <- strsplit(line, ":")[[1]]
          key <- trimws(split_line[1])
          value <- trimws(split_line[2])
          metadata[[key]] <- value
        }
      }
      data_lines <- lines[metadata_end_idx + 1:(stop_idx - metadata_end_idx - 1)]
    }
    if (is.na(metadata_end_idx)) {
      data_lines <- lines[1:(stop_idx - 1)]
    }
    data <- read.csv(text = paste(data_lines, collapse = "\n"))
    if (names(data)[1] != "lineCnt") {
      names(data) <- c("lineCnt", "timeMilliseconds", "burstCount",
                        "accX_mg", "accY_mg", "accZ_mg",
                        "ColorSensRed_cnt", "ColorSensGreen_cnt",
                        "ColorSensBlue_cnt", "ColorSensIR_cnt")
    }
  }
  list(metadata = metadata, data = data)
}

flea_preprocess <- function(data,
                             sampling_rate = NULL,
                             gain = 8,
                             window = 1,
                             flying_column = "rolling_var_PC",
                             flying_threshold = 3.5) {
  require(dplyr)
  require(zoo)

  if (nrow(data) == 0) stop("Input data is empty.")
  if (!gain %in% c(2, 4, 8)) stop("Gain must be 2, 4, or 8")

  if (is.null(sampling_rate)) {
    sampling_rate <- round(1 / mean(diff(data$timeMilliseconds / 1000), na.rm = TRUE))
  }
  window_samples <- round(sampling_rate * window)

  conversion_factor <- 1000
  data <- data %>%
    mutate(timeSeconds = timeMilliseconds / 1000,
           accX_g = accX_mg / conversion_factor,
           accY_g = accY_mg / conversion_factor,
           accZ_g = accZ_mg / conversion_factor)

  data$accX_static <- rollmeanr(data$accX_g, k = window_samples, fill = NA)
  data$accY_static <- rollmeanr(data$accY_g, k = window_samples, fill = NA)
  data$accZ_static <- rollmeanr(data$accZ_g, k = window_samples, fill = NA)
  data$accX_dynamic <- data$accX_g - data$accX_static
  data$accY_dynamic <- data$accY_g - data$accY_static
  data$accZ_dynamic <- data$accZ_g - data$accZ_static

  data <- data %>%
    mutate(
      VM = sqrt(accX_g^2 + accY_g^2 + accZ_g^2),
      ODBA = abs(accX_dynamic) + abs(accY_dynamic) + abs(accZ_dynamic),
      VeDBA = sqrt(accX_dynamic^2 + accY_dynamic^2 + accZ_dynamic^2),
      VeSBA = sqrt(accX_static^2 + accY_static^2 + accZ_static^2),
      ENMO = pmax(VM - 1, 0),
      pitch = atan2(accY_static, sqrt(accX_static^2 + accZ_static^2)) * (180 / pi),
      roll = atan2(accX_static, sqrt(accY_static^2 + accZ_static^2)) * (180 / pi),
      yaw = atan2(accZ_static, sqrt(accX_static^2 + accY_static^2)) * (180 / pi)
    )

  data <- data %>%
    mutate(
      rolling_mean_VeDBA = rollmeanr(VeDBA, k = window_samples, fill = NA),
      rolling_var_VeDBA = rollapplyr(VeDBA, width = window_samples, FUN = var, fill = NA)
    )

  tryCatch({
    complete_cases <- complete.cases(data[, c("accX_g", "accY_g", "accZ_g")])
    if (sum(complete_cases) > 0) {
      pca <- prcomp(data[complete_cases, c("accX_g", "accY_g", "accZ_g")], scale. = TRUE)
      data$pc[complete_cases] <- pca$x[, 1]
      data <- data %>%
        mutate(rolling_mean_PC = rollmeanr(pc, k = window_samples, fill = NA),
               rolling_var_PC = rollapplyr(pc, width = window_samples, FUN = var, fill = NA))
    }
  }, error = function(e) {
    warning("PCA calculation failed: ", e$message)
    data$pc <- NA
    data$rolling_mean_PC <- NA
    data$rolling_var_PC <- NA
  })

  data$is_flying <- FALSE
  if (!is.null(flying_column) && flying_column %in% names(data)) {
    is_flying_temp <- data[[flying_column]] > flying_threshold
    is_flying_temp[is.na(is_flying_temp)] <- FALSE
    data$is_flying <- lead(is_flying_temp, round(window_samples / 4, 0), default = FALSE)
  }

  data
}

ui <- fluidPage(
  titlePanel("3D Trajectory Viewer"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      numericInput("window", "frame Window", value = 300, min = 1),
      selectInput("keypoint", "keypoints", choices = NULL, multiple = TRUE),
      selectInput("colorField", "Color by:",
                  choices = c("frame", "Speed", "TurnAngle", "Heading")),
      radioButtons("units", "Display units", choices = c("m", "mm"),
                   selected = "m", inline = TRUE),
      checkboxInput("showSkeleton", "Connect keypoints at current frame (skeleton)", FALSE),
      checkboxInput("fadeTrail", "Fade trail by recency", TRUE),
      sliderInput("xrange", "X Range (m)", min = -10, max = 10, value = c(-2, 2)),
      sliderInput("yrange", "Y Range (m)", min = -10, max = 10, value = c(-1, 2)),
      sliderInput("zrange", "Z Range (m)", min = -10, max = 10, value = c(-3, 3)),
      sliderInput("maxspeed", "Max Speed (m/s)", min = 0, max = 50, value = 10),

      # frame slider with 60 FPS
      sliderInput("frame", "frame", min = 0, max = 100, value = 0, step = 1,
                  animate = animationOptions(interval = 1000/10, loop = TRUE)),

      hr(),
      downloadButton("downloadCSV", "Download filtered data (CSV)"),
      downloadButton("downloadPlot", "Download XY plot (PNG)")
    ),

    mainPanel(
      plotOutput("timeline", height = "150px",
                 brush = brushOpts(id = "timelineBrush", direction = "x", resetOnNew = FALSE)),
      actionButton("clearBrush", "Clear range selection"),
      tabsetPanel(
        tabPanel("XY", plotOutput("xyplot", height = "600px", width = "100%")),
        tabPanel("XZ", plotOutput("xzplot", height = "600px", width = "100%")),
        tabPanel("YZ", plotOutput("yzplot", height = "600px", width = "100%")),
        tabPanel("3D",
                 fluidRow(
                   actionButton("camTop", "Top"),
                   actionButton("camFront", "Front"),
                   actionButton("camSide", "Side"),
                   actionButton("camReset", "Reset"),
                   downloadButton("downloadGif", "Export rotating GIF")
                 ),
                 plotlyOutput("plot3d", height = "600px", width = "100%")),
        tabPanel("Summary",
                 h4("Movement summary"),
                 tableOutput("summaryTable"),
                 h4("Straight/level flights (elevation change ≤ 0.3 m, duration ≥ 1 s)"),
                 tableOutput("straightFlightTable")),
        tabPanel("Accelerometer",
                 fluidRow(
                   column(6,
                     fileInput("accFile", "Upload FleaTag ACC file (.txt or .csv)",
                               accept = c(".txt", ".csv")),
                     numericInput("accWindow", "Smoothing window (s)", value = 0.5, min = 0.05, step = 0.05),
                     numericInput("accFlyThreshold", "Flying threshold (rolling var, PC1)", value = 0.5, step = 0.1)
                   ),
                   column(6,
                     h4("Align to track time"),
                     numericInput("accOffset", "Offset (s): ACC start relative to track start", value = 0, step = 0.1),
                     numericInput("accNewSR", "Corrected sampling rate (Hz)", value = NA, min = 1, step = 1),
                     actionButton("autoAlign", "Auto-align (rough)"),
                     helpText("Auto-align cross-correlates ACC 'flying' bouts against track speed ",
                              "(gaps where the track is out of view are ignored, same as BORIS) to ",
                              "set a starting offset. Then nudge offset/sampling rate by hand until the ",
                              "ACC flight signal (red points) lines up with track speed (black line)."),
                     downloadButton("downloadAccAligned", "Export aligned ACC data (CSV)")
                   )
                 ),
                 plotOutput("accAlignPlot", height = "400px"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive data loader
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath) |> janitor::clean_names()

    # Convert mm -> m
    df <- df %>%
      mutate(x = x / 1000,
             y = y / 1000,
             z = z / 1000) %>%
      arrange(frame)

    # Compute speed (m/s) and heading
    df <- df %>%
      group_by(id, keypoint) %>%
      arrange(frame) %>%
      mutate(dx = x - lag(x),
             dy = y - lag(y),
             dz = z - lag(z),
             dt = (frame - lag(frame)) * (1/30),
             speed = sqrt(dx^2 + dy^2 + dz^2)/dt,
             heading = atan2(dy, dx)) %>%
      ungroup()

    # Turn angle (clamp to [-1, 1] before acos to avoid NaN from floating-point drift)
    df <- df %>%
      group_by(id, keypoint) %>%
      mutate(vx = dx/dt, vy = dy/dt, vz = dz/dt,
             cos_turn = (lag(vx)*vx + lag(vy)*vy + lag(vz)*vz) /
               (sqrt(lag(vx)^2+lag(vy)^2+lag(vz)^2) *
                  sqrt(vx^2+vy^2+vz^2)),
             TurnAngle = acos(pmin(pmax(cos_turn, -1), 1))) %>%
      select(-cos_turn) %>%
      ungroup()

    df
  })

  # Update frame slider, keypoints, and spatial/speed ranges to match uploaded data
  observe({
    df <- data()

    updateSliderInput(session, "frame",
                      min = min(df$frame), max = max(df$frame),
                      value = min(df$frame))
    updateSelectInput(session, "keypoint",
                      choices = unique(df$keypoint),
                      selected = unique(df$keypoint))

    xr <- range(df$x, na.rm = TRUE)
    yr <- range(df$y, na.rm = TRUE)
    zr <- range(df$z, na.rm = TRUE)
    maxspd <- max(df$speed, na.rm = TRUE)

    updateSliderInput(session, "xrange", min = floor(xr[1]), max = ceiling(xr[2]), value = xr)
    updateSliderInput(session, "yrange", min = floor(yr[1]), max = ceiling(yr[2]), value = yr)
    updateSliderInput(session, "zrange", min = floor(zr[1]), max = ceiling(zr[2]), value = zr)
    updateSliderInput(session, "maxspeed", max = ceiling(maxspd), value = ceiling(maxspd))
  })

  # Custom frame-range selection from brushing the timeline; overrides the
  # frame/window slider pair when active
  selRange <- reactiveVal(NULL)

  observeEvent(input$timelineBrush, {
    b <- input$timelineBrush
    req(b)
    selRange(c(floor(b$xmin), ceiling(b$xmax)))
  })

  observeEvent(input$clearBrush, {
    selRange(NULL)
  })

  # Column backing the selected color field -- mapping this to an actual
  # column name (rather than pre-extracting a vector tied to one data frame's
  # row order) keeps color correct no matter how a downstream df gets
  # reordered/regrouped for plotting.
  color_col <- reactive({
    switch(input$colorField,
           "frame" = "frame",
           "Speed" = "speed",
           "TurnAngle" = "TurnAngle",
           "Heading" = "heading")
  })

  unit_mult <- reactive(if (input$units == "mm") 1000 else 1)

  # Spatial/speed/keypoint-filtered data, independent of the current-frame
  # window. This is the full playable range for the client-side 3D animation;
  # `filtered()` narrows it further to a single trailing window for the 2D
  # plots, skeleton, summary table, and GIF export.
  spatialFiltered <- reactive({
    df <- data()
    req(df, input$keypoint)

    df <- df %>%
      filter(keypoint %in% input$keypoint,
             x >= input$xrange[1], x <= input$xrange[2],
             y >= input$yrange[1], y <= input$yrange[2],
             z >= input$zrange[1], z <= input$zrange[2],
             is.na(speed) | speed <= input$maxspeed)

    if (!is.null(selRange())) {
      rng <- selRange()
      df <- df %>% filter(frame >= rng[1], frame <= rng[2])
    }

    # Bug fix: previously nothing grouped rows by individual/keypoint before
    # plotting, so geom_path/plot_ly would draw a spurious connecting line
    # between different flea IDs (and between different keypoints) whenever
    # more than one was selected. `grp` gives each id+keypoint its own line.
    df %>%
      mutate(grp = interaction(id, keypoint, drop = TRUE)) %>%
      arrange(id, keypoint, frame)
  })

  # Filtered data further narrowed to the trailing window around the current
  # frame (skipped when a timeline-brush range selection is active, since
  # that range IS the window in that case).
  filtered <- reactive({
    df <- spatialFiltered()
    if (is.null(selRange())) {
      # NOTE: naming these the same as the `frame` column previously caused
      # dplyr's data mask to resolve BOTH sides of the filter to df$frame,
      # making the window filter a no-op. Keep these names distinct.
      cur_frame <- input$frame
      win <- input$window
      df <- df %>% filter(frame >= (cur_frame - win/2), frame <= (cur_frame + win/2))
    }
    df
  })

  get_color_range <- reactive({
    df <- filtered()
    vals <- df[[color_col()]]
    req(length(vals) > 0)
    range(vals, na.rm = TRUE)
  })

  # Distance/speed summary per individual + keypoint over the current filter window
  summaryStats <- reactive({
    df <- filtered()
    req(nrow(df) > 0)
    u <- input$units
    df %>%
      group_by(id, keypoint) %>%
      summarise(
        n_frames = n(),
        distance = sum(sqrt(dx^2 + dy^2 + dz^2), na.rm = TRUE) * unit_mult(),
        mean_speed = mean(speed, na.rm = TRUE) * unit_mult(),
        median_speed = median(speed, na.rm = TRUE) * unit_mult(),
        max_speed = max(speed, na.rm = TRUE) * unit_mult(),
        .groups = "drop"
      ) %>%
      rename(!!paste0("distance (", u, ")") := distance,
             !!paste0("mean_speed (", u, "/s)") := mean_speed,
             !!paste0("median_speed (", u, "/s)") := median_speed,
             !!paste0("max_speed (", u, "/s)") := max_speed)
  })

  output$summaryTable <- renderTable(summaryStats(), digits = 3)

  # Contiguous runs where elevation (height, the y column) barely changes --
  # a trailing 1s window whose y-range stays within 0.3m is flagged "level",
  # then merged into bouts and kept if the bout itself lasts >= 1s.
  straightFlights <- reactive({
    df <- filtered()
    req(nrow(df) > 0)
    fps <- 30  # matches the fixed frame-rate assumed elsewhere (dt = 1/30)
    win_frames <- max(round(1 * fps), 2)

    df %>%
      group_by(id, keypoint) %>%
      arrange(frame) %>%
      mutate(elev_range = zoo::rollapply(y, width = win_frames,
                                          FUN = function(v) diff(range(v, na.rm = TRUE)),
                                          fill = NA, align = "right"),
             is_level = !is.na(elev_range) & elev_range <= 0.3) %>%
      group_modify(~ {
        runs <- get_true_groups(.x$is_level)
        if (nrow(runs) == 0) return(tibble())
        runs$frame_start <- .x$frame[runs$start]
        runs$frame_end   <- .x$frame[runs$end]
        runs$duration_s  <- (runs$frame_end - runs$frame_start) / fps
        runs$mean_speed  <- vapply(seq_len(nrow(runs)), function(i)
          mean(.x$speed[runs$start[i]:runs$end[i]], na.rm = TRUE), numeric(1))
        runs
      }) %>%
      ungroup() %>%
      filter(duration_s >= 1)
  })

  straightFlightSummary <- reactive({
    sf <- straightFlights()
    u <- input$units
    if (nrow(sf) == 0) {
      return(tibble(id = integer(), keypoint = character(), n_straight_flights = integer(),
                     total_duration_s = numeric(), mean_duration_s = numeric()))
    }
    sf %>%
      group_by(id, keypoint) %>%
      summarise(
        n_straight_flights = n(),
        total_duration_s = sum(duration_s),
        mean_duration_s = mean(duration_s),
        mean_speed = mean(mean_speed, na.rm = TRUE) * unit_mult(),
        .groups = "drop"
      ) %>%
      rename(!!paste0("mean_speed (", u, "/s)") := mean_speed)
  })

  output$straightFlightTable <- renderTable(straightFlightSummary(), digits = 2)

  # ---- Accelerometer (FleaTag) import & alignment ----
  # Same offset/sampling-rate correction as R/align_boris_flea_shiny.R, just
  # aligned against this app's track (frame/30 -> seconds) instead of BORIS.

  accRaw <- reactive({
    req(input$accFile)
    read_flea_tag_data(input$accFile$datapath)
  })

  accNativeSR <- reactive({
    raw <- accRaw()
    sr <- suppressWarnings(as.numeric(raw$metadata$AccHz))
    if (is.na(sr)) {
      sr <- round(1 / mean(diff(raw$data$timeMilliseconds / 1000), na.rm = TRUE))
    }
    sr
  })

  observeEvent(accNativeSR(), {
    updateNumericInput(session, "accNewSR", value = accNativeSR())
  })

  accProcessed <- reactive({
    flea_preprocess(accRaw()$data, sampling_rate = accNativeSR(),
                     window = input$accWindow, flying_column = "rolling_var_PC",
                     flying_threshold = input$accFlyThreshold)
  })

  accAligned <- reactive({
    df <- accProcessed()
    req(input$accNewSR)
    df %>% mutate(time_aligned_s = timeSeconds * (accNativeSR() / input$accNewSR) + input$accOffset)
  })

  # Rough auto-align: cross-correlate ACC "is_flying" bouts against track
  # speed (both binned into a common time grid) to find the offset that best
  # lines up ACC-detected flight with high track speed. Bins with no track
  # data (out of view -- the same gap problem as BORIS observations) score 0
  # regardless of the ACC value there, so they neither help nor hurt a
  # candidate offset instead of being wrongly treated as "not flying".
  observeEvent(input$autoAlign, {
    trk <- data()
    acc <- accProcessed()
    req(nrow(trk) > 0, nrow(acc) > 0)

    trk <- trk %>% filter(keypoint %in% input$keypoint)
    req(nrow(trk) > 0)

    track_time <- trk$frame / 30
    acc_time <- acc$timeSeconds
    max_time <- max(max(track_time, na.rm = TRUE), max(acc_time, na.rm = TRUE))

    # Keep the FFT-based cross-correlation fast even for long ACC recordings
    # by coarsening the bin size for very long spans; 0.25s is plenty of
    # resolution for a "rough" first pass.
    bin_size <- max(0.25, max_time / 200000)

    bin_signal <- function(t, v, n_bins) {
      bins <- floor(t / bin_size) + 1
      keep <- bins >= 1 & bins <= n_bins & !is.na(v)
      agg <- tapply(v[keep], bins[keep], mean)
      out <- numeric(n_bins)
      out[as.integer(names(agg))] <- agg
      out
    }

    n_track_bins <- floor(max(track_time, na.rm = TRUE) / bin_size) + 1
    n_acc_bins <- floor(max(acc_time, na.rm = TRUE) / bin_size) + 1
    track_signal <- bin_signal(track_time, trk$speed, n_track_bins)
    acc_signal <- bin_signal(acc_time, as.numeric(acc$is_flying), n_acc_bins)

    if (all(track_signal == 0) || all(acc_signal == 0)) {
      showNotification("Auto-align: not enough speed/flying signal to cross-correlate.", type = "warning")
      return(invisible(NULL))
    }

    # stats::convolve(a, b, type="open") (NOT convolve(a, rev(b), ...) -- that
    # computes plain convolution, not cross-correlation) peaks at k where
    # b's own index-1 sample lines up with a's index (k - length(b) + 1).
    # Verified empirically against known impulse/offset pairs.
    cc <- convolve(track_signal, acc_signal, type = "open")
    best_k <- which.max(cc)
    implied_start_bin <- best_k - length(acc_signal) + 1  # acc bin 1 (t=0) lands here in track bins
    offset <- (implied_start_bin - 1) * bin_size

    updateNumericInput(session, "accOffset", value = round(offset, 2))
    showNotification(paste0("Auto-align: offset set to ", round(offset, 2),
                             "s (bin size ", bin_size, "s). Fine-tune by hand from here."),
                      type = "message")
  })

  output$accAlignPlot <- renderPlot({
    validate(need(input$accFile, "Upload a FleaTag ACC file to align."))
    acc <- accAligned()

    track <- data()
    validate(need(!is.null(track) && nrow(track) > 0,
                  "Upload a tracking CSV (sidebar) to compare against."))
    track_speed <- track %>%
      filter(keypoint %in% input$keypoint) %>%
      group_by(frame) %>%
      summarise(speed = mean(speed, na.rm = TRUE), .groups = "drop") %>%
      mutate(time_s = frame / 30, y_norm = speed / max(speed, na.rm = TRUE))

    acc_flying <- acc %>%
      filter(is_flying) %>%
      mutate(y_norm = VeDBA / max(VeDBA, na.rm = TRUE))

    ggplot() +
      geom_line(data = track_speed, aes(x = time_s, y = y_norm), color = "black") +
      geom_point(data = acc_flying, aes(x = time_aligned_s, y = y_norm), color = "firebrick", alpha = 0.3) +
      labs(x = "Track time (s)", y = "Normalized speed / VeDBA",
           title = "Black = track speed | Red = ACC VeDBA while flying") +
      theme_minimal()
  })

  output$downloadAccAligned <- downloadHandler(
    filename = function() paste0("acc_aligned_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- accAligned() %>% mutate(frame_equiv = time_aligned_s * 30)
      write.csv(df, file, row.names = FALSE)
    }
  )

  # Single-frame keypoint set used to draw the skeleton overlay
  skeletonFrame <- reactive({
    req(input$showSkeleton)
    df <- filtered()
    df %>%
      filter(frame == input$frame) %>%
      mutate(keypoint = factor(keypoint, levels = input$keypoint)) %>%
      arrange(id, keypoint)
  })

  # 2D plot function
  make2dPlot <- function(df, ax1, ax2, lab1, lab2) {
    validate(need(nrow(df) > 0, "No points match the current filters."))

    m <- unit_mult()
    df <- df %>% mutate(across(c(x, y, z), ~ . * m))
    lims1 <- input[[paste0(ax1, "range")]] * m
    lims2 <- input[[paste0(ax2, "range")]] * m

    max_gap <- max(abs(df$frame - input$frame), 1)
    df <- df %>% mutate(fade = 1 - 0.85 * pmin(abs(frame - input$frame) / max_gap, 1))
    if (!isTRUE(input$fadeTrail)) df$fade <- 1

    p <- ggplot(df, aes(x = .data[[ax1]], y = .data[[ax2]])) +
      geom_path(aes(group = grp, linetype = factor(id)),
                color = "black", alpha = 0.4, linewidth = 0.3) +
      geom_point(aes(color = .data[[color_col()]], shape = factor(id), alpha = fade),
                 size = 2, show.legend = c(color = TRUE, shape = TRUE, alpha = FALSE)) +
      scale_color_viridis_c(name = input$colorField, option = "D",
                             limits = get_color_range(), na.value = "grey") +
      scale_alpha_identity() +
      labs(linetype = "id", shape = "id", x = lab1, y = lab2) +
      coord_fixed(xlim = lims1, ylim = lims2) +
      theme_minimal()

    if (isTRUE(input$showSkeleton)) {
      sk <- skeletonFrame() %>% mutate(across(c(x, y, z), ~ . * m))
      p <- p + geom_path(data = sk, aes(x = .data[[ax1]], y = .data[[ax2]], group = id),
                          color = "red", linewidth = 0.8, inherit.aes = FALSE)
    }
    p
  }

  output$xyplot <- renderPlot({
    make2dPlot(filtered(), "x", "y", paste0("X (", input$units, ")"), paste0("Y (", input$units, ")"))
  })
  output$xzplot <- renderPlot({
    make2dPlot(filtered(), "z", "x", paste0("Z (", input$units, ")"), paste0("X (", input$units, ")"))
  })
  output$yzplot <- renderPlot({
    make2dPlot(filtered(), "z", "y", paste0("Z (", input$units, ")"), paste0("Y (", input$units, ")"))
  })

  # Mini timeline (speed vs frame) for brushing a custom frame range
  output$timeline <- renderPlot({
    df <- data()
    req(df, input$keypoint)
    df <- df %>% filter(keypoint %in% input$keypoint)
    validate(need(nrow(df) > 0, "No data."))
    ggplot(df, aes(x = frame, y = speed, color = factor(id))) +
      geom_line(na.rm = TRUE) +
      geom_vline(xintercept = input$frame, linetype = "dashed") +
      labs(x = "frame", y = "speed (m/s)", color = "id") +
      theme_minimal()
  })

  # Shared prep for the 3D view: scaled/broken-up data, color range, and the
  # true (non-cube) aspect ratio, reused by both the live plot and the GIF export.
  scene3D <- reactive({
    df <- filtered()
    validate(need(nrow(df) > 0, "No points match the current filters."))

    m <- unit_mult()
    df <- df %>% mutate(across(c(x, y, z), ~ . * m))
    cr <- get_color_range()

    # Insert an NA row after each id+keypoint group so plot_ly/rgl draw separate
    # line segments per individual/keypoint instead of one continuous trace.
    df_breaks <- df %>%
      group_by(grp) %>%
      group_modify(~ dplyr::add_row(.x, x = NA_real_, y = NA_real_, z = NA_real_)) %>%
      ungroup()

    # Plotted axes are x -> Width, z -> Length, y -> Height (y/z swapped).
    # Scale each axis by its actual data-range span so the box reflects real
    # proportions instead of plotly's default cube-shaped scene, which was
    # stretching the (usually short) height axis to match the longer ones.
    rx <- diff(input$xrange) * m
    rz <- diff(input$zrange) * m
    ry <- diff(input$yrange) * m
    maxr <- max(rx, rz, ry, na.rm = TRUE)
    aspectratio <- list(x = rx / maxr, y = rz / maxr, z = ry / maxr)

    sk <- NULL
    if (isTRUE(input$showSkeleton)) {
      sk <- skeletonFrame() %>% mutate(across(c(x, y, z), ~ . * m))
    }

    list(df = df, df_breaks = df_breaks, cr = cr, aspectratio = aspectratio,
         skeleton = sk, m = m)
  })

  # 3D plot (y vertical)
  output$plot3d <- renderPlotly({
    s <- scene3D()
    df <- s$df

    dash_types <- c("solid", "dot", "dash", "longdash", "dashdot")
    ids <- sort(unique(df$id))

    p <- plot_ly()
    for (i in seq_along(ids)) {
      d_i <- s$df_breaks %>% filter(id == ids[i])
      p <- p %>% add_trace(
        data = d_i, x = ~x, y = ~z, z = ~y,  # swap y<->z
        type = "scatter3d", mode = "lines+markers",
        name = paste("id", ids[i]),
        line = list(color = "black", width = 1, dash = dash_types[((i - 1) %% length(dash_types)) + 1]),
        marker = list(size = 3, color = d_i[[color_col()]],
                      colorscale = "Viridis", cmin = s$cr[1], cmax = s$cr[2],
                      showscale = (i == 1),
                      colorbar = list(title = input$colorField))
      )
    }

    if (!is.null(s$skeleton)) {
      for (id_i in unique(s$skeleton$id)) {
        d_i <- s$skeleton %>% filter(id == id_i)
        p <- p %>% add_trace(data = d_i, x = ~x, y = ~z, z = ~y,
                              type = "scatter3d", mode = "lines",
                              name = paste("skeleton", id_i),
                              line = list(color = "red", width = 4), showlegend = FALSE)
      }
    }

    p %>% layout(scene = list(
      xaxis = list(title = paste0("Width (", input$units, ")"), range = input$xrange * s$m),
      yaxis = list(title = paste0("Length (", input$units, ")"), range = input$zrange * s$m),
      zaxis = list(title = paste0("Height (", input$units, ")"), range = input$yrange * s$m),
      aspectmode = "manual",
      aspectratio = s$aspectratio
    ))
  })

  # Camera presets for the 3D view
  observeEvent(input$camTop, {
    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("relayout", scene = list(camera = list(eye = list(x = 0, y = 0, z = 2.5))))
  })
  observeEvent(input$camFront, {
    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("relayout", scene = list(camera = list(eye = list(x = 0, y = -2.5, z = 0))))
  })
  observeEvent(input$camSide, {
    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("relayout", scene = list(camera = list(eye = list(x = 2.5, y = 0, z = 0))))
  })
  observeEvent(input$camReset, {
    plotlyProxy("plot3d", session) %>%
      plotlyProxyInvoke("relayout", scene = list(camera = list(eye = list(x = 1.25, y = 1.25, z = 1.25))))
  })

  # Rotating GIF export, rendered offscreen with rgl (same package used
  # elsewhere in this repo, e.g. R/smooth3D_gaps.R) and encoded with gifski.
  output$downloadGif <- downloadHandler(
    filename = function() paste0("trajectory_3d_", Sys.Date(), ".gif"),
    content = function(file) {
      if (!requireNamespace("rgl", quietly = TRUE)) {
        stop("Please install the 'rgl' package: install.packages('rgl')")
      }
      if (!requireNamespace("gifski", quietly = TRUE)) {
        stop("Please install the 'gifski' package: install.packages('gifski')")
      }

      s <- scene3D()
      df <- s$df
      pal <- viridisLite::viridis(256)
      idx <- pmin(pmax(round((df[[color_col()]] - s$cr[1]) / diff(s$cr) * 255) + 1, 1), 256)
      cols <- ifelse(is.na(idx), "grey70", pal[idx])

      frame_dir <- tempfile("flea3d_")
      dir.create(frame_dir)
      on.exit(unlink(frame_dir, recursive = TRUE), add = TRUE)

      rgl::open3d(useNULL = TRUE)
      on.exit(rgl::close3d(), add = TRUE)
      rgl::bg3d("white")
      rgl::plot3d(df$x, df$z, df$y, col = cols, size = 6, type = "p",
                  aspect = c(s$aspectratio$x, s$aspectratio$y, s$aspectratio$z),
                  xlab = paste0("Width (", input$units, ")"),
                  ylab = paste0("Length (", input$units, ")"),
                  zlab = paste0("Height (", input$units, ")"))
      for (g in unique(s$df_breaks$grp)) {
        d_i <- s$df_breaks %>% filter(grp == g)
        rgl::lines3d(d_i$x, d_i$z, d_i$y, color = "black", alpha = 0.4)
      }
      if (!is.null(s$skeleton)) {
        for (id_i in unique(s$skeleton$id)) {
          d_i <- s$skeleton %>% filter(id == id_i)
          rgl::lines3d(d_i$x, d_i$z, d_i$y, color = "red", lwd = 2)
        }
      }

      # Oscillate azimuth back and forth rather than spinning all the way around
      angles <- c(seq(-20, 20, length.out = 15), seq(20, -20, length.out = 15))
      png_files <- file.path(frame_dir, sprintf("frame_%03d.png", seq_along(angles)))
      for (i in seq_along(angles)) {
        rgl::view3d(theta = angles[i], phi = 15, fov = 30)
        rgl::snapshot3d(png_files[i], width = 800, height = 600, webshot = FALSE)
      }

      gifski::gifski(png_files, gif_file = file, width = 800, height = 600,
                      delay = 1/12, loop = TRUE)
    }
  )

  output$downloadCSV <- downloadHandler(
    filename = function() paste0("filtered_trajectory_", Sys.Date(), ".csv"),
    content = function(file) write.csv(filtered(), file, row.names = FALSE)
  )

  output$downloadPlot <- downloadHandler(
    filename = function() paste0("xy_plot_", Sys.Date(), ".png"),
    content = function(file) {
      p <- make2dPlot(filtered(), "x", "y", paste0("X (", input$units, ")"), paste0("Y (", input$units, ")"))
      ggsave(file, plot = p, width = 8, height = 6, dpi = 150)
    }
  )
}

shinyApp(ui, server)
