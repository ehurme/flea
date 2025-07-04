library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(stringr)

# Function to read flea tag data files
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
      value <- trimws(paste(split_line[-1], collapse = ":"))
      metadata[[key]] <- value
    }
  }

  # Extract the data part starting from the lineCnt header until the stop point
  data_start_idx <- metadata_end_idx + 1
  data_end_idx <- stop_idx - 1
  data_lines <- lines[data_start_idx:data_end_idx]

  # Read the data into a data frame
  data <- read.csv(text = paste(data_lines, collapse = "\n"), header = TRUE)

  # Return both metadata and data
  return(list(metadata = metadata, data = data))
}

# Preprocessing function with integer conversion fix
flea_preprocess <- function(data, sampling_rate = NULL, gain = 8, window = 0.5) {
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

  # Calculate window size in samples and ensure it's integer
  window_samples <- as.integer(round(sampling_rate * window))
  window_samples <- max(1, window_samples)  # Ensure at least 1 sample

  # Convert microgravity units to g based on gain
  conversion_factor <- gain * 1000
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
      VM = sqrt(accX_g^2 + accY_g^2 + accZ_g^2),
      ODBA = abs(accX_dynamic) + abs(accY_dynamic) + abs(accZ_dynamic),
      VeDBA = sqrt(accX_dynamic^2 + accY_dynamic^2 + accZ_dynamic^2),
      VeSBA = sqrt(accX_static^2 + accY_static^2 + accZ_static^2),
      ENMO = pmax(VM - 1, 0),
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
      shift_amount <- window_samples - 1

      # Shift flying indicator backward to align with window start
      data$is_flying <- lag(data$is_flying_temp, shift_amount/2, default = FALSE)

      # Remove temporary column
      data$is_flying_temp <- NULL
    } else {
      warning("Flying column '", flying_column, "' not found. Skipping flying detection.")
    }
  }

  return(data)
}

# UI definition
ui <- fluidPage(
  titlePanel("Flea Tag Accelerometer Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("file", "Upload Flea Tag Data", accept = c(".txt", ".csv")),
      uiOutput("metadata_display"),
      sliderInput("time_range", "Time Range (s):",
                  min = 0, max = 10, value = c(0, 10), step = 0.1),
      sliderInput("window", "Smoothing Window (s):",
                  min = 0.1, max = 2, value = 0.5, step = 0.1),
      radioButtons("gain", "Sensor Gain:",
                   choices = c("2G" = 2, "4G" = 4, "8G" = 8), selected = 8),
      actionButton("save_flight", "Mark as Flight", class = "btn-success"),
      actionButton("save_ground", "Mark as Ground", class = "btn-danger"),
      actionButton("clear", "Clear Selection", class = "btn-warning"),
      downloadButton("download", "Save Labels"),
      tags$hr(),
      h4("Selected Segment:"),
      verbatimTextOutput("segment_info")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Acceleration Plot", plotOutput("accel_plot", brush = "plot_brush")),
        tabPanel("3D Flight Plot",
                 plotOutput("xy_plot"),
                 plotOutput("xz_plot"),
                 plotOutput("yz_plot")),
        tabPanel("Behavior Labels", dataTableOutput("labels_table")),
        tabPanel("Metadata", verbatimTextOutput("metadata_text"))
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive values storage
  rv <- reactiveValues(
    raw_data = NULL,
    processed_data = NULL,
    metadata = NULL,
    labels = data.frame(
      start = numeric(),
      end = numeric(),
      behavior = character(),
      stringsAsFactors = FALSE
    )
  )

  # Load and process data
  observeEvent(input$file, {
    req(input$file)

    # Clear previous data
    rv$raw_data <- NULL
    rv$processed_data <- NULL
    rv$metadata <- NULL

    tryCatch({
      ext <- tools::file_ext(input$file$name)

      if (tolower(ext) == "txt") {
        # Process flea tag text file
        result <- read_flea_tag_data(input$file$datapath)
        rv$metadata <- result$metadata
        rv$raw_data <- result$data  # Directly use without renaming
      } else if (tolower(ext) == "csv") {
        # Process regular CSV file
        rv$raw_data <- read.csv(input$file$datapath)
      }

      # Preprocess data
      if (!is.null(rv$raw_data)) {
        rv$processed_data <- flea_preprocess(
          rv$raw_data,
          gain = as.numeric(input$gain),
          window = input$window
        )

        # Update time slider
        updateSliderInput(session, "time_range",
                          min = 0,
                          max = max(rv$processed_data$timeSeconds, na.rm = TRUE),
                          value = c(0, min(10, max(rv$processed_data$timeSeconds, na.rm = TRUE))))
      }
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })

    # Display metadata
    output$metadata_display <- renderUI({
      if (!is.null(rv$metadata)) {
        tagList(
          h4("Tag Information"),
          p(strong("Device ID:"), rv$metadata$deviceID),
          p(strong("Firmware:"), rv$metadata$firmware),
          p(strong("Sample Rate:"), rv$metadata$sampleRate, "Hz")
        )
      }
    })

    # Filter data based on time selection
    filtered_data <- reactive({
      req(rv$processed_data)
      rv$processed_data %>%
        filter(timeSeconds >= input$time_range[1] & timeSeconds <= input$time_range[2])
    })

    # Flight data
    flight_data <- reactive({
      req(rv$processed_data)
      rv$processed_data %>% filter(is_flying == TRUE)
    })

    # Main acceleration plot
    output$accel_plot <- renderPlot({
      req(filtered_data())
      df <- filtered_data()

      ggplot(df, aes(x = timeSeconds)) +
        geom_path(aes(y = accX_mg, color = "X")) +
        geom_path(aes(y = accY_mg, color = "Y")) +
        geom_path(aes(y = accZ_mg, color = "Z")) +
        geom_rect(data = rv$labels,
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                  fill = "yellow", alpha = 0.2, inherit.aes = FALSE) +
        scale_color_manual(values = c(X = "dodgerblue", Y = "red", Z = "green")) +
        labs(title = "Acceleration Data",
             x = "Time (seconds)",
             y = "Acceleration (mg)",
             color = "Axis") +
        theme_minimal()
    })

    # 3D projection plots for flight segments
    output$xy_plot <- renderPlot({
      req(flight_data())
      ggplot(flight_data(), aes(x = accX_mg, y = accY_mg)) +
        geom_path(alpha = 0.3) +
        geom_point(alpha = 0.2, size = 1) +
        labs(title = "Flight Pattern (X vs Y)",
             x = "X Acceleration (mg)",
             y = "Y Acceleration (mg)") +
        theme_minimal()
    })

    output$xz_plot <- renderPlot({
      req(flight_data())
      ggplot(flight_data(), aes(x = accX_mg, y = accZ_mg)) +
        geom_point(alpha = 0.2, size = 1) +
        labs(title = "Flight Pattern (X vs Z)",
             x = "X Acceleration (mg)",
             y = "Z Acceleration (mg)") +
        theme_minimal()
    })

    output$yz_plot <- renderPlot({
      req(flight_data())
      ggplot(flight_data(), aes(x = accZ_mg, y = accY_mg)) +
        geom_point(alpha = 0.2, size = 1) +
        labs(title = "Flight Pattern (Z vs Y)",
             x = "Z Acceleration (mg)",
             y = "Y Acceleration (mg)") +
        theme_minimal()
    })

    # Segment information display
    output$segment_info <- renderText({
      brush <- input$plot_brush
      if (!is.null(brush)) {
        paste0("Start: ", round(brush$xmin, 2), " s\n",
               "End: ", round(brush$xmax, 2), " s\n",
               "Duration: ", round(brush$xmax - brush$xmin, 2), " s")
      } else {
        "Select a segment by brushing the plot"
      }
    })

    # Behavior labeling
    observeEvent(input$save_flight, {
      brush <- input$plot_brush
      if (!is.null(brush)) {
        new_label <- data.frame(
          start = brush$xmin,
          end = brush$xmax,
          behavior = "flying"
        )
        rv$labels <- rbind(rv$labels, new_label) %>% distinct()
      }
    })

    observeEvent(input$save_ground, {
      brush <- input$plot_brush
      if (!is.null(brush)) {
        new_label <- data.frame(
          start = brush$xmin,
          end = brush$xmax,
          behavior = "not flying"
        )
        rv$labels <- rbind(rv$labels, new_label) %>% distinct()
      }
    })

    observeEvent(input$clear, {
      rv$labels <- data.frame(start = numeric(), end = numeric(), behavior = character())
    })

    # Labels table
    output$labels_table <- renderDataTable({
      rv$labels
    }, options = list(pageLength = 10))

    # Metadata display
    output$metadata_text <- renderPrint({
      if (!is.null(rv$metadata)) {
        cat("Flea Tag Metadata\n")
        cat("=================\n\n")
        for (name in names(rv$metadata)) {
          cat(paste0(name, ": ", rv$metadata[[name]], "\n"))
        }
      } else {
        "No metadata available"
      }
    })

    # Download handler
    output$download <- downloadHandler(
      filename = function() {
        paste0("behavior_labels_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(rv$labels, file, row.names = FALSE)
      }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
