library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(stringr)

source("./R/flea_functions.R")

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

        # Convert column names to match processing function
        df <- result$data # %>%
        #   rename(
        #     timeMilliseconds = time,
        #     accX_mg = accX,
        #     accY_mg = accY,
        #     accZ_mg = accZ
        #   )

        rv$raw_data <- df
      } else if (tolower(ext) == "csv") {
        # Process regular CSV file
        df <- read.csv(input$file$datapath)
        rv$raw_data <- df
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
    output$labels_table <- renderDT({
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
