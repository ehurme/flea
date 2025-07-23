# Shiny align acceleration and trajectory data
library(shiny)
library(ggplot2)
library(data.table)
library(scales)

ui <- fluidPage(
  titlePanel("Align Acceleration and Trajectory Data"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("acc_file", "Load Acceleration Data (CSV)", accept = ".csv"),
      fileInput("traj_file", "Load Trajectory Data (CSV)", accept = ".csv"),
      numericInput("fps", "Trajectory Frame Rate (FPS)", value = 60, min = 1),

      h4("Time Shift Configuration"),
      numericInput("shift_min", "Slider Minimum:", value = -5, step = 0.5),
      numericInput("shift_max", "Slider Maximum:", value = 5, step = 0.5),
      sliderInput("time_shift_slider", "Trajectory Time Shift:",
                  min = -5, max = 5, value = 0, step = 0.01, width = '100%'),
      numericInput("time_shift_num", "Direct Time Shift Value:",
                   value = 0, step = 0.01, width = '100%'),

      actionButton("save_btn", "Save Alignment Parameters", class = "btn-primary"),
      hr(),
      verbatimTextOutput("saved_params")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Acceleration", plotOutput("acc_plot", height = "500px")),
        tabPanel("Trajectory", plotOutput("traj_plot", height = "500px")),
        tabPanel("Overlay", plotOutput("overlay_plot", height = "500px")),
        tabPanel("Data",
                 h4("Acceleration Data"),
                 dataTableOutput("acc_table"),
                 h4("Trajectory Data"),
                 dataTableOutput("traj_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    acc_data = NULL,
    traj_data = NULL,
    alignment_params = NULL,
    from_slider = FALSE,
    from_num = FALSE
  )

  # Update slider min/max when inputs change
  observe({
    updateSliderInput(session, "time_shift_slider",
                      min = input$shift_min,
                      max = input$shift_max)
  })

  # Sync slider and numeric input
  observeEvent(input$time_shift_slider, {
    if (rv$from_num) {
      rv$from_num <- FALSE
      return()
    }
    rv$from_slider <- TRUE
    updateNumericInput(session, "time_shift_num", value = input$time_shift_slider)
  })

  observeEvent(input$time_shift_num, {
    if (rv$from_slider) {
      rv$from_slider <- FALSE
      return()
    }
    rv$from_num <- TRUE
    updateSliderInput(session, "time_shift_slider", value = input$time_shift_num)
  })

  # Load acceleration data
  observeEvent(input$acc_file, {
    req(input$acc_file)
    tryCatch({
      df <- fread(input$acc_file$datapath)
      if (!"timeMilliseconds" %in% names(df)) {
        stop("Acceleration file missing 'timeMilliseconds' column")
      }
      df[, time_seconds := timeMilliseconds / 1000]
      rv$acc_data <- df
    }, error = function(e) {
      showNotification(paste("Error loading acceleration data:", e$message), type = "error")
    })
  })

  # Load trajectory data
  observeEvent(input$traj_file, {
    req(input$traj_file)
    tryCatch({
      df <- fread(input$traj_file$datapath)
      required_cols <- c("Frame", "x", "y", "z")
      if (!all(required_cols %in% names(df))) {
        stop("Trajectory file missing required columns (Frame, x, y, z)")
      }
      rv$traj_data <- df
    }, error = function(e) {
      showNotification(paste("Error loading trajectory data:", e$message), type = "error")
    })
  })

  # Process trajectory data with time shift
  trajectory_processed <- reactive({
    req(rv$traj_data, input$fps)
    traj <- copy(rv$traj_data)

    # Calculate time based on frame rate
    traj[, time_seconds := Frame / input$fps]

    # Apply time shift (use numeric input as source of truth)
    traj[, time_shifted := time_seconds + input$time_shift_num]

    # Aggregate by time (average coordinates)
    traj[, .(x = mean(x), y = mean(y), z = mean(z)), by = .(time_shifted)]
  })

  # Acceleration plot
  output$acc_plot <- renderPlot({
    req(rv$acc_data)
    df <- rv$acc_data

    ggplot(df, aes(x = time_seconds)) +
      geom_line(aes(y = accX_mg, color = "X"), linewidth = 0.8) +
      geom_line(aes(y = accY_mg, color = "Y"), linewidth = 0.8) +
      geom_line(aes(y = accZ_mg, color = "Z"), linewidth = 0.8) +
      labs(title = "Acceleration Data",
           x = "Time (seconds)", y = "Acceleration (mg)",
           color = "Axis") +
      scale_color_manual(values = c(X = "red", Y = "green", Z = "blue")) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5))
  })

  # Trajectory plot
  output$traj_plot <- renderPlot({
    req(trajectory_processed())
    df <- trajectory_processed()

    ggplot(df, aes(x = time_shifted)) +
      geom_line(aes(y = x, color = "X"), linewidth = 0.8) +
      geom_line(aes(y = y, color = "Y"), linewidth = 0.8) +
      geom_line(aes(y = z, color = "Z"), linewidth = 0.8) +
      labs(title = "3D Trajectory (Time Shifted)",
           x = "Time (seconds)", y = "Position", color = "Axis") +
      scale_color_manual(values = c(X = "red", Y = "green", Z = "blue")) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5))
  })

  # Overlay plot
  output$overlay_plot <- renderPlot({
    req(rv$acc_data, trajectory_processed())
    acc_df <- rv$acc_data
    traj_df <- trajectory_processed()

    # Normalize data for overlay
    normalize <- function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }

    acc_df[, norm_accZ := normalize(accZ_mg)]
    traj_df[, norm_z := normalize(z)]

    ggplot() +
      geom_line(data = acc_df, aes(x = time_seconds, y = norm_accZ,
                                   color = "Acceleration Z"), linewidth = 1) +
      geom_line(data = traj_df, aes(x = time_shifted, y = norm_z,
                                    color = "Trajectory Z"), linewidth = 1) +
      labs(title = "Aligned Acceleration & Trajectory (Normalized)",
           x = "Time (seconds)", y = "Normalized Value", color = "") +
      scale_color_manual(values = c("Acceleration Z" = "blue", "Trajectory Z" = "purple")) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(hjust = 0.5))
  })

  # Data tables
  output$acc_table <- renderDataTable({
    req(rv$acc_data)
    rv$acc_data
  }, options = list(pageLength = 5, scrollX = TRUE))

  output$traj_table <- renderDataTable({
    req(trajectory_processed())
    trajectory_processed()
  }, options = list(pageLength = 5, scrollX = TRUE))

  # Save alignment parameters
  observeEvent(input$save_btn, {
    req(rv$acc_data, rv$traj_data)
    rv$alignment_params <- list(
      time_shift = input$time_shift_num,
      fps = input$fps,
      shift_range = c(input$shift_min, input$shift_max),
      acc_file = input$acc_file$name,
      traj_file = input$traj_file$name,
      timestamp = Sys.time()
    )
  })

  # Show saved parameters
  output$saved_params <- renderPrint({
    req(rv$alignment_params)
    params <- rv$alignment_params
    cat("Saved Alignment Parameters:\n")
    cat("--------------------------\n")
    cat(paste("Acceleration file:", params$acc_file, "\n"))
    cat(paste("Trajectory file:", params$traj_file, "\n"))
    cat(paste("Frame rate (FPS):", params$fps, "\n"))
    cat(paste("Time shift (seconds):", round(params$time_shift, 4), "\n"))
    cat(paste("Shift range:", paste(round(params$shift_range, 2), collapse = " to "), "\n"))
    cat(paste("Saved at:", format(params$timestamp, "%Y-%m-%d %H:%M:%S")))
  })
}

shinyApp(ui, server)
