library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

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
                   actionButton("camReset", "Reset")
                 ),
                 plotlyOutput("plot3d", height = "600px", width = "100%")),
        tabPanel("Summary", tableOutput("summaryTable"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive data loader
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)

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

  # Filtered data (all individuals / keypoints matching current filters)
  filtered <- reactive({
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
    } else {
      # NOTE: naming these the same as the `frame` column previously caused
      # dplyr's data mask to resolve BOTH sides of the filter to df$frame,
      # making the window filter a no-op. Keep these names distinct.
      cur_frame <- input$frame
      win <- input$window
      df <- df %>% filter(frame >= (cur_frame - win/2), frame <= (cur_frame + win/2))
    }

    # Bug fix: previously nothing grouped rows by individual/keypoint before
    # plotting, so geom_path/plot_ly would draw a spurious connecting line
    # between different flea IDs (and between different keypoints) whenever
    # more than one was selected. `grp` gives each id+keypoint its own line.
    df <- df %>%
      mutate(grp = interaction(id, keypoint, drop = TRUE)) %>%
      arrange(id, keypoint, frame)

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
        max_speed = max(speed, na.rm = TRUE) * unit_mult(),
        .groups = "drop"
      ) %>%
      rename(!!paste0("distance (", u, ")") := distance,
             !!paste0("mean_speed (", u, "/s)") := mean_speed,
             !!paste0("max_speed (", u, "/s)") := max_speed)
  })

  output$summaryTable <- renderTable(summaryStats(), digits = 3)

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

  # 3D plot (y vertical)
  output$plot3d <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No points match the current filters."))

    m <- unit_mult()
    df <- df %>% mutate(across(c(x, y, z), ~ . * m))
    cr <- get_color_range()

    # Insert an NA row after each id+keypoint group so plot_ly draws separate
    # line segments per individual/keypoint instead of one continuous trace.
    df_breaks <- df %>%
      group_by(grp) %>%
      group_modify(~ dplyr::add_row(.x, x = NA_real_, y = NA_real_, z = NA_real_)) %>%
      ungroup()

    dash_types <- c("solid", "dot", "dash", "longdash", "dashdot")
    ids <- sort(unique(df$id))

    p <- plot_ly()
    for (i in seq_along(ids)) {
      d_i <- df_breaks %>% filter(id == ids[i])
      p <- p %>% add_trace(
        data = d_i, x = ~x, y = ~z, z = ~y,  # swap y<->z
        type = "scatter3d", mode = "lines+markers",
        name = paste("id", ids[i]),
        line = list(color = "black", width = 1, dash = dash_types[((i - 1) %% length(dash_types)) + 1]),
        marker = list(size = 3, color = d_i[[color_col()]],
                      colorscale = "Viridis", cmin = cr[1], cmax = cr[2],
                      showscale = (i == 1),
                      colorbar = list(title = input$colorField))
      )
    }

    if (isTRUE(input$showSkeleton)) {
      sk <- skeletonFrame() %>% mutate(across(c(x, y, z), ~ . * m))
      for (id_i in unique(sk$id)) {
        d_i <- sk %>% filter(id == id_i)
        p <- p %>% add_trace(data = d_i, x = ~x, y = ~z, z = ~y,
                              type = "scatter3d", mode = "lines",
                              name = paste("skeleton", id_i),
                              line = list(color = "red", width = 4), showlegend = FALSE)
      }
    }

    p %>% layout(scene = list(
      xaxis = list(title = paste0("Width (", input$units, ")"), range = input$xrange * m),
      yaxis = list(title = paste0("Length (", input$units, ")"), range = input$zrange * m),
      zaxis = list(title = paste0("Height (", input$units, ")"), range = input$yrange * m)
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
