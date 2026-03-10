library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)

ui <- fluidPage(
  titlePanel("3D Trajectory Viewer"),

  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      numericInput("window", "Frame Window", value = 300, min = 1),
      selectInput("keypoint", "Keypoints", choices = NULL, multiple = TRUE),
      selectInput("colorField", "Color by:",
                  choices = c("Frame", "Speed", "TurnAngle", "Heading")),
      sliderInput("xrange", "X Range (m)", min = -10, max = 10, value = c(-2, 2)),
      sliderInput("yrange", "Y Range (m)", min = -10, max = 10, value = c(-1, 2)),
      sliderInput("zrange", "Z Range (m)", min = -10, max = 10, value = c(-3, 3)),
      sliderInput("maxspeed", "Max Speed (m/s)", min = 0, max = 50, value = 10),

      # Frame: Slider + manuell
     sliderInput("frame", "Frame", min = 0, max = 100, value = 0, step = 1,
                  animate = animationOptions(interval = 1000/10, loop = TRUE)),
      numericInput("frameManual", "Frame (manuell)", value = 0, min = 0, step = 1)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("XY", plotOutput("xyplot", height = "600px", width = "100%")),
        tabPanel("XZ", plotOutput("xzplot", height = "600px", width = "100%")),
        tabPanel("YZ", plotOutput("yzplot", height = "600px", width = "100%")),
        tabPanel("3D", plotlyOutput("plot3d", height = "600px", width = "100%"))
      )
    )
  )

)

server <- function(input, output, session) {

  # -------- Daten laden & aufbereiten --------
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, check.names = FALSE)

    # Erwartete Spalten: Frame, x, y, z, Keypoint, ID
    df <- df %>%
      mutate(x = x/1000, y = y/1000, z = z/1000) %>%
      arrange(Frame)

    fps <- 30  # ggf. anpassen!

    df <- df %>%
      group_by(ID, Keypoint) %>%
      arrange(Frame, .by_group = TRUE) %>%
      mutate(
        dx = x - lag(x),
        dy = y - lag(y),
        dz = z - lag(z),
        dF = Frame - lag(Frame),
        dt = ifelse(dF > 0, dF / fps, NA_real_),
        speed = sqrt(dx^2 + dy^2 + dz^2) / dt,
        heading = atan2(dy, dx)
      ) %>%
      ungroup() %>%
      group_by(ID, Keypoint) %>%
      mutate(
        vx = dx/dt, vy = dy/dt, vz = dz/dt,
        num = lag(vx)*vx + lag(vy)*vy + lag(vz)*vz,
        den = sqrt(lag(vx)^2+lag(vy)^2+lag(vz)^2) * sqrt(vx^2+vy^2+vz^2),
        cosang = pmax(-1, pmin(1, num/den)),
        TurnAngle = acos(cosang)
      ) %>%
      ungroup()

    df
  })

  # -------- UI initialisieren, sobald Daten da sind --------
  observeEvent(data(), {
    df <- data()
    req(nrow(df) > 0)

    fmin <- min(df$Frame, na.rm = TRUE)
    fmax <- max(df$Frame, na.rm = TRUE)

    updateSliderInput(session, "frame", min = fmin, max = fmax, value = fmin, step = 1)
    updateNumericInput(session, "frameManual", value = fmin, min = fmin, max = fmax)

    kps <- sort(unique(df$Keypoint))
    updateSelectInput(session, "keypoint", choices = kps, selected = kps)
  })

  # -------- Slider <-> Numeric synchron --------
  observeEvent(input$frame, ignoreInit = TRUE, {
    if (!identical(input$frame, input$frameManual)) {
      updateNumericInput(session, "frameManual", value = input$frame)
    }
  })

  observeEvent(input$frameManual, ignoreInit = TRUE, {
    df <- data(); req(nrow(df) > 0)
    fmin <- min(df$Frame, na.rm = TRUE)
    fmax <- max(df$Frame, na.rm = TRUE)
    val <- suppressWarnings(as.numeric(input$frameManual))
    if (!is.finite(val)) val <- fmin
    val <- max(fmin, min(fmax, val))
    if (!identical(val, input$frame)) {
      updateSliderInput(session, "frame", value = val)
    }
  })

  # -------- Filter --------
  filtered <- reactive({
    df <- data()
    req(df, input$keypoint)

    df <- df %>%
      filter(
        Keypoint %in% input$keypoint,
        x >= input$xrange[1], x <= input$xrange[2],
        y >= input$yrange[1], y <= input$yrange[2],
        z >= input$zrange[1], z <= input$zrange[2],
        is.na(speed) | speed <= input$maxspeed
      )

    frame <- input$frame
    window <- input$window
    df %>% filter(Frame >= (frame - window/2), Frame <= (frame + window/2))
  })

  # -------- Farbe --------
  get_color <- reactive({
    df <- filtered()
    switch(input$colorField,
           "Frame" = df$Frame,
           "Speed" = df$speed,
           "TurnAngle" = df$TurnAngle,
           "Heading" = df$heading)
  })

  get_color_range <- reactive({
    col <- get_color()
    rng <- suppressWarnings(range(col[is.finite(col)], na.rm = TRUE))
    if (length(rng) != 2 || any(!is.finite(rng))) c(0, 1) else rng
  })

  # -------- 2D Plots (statisch) --------
  make2dPlot <- function(df, ax1, ax2) {
    ggplot(df, aes(x = .data[[ax1]], y = .data[[ax2]], color = get_color())) +
      geom_path(color = "black", size = 0.3, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_color_viridis_c(option = "D", limits = get_color_range(), na.value = "grey") +
      coord_fixed(xlim = input[[paste0(ax1,"range")]], ylim = input[[paste0(ax2,"range")]]) +
      theme_minimal()
  }

  output$xyplot <- renderPlot({ make2dPlot(filtered(), "x", "y") })
  output$xzplot <- renderPlot({ make2dPlot(filtered(), "z", "x") })
  output$yzplot <- renderPlot({ make2dPlot(filtered(), "z", "y") })

  # -------- 3D Plot (Hover: Frame + Koordinaten, einheitliche Skala) --------
  output$plot3d <- renderPlotly({
    df <- filtered()
    req(df); if (!nrow(df)) return(NULL)

    col <- get_color()
    rng <- get_color_range()
    if (!length(col) || all(!is.finite(col))) {
      col <- rep(NA_real_, nrow(df))
      rng <- c(0, 1)
    }

    # gleiche Achsenskalierung vorbereiten
    rx <- input$xrange
    ry <- input$yrange
    rz <- input$zrange
    span <- max(diff(rx), diff(ry), diff(rz)) / 2
    cx <- mean(rx); cy <- mean(ry); cz <- mean(rz)
    rx2 <- c(cx - span, cx + span)
    ry2 <- c(cy - span, cy + span)
    rz2 <- c(cz - span, cz + span)

    plot_ly(
      data = df,
      x = ~x,
      y = ~z,   # y<->z getauscht (y vertikal)
      z = ~y,
      type = "scatter3d",
      mode = "lines+markers",
      line = list(color = "black", width = 1),
      marker = list(
        size = 3,
        color = col,
        colorscale = "Viridis",
        cmin = rng[1],
        cmax = rng[2],
        showscale = FALSE
      ),
      text = ~paste(
        "Frame:", Frame,
        "<br>x:", round(x, 3), "m",
        "<br>y:", round(y, 3), "m",
        "<br>z:", round(z, 3), "m"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = "Width (m)",  range = rx2),
          yaxis = list(title = "Length (m)", range = rz2),  # Achtung: y<->z getauscht
          zaxis = list(title = "Height (m)", range = ry2),
          aspectmode = "cube"  # gleicher Maßstab für x,y,z
        )
      )
  })
}

shinyApp(ui, server)
