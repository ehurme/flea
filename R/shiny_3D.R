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

      # Frame slider with 60 FPS
      sliderInput("frame", "Frame", min = 0, max = 100, value = 0, step = 1,
                  animate = animationOptions(interval = 1000/10, loop = TRUE))
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

  # Reactive data loader
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)

    # Convert mm → m
    df <- df %>%
      mutate(x = x / 1000,
             y = y / 1000,
             z = z / 1000) %>%
      arrange(Frame)

    # Compute speed (m/s) and heading
    df <- df %>%
      group_by(ID, Keypoint) %>%
      arrange(Frame) %>%
      mutate(dx = x - lag(x),
             dy = y - lag(y),
             dz = z - lag(z),
             dt = (Frame - lag(Frame)) * (1/30),
             speed = sqrt(dx^2 + dy^2 + dz^2)/dt,
             heading = atan2(dy, dx)) %>%
      ungroup()

    # Turn angle
    df <- df %>%
      group_by(ID, Keypoint) %>%
      mutate(vx = dx/dt, vy = dy/dt, vz = dz/dt,
             TurnAngle = acos(
               (lag(vx)*vx + lag(vy)*vy + lag(vz)*vz) /
                 (sqrt(lag(vx)^2+lag(vy)^2+lag(vz)^2) *
                    sqrt(vx^2+vy^2+vz^2))
             )) %>%
      ungroup()

    df
  })

  # Update frame slider & keypoints
  observe({
    df <- data()
    updateSliderInput(session, "frame",
                      min = min(df$Frame), max = max(df$Frame),
                      value = min(df$Frame))
    updateSelectInput(session, "keypoint",
                      choices = unique(df$Keypoint),
                      selected = unique(df$Keypoint)[1])
  })

  # Filtered data
  filtered <- reactive({
    df <- data()
    req(df)

    df <- df %>%
      filter(Keypoint %in% input$keypoint,
             x >= input$xrange[1], x <= input$xrange[2],
             y >= input$yrange[1], y <= input$yrange[2],
             z >= input$zrange[1], z <= input$zrange[2],
             is.na(speed) | speed <= input$maxspeed)

    frame <- input$frame
    window <- input$window
    df <- df %>% filter(Frame >= (frame - window/2), Frame <= (frame + window/2))
    df
  })

  get_color <- reactive({
    df <- filtered()
    switch(input$colorField,
           "Frame" = df$Frame,
           "Speed" = df$speed,
           "TurnAngle" = df$TurnAngle,
           "Heading" = df$heading)
  })

  get_color_range <- reactive({
    color <- get_color()
    range(color, na.rm = TRUE)
  })

  # 2D plot function
  make2dPlot <- function(df, ax1, ax2) {
    ggplot(df, aes(x = .data[[ax1]], y = .data[[ax2]], color = get_color())) +
      geom_path(color = "black", size = 0.3) +
      geom_point(size = 2) +
      scale_color_viridis_c(option = "D", limits = get_color_range(), na.value = "grey") +
      coord_fixed(xlim = input[[paste0(ax1,"range")]], ylim = input[[paste0(ax2,"range")]]) +
      theme_minimal()
  }

  output$xyplot <- renderPlot({ make2dPlot(filtered(), "x", "y") })
  output$xzplot <- renderPlot({ make2dPlot(filtered(), "z", "x") })
  output$yzplot <- renderPlot({ make2dPlot(filtered(), "z", "y") })

  # 3D plot (y vertical)
  output$plot3d <- renderPlotly({
    df <- filtered()
    plot_ly(df, x = ~x, y = ~z, z = ~y,  # swap y<->z
            type = "scatter3d", mode = "lines+markers",
            line = list(color = "black", width = 1),
            marker = list(size = 3, color = get_color(), colorscale = "Viridis")) %>%
      layout(scene = list(
        xaxis = list(title = "Width (m)", range = input$xrange),
        yaxis = list(title = "Length (m)", range = input$zrange),
        zaxis = list(title = "Height (m)", range = input$yrange)
      ))
  })
}

shinyApp(ui, server)

