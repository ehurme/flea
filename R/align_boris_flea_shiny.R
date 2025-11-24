# app.R

library(shiny)
library(tidyverse)
library(data.table)
library(readxl)
library(plotly)

# ---- Helper functions for FleaTag ----
# assumes to_sec(), read_flea_tag_data(), flea_preprocess(), etc. live here
source("./R/flea_functions.R")

# ---- Load & prepare data once (manual paths) ----

# Video metadata
video_data <- readxl::read_xlsx(
  "C:/Users/Edward/Downloads/HUMMINGBIRD VIDEOS.xlsx",
  sheet = 1
)

# BORIS / observations data
# (you had both read_xlsx(sheet = 2) and fread(); keeping fread version)
boris_data <- fread(
  "C:/Users/Edward/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/observations.csv"
)

video_data$t_rel <- NA
for(i in 1:nrow(video_data)){
  try({
    video_data$t_rel[i] <- to_sec(video_data$`ACC START TIME`[i]) -
      (to_sec(video_data$`CAMERA FRAME...16`[i]) +
      to_sec(video_data$`PHONE TIME`[i]))
  })
}


# FleaTag data (single file for now; you can change this path by hand)
file_path <- "C:/Users/Edward/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/202502_Fleatag_Hummingbirds_Trials/Trial_008/20250220_161130_Colibri-coruscans_210HzCont_3054_Trial008.txt"

flea_data <- read_flea_tag_data(file_path)
sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()

processed_data <- flea_preprocess(
  data             = flea_data$data,
  window           = 0.5,
  flying_threshold = 0.5,
  flying_column    = "rolling_var_PC"
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Align FleaTag ACC data with BORIS video behaviors"),

  sidebarLayout(
    sidebarPanel(
      h4("Trial selection"),
      selectInput(
        "trial_id",
        "Select trial (Observation id):",
        choices  = sort(unique(boris_data$`Observation id`)),
        selected = sort(unique(boris_data$`Observation id`))[1]
      ),
      tags$hr(),

      h4("Alignment settings"),
      numericInput(
        inputId = "offset",
        label   = "Offset (s) between tag recording and video",
        value   = 30,
        step    = 0.1
      ),
      numericInput(
        inputId = "new_sr",
        label   = "New sampling rate used in alignment (Hz)",
        value   = 230,
        min     = 1,
        step    = 1
      ),
      numericInput(
        inputId = "t_rel",
        label   = "t_rel (s): tag activation relative to video start",
        value   = -1.523,
        step    = 0.001
      ),
      helpText("Tweak offset, sampling rate, and t_rel until ACC peaks align with Flight bouts.")
    ),

    mainPanel(
      plotlyOutput("aligned_plot", height = "500px"),
      br(),
      strong("Notes:"),
      p("Points = normalized VeDBA for flying segments from FleaTag."),
      p("Red bars = BORIS Flight bouts (Behavioral category = Moving, Behavior = Flight).")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {

  # Trial-specific BORIS data
  trial_data <- reactive({
    boris_data %>%
      filter(`Observation id` == input$trial_id) %>%
      mutate(Behavior = factor(Behavior))
  })

  # Aligned ACC data
  aligned_acc <- reactive({
    processed_data %>%
      filter(is_flying) %>%
      mutate(
        x_aligned = (timeSeconds * (sampling_rate / input$new_sr)) +
          input$offset + input$t_rel,
        y_norm    = VeDBA / max(VeDBA, na.rm = TRUE)
      )
  })

  # Interactive aligned plot
  output$aligned_plot <- renderPlotly({
    beh_df <- trial_data()

    g <- ggplot() +
      # ACC from FleaTag
      geom_point(
        data  = aligned_acc(),
        aes(x = x_aligned, y = y_norm),
        alpha = 0.25
      ) +
      # Flight behavior from BORIS
      geom_segment(
        data = beh_df %>%
          filter(`Behavioral category` == "Moving",
                 Behavior == "Flight"),
        aes(
          x    = `Start (s)`,
          xend = `Stop (s)`,
          y    = 0.1,
          yend = 0.1
        ),
        color     = "red",
        linewidth = 3
      ) +
      xlim(c(0, max(beh_df$`Stop (s)`, na.rm = TRUE))) +
      ylim(c(-1, 1)) +
      labs(
        x = "Video time (s)",
        y = "Normalized VeDBA",
        title = paste(
          "Alignment of FleaTag ACC with BORIS Flight behavior\nTrial:",
          input$trial_id
        )
      ) +
      theme_minimal()

    ggplotly(g) %>% layout(dragmode = "zoom")
  })
}

shinyApp(ui = ui, server = server)
