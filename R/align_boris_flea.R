# align boris and flea tag data

library(tidyverse)
library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)
library(readxl)

video_data <- readxl::read_xlsx("C:/Users/Edward/Downloads/HUMMINGBIRD VIDEOS.xlsx", sheet = 1)
boris_data <- readxl::read_xlsx("C:/Users/Edward/Downloads/HUMMINGBIRD VIDEOS.xlsx", sheet = 2)
boris_data <- fread("C:/Users/Edward/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/observations.csv")
boris_data

# trial <- boris_data %>% filter(`Media file name` == "D:/Grabaciones/Finca_Flights/20250220/finca3/finca3_20250220_C0007.MP4")
trial <- boris_data %>% filter(grepl(pattern = "CCO20_1", x = `Observation id`)) %>%
  mutate(Behavior = factor(Behavior))
trial$Source %>% table()
video_plot <- ggplot(
  trial,
  aes(
    x    = `Start (s)`,
    xend = `Stop (s)`,
    y    = Behavior,
    yend = Behavior,
    colour = Behavior
  )
) +
  geom_segment(linewidth = 3, lineend = "round") +
  labs(
    x = "Time (s)",
    y = NULL,
    colour = "Behavior"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    legend.position = "none"
  )+facet_wrap(~`Media file name`)+
  xlim(c(0, max(trial$`Stop (s)`)))
video_plot

trial$`Behavioral category` %>% table()
trial$`Behavior` %>% table()

source("./R/flea_functions.R")

file_path = "C:/Users/Edward/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/202502_Fleatag_Hummingbirds_Trials/Trial_008/20250220_161130_Colibri-coruscans_210HzCont_3054_Trial008.txt"
flea_data <- read_flea_tag_data(file_path)
flea_data$metadata
plot(flea_data$data$timeMilliseconds/1000, flea_data$data$accZ_mg)

sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()
processed_data <- flea_preprocess(data = flea_data$data,
                                  window = 0.5,
                                  flying_threshold = 0.5,
                                  flying_column = "rolling_var_PC")
duration_flea = nrow(processed_data)/sampling_rate
p <- flea_plot(processed_data)
p



acc_plot <- ggplot()+
  geom_point(data = processed_data %>% filter(is_flying),
             aes(x = timeSeconds, y = VeDBA))+
  xlim(c(0, max(trial$`Stop (s)`)))

ggpubr::ggarrange(video_plot, acc_plot, ncol = 1)


t_rel = -1.523
offset = 30
new_sampling_rate <- 230
ggplot()+
  geom_point(data = processed_data %>% filter(is_flying),
             aes(x = (timeSeconds*(sampling_rate/new_sampling_rate))+offset+t_rel, y = VeDBA/max(VeDBA)), alpha = 0.25)+
  geom_segment(data = beh_plot %>% filter(`Behavioral category` == "Moving",
                                          Behavior == "Flight"),
               aes(
                 x    = `Start (s)`,
                 xend = `Stop (s)`,
                 y    = .1,
                 yend = .1
               ), col = "red", lwd = 3)+
  xlim(c(0, max(beh_plot$`Stop (s)`)))+
  ylim(c(-1, 1))
