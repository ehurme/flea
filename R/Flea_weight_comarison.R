library(googlesheets4)
library(dplyr)
library(purrr)
library(ggplot2)
library(data.table)
library(stringr)


directory <-"F:/Flanders25/Videos/20250520/data"


data_flightcage <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 3)
data_Straight_flights <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 2)
#print(data_Straight_flights)
#print(data_flightcage)
data_flightcage <- as.data.table(data_flightcage)
data_Straight_flights <- as.data.table(data_Straight_flights)

data1 <- data_flightcage %>%
  select (VideoID, bat, trial,  `initial weight`, `total weight`, percent)
  data1_leis <- data1[3:(nrow(data1)-24),]

data2 <- data_Straight_flights %>%
  select (VideoID, bat, trial, start,  `1st Qu.`, Median, Mean, `3rd Qu.`)
  data2_leis <- data2[1:(nrow(data2)),]


data1_leis <- data1_leis %>% mutate(
  bat = map_chr(bat, as.character),
  trial = as.character(trial)
  )

data2_leis <- data2_leis %>% mutate(
  bat = map_chr(bat, as.character),
  trial =as.character(trial)
  )
print(data1_leis)
nrow(data2_leis)
print (data2_leis)
 str(data2_leis$bat)


merged_leis <- data2_leis %>%
  left_join(data1_leis, by = c("VideoID", "bat", "trial"))

merged_leis <- merged_leis %>%
  select(VideoID, bat, `1st Qu.`, Median, Mean, `3rd Qu.`, percent)
print(merged_leis)

ggplot(merged_leis, aes(x = percent, y= Median )) +
  geom_path(col = 1, size = .1) +
  geom_point(size = 2) +
  xlim(0,13) +
  ylim(2,6 ) +
  #geom_text(

  scale_color_viridis_c()


Summary_tab <- merged_leis %>%
  group_by(VideoID) %>%
  summarise(
    Medians_speed = median(Mean, na.rm = TRUE),
    Mean_percent = mean(percent, na.rm = TRUE),
    .groups = "drop"
  )
print(Summary_tab)

px = ggplot(Summary_tab, aes(x = Mean_percent, y= Medians_speed )) +
  #geom_path(col = 1, size = .1) +
  geom_point(size = 2) +
  xlim(0,13) +
  ylim(3,6 ) +
  geom_text( aes(label = paste0(VideoID, " (", round(Mean_percent, 2) ,"%)")),
             vjust = -0.7, size = 3.5
             )+
  theme_minimal()+
  labs(x = "Mean percent", y = "Median speed")
  scale_color_viridis_c()

print(px)



outdir <- "F:/Flanders25/Videos/Data_leis/Graphs_leis"
  #Save Plot
  ggsave(
    filename = paste0("plot_leis_20250520_", format(Sys.time(),"%Y%m%d_%H%M%S"),".png"),
    plot     = p1,
    path     = outdir,
    width    = 8, height = 10, dpi = 300
  )



