library(data.table)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(stringr)
library(googlesheets4)
#Google sheet
Sheet_id <- "1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA"
data_Straight_flights <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = "Straight_flights")
data_flightcage <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = "FlightCage")

#file calculation
directory <- "F:/Flanders25/Videos/20250521/data"
infile <- file.path(directory, "FinalOut3DDict2_20250521_C0009_aligned.csv")
df <- fread(infile)




# rescale toinfile rescale to meters from mm
df <- df %>%
  mutate(x = x/1000, y = y/1000, z = z/1000) %>%
  arrange(Frame)

# set fps
fps <- 59.94

# calculate speed and turn angle
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




speedlow <- 2
speedhigh <-12

start  <- 5730
length <- 70
end    <- (start + length)




df %>% filter(Frame > start, Frame < (start + length), Keypoint == "centroid_cm") -> flight#only one filter
df_plot <- subset(flight, speed > speedlow & speed < speedhigh)


p1 = ggplot(df_plot, aes(z, x, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, , drop = FALSE ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()+
  scale_y_continuous(limits = c(-2, 2))
p1

p2 = ggplot(df_plot, aes(z, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_color_viridis_c()

p2


p3 = ggplot(df_plot, aes(x, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],    # erste Zeile aus dem gefilterten Datensatz
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
    scale_y_continuous(limits = c(-1, 1)) +
    scale_x_continuous(limits = c(-2, 2))+       # y-Achse
  scale_color_viridis_c()
print(p3)

combined_plot <- ggpubr::ggarrange(p1, p2, p3, common.legend = TRUE, ncol=1)
print(combined_plot)


ggplot(subset(flight, TurnAngle < 1 & TurnAngle > 0.2),
       aes(z, x, col = TurnAngle))+
  geom_path(col = 1, size = .1)+
  geom_point()+scale_color_viridis_c()


ggplot(subset(flight, heading < 4 &heading > -4),
       aes(z, x, col = heading))+
  geom_path(col = 1, size = .1)+
  geom_point() +
  scale_color_viridis_c()


ggplot(subset(flight, heading < 4 &heading > -4),
       aes(z, y, col = heading))+
  geom_path(col = 1, size = .1)+
  geom_point()+scale_color_viridis_c()

hist(flight$speed[flight$speed <30], breaks = 30 )
hist(flight$heading)
hist(flight$TurnAngle[flight$TurnAngle <4])



#play with filters to see what gives good results
flight %>% filter(TurnAngle < 1, ) -> ff1
ff1 %>% filter(speed > speedlow, speed < speedhigh) ->ff1

summary(ff1$speed)


p4 = ggplot(ff1, aes(z, x, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, , drop = FALSE ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()+
  scale_y_continuous(limits = c(-2, 2))
p4

p5 = ggplot(ff1, aes(z, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  scale_color_viridis_c()


p6 = ggplot(ff1, aes(x, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],    # erste Zeile aus dem gefilterten Datensatz
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_x_continuous(limits = c(-2, 2))+       # y-Achse
  scale_color_viridis_c()


combined_plot <- ggpubr::ggarrange(p4, p5, p6, common.legend = TRUE, ncol=1)
print(combined_plot)


stats <- summary(ff1$speed)
colnames(stats)



datum <- str_extract(file_3D, "\\d{8}")
C000X <- str_extract(file_3D, "C\\d+")
print (C000X)
file_3D <- basename(infile)
model_3D <- str_extract(file_3D, "^[^_]+")
VideoID <- paste0(datum, "_", C000X)
print(VideoID)
map_row <- data_flightcage[data_flightcage$VideoID == VideoID, ]
if (nrow(map_row) > 0) {
  bat_id <- as.character(map_row$bat[1])
  trial  <- as.character(map_row$trial[1])
} else {
  bat_id <- NA_character_
  trial  <- NA_character_
}
print (trial)
stats <- summary(ff1$speed)
colnames(stats)

Min    <- as.numeric(stats["Min."])
Q1     <- as.numeric(stats["1st Qu."])
Median <- as.numeric(stats["Median"])
Mean   <- as.numeric(stats["Mean"])
Q3     <- as.numeric(stats["3rd Qu."])
Max    <- as.numeric(stats["Max."])

print(c(Min, Q1, Median, Mean, Q3, Max))


new_row <- tibble(
  bat       = bat_id,
  date      = datum,
  trial     = trial,
  videofile = VideoID,
  Tracking_Quality = NA_character_,
  `3D_model` = model_3D,
  `3D_file`  = file_3D,
  start     = start,
  duration  = length,
  end       = end,
  Min.      = Min,
  `1st Qu.` = Q1,
  Median    = Median,
  Mean      = Mean,
  `3rd Qu.` = Q3,
  Max.      = Max
)

print (new_row)

nrow(new_row)

sheet_append(new_row,
             ss = Sheet_id,
             sheet = "Straight_flights")

#ff to CSV
#outdir_CSV <- "F:/Flanders25/Videos/20250520/Data/Flight_CSV"
#outfile_CSV <- paste0 ("F:/Flanders25/Videos/20250520/Data/Flight_CSV/" ,VideoID,"_", start, ".csv")
#print (outfile_CSV)
#write.csv(ff1, outfile_CSV, row.names = FALSE)


ggplot(ff1, aes(x = "", y = speed)) +
    geom_boxplot(width = 0.2, outlier.shape = NA, fill = "skyblue") +
  geom_jitter(width = 0.1, size = 1.5, alpha = 0.6) +
  ylim(0,12)+
  theme_minimal() +
  labs(
    title = paste0("speed_", VideoID, "_", start),
    y = "Speed [m/s]", x = NULL
  ) +
  theme(
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )



