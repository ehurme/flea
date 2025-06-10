# View 3D track
library(tidyverse)
library(data.table)
library(gganimate)
library(rgl)
library(plotly)

df <- fread("C:/Users/Edward/Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/Key3D.csv")
df <- fread("C:/Users/Edward/Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/Key3D_20250226_C0001.csv")
df %>% summary()

data <- df %>% dplyr::filter(x > -3000 & x < 3000,
                     y > -3000 & y < 3000,
                     z > -3000 & z < 3000,
                     Keypoint == "pose0")



# calculate speed and distance between points
# units = millimeters per frame 1/60
data %>% mutate(
  speed = c(NA, sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)*60/1000)
  # distance = c(0, cumsum(speed))
) -> data

ggplot(data, aes(x = Frame, y = speed)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Speed over Time", x = "Frame", y = "Speed (m/s)")# +

ggplot(data %>% filter(speed < 5),
       aes(z,y, col = speed))+
  geom_point()+
  #geom_path()+
  ylim(c(-1000, 1000))+
  xlim(c(1000, 3000))+
  scale_color_viridis_c(name = "Speed (m/s)")

ggplot(data %>% filter(speed < 5),
       aes(x,y, col = speed))+
  geom_point()+
  #geom_path()+
  ylim(c(-1000, 0))+
  xlim(c(-100, 1200))+
  scale_color_viridis_c(name = "Speed (m/s)")

data$speed %>% summary
data$speed %>% hist(breaks = 10000, xlim = c(0,1))


library(gganimate)
ggplot(data %>% filter(speed < 5),
       aes(z,y, col = speed, group = Keypoint))+
  geom_point()+
  #geom_line()+
  ylim(c(-1000, 1000))+
  xlim(c(1000, 3000))+
  transition_time(time = Frame)

# with(data %>% filter(speed < 10,
#                      x > -3000 & x < 3000,
#                      y > -3000 & y < 3000,
#                      z > -3000 & z < 3000),
#      rgl::plot3d(x, y, z, size = 1,
#             xlab = "X", ylab = "Y", zlab = "Z"))

t=1:1000
tt = seq(1,1000,len=10000)
sdata = data.frame(
  x=splinefun(t, df$x)(tt),
  y=splinefun(t, df$y)(tt),
  z=splinefun(t, df$z)(tt))

fig <- plot_ly(data = df %>% dplyr::filter(x > -3000 & x < 3000,
                y > -3000 & y < 3000,
                z > -3000 & z < 3000,
                Keypoint == "pose0"), #, Frame < 1000),
                x = ~x, y = ~y, z = ~z,
                # type = "scatter3d", mode = "markers",
                # marker = list(size = 1, colorbar = list(title = "Frame")),
                color = ~Frame, colors = rainbow(5000))


fig
df
df$Keypoint %>% table()
