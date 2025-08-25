# View 3D track
library(tidyverse)
library(data.table)
library(gganimate)
library(rgl)
library(plotly)
library(magrittr)

df <- fread("../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/Key3D.csv")
df <- fread("../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/Key3D_20250226_C0001.csv")
df <- fread("F:/Flanders25/Videos/20250521/Data/Key3D_202500521_C0002_aligned.csv")
df %>% head()

data <- df %>% dplyr::filter(x > -5000 & x < 5000,
                     y > -5000 & y < 5000,
                     z > -5000 & z < 5000)

plot(data$x, data$z, col = rgb(0,0,0,.1), cex = 0.4)
plot(data$x, data$y, col = rgb(0,0,0,.1), cex = 0.4)

# calculate speed and distance between points
# units = millimeters per frame 1/60
data %>% mutate(
  speed = c(NA, sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)*60/1000)
  # distance = c(0, cumsum(speed))
) -> data

hist(data$speed, breaks = 100)
ggplot(data %>% filter(speed < 20),
       aes(x = Frame, y = speed)) +
  geom_point(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Speed over Time", x = "Frame", y = "Speed (m/s)")# +


data %>% filter(Frame > 11500, Frame < 12000) %$%  hist(speed, breaks = 100)


ggplot(data %>% filter(speed < 20,
                       Frame > 11500, Frame < 12000),
       aes(z,x, col = Frame))+
  geom_point()+
  geom_path()+
  # ylim(c(-2000, 2000))+
  # xlim(c(-5000, 5000))+
  scale_color_viridis_c(name = "Frame")

ggplot(data %>% filter(speed < 15,
                       Frame > 11500, Frame < 12000),
       aes(x,y, col = speed))+
  geom_point()+
  #geom_path()+
  # ylim(c(-100, 2000))+
  # xlim(c(-2000, 2000))+
  scale_color_viridis_c(name = "Speed (m/s)")+
  coord_equal()

ggplot(data %>% filter(speed < 15,
                       Frame > 11500, Frame < 12000),
       aes(z,x, col = speed))+
  geom_point()+
  #geom_path()+
  # ylim(c(-3000, 3000))+
  # xlim(c(-5000, 5000))+
  scale_color_viridis_c(name = "Speed (m/s)")+
  coord_equal()

data$speed %>% summary
data$speed %>% hist(breaks = 10000, xlim = c(0,1))

ggplot(data %>% filter(speed < 10),
       aes(Frame, z, col = speed))+
  geom_point()+
  scale_color_viridis_c()

data_speed10 <- data %>% filter(speed < 10)
tail <- 5
pdf(file = "test.pdf")
  for(i in seq(1, max(data$Frame), 10)){ #seq(1, nrow(data_speed10), 5)){
    idx <- 1:i
    if(length(idx) > tail*60){
      idx <- idx[(length(idx)-tail*60):length(idx)]
    }
    p <- ggplot(data_speed10[idx,],
           aes(x, y, col = speed))+
      geom_point()+
      geom_path()+
      scale_color_viridis_c()+
      ggtitle(paste("Time: ", round(max(data_speed10$Frame[idx])/60)))+theme_bw()
    print(p)
  }
dev.off()


library(gganimate)
ggplot(data %>% filter(speed < 10),
       aes(z,y, col = speed, group = Keypoint))+
  geom_point()+
  #geom_line()+
  ylim(c(-100, 2000))+
  xlim(c(-2000, 2000))+
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

fig <- plot_ly(data = data %>%
                 dplyr::filter(x > -2000 & x < 2000,
                               y > -200 & y < 3000,
                               z > -5000 & z < 5000,
                Keypoint == "pose0", Frame < 1000),
                x = ~x, y = ~y, z = ~z,
                # type = "scatter3d", mode = "markers",
                # marker = list(size = 1, colorbar = list(title = "Frame")),
                color = ~Frame, colors = rainbow(5000))


fig
df
df$Keypoint %>% table()
