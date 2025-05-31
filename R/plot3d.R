# View 3D track
library(tidyverse)
library(data.table)
library(gganimate)
library(rgl)
library(plotly)

df <- fread("C:/Users/Edward/Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/Key3D.csv")
df %>% summary()


ggplot(df[1:100,], aes(x,y, col = z))+
  geom_point()+
  #geom_path()+
  ylim(c(-3000, 3000))+
  xlim(c(-3000, 3000))

with(df, rgl::plot3d(x, y, z, size = 1,
            xlab = "X", ylab = "Y", zlab = "Z"),
            zlim = c(-3000, 3000),
            xlim = c(-3000, 3000),
            ylim = c(-3000, 3000))

t=1:1000
tt = seq(1,1000,len=10000)
sdata = data.frame(
  x=splinefun(t, df$x)(tt),
  y=splinefun(t, df$y)(tt),
  z=splinefun(t, df$z)(tt))

fig <- plot_ly(df %>% filter(x > -3000 | x < 3000,
                y > -3000 | y < 3000,
                z > -3000 | z < 3000,
                Keypoint == "pose0", Frame < 1000),
                x = ~x, y = ~y, z = ~z,
                # type = "scatter3d", mode = "markers",
                # marker = list(size = 1, colorbar = list(title = "Frame")),
                color = ~Frame, colors = rainbow(5000))


fig
