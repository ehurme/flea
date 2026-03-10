#file calculation
directory <- "F:/Flanders25/Videos/20250520/data"
infile <- file.path(directory, "Out3DDict2_20250520_C0003_aligned.csv")
ff4 <- fread(infile)

# rescale toinfile rescale to meters from mm
ff4 <- ff4 %>%
  mutate(x = x/1000, y = y/1000, z = z/1000) %>%
  arrange(Frame)

# set fps
fps <- 59.94

# calculate speed and turn angle
ff4 <- ff4 %>%
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

start  <- 7450
length <- 60
end    <- (start + length)

        # nur realistische Änderungen




ff4 %>% filter(Frame > start, Frame < (start + length), Keypoint == "centroid_cm") -> ff4
#only one filter
ff4 %>% subset(speed > speedlow & speed < speedhigh) -> ff4


pa = ggplot(ff4, aes(z, x, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()

pb = ggplot(ff4, aes(x, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()

pc = ggplot(ff4, aes(z, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()

combined_plot_speed <- ggpubr::ggarrange(pa, pb, pc, common.legend = TRUE, ncol=1)
combined_plot_speed <- annotate_figure(
  combined_plot_speed,
  top = text_grob("Filter_speed_Turnangle", face = "bold", size = 10)
)
print(combined_plot_speed)
summary(ff4$speed)


#Other set of filters

ff4 <- ff4 %>%
  arrange(Frame) %>%
  mutate(
    dspeed = speed - lag(speed)
  ) %>%
  filter(
    speed > speedlow,
    speed < speedhigh,
    abs(dspeed) < 1
  )



ff4 <- ff4 %>%
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

ff4 %>% filter(speed > speedlow, speed < speedhigh) ->ff4

pa2 = ggplot(ff4, aes(z, x, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()

pb2 = ggplot(ff4, aes(x, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()

pc2 = ggplot(ff4, aes(z, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()

combined_plot_dspeed <- ggpubr::ggarrange(pa2, pb2, pc2, common.legend = TRUE, ncol=1)
combined_plot_dspeed <- ggpubr::annotate_figure(combined_plot_dspeed, top = text_grob("Filter_speed_change"))
print(combined_plot_dspeed)
summary(ff4$speed)




ff4 <- ff4 %>%
  arrange(Frame) %>%
  mutate(
    heading = atan2(dy, dx) * 180/pi,           # Heading in Grad
    dheading = heading - lag(heading),
    dheading = (dheading + 180) %% 360 - 180    # normalisieren auf -180..180
  ) %>%
  filter(abs(dheading) < 60)                    # nur realistische Änderungen

# --- Geschwindigkeit & TurnAngle neu berechnen ---
ff4 <- ff4 %>%
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

# --- Letzter Speed-Filter ---
ff4 <- ff4 %>%
  filter(speed > speedlow, speed < speedhigh)

# --- Plots ---
pa3 = ggplot(ff4, aes(z, x, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  scale_color_viridis_c()

pb3 = ggplot(ff4, aes(x, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  scale_color_viridis_c()

pc3 = ggplot(ff4, aes(z, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  scale_color_viridis_c()

combined_plot_dheading <- ggpubr::ggarrange(pa3, pb3, pc3, common.legend = TRUE, ncol=1)
combined_plot_dheading <- ggpubr::annotate_figure(combined_plot_dheading, top = text_grob("Filter_heading_change"))
print(combined_plot_dheading)

summary(ff4$speed)

