library(data.table)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(stringr)
library(googlesheets4)

directory <- "F:/Flanders25/Videos/20250520/data"
infile <- file.path(directory, "Out3DDict2_20250520_C0002_aligned.csv")
df <- fread(infile)

Sheet_id <- "1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA"
data_Straight_flights <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 2)
print(data_Straight_flights)


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


# look at a flight segment
start  <- 4070
length <- 55
end    <- (start + length)
speedlow <- 2
speedhigh <-11

df %>% filter(Frame > start, Frame < (start + length), Keypoint == "centroid_cm") -> flight

#only one filter
df_plot <- subset(flight, speed > speedlow & speed < speedhigh)

p1 = ggplot(df_plot, aes(z, x, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, , drop = FALSE ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
  scale_color_viridis_c()
p1




p2 = ggplot(df_plot, aes(z, y, col = speed)) +
  geom_path(col = 1, size = .1) +
  geom_point() +
  geom_text(
    data = df_plot[1, ],
    aes(label = Frame),
    vjust = -0.5, size = 3, inherit.aes = TRUE
  ) +
#  scale_x_continuous(limits = c(-3, 2.5)) +
#  scale_y_continuous(limits = c(-1.5, 1)) +
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
#  scale_x_continuous(limits = c(-2.5, 2.5)) +   # x-Achse
#   scale_y_continuous(limits = c(-1, 1)) +       # y-Achse
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
  geom_point()+scale_color_viridis_c()

hist(flight$speed[flight$speed <30], breaks = 100 )
hist(flight$heading)
hist(flight$TurnAngle[flight$TurnAngle <4])

#play with filters to see what gives good results
flight %>% filter(TurnAngle < 1, speed >2.2, speed <12) -> ff
summary(ff$speed)

#set file name
basename_in <- basename(infile)

datum <- str_extract(basename_in, "\\d{8}")
trail <- str_extract(basename_in, "C\\d+")
outfile <- paste0(datum,"_", trail, "_frame_", start, ".png")
print (outfile)

#Save Plot
ggsave(
  filename = outfile,       # Dateiname
  plot     = combined_plot,         # Plot-Objekt
  path     = "F:/Flanders25/Videos/20250520/Data/Fligt_Graphs", # Ordner
  width    = 8, height = 10, dpi = 300
)


# Speed between 2 frames

# ---- Speed zwischen zwei Frames (snapt auf nächstliegende existierende Frames) ----
#frame_dist_speed <- function(df, frame1, frame2,
 #                            keypoint = "centroid_cm", id = NULL, fps = 59.94) {
  #stopifnot(is.numeric(frame1), is.numeric(frame2), fps > 0)
#
 # # Daten auf Keypoint/ID einschränken
  #dat <- df %>% dplyr::filter(Keypoint == keypoint)
 #f (!is.null(id)) dat <- dat %>% dplyr::filter(ID == id)
#
#  # vorhandene Frames
#  frames_available <- dat$Frame
#  if (!length(frames_available)) stop("Keine Frames für diesen Keypoint/ID vorhanden.")#

  # nächstliegende vorhandene Frames zu den gewünschten
#  f1 <- frames_available[ which.min(abs(frames_available - frame1)) ]
 # f2 <- frames_available[ which.min(abs(frames_available - frame2)) ]

  # Punkte extrahieren
#  pts <- dat %>%
#    dplyr::filter(Frame %in% c(f1, f2)) %>%
#    dplyr::arrange(Frame) %>%
#    dplyr::group_by(Frame) %>% dplyr::slice(1) %>% dplyr::ungroup() %>%
#    dplyr::select(Frame, x, y, z)

 # if (nrow(pts) < 2) stop("Konnte keine zwei Frames finden (nach Snapping).")

#  d  <- sqrt((pts$x[2]-pts$x[1])^2 + (pts$y[2]-pts$y[1])^2 + (pts$z[2]-pts$z[1])^2)  # m
#  dt <- abs(diff(pts$Frame)) / fps                                                     # s
#  v  <- if (isTRUE(dt > 0)) d / dt else NA_real_                                       # m/s

#  tibble::tibble(
#    frame1 = pts$Frame[1], frame2 = pts$Frame[2],
#    distance_m = d, dt_s = dt, speed_m_s = v,
#    x1 = pts$x[1], y1 = pts$y[1], z1 = pts$z[1],
#    x2 = pts$x[2], y2 = pts$y[2], z2 = pts$z[2]
#  )
#}#


res <- frame_dist_speed(df,
                        frame1 <- frames_available[ which.min(abs(frames_available - start)) ],
                        frame2 <- frames_available[ which.min(abs(frames_available - end)) ],
                        keypoint = "centroid_cm", fps = fps)



df_turn_segment <- df %>%
  filter(Frame >= start, Frame <= end,
         Keypoint == "centroid_cm") %>%
  select(Frame, TurnAngle)

print(df_turn_segment)


