# Get exported trex files in the right structure for MAAP3D
# Code for Till
#

source("C:/Users/Edward/Desktop/Github/flea/R/transform_trex_pose_data.R")

# edit these and nothing else
directory <- "F:/Flanders25/Videos"
#directory <- "F:/Flanders25/Videos"
date <- "20250520"
video <- "C0009"
folder <- "checked/data"



flanders1 <- transform_trex_pose_data(file = paste0(directory, "/", date,
                                                    "/F1/", folder, "/F1_", date,
                                                    "_", video, "_id0.csv"),
                                      camera_name = "F1",
                                      frame_diff = 0,
                                      individual_id = "bat0")
flanders2 <- transform_trex_pose_data(file = paste0(directory, "/", date,
                                                    "/F2/", folder, "/F2_", date,
                                                    "_", video, "_id0.csv"),
                                      camera_name = "F2",
                                      frame_diff = 0,
                                      individual_id = "bat0")
flanders3 <- transform_trex_pose_data(file = paste0(directory, "/", date,
                                                    "/F3/", folder, "/F3_", date,
                                                    "_", video, "_id0.csv"),
                                      camera_name = "F3",
                                      frame_diff = 0,
                                      individual_id = "bat0")
flanders4 <- transform_trex_pose_data(file = paste0(directory, "/", date,
                                                    "/F4/", folder, "/F4_", date,
                                                    "_", video, "_id0.csv"),
                                      camera_name = "F4",
                                      frame_diff = 0,
                                      individual_id = "bat0")
flanders5 <- transform_trex_pose_data(file = paste0(directory, "/", date,
                                                    "/F5/", folder, "/F5_", date,
                                                    "_", video, "_id0.csv"),
                                      camera_name = "F5",
                                      frame_diff = 0,
                                      individual_id = "bat0")
flanders6 <- transform_trex_pose_data(file = paste0(directory, "/", date,
                                                    "/F6/", folder, "/F6_", date,
                                                    "_", video, "_id0.csv"),
                                      camera_name = "F6",
                                      frame_diff = 0,
                                      individual_id = "bat0")

all_flanders <- rbind(flanders1, flanders2, flanders3, flanders4,
                      flanders5, flanders6)
write.csv(all_flanders, #%>% filter(Keypoint == "centroid_cm"),
          paste0("F:/Flanders25/Videos/", date, "/Data/", "trex_", date, "_", video, "_all_cameras.csv"),
          row.names = FALSE)

#../../../Dropbox/MPI/Wingbeat/Flanders25/Videos/

#p1 <-
  ggplot(all_flanders %>%
         filter(Keypoint == "centroid_cm",
                Frame > 5000, Frame < 10000),
       aes(x = Frame, y = x, col = Cam)) +
  geom_point(alpha = 0.5) +
  theme_minimal()+
  facet_wrap(~Cam)

library(ggpubr)
ggarrange(p1, p2, ncol = 1)


all_flanders$Keypoint %>% table()


# calculate speed for each camera
all_flanders %>% group_by(Cam, Keypoint) %>%
  arrange(Frame) %>%
  mutate(diff_x = c(NA, diff(x)),
         diff_y = c(NA, diff(y)),
         speed = sqrt(diff_x^2 + diff_y^2)) -> all_flanders


ggplot(all_flanders %>%
         filter(Keypoint == "centroid_cm",
                Frame > 4000, Frame < 15000),
       aes(x = Frame, y = speed, col = Cam)) +
  geom_point(alpha = 0.5) +
  theme_minimal()+
  facet_wrap(~Cam)

 # finca1 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0005_fish0.csv"),
#                                          camera_name = "finca1", frame_diff = 0, individual_id = "bat0"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0005_fish1.csv"),
#                                          camera_name = "finca1", frame_diff = 0, individual_id = "bat1"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0005_fish2.csv"),
#                                          camera_name = "finca1", frame_diff = 0, individual_id = "bat2"))
#
# finca2 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0005_fish0.csv"),
#                                          camera_name = "finca2", frame_diff = 0, individual_id = "bat0"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0005_fish1.csv"),
#                                          camera_name = "finca2", frame_diff = 0, individual_id = "bat1"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0005_fish2.csv"),
#                                          camera_name = "finca2", frame_diff = 0, individual_id = "bat2"))
#
# finca3 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0005_fish0.csv"),
#                                          camera_name = "finca3", frame_diff = 0, individual_id = "bat0"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0005_fish1.csv"),
#                                          camera_name = "finca3", frame_diff = 0, individual_id = "bat1"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0005_fish2.csv"),
#                                          camera_name = "finca3", frame_diff = 0, individual_id = "bat2"))
#
# finca4 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_fish0.csv"),
#                                          camera_name = "finca4", frame_diff = 0, individual_id = "bat0"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_fish1.csv"),
#                                          camera_name = "finca4", frame_diff = 0, individual_id = "bat1"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_fish2.csv"),
#                                          camera_name = "finca4", frame_diff = 0, individual_id = "bat2"))
#
# finca5 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0005_fish0.csv"),
#                                          camera_name = "finca5", frame_diff = 0, individual_id = "bat0"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0005_fish1.csv"),
#                                          camera_name = "finca5", frame_diff = 0, individual_id = "bat1"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0005_fish2.csv"),
#                                          camera_name = "finca5", frame_diff = 0, individual_id = "bat2"))
#
# finca6 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish0.csv"),
#                                          camera_name = "finca6", frame_diff = 0, individual_id = "bat0"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish1.csv"),
#                                          camera_name = "finca6", frame_diff = 0, individual_id = "bat1"),
#                 transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish2.csv"),
#                                          camera_name = "finca6", frame_diff = 0, individual_id = "bat2"))
#
# rbind(finca1, finca2, finca3, finca4, finca5, finca6) -> all_finca
# bat0 <- all_finca[grepl(all_finca$ID, pattern = "bat-0"),]
#
# # Save the transformed data to a CSV file
# write.csv(all_finca, "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/trex_20250226_C0005_all_cameras.csv", row.names = FALSE)
# write.csv(all_finca, "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/trex_20250226_C0005_all_cameras_bat0.csv", row.names = FALSE)
#
# all_finca$Cam %>% table()
# ggplot(all_finca %>% filter(Frame < 1000),
#        aes(x = Frame, y = y, color = ID)) +
#   geom_point() +
#   facet_wrap(~ Cam) +
#   theme_minimal() +
#   labs(title = "Pose Data from Multiple Cameras",
#        x = "X Coordinate", y = "Y Coordinate")
#
#
#
# df0 <- fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_tracks/data/finca4_20250226_C0005_fish0.csv")
# df1 <- fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_tracks/data/finca4_20250226_C0005_fish1.csv")
# df2 <- fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_tracks/data/finca4_20250226_C0005_fish2.csv")
#
# df0 <- fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0012_fish0.csv")
# df0 <- fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0012_fish0.csv")
# df0 <- df0[,2:ncol(df0)]
#
# with(df6, plot(poseX0, poseY0, type = "l", col = rgb(0,0,0,1)))
#
# ggplot(data = df0 %>% filter(poseX0 < 2000),
#        aes(x = poseX0, y = poseY0, col = frame)) +
#   #geom_path(alpha = 0.1) +
#   geom_point() +
#   theme_minimal()+
#   xlim(c(0,1920))+ylim(c(0,1080))
#
# library(gganimate)
# # animate plot by frame
# ggplot(data = df0 %>% filter(poseX0 < 2000),
#        aes(x = poseX0, y = poseY0, col = frame)) +
#   geom_point() +
#   geom_path()+
#   theme_minimal()+
#   xlim(c(0,1920))+ylim(c(0,1080))+
#   transition_reveal(frame, keep_last = TRUE)
#
#
# plot(df1$poseX0, df1$poseY0, type = "l")
# plot(df2$poseX0, df2$poseY0, type = "l")
#
# plot(df$`X#wcentroid (cm)`, df$poseX0)
#
#
#
# # flanders
# df = fread("E:/Flanders25/Videos/20250521/F1/data/F1_20250521_C0002_id0.csv")
# df <- df[,2:ncol(df)]
# df <- df[is.finite(df$poseX0),]
# plot(df$`Y#wcentroid (cm)`, df$poseY0)
# ggplot(df, aes(time, `num_pixels#wcentroid`, col = detection_p)) + geom_point()
# ggplot(df, aes(time,`Y#wcentroid (cm)`, col = detection_p, size = 1/`num_pixels#wcentroid`)) + geom_point()
# ggplot(df %>% filter(time < 30, time > 22),
#        aes(time, `Y#wcentroid (cm)`, col = detection_p)) + geom_point()
# ggplot(df %>% filter(time < 30, time > 22),
#        aes(time, poseX0, col = detection_p)) + geom_point()
# summary(df$detection_p)
