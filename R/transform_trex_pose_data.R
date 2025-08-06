
# ColumnNames:
#   - Frame: frame number of detection, this needs to be corrected with FrameDiffs above, by **SUBRACTING** from the original frame. So if Framediffs is [1,0,0,0,0,0], then all frames from camera 1 needs to -1.
# - Cam: Camera Name
# - ID: 2D tracking ID, in the format of "camera-class-UniqueNumber". I guess you only have 1 individual, so you can set to always be "{cameraname}-bat-0" or something
# - Keypoint: Name of keypoint
# - x: x value
# - y: y value

library(data.table)
library(tidyverse)
library(janitor)

transform_trex_pose_data <- function(file, camera_name, frame_diff = 0, individual_id = "bat") {

  df <- fread(file) %>% clean_names()

  if(length(which(names(df) == "frame")) > 1){
    df <- df[,2:ncol(df)]
  }

  # Filter and select relevant columns
  df_filtered <- df %>%
    filter(missing == 0) %>%
    select(frame, num_pixels,
           pose_x0, pose_x1, pose_x2,
           pose_y0, pose_y1, pose_y2,
           blob_x_number_wcentroid, blob_y_number_wcentroid,
           x_number_wcentroid_cm, y_number_wcentroid_cm)

  # Adjust frame numbers by subtracting the frame_diff
  df_filtered$frame <- df_filtered$frame - frame_diff

  # Process pose keypoints (0-2)
  pose_data <- df_filtered %>%
    select(frame, num_pixels, starts_with("pose_")) %>%
    pivot_longer(
      cols = -c(frame, num_pixels),
      names_to = c(".value", "kp_num"),
      names_pattern = "pose_([xy])(\\d)"
    ) %>%
    mutate(
      Keypoint = paste0("pose", kp_num),
      Type = "pose"
    ) %>%
    rename(x = x, y = y)

  # Process blob centroid
  blob_data <- df_filtered %>%
    select(frame, num_pixels,
           blob_x = blob_x_number_wcentroid,
           blob_y = blob_y_number_wcentroid) %>%
    mutate(
      Keypoint = "blob_centroid",
      Type = "blob",
      kp_num = NA
    )

  # Process cm centroid
  cm_data <- df_filtered %>%
    select(frame, num_pixels,
           x = x_number_wcentroid_cm,
           y = y_number_wcentroid_cm) %>%
    mutate(
      Keypoint = "centroid_cm",
      Type = "centroid",
      kp_num = NA
    )

  # Combine all data
  combined_data <- bind_rows(pose_data, blob_data, cm_data) %>%
    mutate(
      Frame = frame,
      Cam = camera_name,
      ID = ifelse(Type == "pose",
                  sprintf("%s-%s-%s", camera_name, individual_id, kp_num),
                  sprintf("%s-%s-%s", camera_name, individual_id, Keypoint))
    ) %>%
    select(Frame, Cam, ID, Keypoint, Type, x, y, num_pixels)

  return(combined_data)
}

# Example usage:
ggplot(df, aes(x = `blob_x#wcentroid`, y = `blob_y#wcentroid`, col = `detection_p`)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Blob Centroid Positions",
       x = "X Coordinate", y = "Y Coordinate")
flanders1 <- transform_trex_pose_data(file = "E:/Flanders25/Videos/20250521/F1/data/F1_20250521_C0002_id0.csv",
                                      camera_name = "F1", frame_diff = 0, individual_id = "bat0")
flanders2 <- transform_trex_pose_data(file = "E:/Flanders25/Videos/20250521/F2/data/F2_20250521_C0002_id0.csv",
                                      camera_name = "F2", frame_diff = 3, individual_id = "bat0")
flanders3 <- transform_trex_pose_data(file = "E:/Flanders25/Videos/20250521/F3/data/F3_20250521_C0002_id0.csv",
                                      camera_name = "F3", frame_diff = 1, individual_id = "bat0")
flanders4 <- transform_trex_pose_data(file = "E:/Flanders25/Videos/20250521/F4/data/F4_20250521_C0002_id0.csv",
                                      camera_name = "F4", frame_diff = 4, individual_id = "bat0")
flanders5 <- transform_trex_pose_data(file = "E:/Flanders25/Videos/20250521/F5/data/F5_20250521_C0002_id0.csv",
                                      camera_name = "F5", frame_diff = 0, individual_id ="bat0")
flanders6 <- transform_trex_pose_data(file = "E:/Flanders25/Videos/20250521/F6/data/F6_20250521_C0002_id0.csv",
                                      camera_name = "F6", frame_diff = 2, individual_id = "bat0")
all_flanders <- rbind(flanders1, flanders2, flanders3, #flanders4,
                      flanders5, flanders6)
write.csv(all_flanders, "../../../Dropbox/MPI/Wingbeat/Flanders25/Videos/20250521/trex_202500521_C0002_all_cameras_except_F4.csv", row.names = FALSE)


finca1 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0005_fish0.csv"),
                                   camera_name = "finca1", frame_diff = 0, individual_id = "bat0"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0005_fish1.csv"),
                                   camera_name = "finca1", frame_diff = 0, individual_id = "bat1"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0005_fish2.csv"),
                                   camera_name = "finca1", frame_diff = 0, individual_id = "bat2"))

finca2 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0005_fish0.csv"),
                                         camera_name = "finca2", frame_diff = 0, individual_id = "bat0"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0005_fish1.csv"),
                                         camera_name = "finca2", frame_diff = 0, individual_id = "bat1"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0005_fish2.csv"),
                                         camera_name = "finca2", frame_diff = 0, individual_id = "bat2"))

finca3 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0005_fish0.csv"),
                                         camera_name = "finca3", frame_diff = 0, individual_id = "bat0"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0005_fish1.csv"),
                                         camera_name = "finca3", frame_diff = 0, individual_id = "bat1"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0005_fish2.csv"),
                                         camera_name = "finca3", frame_diff = 0, individual_id = "bat2"))

finca4 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_fish0.csv"),
                                         camera_name = "finca4", frame_diff = 0, individual_id = "bat0"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_fish1.csv"),
                                         camera_name = "finca4", frame_diff = 0, individual_id = "bat1"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_fish2.csv"),
                                         camera_name = "finca4", frame_diff = 0, individual_id = "bat2"))

finca5 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0005_fish0.csv"),
                                         camera_name = "finca5", frame_diff = 0, individual_id = "bat0"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0005_fish1.csv"),
                                         camera_name = "finca5", frame_diff = 0, individual_id = "bat1"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0005_fish2.csv"),
                                         camera_name = "finca5", frame_diff = 0, individual_id = "bat2"))

finca6 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish0.csv"),
                                         camera_name = "finca6", frame_diff = 0, individual_id = "bat0"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish1.csv"),
                                         camera_name = "finca6", frame_diff = 0, individual_id = "bat1"),
                transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish2.csv"),
                                         camera_name = "finca6", frame_diff = 0, individual_id = "bat2"))

rbind(finca1, finca2, finca3, finca4, finca5, finca6) -> all_finca
bat0 <- all_finca[grepl(all_finca$ID, pattern = "bat-0"),]

# Save the transformed data to a CSV file
write.csv(all_finca, "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/trex_20250226_C0005_all_cameras.csv", row.names = FALSE)
write.csv(all_finca, "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/trex_20250226_C0005_all_cameras_bat0.csv", row.names = FALSE)

all_finca$Cam %>% table()
ggplot(all_finca %>% filter(Frame < 1000),
       aes(x = x, y = y, color = ID)) +
  geom_point() +
  facet_wrap(~ Cam) +
  theme_minimal() +
  labs(title = "Pose Data from Multiple Cameras",
       x = "X Coordinate", y = "Y Coordinate")



df0 <- fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_tracks/data/finca4_20250226_C0005_fish0.csv")
df1 <- fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_tracks/data/finca4_20250226_C0005_fish1.csv")
df2 <- fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_tracks/data/finca4_20250226_C0005_fish2.csv")

df0 <- fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0012_fish0.csv")
df0 <- fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0012_fish0.csv")
df0 <- df0[,2:ncol(df0)]

with(df6, plot(poseX0, poseY0, type = "l", col = rgb(0,0,0,1)))

ggplot(data = df0 %>% filter(poseX0 < 2000),
       aes(x = poseX0, y = poseY0, col = frame)) +
  #geom_path(alpha = 0.1) +
  geom_point() +
  theme_minimal()+
  xlim(c(0,1920))+ylim(c(0,1080))

library(gganimate)
# animate plot by frame
ggplot(data = df0 %>% filter(poseX0 < 2000),
       aes(x = poseX0, y = poseY0, col = frame)) +
  geom_point() +
  geom_path()+
  theme_minimal()+
  xlim(c(0,1920))+ylim(c(0,1080))+
  transition_reveal(frame, keep_last = TRUE)


plot(df1$poseX0, df1$poseY0, type = "l")
plot(df2$poseX0, df2$poseY0, type = "l")

plot(df$`X#wcentroid (cm)`, df$poseX0)



# flanders
df = fread("E:/Flanders25/Videos/20250521/F1/data/F1_20250521_C0002_id0.csv")
df <- df[,2:ncol(df)]
df <- df[is.finite(df$poseX0),]
plot(df$`Y#wcentroid (cm)`, df$poseY0)
ggplot(df, aes(time, `num_pixels#wcentroid`, col = detection_p)) + geom_point()
ggplot(df, aes(time,`Y#wcentroid (cm)`, col = detection_p, size = 1/`num_pixels#wcentroid`)) + geom_point()
ggplot(df %>% filter(time < 30, time > 22),
       aes(time, `Y#wcentroid (cm)`, col = detection_p)) + geom_point()
ggplot(df %>% filter(time < 30, time > 22),
       aes(time, poseX0, col = detection_p)) + geom_point()
summary(df$detection_p)
