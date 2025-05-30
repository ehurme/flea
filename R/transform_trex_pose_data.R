
# ColumnNames:
#   - Frame: frame number of detection, this needs to be corrected with FrameDiffs above, by **SUBRACTING** from the original frame. So if Framediffs is [1,0,0,0,0,0], then all frames from camera 1 needs to -1.
# - Cam: Camera Name
# - ID: 2D tracking ID, in the format of "camera-class-UniqueNumber". I guess you only have 1 individual, so you can set to always be "{cameraname}-bat-0" or something
# - Keypoint: Name of keypoint
# - x: x value
# - y: y value

library(data.table)
library(tidyverse)

transform_trex_pose_data <- function(df, camera_name, frame_diff = 0, individual_id = "bat") {
  if(length(which(names(df) == "frame")) > 1){
    df <- df[,2:ncol(df)]
  }

  # Filter and select relevant columns
  df_filtered <- df %>%
    filter(missing == 0) %>%
    select(frame, num_pixels,
           poseX0, poseX1, poseX2,
           poseY0, poseY1, poseY2)

  # Adjust frame numbers by subtracting the frame_diff
  df_filtered$frame <- df_filtered$frame - frame_diff

  # Reshape the data from wide to long format
  df_long <- df_filtered %>%
    pivot_longer(
      cols = -c(frame, num_pixels),
      names_to = c(".value", "pose_num"),
      names_pattern = "(pose[X|Y])(\\d)"
    ) %>%
    rename(x = poseX, y = poseY) %>%
    mutate(pose_num = as.numeric(pose_num))

  # Create the final formatted dataframe
  result <- df_long %>%
    mutate(
      Frame = frame,
      Cam = camera_name,
      ID = sprintf("%s-%s-%d", camera_name, individual_id, pose_num),
      Keypoint = sprintf("pose%d", pose_num),
      x = x,
      y = y
    ) %>%
    select(Frame, Cam, ID, Keypoint, x, y)

  return(result)
}

# Example usage:
df <- fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish0.csv")
df <- df[,2:ncol(df)]

finca1 <- transform_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0005_fish0.csv"),
                              camera_name = "finca1", frame_diff = 0)
finca2 <- transform_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0005_fish0.csv"),
                              camera_name = "finca2", frame_diff = 0)
finca3 <- transform_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0005_fish0.csv"),
                              camera_name = "finca3", frame_diff = 0)
finca4 <- transform_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0005_fish0.csv"),
                              camera_name = "finca4", frame_diff = 0)
finca5 <- transform_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0005_fish0.csv"),
                              camera_name = "finca5", frame_diff = 0)
finca6 <- transform_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0005_fish0.csv"),
                              camera_name = "finca6", frame_diff = 0)

rbind(finca1, finca2, finca3, finca4, finca5, finca6) -> all_finca

# Save the transformed data to a CSV file
write.csv(all_finca, "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/trex_20250226_C0005_all_cameras.csv", row.names = FALSE)

all_finca$Cam %>% table()
ggplot(all_finca, aes(x = x, y = y, color = ID)) +
  geom_point() +
  facet_wrap(~ Cam) +
  theme_minimal() +
  labs(title = "Pose Data from Multiple Cameras", x = "X Coordinate", y = "Y Coordinate")
