
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
                  sprintf("%s-%s-%s", camera_name, "bat", individual_id),
                  sprintf("%s-%s-%s", camera_name, "bat", individual_id))
    ) %>%
    select(Frame, Cam, ID, Keypoint, Type, x, y, num_pixels)

  return(combined_data)
}

# Example usage:

