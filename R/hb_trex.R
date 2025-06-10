# export hummingbird trex data
source("./R/transform_trex_pose_data.R")

# hummingbird tracking
df1 <- fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0001_fish0.csv")
df2 <- fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0001_fish0.csv")
df3 <- fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0001_fish0.csv")
df4 <- fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0001_fish0.csv")
df5 <- fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0001_fish0.csv")
df6 <- fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0001_fish0.csv")

with(df6, plot(poseX0, poseY0, type = "l", col = rgb(0,0,0,1)))

finca1 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca1/data/finca1_20250226_C0001_fish0.csv"),
                                         camera_name = "finca1", frame_diff = 0, individual_id = "bat0"))

finca2 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca2/data/finca2_20250226_C0001_fish0.csv"),
                                         camera_name = "finca2", frame_diff = 0, individual_id = "bat0"))

finca3 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca3/data/finca3_20250226_C0001_fish0.csv"),
                                         camera_name = "finca3", frame_diff = 0, individual_id = "bat0"))

finca4 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca4/data/finca4_20250226_C0001_fish0.csv"),
                                         camera_name = "finca4", frame_diff = 0, individual_id = "bat0"))

finca5 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca5/data/finca5_20250226_C0001_fish0.csv"),
                                         camera_name = "finca5", frame_diff = 0, individual_id = "bat0"))

finca6 <- rbind(transform_trex_pose_data(df = fread("E:/Finca_Flights/20250226/finca6/data/finca6_20250226_C0001_fish0.csv"),
                                         camera_name = "finca6", frame_diff = 0, individual_id = "bat0"))

rbind(finca1, finca2, finca3, finca4, finca5, finca6) -> all_finca
# bat0 <- all_finca[grepl(all_finca$ID, pattern = "bat-0"),]

# Save the transformed data to a CSV file
write.csv(all_finca, "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/trex_20250226_C0001_all_cameras.csv", row.names = FALSE)
# write.csv(all_finca, "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/20250226/trex_20250226_C0001_all_cameras_bat0.csv", row.names = FALSE)
