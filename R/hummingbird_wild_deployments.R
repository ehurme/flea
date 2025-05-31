file_path <- "C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/AD001/20250315_184835_FleaTagData_AD001_Anthracothorax-nigricollis_0.45.txt"
file_path <- "C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/AD003/20250315_185515_FleaTagData_AD003_Anthracothorax-nigricollis_0.45.txt"

files <- list.files("C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/", pattern = ".txt", full.names = TRUE, recursive = TRUE)
files_short <- list.files("C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/", pattern = ".txt", full.names = FALSE, recursive = TRUE)
file = files[7]

i = 10
pdf(file = "C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbirds/Plots/wild_hummingbirds.pdf")
for(i in 1:length(files)){
  flea_data <- read_flea_tag_data(files[i])
  sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()

  # add VeDBA to data
  plot(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$accX_mg, type = "l",
       xlab = "time (hours)", ylab = "Acceleration (mg)", main = files_short[i])
  lines(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$accY_mg, col = 2)
  lines(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$accZ_mg, col = 3)
}
dev.off()


