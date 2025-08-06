
source("./R/flea_functions.R")

file_path <- "C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/AD001/20250315_184835_FleaTagData_AD001_Anthracothorax-nigricollis_0.45.txt"
file_path <- "C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/AD003/20250315_185515_FleaTagData_AD003_Anthracothorax-nigricollis_0.45.txt"

files <- list.files("C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/", pattern = ".txt", full.names = TRUE, recursive = TRUE)
files_short <- list.files("C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbird/WIN 2025/Deployment recap data/", pattern = ".txt", full.names = FALSE, recursive = TRUE)
file = files[7]

i = 11
pdf(file = "C:/Users/ehumre/Dropbox/MPI/Wingbeat/Colombia25/Hummingbirds/Plots/wild_hummingbirds.pdf")
for(i in 1:length(files)){
  flea_data <- read_flea_tag_data(files[i])
  # plot(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$ColorSensRed_cnt, col = 2, cex = 0.8)
  # points(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$ColorSensGreen_cnt, col = 3, cex = 0.8)
  # points(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$ColorSensBlue_cnt, col = 4, cex = 0.8)
  # points(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$ColorSensIR_cnt, col = 1, cex = 0.8)
  #
  # # plot the ratio between red, green, blue and IR values
  # cols <- na.omit(flea_data$data)[,c(7:10)]
  # pc <- princomp(cols)
  # plot(pc$scores)

  sampling_rate <- flea_data$metadata$AccHz %>% as.numeric()

  # add VeDBA to data
  plot(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$accX_mg, type = "l",
       xlab = "time (hours)", ylab = "Acceleration (mg)", main = files_short[i])
  lines(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$accY_mg, col = 2)
  lines(flea_data$data$timeMilliseconds/1000/3600, flea_data$data$accZ_mg, col = 3)
}
dev.off()

# flea_data[[2]] %>% reframe(var_z = var(accZ_mg, na.rm = TRUE),
#                       .by = burstCount) -> burst_sum
# hist(burst_sum$var_z, breaks = 100)
# abline(v = 1e6, col = "red", lwd = 2)
# idx <- which(burst_sum$var_z > 1e6)

if(table(flea_data$data$burstCount)[1] > 50){
  idx <- flea_data$data$burstCount[which(flea_data$data$accZ_mg > 2000)] %>% unique()
  ggplot(flea_data[[2]] %>% filter(burstCount %in% idx),
         aes(x = timeMilliseconds/1000/3600)) +
    geom_line(aes(y = accX_mg), col = 1) +
    geom_line(aes(y = accY_mg), col = 2) +
    geom_line(aes(y = accZ_mg), col = 3) +
    ylim(c(-8000, 8000))+
    facet_wrap(~burstCount, scales = "free_x") +
    labs(title = "High Variance Bursts", x = "Time (hours)", y = "Acceleration (mg)")
}

