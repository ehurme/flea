read_flea_tag_data <- function(file_path) {
  csv <- grepl(pattern = "csv", x = file_path)
  if(csv){
    metadata <- NULL
    data = read.csv(file_path)
  }

  if(!csv){
    # Open the file
    lines <- readLines(file_path)

    # Find the index of the line containing "Delete memory by pressing button for 8"
    stop_idx <- grep("Delete memory by pressing button for 8", lines)[1]

    # Extract the metadata into a list
    metadata <- list()

    # Split metadata (everything before the line starting with "lineCnt")
    metadata_end_idx <- (grep("^lineCnt", lines) - 1)[1]
    metadata_lines <- NULL

    if(!is.na(metadata_end_idx)){
      metadata_lines <- lines[1:metadata_end_idx]
      for (line in metadata_lines) {
        if (grepl(":", line)) {
          split_line <- strsplit(line, ":")[[1]]
          key <- trimws(split_line[1])
          value <- trimws(split_line[2])
          metadata[[key]] <- value
        }
      }
    }

    # Extract the data part starting from the lineCnt header until the stop point
    if(is.na(metadata_end_idx)) metadata_end_idx <- 0
    data_lines <- lines[metadata_end_idx + 1:(stop_idx - metadata_end_idx - 1)]

    # Read the data into a data frame
    if(any(grepl("lineCnt", data_lines))){
      data <- read.csv(text = paste(data_lines, collapse = "\n"))
    }
    if(!any(grepl("lineCnt", data_lines))){
      data <- read.csv(text = paste(data_lines, collapse = "\n"), header = FALSE)
      names(data) <- c('lineCnt','timeMilliseconds','burstCount',
                       'accX_mg','accY_mg','accZ_mg',
                       'ColorSensRed_cnt',
                       'ColorSensGreen_cnt',
                       'ColorSensBlue_cnt',
                       'ColorSensIR_cnt')
    }
  }
  # Return both metadata and data
  return(list(metadata = metadata, data = data))
}

# Example usage
file_path <- "../../../Dropbox/MPI/Wingbeat/Flanders25/Data/202505_FleaTagging_Flanders/20250522_FleaTagging_Flanders/20250522_Bat5_Trial2_3037.txt"

file_path <- "../../../Dropbox/MPI/Wingbeat/color_calibration/20241011_Mathias/tag3025_trial2_20241011_142625_FleaTagData.txt"


file_path <- "C:/Users/ehumre/ownCloud/2024-Fall-Moth-Barn/20240920/Moth6_beautifulflight_20240920_133313_FleaTagData.txt"
# does wingbeat frequency level out?


file_path <- "../../../ownCloud/FleaTagColorTest/20241011/Data/Tag_data/tag3025_trial1_20241011_140820_FleaTagData.txt"
file_path <- "../../../ownCloud/FleaTagColorTest/20241011/Data/Tag_data/tag3025_trial2_20241011_142625_FleaTagData.txt"

file_path <- "../../../ownCloud/FleaTagColorTest/20241011/Data/Tag_data/tag302D_trial1_20241011_143730_FleaTagData.txt"

# file_path <- "../../../ownCloud/FleaTagColorTest/20241011/Data/Tag_data/tag302e_trial1_20241011_135944_FleaTagData.txt"
## bad recordings

flea_data <- read_flea_tag_data(file_path)
processed_data <- flea_preprocess(data = flea_data$data, window = 1,
                                  flying_column = "rolling_var_VeDBA",
                                  flying_threshold = 0.5)
processed_data$spectro_freq <- NA
p <- flea_plot(processed_data, ) #, label_high_variability = FALSE, plot_variance = FALSE)
p


flea_plot_spectrogram(data = processed_data, sampling_rate = 210, window = 2)



# Print the metadata
print(flea_data$metadata)

# View the first few rows of the data
head(flea_data$data)

data_clean <- na.omit(processed_data)
ymax <- max(c(data_clean$ColorSensRed_cnt, data_clean$ColorSensBlue_cnt, data_clean$ColorSensGreen_cnt, data_clean$ColorSensIR_cnt))
plot(data_clean$timeMilliseconds/1000, data_clean$ColorSensIR_cnt, type = "l", col = "darkred",
     ylim = c(0,ymax*1.5), xlab = "time (secs)", ylab = "Color count")
lines(data_clean$timeMilliseconds/1000, data_clean$ColorSensRed_cnt, col = "red")
lines(data_clean$timeMilliseconds/1000, data_clean$ColorSensGreen_cnt, col = "green")
lines(data_clean$timeMilliseconds/1000, data_clean$ColorSensBlue_cnt, col = "blue")



plot(data$ColorSensBlue_cnt, data$ColorSensRed_cnt)
plot(data$ColorSensBlue_cnt, data$ColorSensGreen_cnt)

names(data)
pc <- prcomp(x = na.omit(data[,7:10]))
plot(pc$x[,1], pc$x[,2])
pc

