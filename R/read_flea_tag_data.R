read_flea_tag_data <- function(file_path) {
  # Open the file
  lines <- readLines(file_path)

  # Find the index of the line containing "Delete memory by pressing button for 8"
  stop_idx <- grep("Delete memory by pressing button for 8", lines)

  # Split metadata (everything before the line starting with "lineCnt")
  metadata_end_idx <- grep("^lineCnt", lines) - 1
  metadata_lines <- lines[1:metadata_end_idx]

  # Extract the metadata into a list
  metadata <- list()
  for (line in metadata_lines) {
    if (grepl(":", line)) {
      split_line <- strsplit(line, ":")[[1]]
      key <- trimws(split_line[1])
      value <- trimws(split_line[2])
      metadata[[key]] <- value
    }
  }

  # Extract the data part starting from the lineCnt header until the stop point
  data_lines <- lines[metadata_end_idx + 1:(stop_idx - metadata_end_idx - 1)]

  # Read the data into a data frame
  data <- read.csv(text = paste(data_lines, collapse = "\n"))

  # Return both metadata and data
  return(list(metadata = metadata, data = data))
}

# Example usage
file_path <- "../../../Dropbox/MPI/Wingbeat/color_calibration/20241011_Mathias/tag3025_trial2_20241011_142625_FleaTagData.txt"
flea_data <- read_flea_tag_data(file_path)

# Print the metadata
print(flea_data$metadata)

# View the first few rows of the data
head(flea_data$data)



plot(data$timeMilliseconds/1000, data$ColorSensIR_cnt)
plot(data$timeMilliseconds/1000, data$ColorSensRed_cnt)
plot(data$timeMilliseconds/1000, data$ColorSensGreen_cnt)
plot(data$timeMilliseconds/1000, data$ColorSensBlue_cnt)

plot(data$ColorSensBlue_cnt, data$ColorSensRed_cnt)

names(data)
pc <- prcomp(x = na.omit(data[,7:10]))
plot(pc$x[,1], pc$x[,2])
