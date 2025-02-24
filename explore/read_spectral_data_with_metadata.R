library(data.table)
library(dplyr)
library(lubridate)

# Function to read spectral data with metadata
read_spectral_data_with_metadata <- function(file_path) {
  # Read all lines of the file
  lines <- readLines(file_path)

  # Extract metadata (everything before the spectral data starts)
  metadata_lines <- lines[1:grep(">>>>>Begin Spectral Data<<<<<", lines) - 1]

  # Extract spectral data starting after the line ">>>>>Begin Spectral Data<<<<<"
  spectral_data_start <- grep(">>>>>Begin Spectral Data<<<<<", lines) + 1
  spectral_data_lines <- lines[spectral_data_start:length(lines)]

  # Convert spectral data into a data.table (assume two columns: Wavelength and Irradiance)
  spectral_data <- fread(text = paste(spectral_data_lines, collapse = "\n"), col.names = c("Wavelength", "Irradiance"))

  # Parse the metadata into a list
  metadata <- list()
  for (line in metadata_lines) {
    if (grepl(":", line)) {
      key_value <- strsplit(line, ": ")[[1]]
      key <- trimws(key_value[1])
      value <- trimws(key_value[2])
      metadata[[key]] <- value
    }
  }

  # Extract the date from metadata
  date_str <- metadata[["Date"]]
  date_time <- parse_date_time(date_str, orders = "a b d H:M:S z Y")
  ?parse_date_time

  date_time <- dmy_hms(date_str)  # Convert to POSIXct

  # Add the date to the spectral data
  spectral_data <- spectral_data %>%
    mutate(Timestamp = date_time)

  # Return both metadata and spectral data
  return(list(metadata = metadata, spectral_data = spectral_data))
}

# Example usage
file_path <- "../../../ownCloud/FleaTagColorTest/20241011/Data/Spectrogram/3025_AbsoluteIrradiance__0__14-02-17-713.txt"  # Adjust path as needed
spectral_data_result <- read_spectral_data_with_metadata(file_path)

# Print metadata
print(spectral_data_result$metadata)

# View spectral data
head(spectral_data_result$spectral_data)


x <- c("09-01-01", "09-01-02", "09-01-03")
parse_date_time(x, "ymd")
parse_date_time(x, "y m d")
parse_date_time(x, "%y%m%d")

x <- c("09-01-01", "090102", "09-01 03", "09-01-03 12:02")
parse_date_time(x, c("ymd", "ymd HM"))

date_str
parse_date_time(date_str, orders = "a b d H:M:S z Y")
