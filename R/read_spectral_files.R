library(data.table)
library(dplyr)
library(lubridate)

# Function to import spectral data and handle timestamps
read_spectral_files <- function(folder_path, tagID, file_creation_date) {
  # List all files in the folder that start with the tagID
  files <- list.files(path = folder_path, pattern = paste0("^", tagID), full.names = TRUE)

  # Initialize an empty list to store data from all files
  spectral_data_list <- list()

  # Loop through the files and read each one
  for (i in 1:length(files)) {
    # Read the file into a data table
    file_data <- fread(files[i])

    # Rename the columns to Wavelength and Irradiance
    names(file_data) <- c("Wavelength", "Irradiance")

    # Extract the timestamp from the file name
    file_info <- strsplit(basename(files[i]), "__")[[1]]
    timestamp_raw <- gsub(".txt", "", file_info[3])  # Extract the raw timestamp (e.g., 14-25-53-559)

    # Add the POSIX timestamp to the data
    file_data <- file_data %>%
      mutate(Timestamp = timestamp_raw,
             index = i)

    # Store the file's data in the list
    spectral_data_list[[files[i]]] <- file_data
  }

  # Combine all the data into one data frame
  combined_spectral_data <- bind_rows(spectral_data_list)

  return(combined_spectral_data)
}

# Example usage
folder_path <- "../../../Dropbox/MPI/Wingbeat/color_calibration/20241011_Mathias/FleaTag/"  # Path where your files are located
tagID <- "302D"  # The tag ID to match
spectral_data <- read_spectral_files(folder_path, tagID)

# View the first few rows of the combined data
head(spectral_data)

