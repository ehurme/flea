library(ggplot2)
library(dplyr)
library(tidyr)

# Function to visualize color space from spectral data
visualize_colorspace <- function(spectral_data) {
  # Ensure the data has the required columns: Timestamp, Wavelength, and Irradiance

  # Create a heatmap of irradiance over time and wavelength
  ggplot(spectral_data, aes(x = index, y = Wavelength, fill = Irradiance)) +
    geom_tile() +  # Create heatmap
    scale_fill_viridis_c() +  # Use a perceptually uniform color scale
    labs(title = "Color Space Over Time", x = "Time (s)", y = "Wavelength (nm)", fill = "Irradiance") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
}

# Example usage
# Assuming spectral_data is the combined spectral data with 'Timestamp', 'Wavelength', and 'Irradiance'
visualize_colorspace(spectral_data)
