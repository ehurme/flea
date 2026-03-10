
# Function to generate trex commands for video files
generate_trex_commands <- function(drive_letter = "F:\\FlandersAugust2025\\Video",
                                   settings_path = "C:\\Users\\Edward\\default_convert.settings",
                                   video_extensions = c(".mp4", ".MP4", ".mov", ".MOV", ".avi", ".AVI",
                                                        ".mkv", ".MKV", ".wmv", ".WMV")) {

  # Get all video files recursively
  video_files <- list.files(
    path = drive_letter,
    pattern = paste0("\\.(", paste0(sub("^\\.", "", video_extensions), collapse = "|"), ")$"),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  # Generate trex commands
  commands <- sapply(video_files, function(file) {
    sprintf('trex -i "%s" -task convert -s "%s" -auto_quit', file, settings_path)
  })

  return(commands)
}

# Usage example:
# Generate commands for drive E:
commands <- generate_trex_commands(drive_letter = "F:\\FlandersAugust2025\\Video")

# Print commands to console (you can copy-paste these to command line)
cat(commands, sep = "\n")

# Optionally save to a text file
writeLines(commands, "trex_commands.txt")

cat(sprintf("\nGenerated %d commands. Saved to 'trex_commands.txt'\n", length(commands)))


