library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(googlesheets4)

library(stringr)
library(stringr)

base_dir <- "F:/Flanders25/Videos"

data_Straight_flights <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 2)

meta <- as.data.table(data_Straight_flights)
meta <- meta[!is.na(`3D_file`) & `3D_file` != ""]

meta$start    <- as.numeric(meta$start)
meta$duration <- as.numeric(meta$duration)
meta$end      <- as.numeric(meta$end)

fps <- 59.94

for (i in seq_len(nrow(meta))) {
  Name_infile <- meta$`3D_file`[i]
  if (is.na(Name_infile) || Name_infile == "") {
    message("Empty at row ", i, " – skip.")
    next
  }

  file_date <- str_extract(Name_infile, "\\d{8}")
  if (is.na(file_date)) {
    message("Kein Datum gefunden in: ", Name_infile)
    next
  }

  ### ÄNDERN: passendes Directory zum Datum bauen
  directory <- file.path(base_dir, file_date, "Data")
  infile <- file.path(directory, Name_infile)

  if (!file.exists(infile)) {
    message("File not found: ", infile, " – skip.")
    next
  }

  start  <- meta$start[i]
  length <- meta$duration[i]
  end    <- meta$end[i]
  Bat_id <- meta$bat [i]
  VideoID  <- meta$VideoID[i]


  message("\n--- Processing file ", infile, " ---")

  # Einlesen & Skalieren
  df <- tryCatch({
    fread(infile) %>%
      mutate(x = x/1000, y = y/1000, z = z/1000) %>%
      arrange(Frame)
  }, error = function(e) {
    message("Error reading file: ", e$message)
    return(NULL)
  })
  if (is.null(df)) next

  # Speed & TurnAngle berechnen
  df <- tryCatch({
    df %>%
      group_by(ID, Keypoint) %>%
      arrange(Frame, .by_group = TRUE) %>%
      mutate(
        dx = x - lag(x),
        dy = y - lag(y),
        dz = z - lag(z),
        dF = Frame - lag(Frame),
        dt = ifelse(dF > 0, dF / fps, NA_real_),
        speed = sqrt(dx^2 + dy^2 + dz^2) / dt,
        heading = atan2(dy, dx)
      ) %>%
      ungroup() %>%
      group_by(ID, Keypoint) %>%
      mutate(
        vx = dx/dt, vy = dy/dt, vz = dz/dt,
        num = lag(vx)*vx + lag(vy)*vy + lag(vz)*vz,
        den = sqrt(lag(vx)^2+lag(vy)^2+lag(vz)^2) * sqrt(vx^2+vy^2+vz^2),
        cosang = pmax(-1, pmin(1, num/den)),
        TurnAngle = acos(cosang)
      ) %>%
      ungroup()
  }, error = function(e) {
    message("Error in speed/angle calc: ", e$message)
    return(NULL)
  })
  if (is.null(df)) next

  # Debug-Ausgabe
  message("  Rows total: ", nrow(df),
          " | Speed range: ", round(min(df$speed, na.rm = TRUE), 2),
          "–", round(max(df$speed, na.rm = TRUE), 2))

  # Segment auswählen
  flight <- df %>% filter(Frame > start, Frame < (start + length), Keypoint == "centroid_cm")
  if (nrow(flight) < 3) {
    message("  Not enough points in flight segment – skip.")
    next
  }

  # Filter auf Speed
  df_plot <- subset(flight, speed > 2 & speed < 12)
  message("  Segment rows: ", nrow(flight),
          " | After speed filter: ", nrow(df_plot))
  df_plot %>% filter(TurnAngle < 1, ) -> df_plot

  if (nrow(df_plot) < 3) {
    message("  Too few points after filter – skip plotting.")
    next
  }

  # Plots erstellen
  p1 <- ggplot(df_plot, aes(z, x, col = speed)) +
    geom_path(col = 1, linewidth = .1) +
    geom_point() +
    geom_text(data = df_plot[1, , drop = FALSE],
              aes(label = Frame),
              vjust = -0.5, size = 3, inherit.aes = TRUE) +
    scale_color_viridis_c()+
    scale_x_continuous(limits = c(-3.5, 3.5), name = "Z-Achse [m]") +
    scale_y_continuous(limits = c(-3.5, 3.5), name = "X-Achse [m]")

  p2 <- ggplot(df_plot, aes(z, y, col = speed)) +
    geom_path(col = 1, size = .1) +
    geom_point() +
    geom_text(data = df_plot[1, , drop = FALSE],
              aes(label = Frame),
              vjust = -0.5, size = 3, inherit.aes = TRUE) +
    scale_color_viridis_c()+
    scale_x_continuous(limits = c(-3.5, 3.5), name = "Z-Achse [m]") +
    scale_y_continuous(limits = c(-1.5, 1.5), name = "X-Achse [m]")

  p3 <- ggplot(df_plot, aes(x, y, col = speed)) +
    geom_path(col = 1, size = .1) +
    geom_point() +
    geom_text(data = df_plot[1, , drop = FALSE],
              aes(label = Frame),
              vjust = -0.5, size = 3, inherit.aes = TRUE) +
    scale_color_viridis_c()+
    scale_x_continuous(limits = c(-3.5, 3.5), name = "Z-Achse [m]") +
    scale_y_continuous(limits = c(-1.5, 1.5), name = "X-Achse [m]")

  combined_plot <- ggpubr::ggarrange(p1, p2, p3, common.legend = TRUE, ncol=1)



  # Titel einfügen, z. B. aus trail und date
  combined_plot <- ggpubr::annotate_figure(
    combined_plot,
    top = ggpubr::text_grob(
      paste(VideoID,"_", start),
      face = "bold", size = 14
    )
  )
  print(combined_plot)
  outdir <- "F:/Flanders25/Videos/Data_leis/Graphs_leis"
  basename_in <- basename(infile)
  print(basename_in)

  outfile <- paste0(VideoID, "_frame_", start)
  print (outfile)

  #Save Plot
  ggsave(
    filename = paste0 (outfile,"_", format(Sys.time(),"%Y%m%d_%H%M%S"), ".png"   ) ,   # Dateiname
    plot     = combined_plot,         # Plot-Objekt
    path     = outdir, # Ordner
    width    = 8, height = 10, dpi = 300
  )


  print(combined_plot)

}

