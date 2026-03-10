library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(stringr)
library(googlesheets4)

base_dir <- "F:/Flanders25/Videos/"


data_Straight_flights <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 2)

meta <- as.data.table(data_Straight_flights)
meta <- meta[!is.na(`3D_file`) & `3D_file` != ""]

meta$start    <- as.numeric(meta$start)
meta$duration <- as.numeric(meta$duration)
meta$end      <- as.numeric(meta$end)

fps <- 59.94
speedlow <- 2
speedhigh <- 12

for (i in seq_len(nrow(meta))) {
  Name_infile <- meta$`3D_file`[i]
  if (is.na(Name_infile) || Name_infile == "") {
    message("Empty at row ", i, " – skip.")
    next
  }

  # Datum aus Dateinamen ziehen (8-stellig, z.B. 20250520 oder 20250521)
  file_date <- str_extract(Name_infile, "\\d{8}")
  if (is.na(file_date)) {
    message("Kein Datum im Dateinamen gefunden: ", Name_infile)
    next
  }

  # passendes Directory bauen
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
  trail  <- meta$videofile[i]
  date   <- meta$date[i]
  VideoID <- meta$VideoID[i]


  message("\n--- Processing file ", infile, " ---")

  # Einlesen & Skalieren
  df <- fread(infile) %>%
      mutate(x = x/1000, y = y/1000, z = z/1000) %>%
      arrange(Frame)

  # Speed & TurnAngle berechnen
  df <-   df %>%
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

 ff<- df %>% filter(Frame > start, Frame < (start + length), Keypoint == "centroid_cm")
 ff <- ff %>% filter(TurnAngle < 1, )
 ff <- ff %>% filter(speed > speedlow, speed < 10) ->ff
    summary(ff$speed)





    outdir_CSV <- "F:/Flanders25/Videos/Data_leis/CSV_leis/"
    outfile_CSV <- paste0(outdir_CSV, VideoID,"_", start,"_Speed10.csv")

    # prüfen ob Datei schon existiert
    if (file.exists(outfile_CSV)) {
      message("File already exists: ", outfile_CSV, " – skip.")
      next   # springt direkt zur nächsten Iteration im for-loop


    }
 print(outfile_CSV)
    write.csv(ff, outfile_CSV, row.names = FALSE)


}
