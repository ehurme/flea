library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

directory <- "F:/Flanders25/Videos/Data_leis/CSV_leis"

# Alle gespeicherten CSVs einlesen
all_files <- list.files(directory, pattern = "_filtered.csv$", full.names = TRUE)
print (all_files)
# Alle CSVs einlesen mit Dateiname als Spalte
all_data <- rbindlist(
  lapply(all_files, function(f) {
    dt <- fread(f)
    dt$source <- f   # <-- hier kommt der volle Pfad in jede Zeile
    dt
  }),
  fill = TRUE
)


data_flightcage <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA", sheet = 3)

data_flightcage <- as.data.table(data_flightcage)
print (data_flightcage)

data_flightcage <- data_flightcage %>%
  select (VideoID, bat, trial, percent)
data_flightcage <- data_flightcage[3:(nrow(data_flightcage)-16),]


data_flightcage <- data_flightcage %>% mutate(
  bat = map_chr(bat, as.character),
  trial = as.character(trial),
  percent = as.factor(percent)

)

# sicherstellen, dass es ein Character ist
all_data$source <- as.character(all_data$source)

# Datei-Infos extrahieren
all_data <- all_data %>%
  mutate(
    FileName   = basename(source),
    Date       = str_extract(FileName, "^\\d{8}"),
    VideoID    = paste0 (Date,"_", str_extract(FileName, "C\\d+")),
    StartFrame = str_match(FileName, "_(\\d+)_speed10_filtered.csv$")[,2] %>% as.numeric(),
    SegmentID  = paste0(VideoID, "_", StartFrame)
  )

head(all_data[, c("FileName", "Date", "VideoID", "StartFrame", "SegmentID")])

all_data <- all_data %>%
  left_join(data_flightcage %>%
              select(VideoID, bat, percent),
            by = "VideoID")

print(all_data)

# Alle Boxplots nebeneinander
ggplot(all_data, aes(x = SegmentID, y = speed)) +
  geom_violin(outlier.shape = NA, fill = "skyblue", alpha = 0.6) +
  geom_jitter(width = 0.2, size = 0.8, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Geschwindigkeit pro Segment",
       x = "relative weight",
       y = "Speed [m/s]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplots pro C-Nummer (VideoID)
ggplot(all_data, aes(x = SegmentID, y = speed)) +
  geom_boxplot(outlier.shape = NA, fill = "skyblue", alpha = 0.6) +
  geom_jitter(width = 0.2, size = 0.8, alpha = 0.4) +
  theme_minimal() +
  labs(title = "Geschwindigkeit pro Video",
       x = "VideoID",
       y = "Speed [m/s]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

all_data <- all_data %>%
  mutate(weight_id = paste0(percent, " % (", VideoID, ")"),
         weight_id = factor(weight_id))


fidle1 <- ggplot(all_data, aes(x = weight_id, y = speed)) +
  geom_violin(fill = "skyblue", alpha = 0.6, trim = FALSE) +   # Violin statt Boxplot
  geom_jitter(width = 0.2, size = 0.8, alpha = 0.4) +          # Punkte
  theme_minimal() +
  labs(title = "Geschwindigkeit pro Gewicht (%)",
       x = "relatives Gewicht [%]",
       y = "Speed [m/s]") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(fidle1)

outdir <- "F:/Flanders25/Videos/Data_leis/Graphs_leis"
ggsave(
  filename = paste0("fidle_plot_leis_20250520_", format(Sys.time(),"%Y%m%d_%H%M%S"),".png"),
  plot     = fidle1,
  path     = outdir,
  width    = 8, height = 10, dpi = 300
)

