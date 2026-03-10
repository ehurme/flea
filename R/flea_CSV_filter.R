library(dplyr)
library (ggplot2)


directory <- "F:/Flanders25/Videos/Data_leis/CSV_leis"
#infile <- file.path(directory, "20250520_C0002_4100.csv")



all_files <- list.files(directory, pattern = "speed10.csv$", full.names = TRUE)

for (file in all_files) {
  df <- read.csv(file)
infile <- basename(file)

infile = strsplit(infile, ".csv")[[1]][1]
df <- read.csv (file)

min_y <- min(df$y, na.rm = TRUE)
df <- df[df$y <= min_y + 0.4,]

  outdir <- "F:/Flanders25/Videos/Data_leis/CSV_leis/"
  outfile_CSV <- paste0(outdir, infile, "_filtered", ".csv")
  print (outfile_CSV)
  write.csv(df, outfile_CSV, row.names = FALSE)
print

}


all_files <- list.files(directory, pattern = "\\_filtered.csv$", full.names = TRUE)

results <- data.frame(FileName = character(), speed = numeric())
for (file in all_files) {
  df <- read.csv(file)


 end <- nrow (df)

 start_end <- df[c(1, end),]
 start_end <- start_end [,c(1,4,5,6),]


 distance <- sqrt(
   (start_end$x[2] - start_end$x[1])^2 +
     (start_end$y[2] - start_end$y[1])^2 +
     (start_end$z[2] - start_end$z[1])^2
 )


 delta_t <- (start_end$Frame[2] - start_end$Frame[1]) / 60

 speed <- distance / delta_t
 results <- rbind(results, data.frame(FileName = basename(file), speed = speed))



}

 results <- results %>%
   mutate(
     video_id = str_extract(FileName, "\\d{8}_C\\d+")
   )



 data_flightcage <- read_sheet("1_khKqNaFJBH0Awyg4olOCRySjCSQpk8JBAA3b83wKBA",
                               sheet = 3) %>% janitor::clean_names()

 data_flightcage <- as.data.table(data_flightcage)


 data_flightcage <- data_flightcage %>%
   select (video_id, bat, trial, percent, total_weight)
 data_flightcage <- data_flightcage[3:(nrow(data_flightcage)-16),]


 data_flightcage <- data_flightcage %>% mutate(
   bat = map_chr(bat, as.character),
   trial = as.character(trial),
   percent = as.factor(percent)
 )



 results <- results %>%
    left_join(data_flightcage %>%
                select(video_id, bat, percent, total_weight),
              by = "video_id")

 print(results)

 ggplot(results, aes(x = round(as.numeric(percent),2), y = speed)) +
   #geom_violin(fill = "skyblue", alpha = 0.6, trim = FALSE) +   # Violin statt Boxplot
   #geom_jitter(width = 0.2, size = 0.8, alpha = 0.4) +          # Punkte
   geom_point()+
   geom_smooth()+
   theme_minimal() +
   labs(title = "Geschwindigkeit pro Gewicht (%)",
        x = "relatives Gewicht [%]",
        y = "Speed [m/s]") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
   facet_wrap(~bat, nrow = 2)

