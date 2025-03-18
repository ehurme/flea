library(tidyverse)
library(data.table)
library(dplyr)
library(zoo)
library(seewave)
library(tuneR)
library(plotly)
library(readxl)

source("./R/flea_functions.R")
sampling_rate = 105
split_duration_threshold <- 5
df <- read_xlsx("../../../Dropbox/MPI/Wingbeat/Colombia25/Data/Finca_Flights_2025.xlsx", sheet = 2)

# file structure
## date_time_tag_species_bat_trial.txt
files <- list.files(path = "../../../Dropbox/MPI/Wingbeat/Colombia25/Data/",
           pattern = "*txt", recursive = TRUE, full.names = TRUE)
files <- files[grepl(x = files, pattern = "Trial")]
file_path = files[51]

wb <- data.frame()
wbs <- data.frame()
# plot(0,0, xlim = c(0.005, 0.020), ylim = c(0.8,1), xlab = "Frequency", ylab = "Amplitude")
for(file_path in files){
  try({
    flea_data <- read_flea_tag_data(file_path)
    processed_data <- flea_preprocess(data = flea_data$data,
                                      window = 0.5,
                                      flying_threshold = 0.5,
                                      flying_column = "rolling_var_PC")
    duration_flea = nrow(processed_data)/sampling_rate

    # split <- split_by_true_groups(processed_data, flag_column = "is_flying")
    # # i = 12
    # for(i in 1:length(split)){
    #   processed_split <- split[[i]]
    #
    #   # remove first and last second
    #   cut_ends = 1
    #   idx <- (cut_ends*sampling_rate):(nrow(processed_split)-(cut_ends*sampling_rate))
    #   processed_split <- processed_split[idx,]
    #
    #   duration_split <- nrow(processed_split)/sampling_rate
    #   print(paste0("Split ", i, " duration: ", round(duration_split, 1), "s"))
    #
    #   # hist(processed_split$VeDBA)
    #   # plot(processed_split$VeDBA)
    #   # lines(processed_split$rolling_VeDBA, col = 2, lwd = 2)
    #   # abline(h = median(processed_split$rolling_VeDBA), col = 2, lty = 2)
    #   # abline(h = median(processed_split$VeDBA), col = 3, lty = 2)
    #   #
    #   # hist(processed_split$rolling_VeDBA)
    #   # abline(v = median(processed_split$rolling_VeDBA))
    #
    #   # summary(processed_split$rolling_VeDBA)
    #   # summary(processed_split$VeDBA)
    #
    #   if(duration_split > split_duration_threshold){
    #     freq_split <- flea_plot_spectrogram(data = processed_split,
    #                                         sampling_rate = sampling_rate,
    #                                         fmin = 5, fmax = 20,
    #                                         window = 2,
    #                                         plot_spectro = FALSE,
    #                                         plot_density = FALSE,
    #                                         pc = TRUE,
    #                                         overlay_dominant_freq = TRUE)
    #     # freq_split
    #     dom_freq_length <- freq_split[[2]][,2] %>% na.omit() %>% length()
    #     peak <- get_peak_range(density = freq_split[[3]],threshold = 0.85,
    #                            smooth = TRUE, plot_smooth = FALSE, spar = 0)
    #
    #     s <- data.frame(file = file_path,
    #                     duration_split,
    #                     dom_freq_length,
    #                     x = peak[[1]]$x,
    #                     y = peak[[1]]$y,
    #                     min_peak = peak[[2]][1] %>% as.numeric(),
    #                     q1_peak = peak[[2]][2] %>% as.numeric(),
    #                     median_peak = peak[[2]][3] %>% as.numeric(),
    #                     mean_peak = peak[[2]][4] %>% as.numeric(),
    #                     q3_peak = peak[[2]][5] %>% as.numeric(),
    #                     max_peak = peak[[2]][6] %>% as.numeric(),
    #                     median_rolling_vedba = median(processed_split$rolling_VeDBA),
    #                     median_vedba = median(processed_split$VeDBA)
    #     )
    #     wbs <- rbind(wbs, s)
    #   }
    # }


    freq <- flea_plot_spectrogram(data = processed_data, #[complete.cases(processed_data$timeMilliseconds),],
                                  sampling_rate = sampling_rate,
                                  fmin = 5, fmax = 40, window = 2,
                                  plot_spectro = FALSE, plot_density = FALSE,
                                  pc = TRUE,
                                  overlay_dominant_freq = FALSE)

    processed_data$spectro_freq <- NA
    idx <- which(!is.na(freq[[2]][,2]))
    for(i in idx){
      pidx <- which.min(abs(freq[[2]][i,1] - processed_data$timeSeconds))
      processed_data$spectro_freq[pidx] <- freq[[2]][i,2]*1000
    }

    p <- flea_plot(processed_data)
    p

    dom_freq_length <- freq[[2]][,2] %>% na.omit() %>% length()
    peak <- get_peak_range(density = freq[[3]],threshold = 0.85,
                           smooth = TRUE, plot_smooth = FALSE, spar = 0)
    peak[[3]] <- file_path

    # lines(x = peak[[1]], col = sample(1:10, size = 1))

    w <- data.frame(file = file_path,
                    duration_flea,
                    dom_freq_length,
                    x = peak[[1]]$x,
                    y = peak[[1]]$y,
                    min_peak = peak[[2]][1] %>% as.numeric(),
                    q1_peak = peak[[2]][2] %>% as.numeric(),
                    median_peak = peak[[2]][3] %>% as.numeric(),
                    mean_peak = peak[[2]][4] %>% as.numeric(),
                    q3_peak = peak[[2]][5] %>% as.numeric(),
                    max_peak = peak[[2]][6] %>% as.numeric(),
                    median_rolling_vedba = median(processed_data$rolling_VeDBA, na.rm = TRUE),
                    median_vedba = median(processed_data$VeDBA, na.rm = TRUE)
                    )
    wb <- rbind(wb, w)
  })
}

wb$date <- ymd(sapply(strsplit(sapply(strsplit(wb$file, "_"), "[", 1), "/"), "[", 9))
wbs$date <- ymd(sapply(strsplit(sapply(strsplit(wbs$file, "_"), "[", 1), "/"), "[", 9))

wb$tag_id <- sapply(strsplit(wb$file, "_"), "[", 5)
wbs$tag_id <- sapply(strsplit(wbs$file, "_"), "[", 5)

wb$species <- sapply(strsplit(wb$file, "_"), "[", 6)
wbs$species <- sapply(strsplit(wbs$file, "_"), "[", 6)

wb$bat <- sapply(strsplit(wb$file, "_"), "[", 7)
wbs$bat <- sapply(strsplit(wbs$file, "_"), "[", 7)

wb$trial <- substr(sapply(strsplit(wb$file, "_"), "[", 8), 6,6) # %>% as.numeric()
wbs$trial <- substr(sapply(strsplit(wbs$file, "_"), "[", 8), 6,6) # %>% as.numeric()

wb$bat_mass <- NA
wbs$bat_mass <- NA

wb$flea_mass <- NA
wbs$flea_mass <- NA

wb$housing_mass <- NA
wbs$housing_mass <- NA

wb$velcro_mass <- NA
wbs$velcro_mass <- NA

wb$tag_mass <- NA
wbs$tag_mass <- NA

bats <- unique(wb$bat)
bat = bats[1]
for(bat in bats){
  # idx <- which(df$bat == bat)
  for(i in 2:5){
    idx <- which(df$bat == bat & df$trial == paste0(i, ".0"))
    if(length(idx) > 0){
      widx <- which(wb$bat == bat & wb$trial == i)
      wb$bat_mass[widx] <- as.numeric(df$`bat weight*`[idx]) - as.numeric(df$`velco weight`[idx])
      wb$flea_mass[widx] <- as.numeric(df$`tag weight`[idx])
      wb$housing_mass[widx] <- as.numeric(df$`housing weight`[idx])
      wb$velcro_mass[widx] <- as.numeric(df$`velco weight`[idx])
      wb$tag_mass[widx] <- as.numeric(df$`tag weight`[idx]) + as.numeric(df$`velco weight`[idx]) +
        as.numeric(df$`housing weight`[idx])
    }
  }
}

wb$total_mass <- wb$bat_mass + wb$tag_mass

wb$dom_freq_length %>% hist()
wb$species %>% table()
wb_filter <- wb %>% filter(species == "Db", duration_flea > 100, dom_freq_length > 30)
p1 <- ggplot(wb_filter, aes(x, y, col = tag_mass))+
  geom_path(linewidth = 0.75, aes(group = c(trial)))+
  geom_point(aes(x = median_peak, y = max(y)))+
  facet_wrap(~species+bat)+
  theme_bw()+
  scale_color_viridis_c()

p1
p2 <- ggplot(wb_filter, aes(total_mass, median_peak, group = bat, col = trial))+
  geom_path(col = 1)+geom_point()+
  facet_wrap(~species+bat, scales = "free")+
  theme_bw()

p2

ggplot(wb_filter, aes(x = total_mass, y = min_peak, group = bat, col = trial))+
  geom_path(aes(group = bat), col = 1)+
  geom_point()+
  facet_wrap(~species+bat)+
  theme_bw()

ggpubr::ggarrange(p1,p2, ncol = 1, common.legend = TRUE)

ggplot(wb_filter, aes(x = median_peak*1000, y = total_mass))+
  geom_point(aes(col = tag_mass), size = 3)+
  geom_smooth(method = "lm")+
  scale_color_viridis_c()+
  xlab("Wingbeat frequency (Hz)")

# remove frequencies near the start and end of flights

ggplot(wb, aes(x = total_mass, y = median_vedba, group = bat, col = trial))+
  geom_path(aes(group = bat), col = 1)+
  geom_point()+
  #facet_wrap(~species+bat)+
  theme_bw()
ggplot(wb, aes(x = total_mass, y = median_rolling_vedba, group = bat, col = trial))+
  geom_path(aes(group = bat), col = 1)+
  geom_point()+
  #facet_wrap(~species+bat)+
  theme_bw()



