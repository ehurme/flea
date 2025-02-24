# Detect heart rate from PPG data
# 2025-02-21
# Edward Hurme

# Load libraries
library(data.table)
library(seewave)
library(tuneR)
library(tidyverse)
library(plotly)

# Load data
df <- fread("../../../Downloads/hr_bats/ppg_batleg_.txt")
df <- fread("../../../Downloads/hr_427/ppg_427_Pa_batleg3.txt")
df <- fread("../../../Downloads/hr_427/ppg_427_Pa_batleg_postflight_.txt")
df <- fread("../../../Downloads/ppg_Chbu_1.txt")

names(df) <- c("time", "ppg")

plot(df$time, df$ppg, type = "l")
df$time

# Convert numeric seconds to custom minute.seconds format
data <- df %>%
  mutate(
    # Calculate minutes (integer part) and seconds (decimal part)
    minutes = floor(time / 100),
    seconds = time %% 100,  # This preserves decimal fractions


    # Optional: formatted string (MM:SS.sss)
    time_formatted = sprintf("%02d:%06.3f", minutes, seconds)
  )

plot(hms(data$time_formatted), type = "l")

# detect sampling rate
1/(df$time %>% diff() %>% median())
df$time %>% diff() %>% table()
sampling_rate = 60 # maybe adjust to what the code says

# define how many peaks you want to detect in the signal
n_peaks = 3

# what is the minimum and maximum HR signal
min_freq = 0.1 # 0.1 * 60 = 6 bpm
max_freq = 10 # 10 * 60 = 600 bpm

# convert signal to a wave
w <- Wave(left = df$ppg, samp.rate = sampling_rate, bit = 8)
plot(w)

# filter by minimum and maximum frequencies
wf <- bwfilter(w, from = min_freq, to = max_freq, f = sampling_rate, output = "Wave")
plot(wf)
spectro(wf, scale = FALSE)

# calculate peak frequencies in the signal
density <- meanspec(wf, f = sampling_rate, plot = FALSE)
peaks <- fpeaks(density, f = sampling_rate, nmax = n_peaks, plot = TRUE)

peaks

#bpm
7.9 * 60 # 474 batleg 3
2.8*60 # 168 - batleg 2
2.69*60 # 161.4 - batleg


2.46 * 60 # 147.6 batleg post flight

# Keicher et al. 2024 https://royalsocietypublishing.org/doi/10.1098/rspb.2024.0855
## bpm ranged from 200- 400 at rest
200/60
400/60
