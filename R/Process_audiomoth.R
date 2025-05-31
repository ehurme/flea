# function to detect Phyllostomid's calls  from audio
# 2025-05-26
# Authors: Edward and Juan David

plot_spectro <- FALSE
get_calls <- FALSE
start <- 0.5; end <- 0.8
min_freq <- 15; max_freq <- 100
min_peak_freq <- 45; max_peak_freq <- 65
min_call_duration <- 0.0001
max_call_duration <- 0.002
call_threshold <- 5

directory <- "D:/Music/Bat_recordings/2025-02-20/Dermanura bogotensis/AM_13_front/"
filenames <- list.files(directory,full.names = FALSE)
file <- filenames[2]

file

process_audiomoth <- function(directory, file,
                              get_calls = FALSE,
                              plot_spectro = FALSE,
                              view_file = FALSE,
                              start, end,
                              min_freq, max_freq,
                              min_peak_freq_start, max_peak_freq_start,
                              min_peak_freq_end, max_peak_freq_end,
                              min_call_duration, max_call_duration,
                              min_call_interval = 0.01,
                              call_threshold)
{
  require(tuneR)
  require(seewave)
  require(tidyverse)
  
  w <- tuneR::readWave(filename = paste0(directory,file))
  
  if(view_file){plot (w)}
  
  if(get_calls) {
    
    sampling_rate <- w@samp.rate 
  
    if(!is.null(start)) {
      
      w <- cutw(w, f = sampling_rate, from = start, to = end, output = "Wave")
    }
    
    wf <- bwfilter(w, from = min_freq * 1000, to = max_freq * 1000, n=4, 
                  output = "Wave")
  
    
  #Get potential calls  
    
    calls <- timer(wf,f = sampling_rate, threshold = call_threshold, msmooth = 
                     c(70,5), plot = TRUE)
    
    calls_df <- data.frame(start = calls$s.start, end= calls$s.end, duration = calls$s)
    
    calls_df <- filter(calls_df, duration > min_call_duration & duration <max_call_duration)
    
    #Get peak frequencies at the start and end of each call
    
    peak_freq_start <- dfreq(wf, f = sampling_rate, threshold = call_threshold, 
                       plot = FALSE, at = calls_df$start)
    peak_freq_end <- dfreq(wf, f = sampling_rate, threshold = call_threshold, 
                          plot = FALSE, at = calls_df$end)
    
    peak_freq_s_df <- as.data.frame(peak_freq_start)
    
    peak_freq_s_df <- peak_freq_s_df[-c(1, nrow(peak_freq_start)),]
    
    peak_freq_e_df <- as.data.frame(peak_freq_end)
    
    peak_freq_e_df <- peak_freq_e_df[-c(1, nrow(peak_freq_end)),]
    
    calls_df$peak_freq_start <- peak_freq_s_df$y
    calls_df$peak_freq_end <- peak_freq_e_df$y
    
    #Filter potential calls by peak frequencies
    
    calls_df <- calls_df[calls_df$peak_freq_start > min_peak_freq_start & calls_df$peak_freq_start < max_peak_freq_start,]
   ## calls_df <- calls_df[calls_df$peak_freq_end > min_peak_freq_end & calls_df$peak_freq_end < max_peak_freq_end,]
    
    #Compute intervals
    
    calls_df$interval <- c(NA, calls_df$start %>% diff)
    
    calls_df <- calls_df[is.na(calls_df$interval) | calls_df$interval > min_call_interval,]
    
    calls_df <- calls_df[!is.na(calls_df$start), ]
    
   hist( calls_df$interval, breaks = 2000, xlim = c(0, .5)) 
    
   
    
    # update timestamps
    if(!is.null(start)){
      calls_df$time <- calls_df$start + start
    }
    if(get_calls == TRUE){
      
      return(calls_df)
    }
    
  } #Close get calls
  
} #Close function

calls <- process_audiomoth(directory = directory, file = file, get_calls = T,
                           view_file = FALSE, start = NULL, end = NULL, 
                           min_freq = 15, max_freq = 100, min_peak_freq_start = 45,
                           max_peak_freq_start = 80, min_peak_freq_end = 45, 
                           max_peak_freq_end = 55,call_threshold = 2,
                           min_call_duration = 0.0003, max_call_duration = 0.004)


calls
