# function to detect Phyllostomid's calls  from audio
# 2025-05-26
# Authors: Edward and Juan David


# new function to extract calls
# cut wave into small segments to inspect for calls

process_audiomoth_segment <- function(directory, file,
                              get_calls = TRUE,
                              plot_spectro = FALSE,
                              view_file = FALSE,
                              start = NULL,
                              end = NULL,
                              pulse_buffer = 0.005,
                              min_freq = 50,
                              max_freq = 100,
                              amp_threshold = 0.15
                              )
{

  require(tuneR)
  require(seewave)
  require(tidyverse)

  w <- tuneR::readWave(filename = paste0(directory,file))

  if(view_file){plot (w)}

  if(get_calls) {

  sampling_rate <- w@samp.rate

  if(!is.null(start)) {

    w <- cutw(w, f = sampling_rate,
              from = start,
              to = end, output = "Wave")
  }

  wf <- bwfilter(w,
                 from = min_freq * 1000,
                 to = max_freq * 1000,
                 n=4,
                 output = "Wave")
  duration <- duration(wf)

  call_parameters <- data.frame()
  i = 8
  for(i in 0:(floor(duration))){
    start_cut <- i
    end_cut <- i + 1
    if(end_cut > duration) end_cut <- duration

    if(end_cut - start_cut < 0.5) next # skip if the segment is too short

    pulse <- cutw(wf, f = sampling_rate,
                  from = start_cut,
                  to = end_cut, output = "Wave")

    calls <- {}
    try({calls <- timer(pulse, f = sampling_rate,
                        threshold = call_threshold, power = 3,
                        msmooth = c(70,5), plot = FALSE)
    })
    length(calls$s.start)
    if(length(calls) > 0){
      calls_df <- data.frame(start = calls$s.start, end= calls$s.end, duration = calls$s)
      calls_df <- filter(calls_df, duration > min_call_duration & duration < max_call_duration)

      echo <- data.frame()
      j = 1
      if(nrow(calls_df) > 0){
        for(j in 1:nrow(calls_df)){
          start_time <- calls_df$start[j]-pulse_buffer
          end_time <- calls_df$end[j]+pulse_buffer

          if(start_time < 0) start_time <- 0
          if(end_time > 1) end_time <- 1

          call <- cutw(pulse, f = sampling_rate,
                       from = start_time, to = end_time, output = "Wave")

          note <- acoustat(call, f = sampling_rate, ovlp = 90, wl = 256, plot = FALSE)
          notes <- as.data.frame(note[3:6])
          notes$peak_amp <- note$time.contour[10:(nrow(note$time.contour)-10),2] %>% max

          # adjust call times back to orginal file times
          notes$start_time <- notes$time.P1 + pulse_buffer + calls_df$start[j] + i
          notes$end_time <- notes$time.P2 + pulse_buffer + calls_df$start[j] + i
          notes$duration <- notes$end_time - notes$start_time

          density <- meanspec(call, f = sampling_rate, plot = FALSE)
          peaks <- fpeaks(density, f = sampling_rate, nmax = n_peaks, plot = FALSE)
          notes$peak_freq <- peaks[which.max(peaks[,2]), 1]

          echo <- rbind(echo, notes)

          # plot(call)
          # spectro(call, f = sampling_rate, ovlp = 50, wl = 256, scale = FALSE, grid = FALSE)
          # par(new = TRUE)
          # df <- dfreq(call, f = sampling_rate, ovlp = 50, wl = 256)
          # #
        }
      }
      call_parameters <- rbind(call_parameters, echo)
    }
  }

  # filter out low amplitude calls
  call_parameters_highamp <- call_parameters %>% filter(peak_amp > amp_threshold)
  if(nrow(call_parameters_highamp) > 0){
    hist(call_parameters_highamp$duration, breaks = 10, xlim = c(0, 0.015))
    abline(v = c(min_call_duration, max_call_duration), col = "red")
    hist(call_parameters_highamp$peak_amp, breaks = 100)
    abline(v =0.15, col = "red")

    # Calculate inter-pulse intervals (IPI)
    call_parameters_highamp <- call_parameters_highamp %>%
      mutate(ipi = c(NA, start_time %>% diff))

    # # Save the call parameters to a CSV file
    # write.csv(call_parameters_highamp,
    #           paste0(directory, "call_parameters_", file, ".csv"),
    #           row.names = FALSE)
  }
  return(list(call_parameters, call_parameters_highamp))
  }
}

directory <- "E:/Bat_acoustic_recordings_CICG/2025-02-20/Dermanura bogotensis/AM_9_back/"
# "D:/Music/Bat_recordings/2025-02-20/Dermanura bogotensis/AM_13_front/"
filenames <- list.files(directory,full.names = FALSE)
file <- filenames[5]

file
call_parameters <- process_audiomoth_segment(directory, file,
                                              get_calls = TRUE,
                                              plot_spectro = FALSE,
                                              view_file = FALSE,
                                              start = NULL,
                                              end = NULL,
                                              pulse_buffer = 0.005,
                                              min_freq = 50,
                                              max_freq = 100,
                                              amp_threshold = 0.05)

call_parameters[[1]] %>% summary()
plot(call_parameters[[1]]$duration, call_parameters[[1]]$peak_amp)
ggplot(call_parameters[[1]], aes(duration, peak_freq, size = peak_amp))+
  geom_point()
ggplot(call_parameters[[2]], aes(duration, peak_freq, size = peak_amp))+
  geom_point()


