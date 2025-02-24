# function to detect calls from audiomoth audio

plot_spectro <- FALSE # plot the spectrogram - not recommended for large files
get_freq <- FALSE # get frequency and time above a certain threshold - not recommended for large files
start <- 200; end <- 222 # trim the file between these seconds
min_freq <- 50 # set minimum call frequency
max_freq <- 120 # set maximum call frequecy
min_call_duration <- 0.002
min_call_interval <- 0.01
call_threshold = 20 # amplitude threshold for signal detection (%)

directory <- "F:/Bat_acoustic_recordings_CICG/2024-02-14/AM_13_Front/Eptesicus brasiliensis/"
filenames <- list.files(directory, full.names = FALSE)
file = filenames[3]
directory <- "D:/Bat_acoustic_recordings_CICG/Dermanura bogotensis/"
file = "20250213_222301.WAV"


process_audiomoth <- function(directory, file, view_file = TRUE,
                              detect_calls = TRUE,
                              start = NULL, end = NULL,
                              min_freq = 20, max_freq = 80,
                              min_call_duration = 0.002,
                              min_call_interval = 0.01,
                              call_threshold = 20){

  require(tuneR)
  require(seewave)
  require(tidyverse)

  w <- tuneR::readWave(filename = paste0(directory, file))
  sampling_rate <- w@samp.rate

  if(amplitude_flatten){
    w <- cutw(w, f = sampling_rate,
              from = start, to = end,
              output = "Wave")
    layout(1:2)
    plot(w)
    # remove amplitude modulations
    wn <- rmam(w, f = sampling_rate, output = "Wave")
    plot(wn)
    # remove noise
    # wa <- rmnoise(w, f = sampling_rate, output = "Wave") # duration changes...
    # plot(wa)
  }

  if(view_file){
    plot(w)
  }

  if(detect_calls){


    if(!is.null(start)){
      w <- cutw(w, f = sampling_rate,
                from = start, to = end,
                output = "Wave")
    }

    wf <- bwfilter(w, from = min_freq * 1000, to =  max_freq * 1000, output = "Wave")
    # plot(wf)
    if(plot_spectro){
      # layout(1)
      spectro(wf, f = sampling_rate, scale = FALSE, ovlp = 75, fastdisp = TRUE)

    }
    if(get_freq){
      par(new=TRUE)
      dom_freq <- dfreq(wf, f=sampling_rate,
                        #clip = call_threshold/100,
                        ovlp=75,
                        threshold= call_threshold,
                        bandpass = c(min_freq * 1000,  max_freq * 1000),
                        type="l", col="red", lwd=2, plot = get_freq)
      dom_freq <- dom_freq %>% as.data.frame()
      names(dom_freq) <- c("time_s", "frequency_kHz")
      # dom_freq %>% plot(ylim = c(min_freq, max_freq))
    }

    # get first time in cluster
    call_times <- timer(wf, f = sampling_rate, threshold = call_threshold,
                        msmooth = c(500,10), plot = TRUE)

    call <- data.frame(start = call_times$s.start, duration = call_times$s)

    # get max amplitude between start and end
    # wf

    # call$duration
    call <- call[call$duration > min_call_duration,]

    call$intervals <- c(NA, call$start %>% diff)

    call <- call[call$intervals > min_call_interval,]

    call$intervals %>% hist(breaks = 2000, xlim = c(0, .5))

    # update timestamps
    if(!is.null(cut)){
      call$time <- call$start + cut[1]
    }

    return(list(sampling_rate, call, dom_freq))
  }
}

### Example
# view amplitude file to see where to adjust start and end times
process_audiomoth(directory = directory, file = file, view_file = TRUE, detect_calls = FALSE)

# get call values
process_audiomoth(directory = directory, file = file, view_file = FALSE, detect_calls = TRUE)

