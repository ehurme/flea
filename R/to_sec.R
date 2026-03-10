# helper to convert "HH:MM:SS.xxx" to seconds
to_sec <- function(x) {
  parts <- unlist(strsplit(x, ":"))
  h <- as.numeric(parts[1])
  m <- as.numeric(parts[2])

  # seconds may contain decimals
  s <- as.numeric(parts[3])

  h*3600 + m*60 + s
}

# # INPUTS
# phone_time  <- "13:58:55.82"
# video_time  <- "00:00:10.727"
# act_time    <- "13:58:43.570"
#
# # Convert times
# T_phone  <- to_sec(phone_time)
# t_video  <- to_sec(video_time)
# T_act    <- to_sec(act_time)
#
# # Compute tag start relative to video start
# t_rel <- T_act - T_phone + t_video
# t_rel
