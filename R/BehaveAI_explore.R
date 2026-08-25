# analyze BehaveAI tracking output: filter outliers, extract wingbeat frequency
library(tidyverse)
library(data.table)
library(zoo)

# ---- params ----
fps          <- 60     # video framerate, set actual
min_conf     <- 0.5    # min primary_motion_conf to keep
max_speed_px_s <- 3000 # px/sec, tune from quantile(speed_per_sec) below
max_gap      <- 5      # max frame gap to trust a speed calc
roll_win     <- 15     # rolling window (frames) for detrending width signal

# ---- load ----
df <- fread("D:/BehaveAI-main/projects/bat/output/F2_20250521_C0009_tracking.csv")
df <- fread("D:/BehaveAI-main/projects/bat/output/F2_20250521_C0011_tracking.csv")
setorder(df, frame)   # single bat per video: id resets are track breaks, not separate animals, order by frame alone

# ---- derived measures ----
df[, `:=`(
  dx = x - shift(x),
  dy = y - shift(y),
  dframe = frame - shift(frame)
)]
df[, dist := sqrt(dx^2 + dy^2)]
df[, speed := dist / dframe]
df[, speed_per_sec := speed * fps]

df[, size := width * height]
df[, aspect := width / height]
df[, size_jump := abs(size - shift(size)) / shift(size)]

# inspect before picking thresholds
hist(df$speed_per_sec, breaks = 100)
quantile(df$speed_per_sec, probs = c(.5, .9, .95, .99, .999), na.rm = TRUE)
hist(df$aspect, breaks = 100)

# ---- outlier filter ----
df[, outlier :=
     (!is.na(speed_per_sec) & speed_per_sec > max_speed_px_s) |
     primary_motion_conf < min_conf |
     (!is.na(dframe) & dframe > max_gap)
]

df_clean <- df[outlier == FALSE]

ggplot(df, aes(x, y, col = outlier)) +
  geom_point(alpha = .4) +
  coord_equal()

# plot a single frame bounding box and centroid


# ---- wingbeat frequency (FFT on width oscillation) ----
# fill frame gaps so signal is continuous, but cap interpolation at max_gap
# frame is now every integer (from frame_range), so frame diff is always 1 --
# real tracking gaps must be tracked via NA in width, not via frame diff.
frame_range <- data.table(frame = min(df$frame):max(df$frame))
dt <- merge(frame_range, df_clean, by = "frame", all.x = TRUE)
dt[, width_interp := na.approx(size, x = frame, na.rm = FALSE, maxgap = max_gap)]

# detrend: remove slow drift from flight-path distance changes, keep wingbeat oscillation
dt[, width_detrend := width_interp - rollmean(width_interp, roll_win, fill = NA, align = "center")]
dt$width_detrend[5500:5700] %>% plot(type = "l")

wingbeat_freq <- function(segment_frames, dt) {
  seg <- dt[frame %in% segment_frames & !is.na(width_detrend), width_detrend]
  if (length(seg) < roll_win * 2) return(NA_real_)
  spec <- spectrum(seg, plot = FALSE)
  spec$freq[which.max(spec$spec)] * fps
}

# example: one clean stretch
wingbeat_freq(5500:5700, dt)

# segment = contiguous run of real (non-interpolated-over-gap) data
# breaks whenever width_interp is NA (gap too big to trust) or track missing
dt[, segment := rleid(!is.na(width_interp))]
dt[is.na(width_interp), segment := NA]  # NA runs aren't real segments

wingbeat_by_segment <- dt[!is.na(segment), .(freq_hz = wingbeat_freq(frame, dt), n = .N), by = segment]
wingbeat_by_segment[n > roll_win * 2]

ggplot(wingbeat_by_segment, aes(freq_hz, n, col = segment)) + geom_point()

# plot a single frame bounding box and centroid
frame_i <- 1240:1250 #80:140
row <- df_clean[frame_i,]

ggplot(row) +
  geom_path(aes(x, y), col = "gray", size = 1) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, col = frame),
            fill = NA) +
  geom_point(aes(x, y, col = frame), size = 2) +
  coord_equal()+
  xlim(c(min(df_clean$x)-100, max(df_clean$x)+100))+
  ylim(c(min(df_clean$y)-100, max(df_clean$y)+100))

plot(row$frame, row$speed_per_sec, type = "o")
