library(colorspace)
rgb <- readRGB("pastel.rgb")
hsv <- readRGB("pastel.rgb", "HSV")


rgb <- na.omit(flea_data$data[,7:9])/300
rgb


x <- sRGB(rgb[,1], rgb[,2], rgb[,3])
plot(x)
polarLAB <- as(x, "polarLAB")
plot(polarLAB)

hsv <- as(x, "HSV")
plot(hsv)


specplot(rainbow(100), rgb = TRUE)
