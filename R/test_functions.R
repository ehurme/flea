# Load necessary libraries
library(data.table)
library(dplyr)
library(plotly)
library(zoo)

source("./R/flea_functions.R")

data <- fread("../../Downloads/02_302F.csv")
data <- fread("../../Downloads/02_302F.csv")

processed_data <- flea_preprocess(data)

p <- flea_plot(processed_data, label_high_variability = TRUE, plot_variance = FALSE)
p

