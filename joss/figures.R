# Packages
library(activAnalyzer)
library(ggplot2)
library(ragg)
library(gridExtra)

# Nonwear time detection ---------------------------------------------------------------------
file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
mydata <- prepare_dataset(data = file)
mydata_with_wear_marks <- mark_wear_time(
  dataset = mydata, 
  TS = "TimeStamp", 
  to_epoch = 60,
  cts  = "vm",
  frame = 90, 
  allowanceFrame = 2, 
  streamFrame = 30
)
g1 <-
  plot_data(
  data = mydata_with_wear_marks, 
  metric = "vm", 
  col_time = "time", 
  col_nonwear = "non_wearing_count", 
  col_wear = "wearing_count"
)

agg_png("joss/nonwear.png", width = 15, height = 7, unit = "cm",  scaling = 0.6, res = 300)
g1
dev.off()

# Intensity analysis -------------------------------------------------------------------------
mydata_with_intensity_marks <- mark_intensity(
  data = mydata_with_wear_marks, 
  col_axis = "vm", 
  equation = "Sasaki et al. (2011) [Adults]",
  sed_cutpoint = 200, 
  mpa_cutpoint = 2690, 
  vpa_cutpoint = 6167, 
  age = 32,
  weight = 67,
  sex = "male",
)
g2 <- plot_data_with_intensity(
  data = mydata_with_intensity_marks,
  metric = "vm",
  valid_wear_time_start = "00:00:00",
  valid_wear_time_end = "23:59:00"
)

agg_png("joss/intensity.png", width = 15, height = 7, unit = "cm",  scaling = 0.6, res = 300)
g2
dev.off()

# Table recap by day ------------------------------------------------------------------------
results <-
  recap_by_day(
  data = mydata_with_intensity_marks, 
  age = 32, 
  weight = 67, 
  sex = "male",
  valid_wear_time_start = "07:00:00",
  valid_wear_time_end = "22:00:00"
)

agg_png("joss/results_days.png", width = 15, height = 3, unit = "cm",  scaling = 0.6, res = 300)
grid.table(results)
dev.off()

