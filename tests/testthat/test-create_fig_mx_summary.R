test_that("The function correctly provides a ggplot object", {
  
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
  mydata_with_intensity_marks <- mark_intensity(
    data = mydata_with_wear_marks, 
    col_axis = "vm", 
    equation = "Sasaki et al. (2011) [Adults]",
    sed_cutpoint = 200, 
    mpa_cutpoint = 2690, 
    vpa_cutpoint = 6167, 
    age = 32,
    weight = 67,
    sex = "male"
  )
  summary_by_day <- recap_by_day(
    data = mydata_with_intensity_marks, 
    col_axis = "vm",
    age = 32, 
    weight = 67, 
    sex = "male",
    valid_wear_time_start = "07:00:00",
    valid_wear_time_end = "22:00:00",
    start_first_bin = 0,
    start_last_bin = 10000,
    bin_width = 500
  )$df_all_metrics
  recap <- average_results(data = summary_by_day, minimum_wear_time = 10, fun  = "median")
  plot <- create_fig_mx_summary(
      data = recap,
      labels = seq(2500, 12500, 2500)
  )
  
  expect_s3_class(plot, "ggplot")
  
})