test_that("generate a flextable object", {
  
  # Preparing data
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  mydata <- prepare_dataset(data = file)
  mydata_with_wear_marks <- mark_wear_time(
    dataset = mydata, 
    TS = "TimeStamp", 
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
    sex = "male",
  )
  summary_by_day <- recap_by_day(
    data = mydata_with_intensity_marks, 
    age = 32, 
    weight = 67, 
    sex = "male",
    valid_wear_time_start = "07:00:00",
    valid_wear_time_end = "22:00:00"
  )
  results_summary_means <- average_results(data = summary_by_day, minimum_wear_time = 10, fun = "mean")
  results_summary_medians <- average_results(data = summary_by_day, minimum_wear_time = 10, fun = "median")
  
  # Creating flextable objects
  test_object_en <- create_flextable_summary(results_summary_means, results_summary_medians, "en")
  test_object_fr <- create_flextable_summary(results_summary_means, results_summary_medians, "fr")
  
  # Tests
  expect_s3_class(test_object_en, "flextable")
  expect_s3_class(test_object_fr, "flextable")
  
})
