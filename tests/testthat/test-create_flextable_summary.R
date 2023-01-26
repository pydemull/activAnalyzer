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
  results_summary_means <- average_results(data = summary_by_day$df_all_metrics, minimum_wear_time = 10, fun = "mean")
  results_summary_medians <- average_results(data = summary_by_day$df_all_metrics, minimum_wear_time = 10, fun = "median")
  
  # Creating flextable objects
  test_object_1 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "en", metrics = "all")
  test_object_2 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "en", metrics = "volume")
  test_object_3 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "en", metrics = "step_acc")
  test_object_4 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "en", metrics = "int_distri")
  
  test_object_5 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "fr", metrics = "all")
  test_object_6 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "fr", metrics = "volume")
  test_object_7 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "fr", metrics = "step_acc")
  test_object_8 <- create_flextable_summary(results_summary_means, results_summary_medians, language = "fr", metrics = "int_distri")
  
  
  # Tests
  expect_s3_class(test_object_1, "flextable")
  expect_s3_class(test_object_2, "flextable")
  expect_s3_class(test_object_3, "flextable")
  expect_s3_class(test_object_4, "flextable")
  expect_s3_class(test_object_5, "flextable")
  expect_s3_class(test_object_6, "flextable")
  expect_s3_class(test_object_7, "flextable")
  expect_s3_class(test_object_8, "flextable")
  
})
