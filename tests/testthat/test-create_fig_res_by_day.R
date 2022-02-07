test_that("Create a figure with metrics for each day", {
  
  # Preparing data
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  mydata <- prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp")
  mydata_with_wear_marks <- mark_wear_time(
    dataset = mydata, 
    TS = "timestamp", 
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
    col_steps = "steps"
  )
  summary_by_day <- recap_by_day(
    data = mydata_with_intensity_marks, 
    age = 32, 
    weight = 67, 
    sex = "male",
    valid_wear_time_start = "07:00:00",
    valid_wear_time_end = "22:00:00"
  )

  
  # Creating flextable objects
  test_object_en <- create_fig_res_by_day(summary_by_day, 
                        minimum_wear_time_for_analysis = 10, 
                        start_day_analysis = "00:00:00", 
                        end_day_analysis = "23:59:00", 
                        language = "en")
  
  test_object_fr <- create_fig_res_by_day(summary_by_day, 
                                          minimum_wear_time_for_analysis = 10, 
                                          start_day_analysis = "00:00:00", 
                                          end_day_analysis = "23:59:00", 
                                          language = "fr")
  
  # Tests
  expect_s3_class(test_object_en, "ggplot")
  expect_s3_class(test_object_fr, "ggplot")

  })
  