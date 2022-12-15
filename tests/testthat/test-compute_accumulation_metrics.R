test_that("The function provides numeric and graphic objects", {
  
  # Setting test results
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
    sex = "male",
  )
  list_test <-
    compute_accumulation_metrics(
    data = mydata_with_intensity_marks,
    behaviour = "sed",
    dates = c("2021-04-07", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11"),
    valid_wear_time_start = "00:00:00",
    valid_wear_time_end = "23:59:59",
    zoom_from = "00:00:00",
    zoom_to = "23:59:59"
  )
  
  # Setting reference dataframe
  df_actual <- data.frame(
    mean_breaks = 68.6,
    alpha = 2.18,
    MBD = 1.8,
    UBD = 7.56,
    gini = 0.57
  )
  
  # Tests
  expect_s3_class(list_test$metrics, "data.frame")
  expect_s3_class(list_test$p_breaks, "ggplot")
  expect_s3_class(list_test$p_alpha, "ggplot")
  expect_s3_class(list_test$p_MBD, "ggplot")
  expect_s3_class(list_test$p_UBD, "ggplot")
  expect_s3_class(list_test$p_gini, "ggplot")
  expect_equal(list_test$metrics, df_actual)
  
})

test_that("The function provides numeric and graphic objects even with customized variable
          names", {
  
# Setting test results
file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
mydata <- prepare_dataset(data = file) %>% dplyr::rename(TIMESTAMP = "TimeStamp")
mydata_with_wear_marks <- mark_wear_time(
  dataset = mydata, 
  TS = "TIMESTAMP", 
  to_epoch = 60,
  cts  = "vm",
  frame = 90, 
  allowanceFrame = 2, 
  streamFrame = 30
)  %>% dplyr::rename(TIME = "time", NON_WEARING_COUNT = "non_wearing_count", WEARING_COUNT = "wearing_count")
mydata_with_intensity_marks <- mark_intensity(
  data = mydata_with_wear_marks, 
  col_axis = "vm", 
  col_time = "TIME", col_nonwear  = "NON_WEARING_COUNT", col_wear = "WEARING_COUNT",
  equation = "Sasaki et al. (2011) [Adults]",
  sed_cutpoint = 200, 
  mpa_cutpoint = 2690, 
  vpa_cutpoint = 6167, 
  age = 32,
  weight = 67,
  sex = "male",
) %>%
  dplyr::mutate(
    kcal = 2 # set dummy data for easier calculations
  )

list_test <-
  compute_accumulation_metrics(
    data = mydata_with_intensity_marks,
    col_time = "TIME",
    behaviour = "sed",
    dates = c("2021-04-07", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11"),
    valid_wear_time_start = "00:00:00",
    valid_wear_time_end = "23:59:59",
    zoom_from = "00:00:00",
    zoom_to = "23:59:59"
  )

# Setting reference dataframe
df_actual <- data.frame(
  mean_breaks = 68.6,
  alpha = 2.18,
  MBD = 1.8,
  UBD = 7.56,
  gini = 0.57
)
  
  # Tests
  expect_s3_class(list_test$metrics, "data.frame")
  expect_s3_class(list_test$p_breaks, "ggplot")
  expect_s3_class(list_test$p_alpha, "ggplot")
  expect_s3_class(list_test$p_MBD, "ggplot")
  expect_s3_class(list_test$p_UBD, "ggplot")
  expect_s3_class(list_test$p_gini, "ggplot")
  expect_equal(list_test$metrics, df_actual)
  
})
