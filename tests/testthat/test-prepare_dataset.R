
test_that("Dataset is properly prepared", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  my_data <- prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp")
  
  expect_s3_class(my_data, "data.frame")
  expect_condition(my_data[1, col_time_stamp] < my_data[2, col_time_stamp])
  expect_condition((as_hms( my_data[2, col_time_stamp]) - as_hms(my_data[1, col_time_stamp])) == 60)
})
