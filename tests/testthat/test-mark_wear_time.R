test_that("Total wear time is correctly computed", {
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
  
  total_wear_time <- sum(mydata_with_wear_marks$wearing_count)
  
  expect_equal(total_wear_time, 3839)
})
