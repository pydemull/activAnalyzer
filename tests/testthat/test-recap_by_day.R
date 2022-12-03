test_that("The function provides similar summary by day for appropriate metrics when using 60-s and 10-s epochs", {
  
  # Compute results with 10-s epochs
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  mydata1 <- prepare_dataset(data = file)
  mydata1_with_wear_marks <- mark_wear_time(
    dataset = mydata1, 
    TS = "TimeStamp", 
    to_epoch = 10,
    cts  = "vm",
    frame = 90, 
    allowanceFrame = 2, 
    streamFrame = 30
  )
  mydata1_with_intensity_marks <- mark_intensity(
    data = mydata1_with_wear_marks, 
    col_axis = "vm", 
    equation = "Sasaki et al. (2011) [Adults]",
    sed_cutpoint = 200, 
    mpa_cutpoint = 2690, 
    vpa_cutpoint = 6167, 
    age = 32,
    weight = 67,
    sex = "male",
  )
  recap1 <-
    recap_by_day(
    data = mydata1_with_intensity_marks, 
    age = 32, 
    weight = 67, 
    sex = "male",
  ) %>%
    dplyr::select(wear_time:vm_per_min)
  

  # Compute results with 60-s epochs
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  mydata2 <- prepare_dataset(data = file)
  mydata2_with_wear_marks <- mark_wear_time(
    dataset = mydata2, 
    TS = "TimeStamp", 
    to_epoch = 60,
    cts  = "vm",
    frame = 90, 
    allowanceFrame = 2, 
    streamFrame = 30
  )
  mydata2_with_intensity_marks <- mark_intensity(
    data = mydata2_with_wear_marks, 
    col_axis = "vm", 
    equation = "Sasaki et al. (2011) [Adults]",
    sed_cutpoint = 200, 
    mpa_cutpoint = 2690, 
    vpa_cutpoint = 6167, 
    age = 32,
    weight = 67,
    sex = "male",
  )
  recap2 <-
    recap_by_day(
      data = mydata2_with_intensity_marks, 
      age = 32, 
      weight = 67, 
      sex = "male",
    ) %>%
    dplyr::select(wear_time: vm_per_min)
    
  # Test
  expect_equal(recap1, recap2)
  
})
