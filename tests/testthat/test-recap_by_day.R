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
  recap2 <-
    recap_by_day(
      data = mydata_with_intensity_marks, 
      age = 32, 
      weight = 67, 
      sex = "male",
    ) %>%
    dplyr::select(wear_time: vm_per_min)
    
  # Test
  expect_equal(recap1, recap2)
  
})


test_that("Kilocalories are correctly summed when changing the period of the day considered for analysis", {
  
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
  ) %>%
  dplyr::mutate(
    kcal = 2 # set dummy data for easier calculations
  )
  recap1 <-
    recap_by_day(
      data = mydata_with_intensity_marks %>% dplyr::filter(days == 1), 
      valid_wear_time_start = "10:00:01",
      valid_wear_time_end = "20:00:00",
      age = 32, 
      weight = 67, 
      sex = "male",
    ) %>%
    dplyr::select(total_kcal, pal) %>%
    as.data.frame()
  
  recap2 <-
    data.frame(
      total_kcal = 10*60*2,
      pal = (10*60*2) *(10/9) / (compute_bmr(age = 32, sex = "male", weight = 67) / (24*60) * 10*60)
    )
  
  expect_equal(round(recap1,1), round(recap2,1))
  
})


