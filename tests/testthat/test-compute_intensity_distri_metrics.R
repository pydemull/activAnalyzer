test_that("The function returns all the objects as expected whatever the epoch length and the axis considered", {
  
  # Tests with 60-epochs and VM
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
    )
    res <- compute_intensity_distri_metrics(
      data = mydata_with_intensity_marks,
      col_axis = "vm",
      valid_wear_time_start = "00:00:00",
      valid_wear_time_end = "23:59:59",
      start_first_bin = 0,
      start_last_bin = 10000,
      bin_width = 500
    )
  
    expect_s3_class(res$metrics, "data.frame")
    expect_s3_class(res$p_band, "ggplot")
    expect_s3_class(res$p_log, "ggplot")
    
    # Tests with 15-epochs and axis1
      file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
      mydata <- prepare_dataset(data = file)
      mydata_with_wear_marks <- mark_wear_time(
        dataset = mydata, 
        TS = "TimeStamp", 
        to_epoch = 10,
        cts  = "vm",
        frame = 90, 
        allowanceFrame = 2, 
        streamFrame = 30
      )
      mydata_with_intensity_marks <- mark_intensity(
        data = mydata_with_wear_marks, 
        col_axis = "vm", 
      )
      res <- compute_intensity_distri_metrics(
        data = mydata_with_intensity_marks,
        col_axis = "axis1",
        valid_wear_time_start = "00:00:00",
        valid_wear_time_end = "23:59:59",
        start_first_bin = 0,
        start_last_bin = 10000/6,
        bin_width = 500/6
      )
      
      expect_s3_class(res$metrics, "data.frame")
      expect_s3_class(res$p_band, "ggplot")
      expect_s3_class(res$p_log, "ggplot")
    
    
})
