
test_that("Dataset is properly prepared", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  my_data <- prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp")
  
  # Testing the class of the dataset
    expect_s3_class(my_data, "data.frame")
  
  # Testing the order of the lines and the length of the epochs of the dataset
    expect_condition(my_data[1, col_time_stamp] < my_data[2, col_time_stamp])
    expect_condition((as_hms(my_data[2, col_time_stamp]) - as_hms(my_data[1, col_time_stamp])) == 60)
  
  # Testing if activity data are correctly collapsed (sum of the counts over the defined epoch length)
    ref <-
      file %>% 
      actigraph.sleepr::read_agd() %>%
      dplyr::mutate(vm = round(sqrt(axis1^2 + axis2^2 + axis3^2), 2),
                    col_time_stamp = as.character(timestamp),
                    timeStamp2 = col_time_stamp)%>%
      tidyr::separate("timeStamp2", c("date", "time"), sep = " ") %>%
      dplyr::filter(date == "2021-04-07") %>%
      dplyr::filter(time >= "15:49:00" & time < "15:50:00") %>%
      dplyr::summarise(axis1 = sum(axis1), axis2 = sum(axis2), axis3 = sum(axis3)) %>%
      as.data.frame()
    
    test <-
      my_data %>% 
      dplyr::filter(date == "2021-04-07") %>%
      dplyr::filter(as.character(time) >= "15:49:00" & as.character(time) < "15:50:00") %>%
      dplyr::select(axis1, axis2, axis3)
    
    expect_condition(all.equals(test, ref) == TRUE)
    
    
})
