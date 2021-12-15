test_that("a plot with data marked for non wear time is obtained", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  g <- 
    prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp") %>%
    mark_wear_time() %>% 
    plot_data()
  
  # Testing that g is a ggplot object
  expect_s3_class(g, "ggplot") 
  
})
