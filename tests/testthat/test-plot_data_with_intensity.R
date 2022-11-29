test_that("A plot with data marked for intensity is obtained", {
  
  # Test 1
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  g <- 
    prepare_dataset(data = file) %>%
    mark_wear_time() %>% 
    mark_intensity() %>%
    plot_data_with_intensity()
  
  expect_s3_class(g, "ggplot") 
  
  # Test 2
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  g <- 
    prepare_dataset(data = file) %>%
    mark_wear_time() %>% 
    mark_intensity(dates = c("2021-04-09", "2021-04-10")) %>%
    plot_data_with_intensity()
  
  expect_s3_class(g, "ggplot") 
  
})
  

