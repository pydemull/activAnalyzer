test_that("a plot with data marked for intensity is obtained", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  g <- 
    prepare_dataset(data = file) %>%
    mark_wear_time() %>% 
    mark_intensity() %>%
    plot_data_with_intensity()
  
  # Testing that g is a ggplot object
  expect_s3_class(g, "ggplot") 
  
})
  

