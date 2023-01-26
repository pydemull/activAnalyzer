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

test_that("A plot with data marked for intensity is obtained using zoom arguments", {
  
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
    plot_data_with_intensity(zoom_from = "12:00:00", zoom_to = "13:30:30")
  
  expect_s3_class(g, "ggplot") 
  
})


test_that("The function works with customized variable names", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  g <- 
    prepare_dataset(data = file) %>%
    dplyr::rename(TIMESTAMP = "TimeStamp") %>%
    mark_wear_time(TS = "TIMESTAMP") %>% 
    dplyr::rename(TIME = "time", NON_WEARING_COUNT = "non_wearing_count", WEARING_COUNT = "wearing_count") %>%
    mark_intensity(col_time = "TIME", col_nonwear  = "NON_WEARING_COUNT", col_wear = "WEARING_COUNT") %>%
    plot_data_with_intensity(col_time = "TIME", col_nonwear  = "NON_WEARING_COUNT", col_wear = "WEARING_COUNT")
  
  # Testing that g is a ggplot object
  expect_s3_class(g, "ggplot") 
  
})  

