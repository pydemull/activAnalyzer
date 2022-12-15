test_that("A plot with data marked for non wear time is obtained", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  g <- 
    prepare_dataset(data = file) %>%
    mark_wear_time() %>% 
    plot_data()
  
  # Testing that g is a ggplot object
  expect_s3_class(g, "ggplot") 
  
})

test_that("The function works with customized variable names", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  g <- 
    prepare_dataset(data = file) %>%
    dplyr::rename(TIMESTAMP = "TimeStamp") %>%
    mark_wear_time(TS = "TIMESTAMP") %>% 
    dplyr::rename(TIME = "time", NON_WEARING_COUNT = "non_wearing_count", WEARING_COUNT = "wearing_count") %>%
    plot_data(col_time = "TIME", col_nonwear  = "NON_WEARING_COUNT", col_wear = "WEARING_COUNT")
  
  # Testing that g is a ggplot object
  expect_s3_class(g, "ggplot") 
  
})
