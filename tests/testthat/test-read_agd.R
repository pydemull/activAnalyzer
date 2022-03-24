test_that("The function provides a tibble", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  data <- read_agd(file)
  expect_s3_class(data, "tbl_df")
})
