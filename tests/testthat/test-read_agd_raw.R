test_that("The function provides a list with the correct number of elements", {
  file <- system.file("extdata", "acc.agd",
                      package = "activAnalyzer"
  )
  
  list <- read_agd_raw(file)
  
  expect_equal(length(list), 6)
})
