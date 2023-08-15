test_that("The function runs without error", {
  results <- do_all_analyses()
  expect_s3_class(results, "data.frame")
})


# The section below provides the results of performance tests for doing all analyses at once.
# Code used for the tests: microbenchmark::microbenchmark(do_all_analyses(), times = 30)

# Test with version 2.0.1.9000 (15/08/2023)
## Unit: seconds
## expr                   min       lq     mean   median       uq      max neval
## do_all_analyses() 2.874977 2.968439 3.063673 3.019497 3.170654 3.571439    30

