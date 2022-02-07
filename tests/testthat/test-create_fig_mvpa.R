test_that("Figure for MVPA is created", {
  
  # Preparing data
  test_fig_en <- create_fig_mvpa(score = 34, "en")
  test_fig_fr <- create_fig_mvpa(score = 34, "fr")
  
  # Tests
  expect_s3_class(test_fig_en, "ggplot")
  expect_s3_class(test_fig_fr, "ggplot")
  
})
