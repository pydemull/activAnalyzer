test_that("Figure for steps is created", {
  
  # Preparing data
  test_fig_en <- create_fig_steps(score = 9000, "en")
  test_fig_fr <- create_fig_steps(score = 2000, "fr")
  
  # Tests
  expect_s3_class(test_fig_en, "ggplot")
  expect_s3_class(test_fig_fr, "ggplot")
  
})
