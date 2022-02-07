test_that("Figure for SED is created", {
  
  # Preparing data
  test_fig_en <- create_fig_sed(score = 500, "en")
  test_fig_fr <- create_fig_sed(score = 500, "fr")
  
  # Tests
  expect_s3_class(test_fig_en, "ggplot")
  expect_s3_class(test_fig_fr, "ggplot")
  
})
