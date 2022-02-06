test_that("Figure for MVPA/SED ratio is created", {
  
  # Preparing data
  test_fig_en <- create_fig_ratio_mvpa_sed(score = 0.04, "en")
  test_fig_fr <- create_fig_ratio_mvpa_sed(0.04, "fr")
  
  # Tests
  expect_s3_class(test_fig_en, "ggplot")
  expect_s3_class(test_fig_fr, "ggplot")
  
})
