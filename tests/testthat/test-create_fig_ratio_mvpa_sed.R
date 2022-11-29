test_that("Figure for MVPA/SED ratio is created", {
  
  # Preparing data
  test_fig_en1 <- create_fig_ratio_mvpa_sed(score = 0.04, "en")
  test_fig_fr1 <- create_fig_ratio_mvpa_sed(0.04, "fr")
  test_fig_en2 <- create_fig_ratio_mvpa_sed(score = 0.26, "en")
  test_fig_fr2 <- create_fig_ratio_mvpa_sed(0.26, "fr")
  
  # Tests
  expect_s3_class(test_fig_en1, "ggplot")
  expect_s3_class(test_fig_fr1, "ggplot")
  expect_s3_class(test_fig_en2, "ggplot")
  expect_s3_class(test_fig_fr2, "ggplot")
  
})
