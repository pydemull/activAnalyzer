test_that("Figure for SED is created", {
  
  # Preparing data
  test_fig_en1 <- create_fig_sed(score = 500, "en")
  test_fig_fr1 <- create_fig_sed(score = 500, "fr")
  test_fig_en2 <- create_fig_sed(score = 100, "en")
  test_fig_fr2 <- create_fig_sed(score = 100, "fr")
  test_fig_en3 <- create_fig_sed(score = 800, "en")
  test_fig_fr3 <- create_fig_sed(score = 800, "fr")
  
  # Tests
  expect_s3_class(test_fig_en1, "ggplot")
  expect_s3_class(test_fig_fr1, "ggplot")
  expect_s3_class(test_fig_en2, "ggplot")
  expect_s3_class(test_fig_fr2, "ggplot")
  expect_s3_class(test_fig_en3, "ggplot")
  expect_s3_class(test_fig_fr3, "ggplot")
  
})
