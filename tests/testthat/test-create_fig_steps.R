test_that("Figure for steps is created", {
  
  # Preparing data
  test_fig_en1 <- create_fig_steps(score = 9000, "en")
  test_fig_fr1 <- create_fig_steps(score = 9000, "fr")
  test_fig_en2 <- create_fig_steps(score = 20000, "en")
  test_fig_fr2 <- create_fig_steps(score = 20000, "fr")
  
  # Tests
  expect_s3_class(test_fig_en1, "ggplot")
  expect_s3_class(test_fig_fr1, "ggplot")
  expect_s3_class(test_fig_en2, "ggplot")
  expect_s3_class(test_fig_fr2, "ggplot")
  
})
