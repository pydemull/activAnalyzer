test_that("Figure for MVPA is created", {
  
  # Preparing data
  test_fig_en1 <- create_fig_mvpa(score = 34, "en")
  test_fig_fr1 <- create_fig_mvpa(score = 34, "fr")
  test_fig_en2 <- create_fig_mvpa(score = 66, "en")
  test_fig_fr2 <- create_fig_mvpa(score = 66, "fr")
  
  # Tests
  expect_s3_class(test_fig_en1, "ggplot")
  expect_s3_class(test_fig_fr1, "ggplot")
  expect_s3_class(test_fig_en2, "ggplot")
  expect_s3_class(test_fig_fr2, "ggplot")
  
})
