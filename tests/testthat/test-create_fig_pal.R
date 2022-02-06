test_that("Figure for PAL is created", {
  
  # Preparing data
    test_fig_en <- create_fig_pal(score = 1.8, "en")
    test_fig_fr <- create_fig_pal(1.8, "fr")
    
  # Tests
    expect_s3_class(test_fig_en, "ggplot")
    expect_s3_class(test_fig_fr, "ggplot")
    
})
