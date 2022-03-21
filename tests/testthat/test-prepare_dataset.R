
test_that("Dataset is properly prepared", {
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  my_data <- prepare_dataset(data = file)
  
  # Testing the class of the dataset
    expect_s3_class(my_data, "data.frame")

})
