##############
# TESTS FOR UI
##############

test_that("app ui", {
  ui <- app_ui()
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(app_ui)
  for (i in c("request")){
    expect_true(i %in% names(fmls))
  }
})

##################
# TESTS FOR SERVER
##################

# Test server
test_that("app server", {
  server <- app_server
  expect_type(server, "closure")
  # Check that formals have not been removed
  fmls <- formals(app_server)
  for (i in c("input", "output", "session")){
    expect_true(i %in% names(fmls))
  }
})

# Configure this test to fit your need
#test_that(
#  "app launches",{
#    golem::expect_running(sleep = 5)
#  }
#)

# Test file() reactive
test_that("file reactive", {
  server <- app_server
  testServer(server, {
    session$setInputs(upload = list(datapath =  system.file("extdata", "acc.agd", package = "activAnalyzer")))
    
    test_file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
    test_data <- prepare_dataset(data = test_file)
    
    actual_data <- data()

    expect_identical(actual_data, test_data)
  })
})


# Test wear time analysis n°1
test_that("wear time", {
  server <- app_server
  testServer(server, {
    session$setInputs(upload = list(datapath =  system.file("extdata", "acc.agd", package = "activAnalyzer")),
                      axis_weartime = "vertical axis", 
                      frame_size = 60, 
                      allowanceFrame_size = 1)
    
    test_file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
    test_marked_wear_time_data <- 
      prepare_dataset(data = test_file) %>%
      mark_wear_time(cts  = "axis1", frame = 60, allowanceFrame = 1)
    
    actual_data <- data() %>%  mark_wear_time()
    
    expect_identical(actual_data, test_marked_wear_time_data)
    
    
  })
})

# Test wear time analysis n°1
test_that("wear time", {
  server <- app_server
  testServer(server, {
    session$setInputs(upload = list(datapath =  system.file("extdata", "acc.agd", package = "activAnalyzer")),
                      axis_weartime = "vector magnitude", 
                      frame_size = 30, 
                      allowanceFrame_size = 0)
    
    test_file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
    test_marked_wear_time_data <- 
      prepare_dataset(data = test_file) %>%
      mark_wear_time(cts  = "vm", frame = 30, allowanceFrame = 0)
    
    actual_data <- data() %>%  mark_wear_time()
    
    expect_identical(actual_data, test_marked_wear_time_data)
    
    
  })
})




