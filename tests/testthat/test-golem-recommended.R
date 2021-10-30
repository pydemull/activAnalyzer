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


test_that("The app correctly manages dataframes", {
  
  # Loading app
    devtools::load_all()
  
  # Getting need global objects
     Study <<- "Study"
  
  # Creating shinyDriver object
    app <- shinytest::ShinyDriver$new(activAnalyzer::run_app(),
                                    loadTimeout = 1e+05,
                                    shinyOptions = list(test.mode = TRUE))
 
  # Loading file inside and outside the app
    app$uploadFile(upload = "inst/extdata/acc.agd")
    test_file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
    

  
  # Testing dataframe marked for wear time
      # Test n°1
        app$setInputs(axis_weartime = "vertical axis", 
                      frame_size = 60, 
                      allowanceFrame_size = 1)
        
        app$setInputs(validate = "click")
        
        actual_df <- app$getAllValues()$export[["df"]]
        test_df <- 
          prepare_dataset(data = test_file) %>%
          mark_wear_time(cts  = "axis1", frame = 60, allowanceFrame = 1) 
        
        expect_identical(actual_df, test_df)
     
     # Test n°2
       app$setInputs(axis_weartime = "vector magnitude", 
                     frame_size = 30, 
                     allowanceFrame_size = 0)
       
       app$setInputs(validate = "click")
       
       actual_df <- app$getAllValues()$export[["df"]]
       test_df <- 
         prepare_dataset(data = test_file) %>%
         mark_wear_time(cts  = "vm", frame = 30, allowanceFrame = 0) 
       
       expect_identical(actual_df, test_df)
       
    # Testing dataframe with results by day
       
        # Test 1 / Equation: Sasaki et al. (2011) [Adults]; MVPA cut-points: Sasaki et al. (2011) [Adults];
        # SED cut-point: Aguilar-Farias et al. (2014) [Older adults]
          app$setInputs(age = 32,
                        weight = 67,
                        sex = "male",
                        equation_mets = "Sasaki et al. (2011) [Adults]",
                        mvpa_cutpoint = "Sasaki et al. (2011) [Adults]",
                        sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
    
           app$setInputs(Run = "click")
           
           actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
           test_results_by_day <-
             test_df %>%
             mark_intensity(col_axis = "vm", sed_cutpoint = 200, mpa_cutpoint = 2690, vpa_cutpoint = 6167,
                            equation = "Sasaki et al. (2011) [Adults]", age = 32, weight = 67, sex = "male") %>%
             recap_by_day( age = 32, weight = 67, sex = "male")
           
           expect_identical(actual_results_by_day, test_results_by_day)
           
       # Test 2 / Equation: Santos-Lozano et al. (2013) [Older adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Older adults];
       # SED cut-point: Aguilar-Farias et al. (2014) [Older adults]
         app$setInputs(age = 64,
                       weight = 67,
                       sex = "male",
                       equation_mets = "Santos-Lozano et al. (2013) [Older adults]",
                       mvpa_cutpoint = "Santos-Lozano et al. (2013) [Older adults]",
                       sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
         
         app$setInputs(Run = "click")
         
         actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
         test_results_by_day <-
           test_df %>%
           mark_intensity(col_axis = "vm", sed_cutpoint = 200, mpa_cutpoint = 2751, vpa_cutpoint = 9359,
                          equation = "Santos-Lozano et al. (2013) [Older adults]", age = 64, weight = 67, sex = "male") %>%
           recap_by_day( age = 64, weight = 67, sex = "male")
         
         expect_identical(actual_results_by_day, test_results_by_day)
         
      # Test 3 / Equation: Santos-Lozano et al. (2013) [Adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Adults];
      # SED cut-point: Personalized
         app$setInputs(age = 47,
                       weight = 78,
                       sex = "undefined",
                       equation_mets = "Santos-Lozano et al. (2013) [Adults]",
                       mvpa_cutpoint = "Santos-Lozano et al. (2013) [Adults]",
                       sed_cutpoint = "Personalized...",
                       perso_sed_axis = "vector magnitude",
                       perso_sed_cutpoint = 150)
         
         app$setInputs(Run = "click")
         
         actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
         test_results_by_day <-
           test_df %>%
           mark_intensity(col_axis = "vm", sed_cutpoint = 150, mpa_cutpoint = 3208, vpa_cutpoint = 8565,
                          equation = "Santos-Lozano et al. (2013) [Adults]", age = 47, weight = 78, sex = "undefined") %>%
           recap_by_day(age = 47, weight = 78, sex = "undefined")
         
         expect_identical(actual_results_by_day, test_results_by_day)
         
      # Test 4 / Equation: Freedson et al. (1998) [Adults]; MVPA cut-points: Personalized;
      # SED cut-point: Personalized
        app$setInputs(age = 47,
                      weight = 78,
                      sex = "female",
                      equation_mets = "Freedson et al. (1998) [Adults]",
                      mvpa_cutpoint = "Personalized...",
                      perso_mvpa_axis = "vertical axis",
                      perso_mpa_cutpoint = 2990,
                      perso_vpa_cutpoint = 7000,
                      sed_cutpoint = "Personalized...",
                      perso_sed_axis = "vertical axis",
                      perso_sed_cutpoint = 150)
        
        app$setInputs(Run = "click")
        
        actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
        test_results_by_day <-
          test_df %>%
          mark_intensity(col_axis = "axis1", sed_cutpoint = 150, mpa_cutpoint = 2990, vpa_cutpoint = 7000,
                         equation = "Freedson et al. (1998) [Adults]", age = 47, weight = 78, sex = "female") %>%
          recap_by_day(age = 47, weight = 78, sex = "female")
       
       expect_identical(actual_results_by_day, test_results_by_day)
       
       
    # Testing dataframe with averaged results
       app$setInputs(minimum_wear_time_for_analysis = 12)
       app$setInputs(Run = "click")
       
       
       actual_results_summary <- app$getAllValues()$export[["results_summary"]]
       test_results_summary <- 
         test_results_by_day %>%
         average_results(minimum_wear_time = 12)
       
       expect_identical(actual_results_summary, test_results_summary)
       
})      
    
    
    

  
  




