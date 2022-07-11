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


test_that("The server functions correctly work", {
  
  skip_on_cran() # This is set because PhantomJS is not found on Debian platform during CRAN submission pre-tests.
  
  # Preparing environment for shinytest
    Study <<- "Study"
    Metric <<- "Metric"
    Score <<- "Score"
    Date <<- "Date"
    wear_time <<- "wear_time"
    validity <<- "validity"
    col_time_stamp <<- "col_time_stamp"
    Range <<- "Range"
    Intervalle <<- "Intervalle"
    Selected_Day_ID <<- "Selected_Day_ID"
    Steps_score <<- "Steps_score"
    VMU_score <<- "VMU_score"
    Validity <<- "Validity"
    Item <<- "Item"
    Day <<- "Day"
    score_type <<- "score_type"
    difficulty_score_raw <<- "difficulty_score_raw"
    difficulty_score_raw_diff <<- "difficulty_score_raw_diff"
    difficulty_score_raw_quant <<- "difficulty_score_raw_quant"
    difficulty_score_rasch <<- "difficulty_score_rasch"
    amount_score_raw <<- "amount_score_raw"
    amount_score_raw_diff <<- "amount_score_raw_diff"
    amount_score_raw_quant <<- "amount_score_raw_quant"
    amount_score_rasch <<- "amount_score_rasch"
    total_score_raw <<- "total_score_raw"
    total_score_rasch <<- "total_score_rasch"
    Jour <<- "Jour"
    init <<- "init"
    assign("users", shiny::reactiveValues(count = 0), envir = .GlobalEnv)
    assign("equations_mets", activAnalyzer:::equations_mets, envir = .GlobalEnv)
    assign("mvpa_cutpoints", activAnalyzer:::mvpa_cutpoints, envir = .GlobalEnv)
    assign("sed_cutpoints", activAnalyzer:::sed_cutpoints, envir = .GlobalEnv)
    assign("mvpa_lines", activAnalyzer:::mvpa_lines, envir = .GlobalEnv)
    assign("sed_lines", activAnalyzer:::sed_lines, envir = .GlobalEnv)
    assign("ratio_lines", activAnalyzer:::ratio_lines, envir = .GlobalEnv)

  # Creating shinyDriver object
    app <- shinytest::ShinyDriver$new(
      run_app(),
      loadTimeout = 1e+05,
      shinyOptions = list(test.mode = TRUE)
      )

  # Loading data file inside and outside the app
    app$uploadFile(upload = "acc.agd")
    test_file <- "acc.agd"
    
  # Testing auto-filling patient information
    app$setInputs(
      assessor_name = "Doe",
      assessor_surname = "John",
      patient_name = "Doe",
      patient_surname = "Jane", 
      sex = "female",
      age = 67,
      weight = 86,
      side = "right"
    )
    
    app$setInputs(auto_fill_char = "click")
    
    test_list <-
      list(
        "Doe",
        "John",
        "Doe",
        "Jane",
        "female",
        67,
        86,
        "right"
        
      )
    
    actual_list <-
      list(
        app$getAllValues()$export[["assessor_name"]],
        app$getAllValues()$export[["assessor_surname"]],
        app$getAllValues()$export[["patient_name"]],
        app$getAllValues()$export[["patient_surname"]],
        app$getAllValues()$export[["sex"]],
        app$getAllValues()$export[["age"]],
        app$getAllValues()$export[["weight"]],
        app$getAllValues()$export[["side"]]
      )
    
    expect_equal(actual_list, test_list)

  # Testing dataframe marked for wear time
    
      # Test 1
        app$setInputs(
          to_epoch = 60,
          axis_weartime = "vertical axis", 
          frame_size = 60, 
          allowanceFrame_size = 1, 
          streamFrame_size = 20
          )
        
        app$setInputs(validate = "click")
        
        actual_df <- app$getAllValues()$export[["df"]]
        test_df <- 
          prepare_dataset(data = test_file) %>%
          mark_wear_time(
            to_epoch = 60,
            cts  = "axis1", 
            frame = 60, 
            allowanceFrame = 1, 
            streamFrame = 20
            ) 
        
        expect_equal(actual_df, test_df)
     
     # Test 2
       app$setInputs(
         to_epoch = 60,
         axis_weartime = "vector magnitude", 
         frame_size = 30, 
         allowanceFrame_size = 0, 
         streamFrame_size = 0
         )
       
       app$setInputs(validate = "click")
       
       actual_df <- app$getAllValues()$export[["df"]]
       test_df <- 
         prepare_dataset(data = test_file) %>%
         mark_wear_time(
           to_epoch = 60,
           cts  = "vm", 
           frame = 30, 
           allowanceFrame = 0,
           streamFrame = 0
           )
       
       expect_equal(actual_df, test_df)
       
       
    # Testing for reseting inputs for the configuration of nonwear/wear time analysis
       app$setInputs(reset_nonwear = "click")
       
       test_list <-
         list(
           60,
           "vector magnitude",
           90,
           2,
           30
           )
       
       actual_list <-
         list(
           app$getAllValues()$export[["to_epoch"]],
           app$getAllValues()$export[["axis_weartime"]],
           app$getAllValues()$export[["frame_size"]],
           app$getAllValues()$export[["allowanceFrame_size"]],
           app$getAllValues()$export[["streamFrame_size"]]
           )
       
       expect_equal(actual_list, test_list)
       
    
    # Testing plot showing nonwear/wear time
      
       test_gg_nonwear <- str(plot_data(test_df))
       actual_gg_nonwear <- str(app$getAllValues()$export[["gg_plot_data"]])
       expect_equal(actual_gg_nonwear, test_gg_nonwear)
       
    # Testing auto-filling activity intensity analyzis
       app$setInputs(
         equation_mets = "Santos-Lozano et al. (2013) [Older adults]",
         sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]",
         mvpa_cutpoint = "Santos-Lozano et al. (2013) [Older adults]"
       )
       
       app$setInputs(auto_fill_intensity = "click")
       
       test_list <-
         list(
           "Santos-Lozano et al. (2013) [Older adults]",
           "Aguilar-Farias et al. (2014) [Older adults]",
           "Santos-Lozano et al. (2013) [Older adults]"
           
         )
       
       actual_list <-
         list(
           app$getAllValues()$export[["equation_mets"]],
           app$getAllValues()$export[["sed_cutpoint"]],
           app$getAllValues()$export[["mvpa_cutpoint"]]
         )
       
       
       
    # Testing dataframe with results by day
       
        # Test 1 / Equation: Sasaki et al. (2011) [Adults]; MVPA cut-points: Sasaki et al. (2011) [Adults];
        # SED cut-point: Aguilar-Farias et al. (2014) [Older adults]
          app$setInputs(
            age = 32,
            weight = 67,
            sex = "male",
            equation_mets = "Sasaki et al. (2011) [Adults]",
            mvpa_cutpoint = "Sasaki et al. (2011) [Adults]",
            sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
    
           app$setInputs(Run = "click")
           
           actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
           
           test_results_by_day <-
             test_df %>%
             mark_intensity(
               col_axis = "vm", 
               sed_cutpoint = 200, 
               mpa_cutpoint = 2690, 
               vpa_cutpoint = 6167,
               equation = "Sasaki et al. (2011) [Adults]", 
               age = 32, weight = 67, sex = "male"
               ) %>%
             recap_by_day(
               age = 32, 
               weight = 67, 
               sex = "male"
               )
           
           expect_equal(actual_results_by_day, test_results_by_day)
           
       # Test 2 / Equation: Santos-Lozano et al. (2013) [Older adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Older adults];
       # SED cut-point: Aguilar-Farias et al. (2014) [Older adults]
         app$setInputs(
           age = 64,
           weight = 67,
           sex = "male",
           equation_mets = "Santos-Lozano et al. (2013) [Older adults]",
           mvpa_cutpoint = "Santos-Lozano et al. (2013) [Older adults]",
           sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
         
         app$setInputs(Run = "click")
         
         actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
         
         test_results_by_day <-
           test_df %>%
           mark_intensity(
             col_axis = "vm", 
             sed_cutpoint = 200, 
             mpa_cutpoint = 2751, 
             vpa_cutpoint = 9359,
             equation = "Santos-Lozano et al. (2013) [Older adults]", 
             age = 64, 
             weight = 67,
             sex = "male"
             ) %>%
           recap_by_day( 
             age = 64, 
             weight = 67,
             sex = "male"
             )
         
         expect_equal(actual_results_by_day, test_results_by_day)
         
      # Test 3 / Equation: Santos-Lozano et al. (2013) [Adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Adults];
      # SED cut-point: Personalized
         app$setInputs(
           age = 47,
           weight = 78,
           sex = "undefined",
           equation_mets = "Santos-Lozano et al. (2013) [Adults]",
           mvpa_cutpoint = "Santos-Lozano et al. (2013) [Adults]",
           sed_cutpoint = "Personalized...",
           perso_sed_axis = "vector magnitude",
           perso_sed_cutpoint = 150
           )
         
         app$setInputs(Run = "click")
         
         actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
         
         test_results_by_day <-
           test_df %>%
           mark_intensity(
             col_axis = "vm", 
             sed_cutpoint = 150, 
             mpa_cutpoint = 3208, 
             vpa_cutpoint = 8565,
             equation = "Santos-Lozano et al. (2013) [Adults]", 
             age = 47, 
             weight = 78, 
             sex = "undefined"
             ) %>%
           recap_by_day(
             age = 47, 
             weight = 78, 
             sex = "undefined"
             )
         
         expect_equal(actual_results_by_day, test_results_by_day)
         
      # Test 4 / Equation: Freedson et al. (1998) [Adults]; MVPA cut-points: Personalized;
      # SED cut-point: Personalized
        app$setInputs(
          age = 47,
          weight = 78,
          sex = "female",
          equation_mets = "Freedson et al. (1998) [Adults]",
          mvpa_cutpoint = "Personalized...",
          perso_mvpa_axis = "vertical axis",
          perso_mpa_cutpoint = 1952,
          perso_vpa_cutpoint = 5725,
          sed_cutpoint = "Personalized...",
          perso_sed_axis = "vertical axis",
          perso_sed_cutpoint = 100
          )
        
        app$setInputs(Run = "click")
        
        actual_results_by_day <- app$getAllValues()$export[["results_by_day"]]
        
        test_results_by_day <-
          test_df %>%
          mark_intensity(
            col_axis = "axis1", 
            sed_cutpoint = 100, 
            mpa_cutpoint = 1952, 
            vpa_cutpoint = 5725,
            equation = "Freedson et al. (1998) [Adults]", 
            age = 47, 
            weight = 78, 
            sex = "female"
            ) %>%
          recap_by_day(age = 47, weight = 78, sex = "female")
       
       expect_equal(actual_results_by_day, test_results_by_day)
       
    # Testing for setting default configuration to validate a day (period and wear time hours)
       app$setInputs(reset_period = "click")
       
       test_set_default <-
         list(
           "00:00:00",
           "23:59:59",
           10
         )
       
       actual_set_default <- 
         list(
           app$getAllValues()$export[["start_day_analysis"]],
           app$getAllValues()$export[["end_day_analysis"]],
           app$getAllValues()$export[["minimum_wear_time_for_analysis"]]
         )
       
       expect_equal(test_set_default, actual_set_default)
    
    # Testing for setting proactive period (non-sleep wearing protocol)
       app$setInputs(pro_active_period_non_sleep = "click")
       
       test_set_proactive <-
         list(
           "00:00:00",
           "23:59:59",
           8
         )
       
       actual_set_proactive <- 
         list(
           app$getAllValues()$export[["start_day_analysis"]],
           app$getAllValues()$export[["end_day_analysis"]],
           app$getAllValues()$export[["minimum_wear_time_for_analysis"]]
         )
       
       expect_equal(actual_set_proactive, test_set_proactive)
       
       
    # Testing for setting proactive period (24-h wearing protocol)
      app$setInputs(pro_active_period_24h = "click")
      
      test_set_proactive <-
        list(
          "07:00:00",
          "22:00:00",
          8
        )
      
      actual_set_proactive <- 
        list(
          app$getAllValues()$export[["start_day_analysis"]],
          app$getAllValues()$export[["end_day_analysis"]],
          app$getAllValues()$export[["minimum_wear_time_for_analysis"]]
        )
      
      expect_equal(actual_set_proactive, test_set_proactive)
         
       
    # Testing dataframe with daily summary
       app$setInputs(minimum_wear_time_for_analysis = 12)
       app$setInputs(Run = "click")
       
       # With means
         actual_results_summary_means <- app$getAllValues()$export[["results_summary_means"]]
         
         test_results_summary_means <- 
           test_df %>%
           mark_intensity(
             col_axis = "axis1", 
             sed_cutpoint = 100, 
             mpa_cutpoint = 1952, 
             vpa_cutpoint = 5725,
             equation = "Freedson et al. (1998) [Adults]", 
             age = 47, 
             weight = 78, 
             sex = "female"
             ) %>%
           recap_by_day(
             age = 47, 
             weight = 78, 
             sex = "female", 
             valid_wear_time_start = "07:00:00", 
             valid_wear_time_end = "22:00:00") %>%
           average_results(minimum_wear_time = 12, fun = "mean")
         
         expect_equal(actual_results_summary_means, test_results_summary_means)
         
       
       # With medians
         actual_results_summary_medians <- app$getAllValues()$export[["results_summary_medians"]]
         
         test_results_summary_medians <- 
           test_df %>%
           mark_intensity(
             col_axis = "axis1", 
             sed_cutpoint = 100, 
             mpa_cutpoint = 1952, 
             vpa_cutpoint = 5725,
             equation = "Freedson et al. (1998) [Adults]", 
             age = 47, 
             weight = 78, 
             sex = "female"
           ) %>%
           recap_by_day(
             age = 47, 
             weight = 78, 
             sex = "female", 
             valid_wear_time_start = "07:00:00", 
             valid_wear_time_end = "22:00:00") %>%
           average_results(minimum_wear_time = 12, fun = "median")
         
         expect_equal(actual_results_summary_medians, test_results_summary_medians)
         
       
       
    # Testing BMR computation
      test_bmr <- 9.74 * 78 + 694
      actual_bmr <- app$getAllValues()$export[["BMR"]]
      
      expect_equal(actual_bmr, test_bmr)
      
    # Testing computed PROactive scores
      
      # Setting inputs for all tests
        app$setInputs(
          age = 25,
          weight = 50,
          sex = "female",
          reset_nonwear = "click",
          validate = "click",
          equation_mets = "Sasaki et al. (2011) [Adults]",
          mvpa_cutpoint = "Sasaki et al. (2011) [Adults]",
          sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]",
          reset_period = "click",
          Run = "click"
        )
        
      # ============================================================================================================================================  
      # C-PPAC (EN)
      # ============================================================================================================================================
        
         #************
         # Round 1
         #************
        
            # Setting inputs for C-PPAC tests
              app$setInputs(
                cppac_EN_q1 = "None at all",
                cppac_EN_q2 = "None at all",
                cppac_EN_q3 = "None at all",
                cppac_EN_q4 = "None at all",
                cppac_EN_q5 = "Not at all",
                cppac_EN_q6 = "Not at all",
                cppac_EN_q7 = "Not at all",
                cppac_EN_q8 = "Not at all",
                cppac_EN_q9 = "Not at all",
                cppac_EN_q10 = "Not at all",
                cppac_EN_q11 = "Not at all",
                cppac_EN_q12 = "No",
                get_cppac_summary_en = "click"
              )
           
           # Getting final scores computed with the app
             score_cppac_diff_en <- app$getAllValues()$export[["score_cppac_diff_en"]]
             score_cppac_quant_en <- app$getAllValues()$export[["score_cppac_quant_en"]]
             score_cppac_tot_rasch_en <- app$getAllValues()$export[["score_cppac_tot_rasch_en"]]
             
           # Testing Difficulty score (raw)
             expect_equal(score_cppac_diff_en, 10*4)
           
           # Testing Amount score (raw)
             expect_equal(score_cppac_quant_en, 0+0+4+4)
           
           # Testing Total score (rasch)
             expect_equal(score_cppac_tot_rasch_en, round((100+59) / 2, 1))
             
             
        #************
        # Round 2
        #************
              
           # Setting inputs for C-PPAC tests
           app$setInputs(
             cppac_EN_q1 = "A little bit (about 10 minutes every day)", 
             cppac_EN_q2 = "A few", 
             cppac_EN_q3 = "A little bit",
             cppac_EN_q4 = "A little bit",
             cppac_EN_q5 = "Rarely",
             cppac_EN_q6 = "A little bit",  
             cppac_EN_q7 = "Rarely",  
             cppac_EN_q8 = "A little bit",  
             cppac_EN_q9 = "Rarely",  
             cppac_EN_q10 =  "A little bit", 
             cppac_EN_q11 =  "A little bit",
             cppac_EN_q12 =  "A little bit",
             get_cppac_summary_en = "click"
           )
             
           # Getting final scores computed with the app
             score_cppac_diff_en <- app$getAllValues()$export[["score_cppac_diff_en"]]
             score_cppac_quant_en <- app$getAllValues()$export[["score_cppac_quant_en"]]
             score_cppac_tot_rasch_en <- app$getAllValues()$export[["score_cppac_tot_rasch_en"]]
           
           # Testing Difficulty score (raw)
             expect_equal(score_cppac_diff_en, 10*3)
           
           # Testing Amount score (raw)
             expect_equal(score_cppac_quant_en, 1+1+4+4)
           
           # Testing Total score (rasch)
            expect_equal(score_cppac_tot_rasch_en, round((75+67) / 2, 1))
            
          #************
          # Round 3
          #************
            
            # Setting inputs for C-PPAC tests
              app$setInputs(
                cppac_EN_q1 = "Some (about 30 minutes every day)", 
                cppac_EN_q2 = "Some", 
                cppac_EN_q3 = "Some",
                cppac_EN_q4 = "Some",
                cppac_EN_q5 = "Sometimes",
                cppac_EN_q6 = "Moderately",  
                cppac_EN_q7 = "Sometimes",  
                cppac_EN_q8 = "Moderately",  
                cppac_EN_q9 = "Sometimes",  
                cppac_EN_q10 =  "Moderately", 
                cppac_EN_q11 =  "Some",
                cppac_EN_q12 =  "Sometimes",
                get_cppac_summary_en = "click"
              )
            
            # Getting final scores computed with the app
              score_cppac_diff_en <- app$getAllValues()$export[["score_cppac_diff_en"]]
              score_cppac_quant_en <- app$getAllValues()$export[["score_cppac_quant_en"]]
              score_cppac_tot_rasch_en <- app$getAllValues()$export[["score_cppac_tot_rasch_en"]]
            
            # Testing Difficulty score (raw)
              expect_equal(score_cppac_diff_en, 10*2)
            
            # Testing Amount score (raw)
              expect_equal(score_cppac_quant_en, 2+2+4+4)
            
            # Testing Total score (rasch)
              expect_equal(score_cppac_tot_rasch_en, round((58+77) / 2, 1))
              
          #************
          # Round 4
          #************
              
              # Setting inputs for C-PPAC tests
                app$setInputs(
                  cppac_EN_q1 = "A lot (about 1 hour every day)", 
                  cppac_EN_q2 = "A lot", 
                  cppac_EN_q3 = "A lot",
                  cppac_EN_q4 = "A lot",
                  cppac_EN_q5 = "Frequently",
                  cppac_EN_q6 = "Very",  
                  cppac_EN_q7 = "Frequently",  
                  cppac_EN_q8 = "Very",  
                  cppac_EN_q9 = "Frequently",  
                  cppac_EN_q10 =  "Very", 
                  cppac_EN_q11 =  "A lot",
                  cppac_EN_q12 =  "A lot",
                  get_cppac_summary_en = "click"
                )
              
              # Getting final scores computed with the app
                score_cppac_diff_en <- app$getAllValues()$export[["score_cppac_diff_en"]]
                score_cppac_quant_en <- app$getAllValues()$export[["score_cppac_quant_en"]]
                score_cppac_tot_rasch_en <- app$getAllValues()$export[["score_cppac_tot_rasch_en"]]
              
              # Testing Difficulty score (raw)
                expect_equal(score_cppac_diff_en, 10*1)
              
              # Testing Amount score (raw)
                expect_equal(score_cppac_quant_en, 3+3+4+4)
              
              # Testing Total score (rasch)
                expect_equal(score_cppac_tot_rasch_en, round((40+91) / 2, 1))
                
                
           #************
           # Round 5
           #************
              
              # Setting inputs for C-PPAC tests
                app$setInputs(
                  cppac_EN_q1 = "A great deal (more than 1 hour every day)", 
                  cppac_EN_q2 = "A large amount", 
                  cppac_EN_q3 = "A great deal",
                  cppac_EN_q4 = "A great deal",
                  cppac_EN_q5 = "All the time",
                  cppac_EN_q6 = "Extremely",  
                  cppac_EN_q7 = "All the time",  
                  cppac_EN_q8 = "Extremely",  
                  cppac_EN_q9 = "All the time",  
                  cppac_EN_q10 =  "Extremely", 
                  cppac_EN_q11 =  "A great deal",
                  cppac_EN_q12 =  "A great deal",
                  get_cppac_summary_en = "click"
                )
                
              # Getting final scores computed with the app
                score_cppac_diff_en <- app$getAllValues()$export[["score_cppac_diff_en"]]
                score_cppac_quant_en <- app$getAllValues()$export[["score_cppac_quant_en"]]
                score_cppac_tot_rasch_en <- app$getAllValues()$export[["score_cppac_tot_rasch_en"]]
                
              # Testing Difficulty score (raw)
                expect_equal(score_cppac_diff_en, 10*0)
              
              # Testing Amount score (raw)
                expect_equal(score_cppac_quant_en, 3+4+4+4)
              
              # Testing Total score (rasch)
                expect_equal(score_cppac_tot_rasch_en, round((0+100) / 2, 1))
                
                
        # ============================================================================================================================================  
        # C-PPAC (FR)
        # ============================================================================================================================================
                
             #************
             # Round 1
             #************
                
                # Setting inputs for C-PPAC tests
                  app$setInputs(
                    cppac_FR_q1 = "Pas du tout",
                    cppac_FR_q2 = "Aucune",
                    cppac_FR_q3 = "Pas du tout",
                    cppac_FR_q4 = "Pas du tout",
                    cppac_FR_q5 = "Jamais",
                    cppac_FR_q6 = "Pas du tout",
                    cppac_FR_q7 = "Jamais",
                    cppac_FR_q8 = "Pas du tout",
                    cppac_FR_q9 = "Jamais",
                    cppac_FR_q10 = "Pas du tout",
                    cppac_FR_q11 = "Aucun",
                    cppac_FR_q12 = "Non",
                    get_cppac_summary_fr = "click"
                  )
                
                # Getting final scores computed with the app
                  score_cppac_diff_fr <- app$getAllValues()$export[["score_cppac_diff_fr"]]
                  score_cppac_quant_fr <- app$getAllValues()$export[["score_cppac_quant_fr"]]
                  score_cppac_tot_rasch_fr <- app$getAllValues()$export[["score_cppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                  expect_equal(score_cppac_diff_fr, 10*4)
                
                # Testing Amount score (raw)
                  expect_equal(score_cppac_quant_fr, 0+0+4+4)
                
                # Testing Total score (rasch)
                  expect_equal(score_cppac_tot_rasch_fr, round((100+59) / 2, 1))
                
                
            #************
            # Round 2
            #************
                
                # Setting inputs for C-PPAC tests
                app$setInputs(
                  cppac_FR_q1 = "Un petit peu (environ 10 minutes chaque jour)", 
                  cppac_FR_q2 = "Tr\u00e8s peu", 
                  cppac_FR_q3 = "Un petit peu",
                  cppac_FR_q4 = "Un petit peu",
                  cppac_FR_q5 = "Rarement",
                  cppac_FR_q6 = "Un petit peu",  
                  cppac_FR_q7 = "Rarement",  
                  cppac_FR_q8 = "Un petit peu",  
                  cppac_FR_q9 = "Rarement",  
                  cppac_FR_q10 = "Un petit peu", 
                  cppac_FR_q11 = "Un petit peu",
                  cppac_FR_q12 = "Un petit peu",
                  get_cppac_summary_fr = "click"
                )
                
                # Getting final scores computed with the app
                  score_cppac_diff_fr <- app$getAllValues()$export[["score_cppac_diff_fr"]]
                  score_cppac_quant_fr <- app$getAllValues()$export[["score_cppac_quant_fr"]]
                  score_cppac_tot_rasch_fr <- app$getAllValues()$export[["score_cppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                  expect_equal(score_cppac_diff_fr, 10*3)
                
                # Testing Amount score (raw)
                  expect_equal(score_cppac_quant_fr, 1+1+4+4)
                
                # Testing Total score (rasch)
                  expect_equal(score_cppac_tot_rasch_fr, round((75+67) / 2, 1))
                
           #************
           # Round 3
           #************
                
                # Setting inputs for C-PPAC tests
                  app$setInputs(
                    cppac_FR_q1 = "Un peu (environ 30 minutes chaque jour)", 
                    cppac_FR_q2 = "Quelques-unes", 
                    cppac_FR_q3 = "Quelques-unes",
                    cppac_FR_q4 = "Quelques-unes",
                    cppac_FR_q5 = "Quelques fois",
                    cppac_FR_q6 = "Mod\u00e9r\u00e9ment",  
                    cppac_FR_q7 = "Quelques fois",  
                    cppac_FR_q8 = "Mod\u00e9r\u00e9ment",  
                    cppac_FR_q9 = "Quelques fois",  
                    cppac_FR_q10 =  "Mod\u00e9r\u00e9ment", 
                    cppac_FR_q11 =  "Un peu",
                    cppac_FR_q12 =  "Quelques fois",
                    get_cppac_summary_fr = "click"
                  )
                
                # Getting final scores computed with the app
                score_cppac_diff_fr <- app$getAllValues()$export[["score_cppac_diff_fr"]]
                score_cppac_quant_fr <- app$getAllValues()$export[["score_cppac_quant_fr"]]
                score_cppac_tot_rasch_fr <- app$getAllValues()$export[["score_cppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                  expect_equal(score_cppac_diff_fr, 10*2)
                
                # Testing Amount score (raw)
                  expect_equal(score_cppac_quant_fr, 2+2+4+4)
                
                # Testing Total score (rasch)
                expect_equal(score_cppac_tot_rasch_fr, round((58+77) / 2, 1))
                
          #************
          # Round 4
          #************
                
                # Setting inputs for C-PPAC tests
                  app$setInputs(
                   cppac_FR_q1 = "Beaucoup (environ 1 heure chaque jour)", 
                   cppac_FR_q2 = "Beaucoup", 
                   cppac_FR_q3 = "Beaucoup",
                   cppac_FR_q4 = "Beaucoup",
                   cppac_FR_q5 = "Fr\u00e9quemment",
                   cppac_FR_q6 = "Tr\u00e8s",  
                   cppac_FR_q7 = "Fr\u00e9quemment",  
                   cppac_FR_q8 = "Tr\u00e8s",  
                   cppac_FR_q9 = "Fr\u00e9quemment",  
                   cppac_FR_q10 =  "Tr\u00e8s", 
                   cppac_FR_q11 =  "Beaucoup",
                   cppac_FR_q12 =  "Beaucoup",
                   get_cppac_summary_fr = "click"
                  )
                
                # Getting final scores computed with the app
                score_cppac_diff_fr <- app$getAllValues()$export[["score_cppac_diff_fr"]]
                score_cppac_quant_fr <- app$getAllValues()$export[["score_cppac_quant_fr"]]
                score_cppac_tot_rasch_fr <- app$getAllValues()$export[["score_cppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                  expect_equal(score_cppac_diff_fr, 10*1)
                
                # Testing Amount score (raw)
                  expect_equal(score_cppac_quant_fr, 3+3+4+4)
                
                # Testing Total score (rasch)
                  expect_equal(score_cppac_tot_rasch_fr, round((40+91) / 2, 1))
                
                
          #************
          # Round 5
          #************
                
                # Setting inputs for C-PPAC tests
                  app$setInputs(
                    cppac_FR_q1 = "Enorm\u00e9ment (plus d\u20191 heure chaque jour)", 
                    cppac_FR_q2 = "Enorm\u00e9ment", 
                    cppac_FR_q3 = "Enorm\u00e9ment",
                    cppac_FR_q4 = "Enorm\u00e9ment",
                    cppac_FR_q5 = "Tout le temps",
                    cppac_FR_q6 = "Extr\u00eamement",  
                    cppac_FR_q7 = "Tout le temps",  
                    cppac_FR_q8 = "Extr\u00eamement",  
                    cppac_FR_q9 = "Tout le temps",  
                    cppac_FR_q10 =  "Extr\u00eamement", 
                    cppac_FR_q11 =  "Enorm\u00e9ment",
                    cppac_FR_q12 =  "Enorm\u00e9ment",
                    get_cppac_summary_fr = "click"
                  )
                  
                # Getting final scores computed with the app
                  score_cppac_diff_fr <- app$getAllValues()$export[["score_cppac_diff_fr"]]
                  score_cppac_quant_fr <- app$getAllValues()$export[["score_cppac_quant_fr"]]
                  score_cppac_tot_rasch_fr <- app$getAllValues()$export[["score_cppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                  expect_equal(score_cppac_diff_fr, 10*0)
                
                # Testing Amount score (raw)
                  expect_equal(score_cppac_quant_fr, 3+4+4+4)
                
                # Testing Total score (rasch)
                  expect_equal(score_cppac_tot_rasch_fr, round((0+100) / 2, 1))
                  
                  
# ============================================================================================================================================  
# D-PPAC (EN)
# ============================================================================================================================================

         #************
         # Round 1
         #************
                  
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_EN_d1_q1 = "None at all",
                  dppac_EN_d1_q2 = "None at all",
                  dppac_EN_d1_q3 = "None at all",
                  dppac_EN_d1_q4 = "Not at all",
                  dppac_EN_d1_q5 = "Not at all",
                  dppac_EN_d1_q6 = "Not at all",
                  dppac_EN_d1_q7 = "Not at all",
                  dppac_EN_d2_q1 = "None at all",
                  dppac_EN_d2_q2 = "None at all",
                  dppac_EN_d2_q3 = "None at all",
                  dppac_EN_d2_q4 = "Not at all",
                  dppac_EN_d2_q5 = "Not at all",
                  dppac_EN_d2_q6 = "Not at all",
                  dppac_EN_d2_q7 = "Not at all",
                  dppac_EN_d3_q1 = "None at all",
                  dppac_EN_d3_q2 = "None at all",
                  dppac_EN_d3_q3 = "None at all",
                  dppac_EN_d3_q4 = "Not at all",
                  dppac_EN_d3_q5 = "Not at all",
                  dppac_EN_d3_q6 = "Not at all",
                  dppac_EN_d3_q7 = "Not at all",
                  dppac_EN_d4_q1 = "None at all",
                  dppac_EN_d4_q2 = "None at all",
                  dppac_EN_d4_q3 = "None at all",
                  dppac_EN_d4_q4 = "Not at all",
                  dppac_EN_d4_q5 = "Not at all",
                  dppac_EN_d4_q6 = "Not at all",
                  dppac_EN_d4_q7 = "Not at all",
                  dppac_EN_d5_q1 = "None at all",
                  dppac_EN_d5_q2 = "None at all",
                  dppac_EN_d5_q3 = "None at all",
                  dppac_EN_d5_q4 = "Not at all",
                  dppac_EN_d5_q5 = "Not at all",
                  dppac_EN_d5_q6 = "Not at all",
                  dppac_EN_d5_q7 = "Not at all",
                  dppac_EN_d6_q1 = "None at all",
                  dppac_EN_d6_q2 = "None at all",
                  dppac_EN_d6_q3 = "None at all",
                  dppac_EN_d6_q4 = "Not at all",
                  dppac_EN_d6_q5 = "Not at all",
                  dppac_EN_d6_q6 = "Not at all",
                  dppac_EN_d6_q7 = "Not at all",
                  dppac_EN_d7_q1 = "None at all",
                  dppac_EN_d7_q2 = "None at all",
                  dppac_EN_d7_q3 = "None at all",
                  dppac_EN_d7_q4 = "Not at all",
                  dppac_EN_d7_q5 = "Not at all",
                  dppac_EN_d7_q6 = "Not at all",
                  dppac_EN_d7_q7 = "Not at all",
                  get_dppac_summary_en = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_en <- app$getAllValues()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$getAllValues()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$getAllValues()$export[["score_dppac_tot_rasch_en"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_en, 20)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_en, 44/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_en, round((150/2*4+147/2)/5, 1))
                
                
        #************
        # Round 2
        #************
                
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_EN_d1_q1 = "A little bit (up to 10 minutes in total)",
                  dppac_EN_d1_q2 = "A few",
                  dppac_EN_d1_q3 = "A little bit",
                  dppac_EN_d1_q4 = "Rarely",
                  dppac_EN_d1_q5 = "A little bit",
                  dppac_EN_d1_q6 = "A little bit",
                  dppac_EN_d1_q7 = "Rarely",
                  dppac_EN_d2_q1 = "A little bit (up to 10 minutes in total)",
                  dppac_EN_d2_q2 = "A few",
                  dppac_EN_d2_q3 = "A little bit",
                  dppac_EN_d2_q4 = "Rarely",
                  dppac_EN_d2_q5 = "A little bit",
                  dppac_EN_d2_q6 = "A little bit",
                  dppac_EN_d2_q7 = "Rarely",
                  dppac_EN_d3_q1 = "A little bit (up to 10 minutes in total)",
                  dppac_EN_d3_q2 = "A few",
                  dppac_EN_d3_q3 = "A little bit",
                  dppac_EN_d3_q4 = "Rarely",
                  dppac_EN_d3_q5 = "A little bit",
                  dppac_EN_d3_q6 = "A little bit",
                  dppac_EN_d3_q7 = "Rarely",
                  dppac_EN_d4_q1 = "A little bit (up to 10 minutes in total)",
                  dppac_EN_d4_q2 = "A few",
                  dppac_EN_d4_q3 = "A little bit",
                  dppac_EN_d4_q4 = "Rarely",
                  dppac_EN_d4_q5 = "A little bit",
                  dppac_EN_d4_q6 = "A little bit",
                  dppac_EN_d4_q7 = "Rarely",
                  dppac_EN_d5_q1 = "A little bit (up to 10 minutes in total)",
                  dppac_EN_d5_q2 = "A few",
                  dppac_EN_d5_q3 = "A little bit",
                  dppac_EN_d5_q4 = "Rarely",
                  dppac_EN_d5_q5 = "A little bit",
                  dppac_EN_d5_q6 = "A little bit",
                  dppac_EN_d5_q7 = "Rarely",
                  dppac_EN_d6_q1 = "A little bit (up to 10 minutes in total)",
                  dppac_EN_d6_q2 = "A few",
                  dppac_EN_d6_q3 = "A little bit",
                  dppac_EN_d6_q4 = "Rarely",
                  dppac_EN_d6_q5 = "A little bit",
                  dppac_EN_d6_q6 = "A little bit",
                  dppac_EN_d6_q7 = "Rarely",
                  dppac_EN_d7_q1 = "A little bit (up to 10 minutes in total)",
                  dppac_EN_d7_q2 = "A few",
                  dppac_EN_d7_q3 = "A little bit",
                  dppac_EN_d7_q4 = "Rarely",
                  dppac_EN_d7_q5 = "A little bit",
                  dppac_EN_d7_q6 = "A little bit",
                  dppac_EN_d7_q7 = "Rarely",
                  get_dppac_summary_en = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_en <- app$getAllValues()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$getAllValues()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$getAllValues()$export[["score_dppac_tot_rasch_en"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_en, 15)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_en, 54/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_en, round((125/2*4+122/2)/5, 1))
                
        #************
        # Round 3
        #************
                
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_EN_d1_q1 = "Some (up to 30 minutes in total)",
                  dppac_EN_d1_q2 = "Some",
                  dppac_EN_d1_q3 = "Some",
                  dppac_EN_d1_q4 = "Sometimes",
                  dppac_EN_d1_q5 = "Moderately",
                  dppac_EN_d1_q6 = "Moderately",
                  dppac_EN_d1_q7 = "Sometimes",
                  dppac_EN_d2_q1 = "Some (up to 30 minutes in total)",
                  dppac_EN_d2_q2 = "Some",
                  dppac_EN_d2_q3 = "Some",
                  dppac_EN_d2_q4 = "Sometimes",
                  dppac_EN_d2_q5 = "Moderately",
                  dppac_EN_d2_q6 = "Moderately",
                  dppac_EN_d2_q7 = "Sometimes",
                  dppac_EN_d3_q1 = "Some (up to 30 minutes in total)",
                  dppac_EN_d3_q2 = "Some",
                  dppac_EN_d3_q3 = "Some",
                  dppac_EN_d3_q4 = "Sometimes",
                  dppac_EN_d3_q5 = "Moderately",
                  dppac_EN_d3_q6 = "Moderately",
                  dppac_EN_d3_q7 = "Sometimes",
                  dppac_EN_d4_q1 = "Some (up to 30 minutes in total)",
                  dppac_EN_d4_q2 = "Some",
                  dppac_EN_d4_q3 = "Some",
                  dppac_EN_d4_q4 = "Sometimes",
                  dppac_EN_d4_q5 = "Moderately",
                  dppac_EN_d4_q6 = "Moderately",
                  dppac_EN_d4_q7 = "Sometimes",
                  dppac_EN_d5_q1 = "Some (up to 30 minutes in total)",
                  dppac_EN_d5_q2 = "Some",
                  dppac_EN_d5_q3 = "Some",
                  dppac_EN_d5_q4 = "Sometimes",
                  dppac_EN_d5_q5 = "Moderately",
                  dppac_EN_d5_q6 = "Moderately",
                  dppac_EN_d5_q7 = "Sometimes",
                  dppac_EN_d6_q1 = "Some (up to 30 minutes in total)",
                  dppac_EN_d6_q2 = "Some",
                  dppac_EN_d6_q3 = "Some",
                  dppac_EN_d6_q4 = "Sometimes",
                  dppac_EN_d6_q5 = "Moderately",
                  dppac_EN_d6_q6 = "Moderately",
                  dppac_EN_d6_q7 = "Sometimes",
                  dppac_EN_d7_q1 = "Some (up to 30 minutes in total)",
                  dppac_EN_d7_q2 = "Some",
                  dppac_EN_d7_q3 = "Some",
                  dppac_EN_d7_q4 = "Sometimes",
                  dppac_EN_d7_q5 = "Moderately",
                  dppac_EN_d7_q6 = "Moderately",
                  dppac_EN_d7_q7 = "Sometimes",
                  get_dppac_summary_en = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_en <- app$getAllValues()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$getAllValues()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$getAllValues()$export[["score_dppac_tot_rasch_en"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_en, 10)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_en, 64/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_en, round((117/2*4+113/2)/5, 1))
             
                
        #************
        # Round 4
        #************
                
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_EN_d1_q1 = "A lot (up to 1 hour in total)",
                  dppac_EN_d1_q2 = "A lot",
                  dppac_EN_d1_q3 = "A lot",
                  dppac_EN_d1_q4 = "Frequently",
                  dppac_EN_d1_q5 = "Very",
                  dppac_EN_d1_q6 = "Very",
                  dppac_EN_d1_q7 = "Frequently",
                  dppac_EN_d2_q1 = "A lot (up to 1 hour in total)",
                  dppac_EN_d2_q2 = "A lot",
                  dppac_EN_d2_q3 = "A lot",
                  dppac_EN_d2_q4 = "Frequently",
                  dppac_EN_d2_q5 = "Very",
                  dppac_EN_d2_q6 = "Very",
                  dppac_EN_d2_q7 = "Frequently",
                  dppac_EN_d3_q1 = "A lot (up to 1 hour in total)",
                  dppac_EN_d3_q2 = "A lot",
                  dppac_EN_d3_q3 = "A lot",
                  dppac_EN_d3_q4 = "Frequently",
                  dppac_EN_d3_q5 = "Very",
                  dppac_EN_d3_q6 = "Very",
                  dppac_EN_d3_q7 = "Frequently",
                  dppac_EN_d4_q1 = "A lot (up to 1 hour in total)",
                  dppac_EN_d4_q2 = "A lot",
                  dppac_EN_d4_q3 = "A lot",
                  dppac_EN_d4_q4 = "Frequently",
                  dppac_EN_d4_q5 = "Very",
                  dppac_EN_d4_q6 = "Very",
                  dppac_EN_d4_q7 = "Frequently",
                  dppac_EN_d5_q1 = "A lot (up to 1 hour in total)",
                  dppac_EN_d5_q2 = "A lot",
                  dppac_EN_d5_q3 = "A lot",
                  dppac_EN_d5_q4 = "Frequently",
                  dppac_EN_d5_q5 = "Very",
                  dppac_EN_d5_q6 = "Very",
                  dppac_EN_d5_q7 = "Frequently",
                  dppac_EN_d6_q1 = "A lot (up to 1 hour in total)",
                  dppac_EN_d6_q2 = "A lot",
                  dppac_EN_d6_q3 = "A lot",
                  dppac_EN_d6_q4 = "Frequently",
                  dppac_EN_d6_q5 = "Very",
                  dppac_EN_d6_q6 = "Very",
                  dppac_EN_d6_q7 = "Frequently",
                  dppac_EN_d7_q1 = "A lot (up to 1 hour in total)",
                  dppac_EN_d7_q2 = "A lot",
                  dppac_EN_d7_q3 = "A lot",
                  dppac_EN_d7_q4 = "Frequently",
                  dppac_EN_d7_q5 = "Very",
                  dppac_EN_d7_q6 = "Very",
                  dppac_EN_d7_q7 = "Frequently",
                  get_dppac_summary_en = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_en <- app$getAllValues()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$getAllValues()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$getAllValues()$export[["score_dppac_tot_rasch_en"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_en, 5)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_en, 74/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_en, round((116/2*4+107/2)/5, 1))     
        
                
      #************
      # Round 5
      #************
      
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_EN_d1_q1 = "A great deal (more than 1 hour in total)",
                  dppac_EN_d1_q2 = "A large amount",
                  dppac_EN_d1_q3 = "A great deal",
                  dppac_EN_d1_q4 = "All the time",
                  dppac_EN_d1_q5 = "Extremely",
                  dppac_EN_d1_q6 = "Extremely",
                  dppac_EN_d1_q7 = "All the time",
                  dppac_EN_d2_q1 = "A great deal (more than 1 hour in total)",
                  dppac_EN_d2_q2 = "A large amount",
                  dppac_EN_d2_q3 = "A great deal",
                  dppac_EN_d2_q4 = "All the time",
                  dppac_EN_d2_q5 = "Extremely",
                  dppac_EN_d2_q6 = "Extremely",
                  dppac_EN_d2_q7 = "All the time",
                  dppac_EN_d3_q1 = "A great deal (more than 1 hour in total)",
                  dppac_EN_d3_q2 = "A large amount",
                  dppac_EN_d3_q3 = "A great deal",
                  dppac_EN_d3_q4 = "All the time",
                  dppac_EN_d3_q5 = "Extremely",
                  dppac_EN_d3_q6 = "Extremely",
                  dppac_EN_d3_q7 = "All the time",
                  dppac_EN_d4_q1 = "A great deal (more than 1 hour in total)",
                  dppac_EN_d4_q2 = "A large amount",
                  dppac_EN_d4_q3 = "A great deal",
                  dppac_EN_d4_q4 = "All the time",
                  dppac_EN_d4_q5 = "Extremely",
                  dppac_EN_d4_q6 = "Extremely",
                  dppac_EN_d4_q7 = "All the time",
                  dppac_EN_d5_q1 = "A great deal (more than 1 hour in total)",
                  dppac_EN_d5_q2 = "A large amount",
                  dppac_EN_d5_q3 = "A great deal",
                  dppac_EN_d5_q4 = "All the time",
                  dppac_EN_d5_q5 = "Extremely",
                  dppac_EN_d5_q6 = "Extremely",
                  dppac_EN_d5_q7 = "All the time",
                  dppac_EN_d6_q1 = "A great deal (more than 1 hour in total)",
                  dppac_EN_d6_q2 = "A large amount",
                  dppac_EN_d6_q3 = "A great deal",
                  dppac_EN_d6_q4 = "All the time",
                  dppac_EN_d6_q5 = "Extremely",
                  dppac_EN_d6_q6 = "Extremely",
                  dppac_EN_d6_q7 = "All the time",
                  dppac_EN_d7_q1 = "A great deal (more than 1 hour in total)",
                  dppac_EN_d7_q2 = "A large amount",
                  dppac_EN_d7_q3 = "A great deal",
                  dppac_EN_d7_q4 = "All the time",
                  dppac_EN_d7_q5 = "Extremely",
                  dppac_EN_d7_q6 = "Extremely",
                  dppac_EN_d7_q7 = "All the time",
                  get_dppac_summary_en = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_en <- app$getAllValues()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$getAllValues()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$getAllValues()$export[["score_dppac_tot_rasch_en"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_en, 0)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_en, 84/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_en, round((100/2*4+90/2)/5, 1)) 
      
 
                
# ============================================================================================================================================  
# D-PPAC (FR)
# ============================================================================================================================================
                
     #************
     # Round 1
     #************
                
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_FR_d1_q1 = "Pas du tout",
                  dppac_FR_d1_q2 = "Aucune",
                  dppac_FR_d1_q3 = "Pas du tout",
                  dppac_FR_d1_q4 = "Jamais",
                  dppac_FR_d1_q5 = "Pas du tout",
                  dppac_FR_d1_q6 = "Pas du tout",
                  dppac_FR_d1_q7 = "Jamais",
                  dppac_FR_d2_q1 = "Pas du tout",
                  dppac_FR_d2_q2 = "Aucune",
                  dppac_FR_d2_q3 = "Pas du tout",
                  dppac_FR_d2_q4 = "Jamais",
                  dppac_FR_d2_q5 = "Pas du tout",
                  dppac_FR_d2_q6 = "Pas du tout",
                  dppac_FR_d2_q7 = "Jamais",
                  dppac_FR_d3_q1 = "Pas du tout",
                  dppac_FR_d3_q2 = "Aucune",
                  dppac_FR_d3_q3 = "Pas du tout",
                  dppac_FR_d3_q4 = "Jamais",
                  dppac_FR_d3_q5 = "Pas du tout",
                  dppac_FR_d3_q6 = "Pas du tout",
                  dppac_FR_d3_q7 = "Jamais",
                  dppac_FR_d4_q1 = "Pas du tout",
                  dppac_FR_d4_q2 = "Aucune",
                  dppac_FR_d4_q3 = "Pas du tout",
                  dppac_FR_d4_q4 = "Jamais",
                  dppac_FR_d4_q5 = "Pas du tout",
                  dppac_FR_d4_q6 = "Pas du tout",
                  dppac_FR_d4_q7 = "Jamais",
                  dppac_FR_d5_q1 = "Pas du tout",
                  dppac_FR_d5_q2 = "Aucune",
                  dppac_FR_d5_q3 = "Pas du tout",
                  dppac_FR_d5_q4 = "Jamais",
                  dppac_FR_d5_q5 = "Pas du tout",
                  dppac_FR_d5_q6 = "Pas du tout",
                  dppac_FR_d5_q7 = "Jamais",
                  dppac_FR_d6_q1 = "Pas du tout",
                  dppac_FR_d6_q2 = "Aucune",
                  dppac_FR_d6_q3 = "Pas du tout",
                  dppac_FR_d6_q4 = "Jamais",
                  dppac_FR_d6_q5 = "Pas du tout",
                  dppac_FR_d6_q6 = "Pas du tout",
                  dppac_FR_d6_q7 = "Jamais",
                  dppac_FR_d7_q1 = "Pas du tout",
                  dppac_FR_d7_q2 = "Aucune",
                  dppac_FR_d7_q3 = "Pas du tout",
                  dppac_FR_d7_q4 = "Jamais",
                  dppac_FR_d7_q5 = "Pas du tout",
                  dppac_FR_d7_q6 = "Pas du tout",
                  dppac_FR_d7_q7 = "Jamais",
                  get_dppac_summary_fr = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_fr <- app$getAllValues()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$getAllValues()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$getAllValues()$export[["score_dppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_fr, 20)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_fr, 44/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_fr, round((150/2*4+147/2)/5, 1))
                
                
       #************
       # Round 2
       #************
       
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_FR_d1_q1 = "Un petit peu (jusqu’à 10 minutes au total)",
                  dppac_FR_d1_q2 = "Très peu",
                  dppac_FR_d1_q3 = "Un petit peu",
                  dppac_FR_d1_q4 = "Rarement",
                  dppac_FR_d1_q5 = "Un petit peu",
                  dppac_FR_d1_q6 = "Un petit peu",
                  dppac_FR_d1_q7 = "Rarement",
                  dppac_FR_d2_q1 = "Un petit peu (jusqu’à 10 minutes au total)",
                  dppac_FR_d2_q2 = "Très peu",
                  dppac_FR_d2_q3 = "Un petit peu",
                  dppac_FR_d2_q4 = "Rarement",
                  dppac_FR_d2_q5 = "Un petit peu",
                  dppac_FR_d2_q6 = "Un petit peu",
                  dppac_FR_d2_q7 = "Rarement",
                  dppac_FR_d3_q1 = "Un petit peu (jusqu’à 10 minutes au total)",
                  dppac_FR_d3_q2 = "Très peu",
                  dppac_FR_d3_q3 = "Un petit peu",
                  dppac_FR_d3_q4 = "Rarement",
                  dppac_FR_d3_q5 = "Un petit peu",
                  dppac_FR_d3_q6 = "Un petit peu",
                  dppac_FR_d3_q7 = "Rarement",
                  dppac_FR_d4_q1 = "Un petit peu (jusqu’à 10 minutes au total)",
                  dppac_FR_d4_q2 = "Très peu",
                  dppac_FR_d4_q3 = "Un petit peu",
                  dppac_FR_d4_q4 = "Rarement",
                  dppac_FR_d4_q5 = "Un petit peu",
                  dppac_FR_d4_q6 = "Un petit peu",
                  dppac_FR_d4_q7 = "Rarement",
                  dppac_FR_d5_q1 = "Un petit peu (jusqu’à 10 minutes au total)",
                  dppac_FR_d5_q2 = "Très peu",
                  dppac_FR_d5_q3 = "Un petit peu",
                  dppac_FR_d5_q4 = "Rarement",
                  dppac_FR_d5_q5 = "Un petit peu",
                  dppac_FR_d5_q6 = "Un petit peu",
                  dppac_FR_d5_q7 = "Rarement",
                  dppac_FR_d6_q1 = "Un petit peu (jusqu’à 10 minutes au total)",
                  dppac_FR_d6_q2 = "Très peu",
                  dppac_FR_d6_q3 = "Un petit peu",
                  dppac_FR_d6_q4 = "Rarement",
                  dppac_FR_d6_q5 = "Un petit peu",
                  dppac_FR_d6_q6 = "Un petit peu",
                  dppac_FR_d6_q7 = "Rarement",
                  dppac_FR_d7_q1 = "Un petit peu (jusqu’à 10 minutes au total)",
                  dppac_FR_d7_q2 = "Très peu",
                  dppac_FR_d7_q3 = "Un petit peu",
                  dppac_FR_d7_q4 = "Rarement",
                  dppac_FR_d7_q5 = "Un petit peu",
                  dppac_FR_d7_q6 = "Un petit peu",
                  dppac_FR_d7_q7 = "Rarement",
                  get_dppac_summary_fr = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_fr <- app$getAllValues()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$getAllValues()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$getAllValues()$export[["score_dppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_fr, 15)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_fr, 54/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_fr, round((125/2*4+122/2)/5, 1))
                
      #************
      # Round 3
      #************
                
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_FR_d1_q1 = "Un peu (jusqu’à 30 minutes au total)",
                  dppac_FR_d1_q2 = "Quelques-unes",
                  dppac_FR_d1_q3 = "Quelques-unes",
                  dppac_FR_d1_q4 = "Quelques fois",
                  dppac_FR_d1_q5 = "Modérément",
                  dppac_FR_d1_q6 = "Modérément",
                  dppac_FR_d1_q7 = "Quelques fois",
                  dppac_FR_d2_q1 = "Un peu (jusqu’à 30 minutes au total)",
                  dppac_FR_d2_q2 = "Quelques-unes",
                  dppac_FR_d2_q3 = "Quelques-unes",
                  dppac_FR_d2_q4 = "Quelques fois",
                  dppac_FR_d2_q5 = "Modérément",
                  dppac_FR_d2_q6 = "Modérément",
                  dppac_FR_d2_q7 = "Quelques fois",
                  dppac_FR_d3_q1 = "Un peu (jusqu’à 30 minutes au total)",
                  dppac_FR_d3_q2 = "Quelques-unes",
                  dppac_FR_d3_q3 = "Quelques-unes",
                  dppac_FR_d3_q4 = "Quelques fois",
                  dppac_FR_d3_q5 = "Modérément",
                  dppac_FR_d3_q6 = "Modérément",
                  dppac_FR_d3_q7 = "Quelques fois",
                  dppac_FR_d4_q1 = "Un peu (jusqu’à 30 minutes au total)",
                  dppac_FR_d4_q2 = "Quelques-unes",
                  dppac_FR_d4_q3 = "Quelques-unes",
                  dppac_FR_d4_q4 = "Quelques fois",
                  dppac_FR_d4_q5 = "Modérément",
                  dppac_FR_d4_q6 = "Modérément",
                  dppac_FR_d4_q7 = "Quelques fois",
                  dppac_FR_d5_q1 = "Un peu (jusqu’à 30 minutes au total)",
                  dppac_FR_d5_q2 = "Quelques-unes",
                  dppac_FR_d5_q3 = "Quelques-unes",
                  dppac_FR_d5_q4 = "Quelques fois",
                  dppac_FR_d5_q5 = "Modérément",
                  dppac_FR_d5_q6 = "Modérément",
                  dppac_FR_d5_q7 = "Quelques fois",
                  dppac_FR_d6_q1 = "Un peu (jusqu’à 30 minutes au total)",
                  dppac_FR_d6_q2 = "Quelques-unes",
                  dppac_FR_d6_q3 = "Quelques-unes",
                  dppac_FR_d6_q4 = "Quelques fois",
                  dppac_FR_d6_q5 = "Modérément",
                  dppac_FR_d6_q6 = "Modérément",
                  dppac_FR_d6_q7 = "Quelques fois",
                  dppac_FR_d7_q1 = "Un peu (jusqu’à 30 minutes au total)",
                  dppac_FR_d7_q2 = "Quelques-unes",
                  dppac_FR_d7_q3 = "Quelques-unes",
                  dppac_FR_d7_q4 = "Quelques fois",
                  dppac_FR_d7_q5 = "Modérément",
                  dppac_FR_d7_q6 = "Modérément",
                  dppac_FR_d7_q7 = "Quelques fois",
                  get_dppac_summary_fr = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_fr <- app$getAllValues()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$getAllValues()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$getAllValues()$export[["score_dppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_fr, 10)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_fr, 64/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_fr, round((117/2*4+113/2)/5, 1))
                
                
    #************
    # Round 4
    #************
                
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_FR_d1_q1 = "Beaucoup (jusqu’à 1 heure au total)",
                  dppac_FR_d1_q2 = "Beaucoup",
                  dppac_FR_d1_q3 = "Beaucoup",
                  dppac_FR_d1_q4 = "Fréquemment",
                  dppac_FR_d1_q5 = "Très",
                  dppac_FR_d1_q6 = "Très",
                  dppac_FR_d1_q7 = "Fréquemment",
                  dppac_FR_d2_q1 = "Beaucoup (jusqu’à 1 heure au total)",
                  dppac_FR_d2_q2 = "Beaucoup",
                  dppac_FR_d2_q3 = "Beaucoup",
                  dppac_FR_d2_q4 = "Fréquemment",
                  dppac_FR_d2_q5 = "Très",
                  dppac_FR_d2_q6 = "Très",
                  dppac_FR_d2_q7 = "Fréquemment",
                  dppac_FR_d3_q1 = "Beaucoup (jusqu’à 1 heure au total)",
                  dppac_FR_d3_q2 = "Beaucoup",
                  dppac_FR_d3_q3 = "Beaucoup",
                  dppac_FR_d3_q4 = "Fréquemment",
                  dppac_FR_d3_q5 = "Très",
                  dppac_FR_d3_q6 = "Très",
                  dppac_FR_d3_q7 = "Fréquemment",
                  dppac_FR_d4_q1 = "Beaucoup (jusqu’à 1 heure au total)",
                  dppac_FR_d4_q2 = "Beaucoup",
                  dppac_FR_d4_q3 = "Beaucoup",
                  dppac_FR_d4_q4 = "Fréquemment",
                  dppac_FR_d4_q5 = "Très",
                  dppac_FR_d4_q6 = "Très",
                  dppac_FR_d4_q7 = "Fréquemment",
                  dppac_FR_d5_q1 = "Beaucoup (jusqu’à 1 heure au total)",
                  dppac_FR_d5_q2 = "Beaucoup",
                  dppac_FR_d5_q3 = "Beaucoup",
                  dppac_FR_d5_q4 = "Fréquemment",
                  dppac_FR_d5_q5 = "Très",
                  dppac_FR_d5_q6 = "Très",
                  dppac_FR_d5_q7 = "Fréquemment",
                  dppac_FR_d6_q1 = "Beaucoup (jusqu’à 1 heure au total)",
                  dppac_FR_d6_q2 = "Beaucoup",
                  dppac_FR_d6_q3 = "Beaucoup",
                  dppac_FR_d6_q4 = "Fréquemment",
                  dppac_FR_d6_q5 = "Très",
                  dppac_FR_d6_q6 = "Très",
                  dppac_FR_d6_q7 = "Fréquemment",
                  dppac_FR_d7_q1 = "Beaucoup (jusqu’à 1 heure au total)",
                  dppac_FR_d7_q2 = "Beaucoup",
                  dppac_FR_d7_q3 = "Beaucoup",
                  dppac_FR_d7_q4 = "Fréquemment",
                  dppac_FR_d7_q5 = "Très",
                  dppac_FR_d7_q6 = "Très",
                  dppac_FR_d7_q7 = "Fréquemment",
                  get_dppac_summary_fr = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_fr <- app$getAllValues()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$getAllValues()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$getAllValues()$export[["score_dppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_fr, 5)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_fr, 74/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_fr, round((116/2*4+107/2)/5, 1))     
                
                
   #************
   # Round 5
   #************
                
                # Setting inputs for D-PPAC tests
                app$setInputs(
                  dppac_FR_d1_q1 = "Enormément (plus d’1 heure au total)",
                  dppac_FR_d1_q2 = "Enormément",
                  dppac_FR_d1_q3 = "Enormément",
                  dppac_FR_d1_q4 = "Tout le temps",
                  dppac_FR_d1_q5 = "Extrêmement",
                  dppac_FR_d1_q6 = "Extrêmement",
                  dppac_FR_d1_q7 = "Tout le temps",
                  dppac_FR_d2_q1 = "Enormément (plus d’1 heure au total)",
                  dppac_FR_d2_q2 = "Enormément",
                  dppac_FR_d2_q3 = "Enormément",
                  dppac_FR_d2_q4 = "Tout le temps",
                  dppac_FR_d2_q5 = "Extrêmement",
                  dppac_FR_d2_q6 = "Extrêmement",
                  dppac_FR_d2_q7 = "Tout le temps",
                  dppac_FR_d3_q1 = "Enormément (plus d’1 heure au total)",
                  dppac_FR_d3_q2 = "Enormément",
                  dppac_FR_d3_q3 = "Enormément",
                  dppac_FR_d3_q4 = "Tout le temps",
                  dppac_FR_d3_q5 = "Extrêmement",
                  dppac_FR_d3_q6 = "Extrêmement",
                  dppac_FR_d3_q7 = "Tout le temps",
                  dppac_FR_d4_q1 = "Enormément (plus d’1 heure au total)",
                  dppac_FR_d4_q2 = "Enormément",
                  dppac_FR_d4_q3 = "Enormément",
                  dppac_FR_d4_q4 = "Tout le temps",
                  dppac_FR_d4_q5 = "Extrêmement",
                  dppac_FR_d4_q6 = "Extrêmement",
                  dppac_FR_d4_q7 = "Tout le temps",
                  dppac_FR_d5_q1 = "Enormément (plus d’1 heure au total)",
                  dppac_FR_d5_q2 = "Enormément",
                  dppac_FR_d5_q3 = "Enormément",
                  dppac_FR_d5_q4 = "Tout le temps",
                  dppac_FR_d5_q5 = "Extrêmement",
                  dppac_FR_d5_q6 = "Extrêmement",
                  dppac_FR_d5_q7 = "Tout le temps",
                  dppac_FR_d6_q1 = "Enormément (plus d’1 heure au total)",
                  dppac_FR_d6_q2 = "Enormément",
                  dppac_FR_d6_q3 = "Enormément",
                  dppac_FR_d6_q4 = "Tout le temps",
                  dppac_FR_d6_q5 = "Extrêmement",
                  dppac_FR_d6_q6 = "Extrêmement",
                  dppac_FR_d6_q7 = "Tout le temps",
                  dppac_FR_d7_q1 = "Enormément (plus d’1 heure au total)",
                  dppac_FR_d7_q2 = "Enormément",
                  dppac_FR_d7_q3 = "Enormément",
                  dppac_FR_d7_q4 = "Tout le temps",
                  dppac_FR_d7_q5 = "Extrêmement",
                  dppac_FR_d7_q6 = "Extrêmement",
                  dppac_FR_d7_q7 = "Tout le temps",
                  get_dppac_summary_fr = "click"
                )
                
                # Getting final scores computed with the app
                mean_score_dppac_diff_fr <- app$getAllValues()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$getAllValues()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$getAllValues()$export[["score_dppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_fr, 0)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_fr, 84/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_fr, round((100/2*4+90/2)/5, 1))                
})      
    
    
    

  
  




