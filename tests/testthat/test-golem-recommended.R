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
# test_that(
#  "app launches",{
#    golem::expect_running(sleep = 5)
#  }
#)


test_that("The server functions correctly work", {

  skip_on_cran() # Running all the tests would take too much time.
  
#===============================================================================
# Setting environment
#===============================================================================

# Building a new environment with variables having to be seen globally for passing tests
test_activAnalyzer_env <- new.env()
test_activAnalyzer_env$Study <- "Study"
test_activAnalyzer_env$Metric <- "Metric"
test_activAnalyzer_env$Score <- "Score"
test_activAnalyzer_env$Date <- "Date"
test_activAnalyzer_env$wear_time <- "wear_time"
test_activAnalyzer_env$validity <- "validity"
test_activAnalyzer_env$col_time_stamp <- "col_time_stamp"
test_activAnalyzer_env$Range <- "Range"
test_activAnalyzer_env$Intervalle <- "Intervalle"
test_activAnalyzer_env$Selected_Day_ID <- "Selected_Day_ID"
test_activAnalyzer_env$Steps_score <- "Steps_score"
test_activAnalyzer_env$VMU_score <- "VMU_score"
test_activAnalyzer_env$Validity <- "Validity"
test_activAnalyzer_env$Item <- "Item"
test_activAnalyzer_env$Day <- "Day"
test_activAnalyzer_env$score_type <- "score_type"
test_activAnalyzer_env$difficulty_score_raw <- "difficulty_score_raw"
test_activAnalyzer_env$difficulty_score_raw_diff <- "difficulty_score_raw_diff"
test_activAnalyzer_env$difficulty_score_raw_quant <- "difficulty_score_raw_quant"
test_activAnalyzer_env$difficulty_score_rasch <- "difficulty_score_rasch"
test_activAnalyzer_env$amount_score_raw <- "amount_score_raw"
test_activAnalyzer_env$amount_score_raw_diff <- "amount_score_raw_diff"
test_activAnalyzer_env$amount_score_raw_quant <- "amount_score_raw_quant"
test_activAnalyzer_env$amount_score_rasch <- "amount_score_rasch"
test_activAnalyzer_env$total_score_raw <- "total_score_raw"
test_activAnalyzer_env$total_score_rasch <- "total_score_rasch"
test_activAnalyzer_env$Jour <- "Jour"
test_activAnalyzer_env$init <- "init"
test_activAnalyzer_env$users <- "users"
test_activAnalyzer_env$mod_control_pa_period_view_server <- "mod_control_pa_period_view_server"
test_activAnalyzer_env$mod_report_pa_period_server <- "mod_report_pa_period_server"
test_activAnalyzer_env$get_pa_period_info <- "get_pa_period_info"
test_activAnalyzer_env$mod_report_pa_period_ui <- "mod_report_pa_period_ui"
test_activAnalyzer_env$mod_report_pa_period_server <- "mod_report_pa_period_server"
test_activAnalyzer_env$total_steps <- "total_steps"
test_activAnalyzer_env$valid_days <- "valid_days"
test_activAnalyzer_env$theme <- "theme"
test_activAnalyzer_env$margin <- "margin"
test_activAnalyzer_env$max_steps_60min <- "max_steps_60min"
test_activAnalyzer_env$max_steps_30min <- "max_steps_30min"
test_activAnalyzer_env$max_steps_20min <- "max_steps_20min"
test_activAnalyzer_env$max_steps_5min <- "max_steps_5min"
test_activAnalyzer_env$max_steps_1min <- "max_steps_1min"
test_activAnalyzer_env$peack_steps_60min <- "peack_steps_60min"
test_activAnalyzer_env$peack_steps_30min <- "peack_steps_30min"
test_activAnalyzer_env$peack_steps_20min <- "peack_steps_20min"
test_activAnalyzer_env$peack_steps_5min <- "peack_steps_5min"
test_activAnalyzer_env$peak_steps_1min <- "peak_steps_1min"
test_activAnalyzer_env$ig <- "ig"
test_activAnalyzer_env$`M1/3` <- "M1/3"
test_activAnalyzer_env$M120 <- "M120"
test_activAnalyzer_env$M60 <- "M60"
test_activAnalyzer_env$M30 <- "M30"
test_activAnalyzer_env$M15 <- "M15"
test_activAnalyzer_env$M5 <- "M5"
test_activAnalyzer_env$element_text <- "element_text"
test_activAnalyzer_env$session <- "session"
test_activAnalyzer_env$outputName <- "outputName"

assign("equations_mets", activAnalyzer:::equations_mets, envir = test_activAnalyzer_env)
assign("mvpa_cutpoints", activAnalyzer:::mvpa_cutpoints, envir = test_activAnalyzer_env)
assign("sed_cutpoints", activAnalyzer:::sed_cutpoints, envir = test_activAnalyzer_env)

# Attaching new environment so that variables defined above as global can be located during testing
attach(test_activAnalyzer_env)

#===============================================================================
# Creating app object
#===============================================================================
     
# Creating shinyDriver object
app <- shinytest2::AppDriver$new(
  run_app(),
  options = list(test.mode = TRUE),
  timeout = 60 * 1000,
  height = 1024,
  width = 768
)
  
#===============================================================================
# Loading data file inside and outside the app
#===============================================================================

test_file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
app$upload_file(upload = test_file)
 
#===============================================================================
# Testing auto-filling patient information
#===============================================================================
    
# Setting reference inputs
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

# Setting inputs in the app
app$set_inputs(auto_fill_char = "click")
    
# Getting actual inputs in the app
actual_list <-
  list(
    app$get_values()$export[["assessor_name"]],
    app$get_values()$export[["assessor_surname"]],
    app$get_values()$export[["patient_name"]],
    app$get_values()$export[["patient_surname"]],
    app$get_values()$export[["sex"]],
    app$get_values()$export[["age"]],
    app$get_values()$export[["weight"]],
    app$get_values()$export[["side"]]
  )

# Testing
expect_equal(actual_list, test_list)

#===============================================================================
# Testing dataframe marked for wear time
#===============================================================================

# Test 1
      # Setting reference results
      test_df <- 
        prepare_dataset(data = test_file) %>%
        mark_wear_time(
          to_epoch = 60,
          cts  = "axis1", 
          frame = 60, 
          allowanceFrame = 1, 
          streamFrame = 20
        ) 
      
       # Setting inputs in the app
       app$set_inputs(
           to_epoch = 60,
           axis_weartime = "vertical axis", 
           frame_size = 60, 
           allowanceFrame_size = 1, 
           streamFrame_size = 20
          )
          app$set_inputs(validate = "click")
          actual_df <- app$get_values()$export[["df"]]
        

       # Testing    
         expect_equal(actual_df, test_df)
     
# Test 2
      # Setting reference results
         actual_df <- app$get_values()$export[["df"]]
         test_df <- 
           prepare_dataset(data = test_file) %>%
           mark_wear_time(
             to_epoch = 60,
             cts  = "vm", 
             frame = 30, 
             allowanceFrame = 0,
             streamFrame = 0
           )
         
       # Setting inputs in the app  
         app$set_inputs(
           to_epoch = 60,
           axis_weartime = "vector magnitude", 
           frame_size = 30, 
           allowanceFrame_size = 0, 
           streamFrame_size = 0
           )
         app$set_inputs(validate = "click")
       
        # Testing    
          expect_equal(actual_df, test_df)
       
#================================================================================
# Testing for resetting inputs for the configuration of nonwear/wear time analysis
#================================================================================
  
# Setting reference results
test_list <-
  list(
    60,
    "vector magnitude",
    90,
    2,
    30
  )  

# Setting inputs in the app
app$set_inputs(reset_nonwear = "click")
actual_list <-
 list(
   app$get_values()$export[["to_epoch"]],
   app$get_values()$export[["axis_weartime"]],
   app$get_values()$export[["frame_size"]],
   app$get_values()$export[["allowanceFrame_size"]],
   app$get_values()$export[["streamFrame_size"]]
   )

# Testing    
expect_equal(actual_list, test_list)
       
#===================================================================================
# Testing plot showing nonwear/wear time after clicking the "Validate configuration"
# button for the first time
#===================================================================================

# Setting reference results
test_gg_nonwear <- str(plot_data(test_df))

# Setting inputs in the app
app$set_inputs(validate = "click")
actual_gg_nonwear <- str(app$get_values()$export[["gg_plot_data_init"]])

# Testing    
expect_equal(actual_gg_nonwear, test_gg_nonwear)

#==================================================================================
# Testing plot showing nonwear/wear time after clicking the "Update graphic" button 
#==================================================================================

# Setting reference results
test_gg_nonwear <- 
  str(
    plot_data(
      data = test_df,
      metric = "vm",
      zoom_from = "10:00:00",
      zoom_to = "20:00:00"
    )
  )

# Setting inputs in the app
app$set_inputs(
         Metric = "vm", 
         zoom_from_weartime = "10:00:00", 
         zoom_to_weartime = "20:00:00", 
         update_graphic = "click"
         )
actual_gg_nonwear <- str(app$get_values()$export[["gg_plot_data_update"]])

# Testing 
expect_equal(actual_gg_nonwear, test_gg_nonwear)  

#====================================================================================
 # Testing plot showing nonwear/wear time after clicking the "Validate configuration"
 # button after using the "Update graphic" button
#====================================================================================

# Setting reference results
test_gg_nonwear <- str(plot_data(test_df))

# Setting inputs in the app
app$set_inputs(validate = "click")
actual_gg_nonwear <- str(app$get_values()$export[["gg_plot_data_init"]])

# Testing 
expect_equal(actual_gg_nonwear, test_gg_nonwear)

#===============================================================================
# Testing auto-filling activity intensity analyzis
#===============================================================================

# Setting reference results
test_list <-
  list(
    "Santos-Lozano et al. (2013) [Older adults]",
    "Aguilar-Farias et al. (2014) [Older adults]",
    "Santos-Lozano et al. (2013) [Older adults]"
    
  )

# Setting inputs in the app
app$set_inputs(
         equation_mets = "Santos-Lozano et al. (2013) [Older adults]",
         sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]",
         mvpa_cutpoint = "Santos-Lozano et al. (2013) [Older adults]"
       )
app$set_inputs(auto_fill_intensity = "click")
actual_list <-
         list(
           app$get_values()$export[["equation_mets"]],
           app$get_values()$export[["sed_cutpoint"]],
           app$get_values()$export[["mvpa_cutpoint"]]
         )
# Testing 
expect_equal(actual_list, test_list)

       
#===============================================================================
# Testing dataframe with results by day
#===============================================================================

# Test 1 / Equation: Sasaki et al. (2011) [Adults]; MVPA cut-points: Sasaki et al. (2011) [Adults];
# SED cutpoints: Aguilar-Farias et al. (2014) [Older adults]

       # Setting reference results
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
        
        # Setting inputs in the app
        app$set_inputs(
            age = 32,
            weight = 67,
            sex = "male",
            equation_mets = "Sasaki et al. (2011) [Adults]",
            mvpa_cutpoint = "Sasaki et al. (2011) [Adults]",
            sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
         app$set_inputs(Run = "click")
         actual_results_by_day <- app$get_values()$export[["results_by_day"]]
           
         # Testing 
         expect_equal(actual_results_by_day, test_results_by_day$df_all_metrics)
           
# Test 2 / Equation: Santos-Lozano et al. (2013) [Older adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Older adults];
# SED cutpoints: Aguilar-Farias et al. (2014) [Older adults] ; sex: intersex
    
         # Setting reference results
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
             sex = "intersex"
           ) %>%
           recap_by_day( 
             age = 64, 
             weight = 67,
             sex = "intersex"
           )
         
         # Setting inputs the app
           app$set_inputs(
             age = 64,
             weight = 67,
             sex = "intersex",
             equation_mets = "Santos-Lozano et al. (2013) [Older adults]",
             mvpa_cutpoint = "Santos-Lozano et al. (2013) [Older adults]",
             sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
           app$set_inputs(Run = "click")
           actual_results_by_day <- app$get_values()$export[["results_by_day"]]
           
        # Testing 
        expect_equal(actual_results_by_day, test_results_by_day$df_all_metrics)
           
           
# Test 3 / Equation: Santos-Lozano et al. (2013) [Older adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Older adults];
# SED cutpoints: Aguilar-Farias et al. (2014) [Older adults]; sex: prefer not to say      
 
        # Setting reference results
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
            sex = "prefer not to say"
          ) %>%
          recap_by_day( 
            age = 64, 
            weight = 67,
            sex = "prefer not to say"
          )
        
          # Setting inputs in the app
           app$set_inputs(
             age = 64,
             weight = 67,
             sex = "prefer not to say",
             equation_mets = "Santos-Lozano et al. (2013) [Older adults]",
             mvpa_cutpoint = "Santos-Lozano et al. (2013) [Older adults]",
             sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
           app$set_inputs(Run = "click")
           actual_results_by_day <- app$get_values()$export[["results_by_day"]]
           
          # Testing 
          expect_equal(actual_results_by_day, test_results_by_day$df_all_metrics)
           
# Test 4 / Equation: Santos-Lozano et al. (2013) [Older adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Older adults];
# SED cut-point: Aguilar-Farias et al. (2014) [Older adults]; sex: male

          # Setting reference results
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
          
          # Setting inputs in the app
          app$set_inputs(
            age = 64,
            weight = 67,
            sex = "male",
            equation_mets = "Santos-Lozano et al. (2013) [Older adults]",
            mvpa_cutpoint = "Santos-Lozano et al. (2013) [Older adults]",
            sed_cutpoint = "Aguilar-Farias et al. (2014) [Older adults]")
          app$set_inputs(Run = "click")
          actual_results_by_day <- app$get_values()$export[["results_by_day"]]
         
         # Testing  
         expect_equal(actual_results_by_day, test_results_by_day$df_all_metrics)
         
# Test 5 / Equation: Santos-Lozano et al. (2013) [Adults]; MVPA cut-points: Santos-Lozano et al. (2013) [Adults];
# SED cut-point: Personalized
 
         # Setting reference results
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
         
         # Setting inputs in the app
         app$set_inputs(
           age = 47,
           weight = 78,
           sex = "undefined",
           equation_mets = "Santos-Lozano et al. (2013) [Adults]",
           mvpa_cutpoint = "Santos-Lozano et al. (2013) [Adults]",
           sed_cutpoint = "Personalized...",
           perso_sed_axis = "vector magnitude",
           perso_sed_cutpoint = 150
           )
         app$set_inputs(Run = "click")
         actual_results_by_day <- app$get_values()$export[["results_by_day"]]
         
         # Testing  
         expect_equal(actual_results_by_day, test_results_by_day$df_all_metrics)
         
# Test 6 / Equation: Freedson et al. (1998) [Adults]; MVPA cut-points: Personalized;
# SED cut-point: Personalized

        # Setting reference results
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
         
        # Setting inputs in the app
        app$set_inputs(
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
        app$set_inputs(Run = "click")
        actual_results_by_day <- app$get_values()$export[["results_by_day"]]
        
        # Testing  
        expect_equal(actual_results_by_day, test_results_by_day$df_all_metrics)

#=========================================================================================
# Testing for setting default configuration to validate a day (period and wear time hours)
#=========================================================================================

# Setting reference results
test_set_default <-
 list(
   "00:00:00",
   "23:59:59",
   10
 )

# Setting inputs in the app
app$set_inputs(reset_period = "click")
actual_set_default <- 
 list(
   app$get_values()$export[["start_day_analysis"]],
   app$get_values()$export[["end_day_analysis"]],
   app$get_values()$export[["minimum_wear_time_for_analysis"]]
 )
# Testing 
expect_equal(test_set_default, actual_set_default)

#===============================================================================
# Testing for setting proactive period (non-sleep wearing protocol)
#===============================================================================
       
# Setting reference results
test_set_proactive <-
          list(
            "00:00:00",
            "23:59:59",
            8
          )
# Setting inputs in the app
app$set_inputs(pro_active_period_non_sleep = "click")
actual_set_proactive <- 
  list(
    app$get_values()$export[["start_day_analysis"]],
    app$get_values()$export[["end_day_analysis"]],
    app$get_values()$export[["minimum_wear_time_for_analysis"]]
  )

# Testing 
expect_equal(actual_set_proactive, test_set_proactive)
       
#===============================================================================
# Testing for setting proactive period (24-h wearing protocol)
#===============================================================================

# Setting reference results
test_set_proactive <-
  list(
    "07:00:00",
    "22:00:00",
    8
  )

# Setting inputs in the app
app$set_inputs(pro_active_period_24h = "click")
actual_set_proactive <- 
 list(
   app$get_values()$export[["start_day_analysis"]],
   app$get_values()$export[["end_day_analysis"]],
   app$get_values()$export[["minimum_wear_time_for_analysis"]]
 )
      
# Testing 
expect_equal(actual_set_proactive, test_set_proactive)

#===========================================================================================
# Testing plot showing PA intensity marks after clicking the "Run" button for the first time
#===========================================================================================

# Setting reference results
test_results <-
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
        ) 

# Setting inputs in the app
app$set_inputs(Run = "click")
test_gg_nonwear <- str(plot_data_with_intensity(
        test_results
        ))
actual_gg_nonwear <- str(app$get_values()$export[["gg_plot_data_int_init"]])

# Testing 
expect_equal(actual_gg_nonwear, test_gg_nonwear)

#===================================================================================
# Testing plot showing PA intensity marks after clicking the "Update graphic" button
#===================================================================================

# Setting reference results
test_gg_nonwear <- 
  str(
    plot_data_with_intensity(
      data = test_results,
      metric = "vm",
      zoom_from = "10:00:00",
      zoom_to = "20:00:00"
    )
  )

# Setting inputs in the app
app$set_inputs(
        Metric2 = "vm", 
        zoom_from_analysis = "10:00:00", 
        zoom_to_analysis = "20:00:00", 
        update_graphic2 = "click"
        )
actual_gg_nonwear <- str(app$get_values()$export[["gg_plot_data_int_update"]])
      
# Testing 
expect_equal(actual_gg_nonwear, test_gg_nonwear)  

#========================================================================================
# Testing plot showing PA intensity marks after clicking the "Run" button after using the 
# "Update graphic" button
#========================================================================================

# Setting reference results
test_results_summary_means <- 
  (test_df %>%
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
    valid_wear_time_end = "22:00:00"))$df_all_metrics %>%
  average_results(minimum_wear_time = 12, fun = "mean")
test_gg_nonwear <- str(plot_data(test_results))

# Setting inputs in the app
app$set_inputs(Run = "click")
  actual_gg_nonwear <- str(app$get_values()$export[["gg_plot_data_int_init"]])

# Testing 
expect_equal(actual_gg_nonwear, test_gg_nonwear)    

#===============================================================================
# Testing dataframe with daily summary
#===============================================================================

# Setting reference results
test_results_summary_medians <- 
  (test_df %>%
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
    valid_wear_time_end = "22:00:00"))$df_all_metrics %>%
  average_results(minimum_wear_time = 12, fun = "median")


# Setting inputs in the app
app$set_inputs(minimum_wear_time_for_analysis = 12)
app$set_inputs(Run = "click")
       
  # With means
    actual_results_summary_means <- app$get_values()$export[["results_summary_means"]]
  
  # With medians
    actual_results_summary_medians <- app$get_values()$export[["results_summary_medians"]]
  
# Testing 
expect_equal(actual_results_summary_means, test_results_summary_means)      
expect_equal(actual_results_summary_medians, test_results_summary_medians)
         
#===============================================================================
# Testing BMR computation
#===============================================================================
test_bmr <- 9.74 * 78 + 694
actual_bmr <- app$get_values()$export[["BMR"]]
expect_equal(actual_bmr, test_bmr)
   
#===============================================================================
# Testing computed PROactive scores
#===============================================================================

      # Setting inputs for all tests
        app$set_inputs(
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
              app$set_inputs(
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
             score_cppac_diff_en <- app$get_values()$export[["score_cppac_diff_en"]]
             score_cppac_quant_en <- app$get_values()$export[["score_cppac_quant_en"]]
             score_cppac_tot_rasch_en <- app$get_values()$export[["score_cppac_tot_rasch_en"]]
             
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
           app$set_inputs(
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
             score_cppac_diff_en <- app$get_values()$export[["score_cppac_diff_en"]]
             score_cppac_quant_en <- app$get_values()$export[["score_cppac_quant_en"]]
             score_cppac_tot_rasch_en <- app$get_values()$export[["score_cppac_tot_rasch_en"]]
           
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
              app$set_inputs(
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
              score_cppac_diff_en <- app$get_values()$export[["score_cppac_diff_en"]]
              score_cppac_quant_en <- app$get_values()$export[["score_cppac_quant_en"]]
              score_cppac_tot_rasch_en <- app$get_values()$export[["score_cppac_tot_rasch_en"]]
            
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
                app$set_inputs(
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
                score_cppac_diff_en <- app$get_values()$export[["score_cppac_diff_en"]]
                score_cppac_quant_en <- app$get_values()$export[["score_cppac_quant_en"]]
                score_cppac_tot_rasch_en <- app$get_values()$export[["score_cppac_tot_rasch_en"]]
              
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
                app$set_inputs(
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
                score_cppac_diff_en <- app$get_values()$export[["score_cppac_diff_en"]]
                score_cppac_quant_en <- app$get_values()$export[["score_cppac_quant_en"]]
                score_cppac_tot_rasch_en <- app$get_values()$export[["score_cppac_tot_rasch_en"]]
                
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
                  app$set_inputs(
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
                  score_cppac_diff_fr <- app$get_values()$export[["score_cppac_diff_fr"]]
                  score_cppac_quant_fr <- app$get_values()$export[["score_cppac_quant_fr"]]
                  score_cppac_tot_rasch_fr <- app$get_values()$export[["score_cppac_tot_rasch_fr"]]
                
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
                app$set_inputs(
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
                  score_cppac_diff_fr <- app$get_values()$export[["score_cppac_diff_fr"]]
                  score_cppac_quant_fr <- app$get_values()$export[["score_cppac_quant_fr"]]
                  score_cppac_tot_rasch_fr <- app$get_values()$export[["score_cppac_tot_rasch_fr"]]
                
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
                  app$set_inputs(
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
                score_cppac_diff_fr <- app$get_values()$export[["score_cppac_diff_fr"]]
                score_cppac_quant_fr <- app$get_values()$export[["score_cppac_quant_fr"]]
                score_cppac_tot_rasch_fr <- app$get_values()$export[["score_cppac_tot_rasch_fr"]]
                
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
                  app$set_inputs(
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
                score_cppac_diff_fr <- app$get_values()$export[["score_cppac_diff_fr"]]
                score_cppac_quant_fr <- app$get_values()$export[["score_cppac_quant_fr"]]
                score_cppac_tot_rasch_fr <- app$get_values()$export[["score_cppac_tot_rasch_fr"]]
                
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
                  app$set_inputs(
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
                  score_cppac_diff_fr <- app$get_values()$export[["score_cppac_diff_fr"]]
                  score_cppac_quant_fr <- app$get_values()$export[["score_cppac_quant_fr"]]
                  score_cppac_tot_rasch_fr <- app$get_values()$export[["score_cppac_tot_rasch_fr"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_en <- app$get_values()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$get_values()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$get_values()$export[["score_dppac_tot_rasch_en"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_en <- app$get_values()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$get_values()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$get_values()$export[["score_dppac_tot_rasch_en"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_en <- app$get_values()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$get_values()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$get_values()$export[["score_dppac_tot_rasch_en"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_en <- app$get_values()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$get_values()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$get_values()$export[["score_dppac_tot_rasch_en"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_en <- app$get_values()$export[["score_dppac_diff_en"]]
                mean_score_dppac_quant_en <- app$get_values()$export[["score_dppac_quant_en"]]
                mean_score_dppac_tot_rasch_en <- app$get_values()$export[["score_dppac_tot_rasch_en"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_fr <- app$get_values()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$get_values()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$get_values()$export[["score_dppac_tot_rasch_fr"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_fr <- app$get_values()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$get_values()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$get_values()$export[["score_dppac_tot_rasch_fr"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_fr <- app$get_values()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$get_values()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$get_values()$export[["score_dppac_tot_rasch_fr"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_fr <- app$get_values()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$get_values()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$get_values()$export[["score_dppac_tot_rasch_fr"]]
                
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
                app$set_inputs(
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
                mean_score_dppac_diff_fr <- app$get_values()$export[["score_dppac_diff_fr"]]
                mean_score_dppac_quant_fr <- app$get_values()$export[["score_dppac_quant_fr"]]
                mean_score_dppac_tot_rasch_fr <- app$get_values()$export[["score_dppac_tot_rasch_fr"]]
                
                # Testing Difficulty score (raw)
                expect_equal(mean_score_dppac_diff_fr, 0)
                
                # Testing Amount score (raw)
                expect_equal(mean_score_dppac_quant_fr, 84/5)
                
                # Testing Total score (rasch)
                expect_equal(mean_score_dppac_tot_rasch_fr, round((100/2*4+90/2)/5, 1))
                
 
#=============================================================================
# Testing results obtained when setting additionnal PA periods (self-reported)
#=============================================================================

# Table of inputs should initially have 0 line                        
recap_pa_perdiods <- app$get_values()$export[["recap_pa_perdiods"]]
n <- nrow(recap_pa_perdiods)                
expect_equal(n, 0)

# Setting reference inputs
test_bmr <- 9.74 * 78 + 694

ref_inputs_tab <- 
  data.frame(
    date = c(rep("2021-04-08", 4), rep("2021-04-09", 4), rep("2021-04-10", 4), rep("2021-04-11", 3)),
    start_hh_num = c(rep(c(0, 1, 2, 3), 3), c(0, 1, 2)),
    start_mm_num = rep(31, 15),
    end_hh_num = c(rep(c(1, 2, 3, 4), 3), c(1, 2, 3)),
    end_mm_num = rep(30, 15),
    METS = c(rep(1, 4), rep(2, 4), rep(3, 4), rep(8, 3))
  ) %>%
  dplyr::mutate(
    start = hms::as_hms(start_hh_num*3600 + start_mm_num*60),
    end = hms::as_hms(end_hh_num*3600 + end_mm_num*60)
  ) %>%
  dplyr::select(date, start, end, METS)

test_results_summary <- 
  (test_df %>%
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
    sex = "female"
    ))$df_all_metrics %>%
  dplyr::mutate(
    wear_time = dplyr::case_when(
      date == "2021-04-07" ~ wear_time,
      date == "2021-04-08" ~ wear_time + 4*60,
      date == "2021-04-09" ~ wear_time + 4*60,
      date == "2021-04-10" ~ wear_time + 4*60,
      date == "2021-04-11" ~ wear_time + 3*60,
      date == "2021-04-12" ~ wear_time
    ),
    total_kcal = dplyr::case_when(
      date == "2021-04-07" ~ total_kcal,
      date == "2021-04-08" ~ total_kcal,
      date == "2021-04-09" ~ total_kcal - 4*60*(test_bmr/24/60) + 4*60*2*(test_bmr/24/60),
      date == "2021-04-10" ~ total_kcal - 4*60*(test_bmr/24/60) + 4*60*3*(test_bmr/24/60),
      date == "2021-04-11" ~ total_kcal - 3*60*(test_bmr/24/60) + 3*60*8*(test_bmr/24/60),
      date == "2021-04-12" ~ total_kcal
    ),
    minutes_SED = dplyr::case_when(
      date == "2021-04-07" ~ minutes_SED,
      date == "2021-04-08" ~ minutes_SED + 4*60,
      date == "2021-04-09" ~ minutes_SED,
      date == "2021-04-10" ~ minutes_SED,
      date == "2021-04-11" ~ minutes_SED,
      date == "2021-04-12" ~ minutes_SED
    ),
    minutes_LPA = dplyr::case_when(
      date == "2021-04-07" ~ minutes_LPA,
      date == "2021-04-08" ~ minutes_LPA,
      date == "2021-04-09" ~ minutes_LPA + 4*60,
      date == "2021-04-10" ~ minutes_LPA,
      date == "2021-04-11" ~ minutes_LPA,
      date == "2021-04-12" ~ minutes_LPA
    ),
    minutes_MPA = dplyr::case_when(
      date == "2021-04-07" ~ minutes_MPA,
      date == "2021-04-08" ~ minutes_MPA,
      date == "2021-04-09" ~ minutes_MPA,
      date == "2021-04-10" ~ minutes_MPA + 4*60,
      date == "2021-04-11" ~ minutes_MPA,
      date == "2021-04-12" ~ minutes_MPA
    ),
    minutes_VPA = dplyr::case_when(
      date == "2021-04-07" ~ minutes_VPA,
      date == "2021-04-08" ~ minutes_VPA,
      date == "2021-04-09" ~ minutes_VPA,
      date == "2021-04-10" ~ minutes_VPA,
      date == "2021-04-11" ~ minutes_VPA + 3*60,
      date == "2021-04-12" ~ minutes_VPA
    ),
    mets_hours_mvpa = dplyr::case_when(
      date == "2021-04-07" ~ mets_hours_mvpa,
      date == "2021-04-08" ~ mets_hours_mvpa,
      date == "2021-04-09" ~ mets_hours_mvpa,
      date == "2021-04-10" ~ mets_hours_mvpa + 4*60/60*3,
      date == "2021-04-11" ~ mets_hours_mvpa + 3*60/60*8,
      date == "2021-04-12" ~ mets_hours_mvpa
    ),
    total_kcal = round(total_kcal, 2)
      
  ) %>%
  dplyr::select(
    date, 
    wear_time, 
    total_counts_axis1,
    total_counts_vm,
    total_steps,
    total_kcal, 
    minutes_SED, 
    minutes_LPA, 
    minutes_MPA,
    minutes_VPA,
    max_steps_60min,
    max_steps_30min,
    max_steps_20min,
    max_steps_5min,
    max_steps_1min,
    peak_steps_60min,
    peak_steps_30min,
    peak_steps_20min,
    peak_steps_5min,
    peak_steps_1min,
    mets_hours_mvpa
    )


# Getting inputs from the app
app$set_inputs(
  
  # Period 1
  `period_1-corr_date` = "2021-04-08", 
  `period_1-corr_start_time_hh` = 0, 
  `period_1-corr_start_time_mm` = 31,
  `period_1-corr_end_time_hh` = 1,
  `period_1-corr_end_time_mm` = 30,
  `period_1-corr_mets` = 1,

  # Period 2
  `period_2-corr_date` = "2021-04-08", 
  `period_2-corr_start_time_hh` = 1, 
  `period_2-corr_start_time_mm` = 31,
  `period_2-corr_end_time_hh` = 2,
  `period_2-corr_end_time_mm` = 30,
  `period_2-corr_mets` = 1,
  
  # Period 3
  `period_3-corr_date` = "2021-04-08", 
  `period_3-corr_start_time_hh` = 2, 
  `period_3-corr_start_time_mm` = 31,
  `period_3-corr_end_time_hh` = 3,
  `period_3-corr_end_time_mm` = 30,
  `period_3-corr_mets` = 1,
  
  # period 4
  `period_4-corr_date` = "2021-04-08", 
  `period_4-corr_start_time_hh` = 3, 
  `period_4-corr_start_time_mm` = 31,
  `period_4-corr_end_time_hh` = 4,
  `period_4-corr_end_time_mm` = 30,
  `period_4-corr_mets` = 1,
  
  # period 5
  `period_5-corr_date` = "2021-04-09", 
  `period_5-corr_start_time_hh` = 0, 
  `period_5-corr_start_time_mm` = 31,
  `period_5-corr_end_time_hh` = 1,
  `period_5-corr_end_time_mm` = 30,
  `period_5-corr_mets` = 2,
  
  # period 6
  `period_6-corr_date` = "2021-04-09", 
  `period_6-corr_start_time_hh` = 1, 
  `period_6-corr_start_time_mm` = 31,
  `period_6-corr_end_time_hh` = 2,
  `period_6-corr_end_time_mm` = 30,
  `period_6-corr_mets` = 2,
  
  # period 7
  `period_7-corr_date` = "2021-04-09", 
  `period_7-corr_start_time_hh` = 2, 
  `period_7-corr_start_time_mm` = 31,
  `period_7-corr_end_time_hh` = 3,
  `period_7-corr_end_time_mm` = 30,
  `period_7-corr_mets` = 2,
  
  # period 8
  `period_8-corr_date` = "2021-04-09", 
  `period_8-corr_start_time_hh` = 3, 
  `period_8-corr_start_time_mm` = 31,
  `period_8-corr_end_time_hh` = 4,
  `period_8-corr_end_time_mm` = 30,
  `period_8-corr_mets` = 2,
  
  # period 9
  `period_9-corr_date` = "2021-04-10", 
  `period_9-corr_start_time_hh` = 0, 
  `period_9-corr_start_time_mm` = 31,
  `period_9-corr_end_time_hh` = 1,
  `period_9-corr_end_time_mm` = 30,
  `period_9-corr_mets` = 3,
  
  # period 10
  `period_10-corr_date` = "2021-04-10", 
  `period_10-corr_start_time_hh` = 1, 
  `period_10-corr_start_time_mm` = 31,
  `period_10-corr_end_time_hh` = 2,
  `period_10-corr_end_time_mm` = 30,
  `period_10-corr_mets` = 3,
  
  # period 11
  `period_11-corr_date` = "2021-04-10", 
  `period_11-corr_start_time_hh` = 2, 
  `period_11-corr_start_time_mm` = 31,
  `period_11-corr_end_time_hh` = 3,
  `period_11-corr_end_time_mm` = 30,
  `period_11-corr_mets` = 3,
  
  # period 12
  `period_12-corr_date` = "2021-04-10", 
  `period_12-corr_start_time_hh` = 3, 
  `period_12-corr_start_time_mm` = 31,
  `period_12-corr_end_time_hh` = 4,
  `period_12-corr_end_time_mm` = 30,
  `period_12-corr_mets` = 3,
  
  # period 13
  `period_13-corr_date` = "2021-04-11", 
  `period_13-corr_start_time_hh` = 0, 
  `period_13-corr_start_time_mm` = 31,
  `period_13-corr_end_time_hh` = 1,
  `period_13-corr_end_time_mm` = 30,
  `period_13-corr_mets` = 8,
  
  # period 14
  `period_14-corr_date` = "2021-04-11", 
  `period_14-corr_start_time_hh` = 1, 
  `period_14-corr_start_time_mm` = 31,
  `period_14-corr_end_time_hh` = 2,
  `period_14-corr_end_time_mm` = 30,
  `period_14-corr_mets` = 8,
  
  # period 15
  `period_15-corr_date` = "2021-04-11", 
  `period_15-corr_start_time_hh` = 2, 
  `period_15-corr_start_time_mm` = 31,
  `period_15-corr_end_time_hh` = 3,
  `period_15-corr_end_time_mm` = 30,
  `period_15-corr_mets` = 8,
  
  # other inputs
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
    perso_sed_cutpoint = 100,
    reset_period = "click",
  
  # Click on the Run button
  Run = "click"
)

recap_pa_perdiods <- app$get_values()$export[["recap_pa_perdiods"]]
actual_results_summary <- app$get_values()$export[["results_by_day"]] %>%
  dplyr::select(
    date, 
    wear_time, 
    total_counts_axis1,
    total_counts_vm,
    total_steps,
    total_kcal, 
    minutes_SED, 
    minutes_LPA, 
    minutes_MPA,
    minutes_VPA,
    max_steps_60min,
    max_steps_30min,
    max_steps_20min,
    max_steps_5min,
    max_steps_1min,
    peak_steps_60min,
    peak_steps_30min,
    peak_steps_20min,
    peak_steps_5min,
    peak_steps_1min,
    mets_hours_mvpa
  )

# Testing correct inputs
expect_equal(recap_pa_perdiods, ref_inputs_tab)

# Testing correct results summary
expect_equal(round(actual_results_summary %>% dplyr::select(-date), 0), round(test_results_summary %>% dplyr::select(-date), 0))


                
#===============================================================================
# END
#===============================================================================

# Detaching the environment attached at the begining of the file
  detach(test_activAnalyzer_env)
                
})      
    
    

  
  




