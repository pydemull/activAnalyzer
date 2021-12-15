#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  ######################
  # Managing app closing ----
  ######################
  
  # Increasing users count when starting new session
  isolate({users$count <- users$count + 1 
  })
  
  # Decreasing users count when closing session
  # Stopping app when count is 0 without planned reloading
  session$onSessionEnded(function() {
    isolate({
      users$count = users$count - 1
      if (users$count == 0 && is.null(input$ok)) stopApp()
    })
  })
  
  ##############################
  # Uploading and preparing data ----
  ##############################

  # Getting data file without modification (required for extracting device attributes 
  # information when generating the report)
    file <- reactive({
      
      req(input$upload)
      actigraph.sleepr::read_agd(input$upload$datapath)
      
    })
    
  # Getting reactive dataset
    data <- reactive({
      
      req(input$upload)
      
      if(!(
           "axis1" %in% names(file()) &&
           "axis2" %in% names(file()) &&
           "axis3" %in% names(file()) &&
           "steps" %in% names(file()) &&
           "inclinestanding" %in% names(file()) &&
           "inclinesitting" %in% names(file()) &&
           "inclinelying" %in% names(file()))
         ){
        validate("Please load an .agd file with at least the following variables: axis1, axis2, axis3, steps, inclinestanding, inclinesitting, inclinelying")
      }
      
      prepare_dataset(data = input$upload$datapath)
      
    })
  
  ###########################################################################################
  # Getting dataframe with marks for wear/nonwear time when clicking on the "Validate" button ----
  ###########################################################################################
  
  # Controlling for correct inputs
  
    # File input
      observeEvent(input$validate,
                   shinyFeedback::feedbackWarning(
                     "upload", 
                     ((tools::file_ext(input$upload$name) == "agd") == FALSE),
                     "Invalid file format. Please choose an .agd file."
                   )
      )
    
    # Period selected to analyze data
      observeEvent(input$validate,
                 shinyFeedback::feedbackWarning(
                   "start_day_analysis", 
                   input$end_day_analysis <= input$start_day_analysis,
                   "End time should be superior to start time."
                 )
    )
      
      observeEvent(input$validate,
                   shinyFeedback::feedbackWarning(
                     "end_day_analysis", 
                     input$end_day_analysis <= input$start_day_analysis,
                     "End time should be superior to start time."
                   )
      )
      
    # Setting PROactive default values for the period to analyse 
      observeEvent(input$pro_active_period, {
        updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(60*60*7))
        updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*20))
      })
    
    
    # Frame size
      observeEvent(input$validate,
                   shinyFeedback::feedbackWarning(
                     "frame_size", 
                     (is.numeric(input$frame_size) == FALSE | input$frame_size < 0),
                     "Please choose a number >= 0."
                   )
      )
    
    # Allowance frame size
      observeEvent(input$validate,
                   shinyFeedback::feedbackWarning(
                     "allowanceFrame_size", 
                     (is.numeric(input$allowanceFrame_size) == FALSE | input$allowanceFrame_size < 0),
                     "Please choose a number >= 0."
                   )
      )
    
    # Stream frame size
      observeEvent(input$validate,
                   shinyFeedback::feedbackWarning(
                     "streamFrame_size", 
                     (is.numeric(input$streamFrame_size) == FALSE | input$streamFrame_size < 0),
                     "Please choose a number >= 0."
                   )
      )
    
  # Building reactive dataframe marked for nonwear/wear time
  
    df <- eventReactive(input$validate, {
      
      # Waiting for required conditions 
      req(tools::file_ext(input$upload$name) == "agd" & 
            is.numeric(input$frame_size) & 
            input$frame_size >= 0 & 
            is.numeric(input$allowanceFrame_size) & 
            input$allowanceFrame_size >= 0 &
            input$streamFrame_size >= 0 &
            input$end_day_analysis > input$start_day_analysis
          )
      
     # Setting the axis to be used for detecting nonwear time
       if (input$axis_weartime == "vector magnitude") {  
         cts <- "vm"
       } else {
         cts <- "axis1"
       }
     
     # Creating reactive dataframe
       df <- 
         mark_wear_time(dataset = data(),
                        cts = cts, 
                        frame = input$frame_size, 
                        allowanceFrame = input$allowanceFrame_size) %>%
         dplyr::filter(time >= hms::as_hms(input$start_day_analysis) & time <= hms::as_hms(input$end_day_analysis))
         
       return(df)
      
      
      })
  

  # Returning to default values for the wear time detection algorithm
    observeEvent(input$reset_nonwear, {
      axis_weartime <- c("vector magnitude", "vertical axis")
      updateSelectInput(inputId = "axis_weartime", choices = axis_weartime)
      updateNumericInput(inputId = "frame_size", value = 90)
      updateNumericInput(inputId = "allowanceFrame_size", value = 2)
      updateNumericInput(inputId = "streamFrame_size", value = 30)
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(0))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*23+60*59))
    })
    
  
  ########################################
  # Visualizing all data with nonwear time ----
  ########################################
  
  output$graph <- renderPlot({
    plot_data(data = df(), metric = input$Metric)
  }, height = function(){nlevels(as.factor(df()$date)) * 90}, res = 120)
  
  ###################################################
  # Getting results when clicking on the "Run" button ----
  ###################################################
  
  # Controlling for correct inputs
  
    # Sex
      observeEvent(input$Run,
                  shinyFeedback::feedbackWarning(
                    "sex", 
                    (input$sex %in% c("male", "female", "undefined")) == FALSE,
                    "Please provide a value for sex."
                  )
      )
    
    # Age
      observeEvent(input$Run,
                  shinyFeedback::feedbackWarning(
                    "age", 
                    ((is.numeric(input$age) == FALSE | input$age <= 0)),
                    "Please provide a value >0 for age."
                  )
      )
      
    # Weight
      observeEvent(input$Run,
                 shinyFeedback::feedbackWarning(
                   "weight", 
                   ((is.numeric(input$weight) == FALSE | input$weight <= 0)),
                   "Please provide a value >0 for weight."
                 )
     )
     
    # MET equation
      observeEvent(input$Run,
                  shinyFeedback::feedbackWarning(
                    "equation_mets", 
                    (input$equation_mets == "..."),
                    "Please choose a MET equation."
                  )
      )
    
    # SED cut-point
      observeEvent(input$Run,
                  shinyFeedback::feedbackWarning(
                    "sed_cutpoints", 
                    (input$sed_cutpoint == "..."),
                    "Please choose a value for the SED cut-point."
                  )
      )
      
    # SED cut-points
      observeEvent(input$Run,
                  shinyFeedback::feedbackWarning(
                    "mvpa_cutpoints", 
                    (input$sed_cutpoint == "..."),
                    "Please choose values for the MPVA cut-points."
                  )
      )
    
    
  # Showing the table presenting the studies that validated METs equations
    output$table_equations <- reactable::renderReactable({
      
      if(input$equation_mets == "...") {NULL
      } else {
        equations_mets %>% 
          dplyr::filter(Study == input$equation_mets) %>%
          reactable::reactable(striped = TRUE,
                    list('Study' = reactable::colDef(minWidth = 80),
                         'Population' = reactable::colDef(minWidth = 60),
                         'Activities performed' = reactable::colDef(minWidth = 60),
                         'Device used' = reactable::colDef(minWidth = 40),
                         'Axis used' = reactable::colDef(minWidth = 40),
                         'Filter enabled' = reactable::colDef(minWidth = 40))
          )
      }
      
    })
  
  # Switching to show appropriate choices for SED cut-points
    observeEvent(input$sed_cutpoint, {
      updateTabsetPanel(inputId = "switcher_sed", selected = input$sed_cutpoint)
    })
  
  
  # Showing the table presenting the studies that validated SED cut-points
   output$table_sed_cutpoints <- reactable::renderReactable({
     
     if(input$sed_cutpoint == "...") {NULL
     } else {
       sed_cutpoints %>% 
         dplyr::filter(Study == input$sed_cutpoint) %>%
         reactable::reactable(striped = TRUE,
                   list('Study' = reactable::colDef(minWidth = 80),
                        'Population' = reactable::colDef(minWidth = 70),
                        'Activities performed' = reactable::colDef(minWidth = 60),
                        'Device used' = reactable::colDef(minWidth = 40),
                        'Axis used' = reactable::colDef(minWidth = 30),
                        'Filter enabled' = reactable::colDef(minWidth = 40),
                        'SED cut-point in counts/min' = reactable::colDef(minWidth = 60))
         )
     }
     
   })
  
  # Switching to show appropriate choices for MVPA cut-points
  observeEvent(input$mvpa_cutpoint, {
    updateTabsetPanel(inputId = "switcher_mvpa", selected = input$mvpa_cutpoint)
  })
  
  
  # Showing the table presenting the studies that validated MVPA cut-points
  output$table_mvpa_cutpoints_sasaki <- reactable::renderReactable({
    
    if(input$mvpa_cutpoint == "...") {NULL
    } else {
      mvpa_cutpoints %>% 
        dplyr::filter(Study == input$mvpa_cutpoint) %>%
        reactable::reactable(striped = TRUE,
                  list('Study' = reactable::colDef(minWidth = 80),
                       'Population' = reactable::colDef(minWidth = 70),
                       'Activities performed' = reactable::colDef(minWidth = 60),
                       'Device used' = reactable::colDef(minWidth = 40),
                       'Axis used' = reactable::colDef(minWidth = 30),
                       'Filter enabled' = reactable::colDef(minWidth = 40),
                       'MPA cut-point (3 METs) in counts/min' = reactable::colDef(minWidth = 60),
                       'VPA cut-point (6 METs) in counts/min' = reactable::colDef(minWidth = 60))
        )
    }
    
  })
  
  output$table_mvpa_cutpoints_santos_adults <- reactable::renderReactable({
    
    if(input$mvpa_cutpoint == "...") {NULL
    } else {
      mvpa_cutpoints %>% 
        dplyr::filter(Study == input$mvpa_cutpoint) %>%
        reactable::reactable(striped = TRUE,
                  list('Study' = reactable::colDef(minWidth = 80),
                       'Population' = reactable::colDef(minWidth = 70),
                       'Activities performed' = reactable::colDef(minWidth = 60),
                       'Device used' = reactable::colDef(minWidth = 40),
                       'Axis used' = reactable::colDef(minWidth = 30),
                       'Filter enabled' = reactable::colDef(minWidth = 40),
                       'MPA cut-point (3 METs) in counts/min' = reactable::colDef(minWidth = 60),
                       'VPA cut-point (6 METs) in counts/min' = reactable::colDef(minWidth = 60))
        )
    }
    
  })
  
  output$table_mvpa_cutpoints_santos_older <- reactable::renderReactable({
    
    if(input$mvpa_cutpoint == "...") {NULL
    } else {
      mvpa_cutpoints %>% 
        dplyr::filter(Study == input$mvpa_cutpoint) %>%
        reactable::reactable(striped = TRUE,
                  list('Study' = reactable::colDef(minWidth = 80),
                       'Population' = reactable::colDef(minWidth = 70),
                       'Activities performed' = reactable::colDef(minWidth = 60),
                       'Device used' = reactable::colDef(minWidth = 40),
                       'Axis used' = reactable::colDef(minWidth = 30),
                       'Filter enabled' = reactable::colDef(minWidth = 40),
                       'MPA cut-point (3 METs) in counts/min' = reactable::colDef(minWidth = 60),
                       'VPA cut-point (6 METs) in counts/min' = reactable::colDef(minWidth = 60))
        )
    }
    
  })
  
  
  # Getting BMR (kcal/d)
    bmr_kcal_d <- eventReactive(input$Run, {
      
      compute_bmr(age = input$age, sex = input$sex, weight = input$weight)
      
    })
    
    
  # Getting list of results: dataset with metrics; results by day corresponding 
  # to valid wear time  (except for total kcal that also uses nonwear time with
  # attribution of bmr to nonwear epochs); selected equations and cut-points
  
    results_list <- eventReactive(input$Run, {
     
     # Waiting for valid inputs
     
     if (!input$sex %in% c("male", "female", "undefined") | input$age <= 0 | input$weight <= 0) {
       validate("Please provide valid values for the inputs shown in Patient's information section.")
     }
     
     if (input$equation_mets == "...") {
       validate("Please choose a MET equation.")
     }
     
     if (input$sed_cutpoint == "..." | input$mvpa_cutpoint == "...") {
       validate("Please provide values for the cut-points.")
     }
     
     if (input$perso_sed_axis != input$perso_mvpa_axis) {
       validate("Please use the same axis for both SED and MVPA cut-points.")
     }
     
     if (input$sed_cutpoint == "Aguilar-Farias et al. (2014) [Older adults]" && 
         input$perso_mvpa_axis == "vertical axis") {
       validate("Please use the same axis for both SED and MVPA cut-points.")
     }
     
     if (input$perso_sed_axis == "vertical axis" &&
         input$mvpa_cutpoint %in% c("Sasaki et al. (2011) [Adults]", 
                                     "Santos-Lozano et al. (2013) [Adults]", 
                                     "Santos-Lozano et al. (2013) [Older adults]")) {
       validate("Please use the same axis for both SED and MVPA cut-points.")
     }
     
     
    
    # Setting axis and cut-points to compute SED and MVPA times
    
    # SED
    if(input$sed_cutpoint == "Aguilar-Farias et al. (2014) [Older adults]") { 
      axis_sed_chosen <- "vm"
      axis_sed_chosen_name <- "vector magnitude"
      sed_cutpoint_chosen <- 200
    } else if (input$sed_cutpoint == "Personalized...") {
      if(input$perso_sed_axis == "vector magnitude") {
        axis_sed_chosen <- "vm"
        axis_sed_chosen_name <- "vector magnitude"
        sed_cutpoint_chosen <- input$perso_sed_cutpoint
      } else {
        axis_sed_chosen <- "axis1"
        axis_sed_chosen_name <- "vertical axis"
        sed_cutpoint_chosen <- input$perso_sed_cutpoint
      }
    } else {
      NULL}
    
    # MVPA
    if(input$mvpa_cutpoint == "Sasaki et al. (2011) [Adults]") { 
      axis_mvpa_chosen <- "vm"
      axis_mvpa_chosen_name <- "vector magnitude"
      mpa_cutpoint_chosen <- 2690
      vpa_cutpoint_chosen <- 6167
    } else if (input$mvpa_cutpoint == "Santos-Lozano et al. (2013) [Adults]"){
      axis_mvpa_chosen <- "vm"
      axis_mvpa_chosen_name <<- "vector magnitude"
      mpa_cutpoint_chosen <- 3208 
      vpa_cutpoint_chosen <- 8565 
    } else if (input$mvpa_cutpoint == "Santos-Lozano et al. (2013) [Older adults]"){
      axis_mvpa_chosen <- "vm"
      axis_mvpa_chosen_name <- "vector magnitude"
      mpa_cutpoint_chosen <- 2751 
      vpa_cutpoint_chosen <- 9359  
    } else if (input$mvpa_cutpoint == "Personalized...") {
      if(input$perso_mvpa_axis == "vector magnitude") {
        axis_mvpa_chosen <- "vm"
        axis_mvpa_chosen_name <- "vector magnitude"
        mpa_cutpoint_chosen <- input$perso_mpa_cutpoint
        vpa_cutpoint_chosen <- input$perso_vpa_cutpoint
      } else {
        axis_mvpa_chosen <- "axis1"
        axis_mvpa_chosen_name <- "vertical axis"
        mpa_cutpoint_chosen <- input$perso_mpa_cutpoint
        vpa_cutpoint_chosen <- input$perso_vpa_cutpoint
      }
    } else {
      NULL}
    
    
    # Adding variables of interest to the initial dataframe
    df_with_computed_metrics <-
      df() %>%
      mark_intensity(col_axis = axis_mvpa_chosen, 
                     sed_cutpoint = sed_cutpoint_chosen, 
                     mpa_cutpoint = mpa_cutpoint_chosen, 
                     vpa_cutpoint = vpa_cutpoint_chosen,
                     equation = input$equation_mets,
                     age = input$age,
                     weight = input$weight,
                     sex = input$sex)
    
   # Creating a dataframe with results by day and corresponding to valid wear time only  
   results_by_day <-
     df_with_computed_metrics %>%
     recap_by_day(age = input$age, weight = input$weight, sex = input$sex)
   
   # Returning a list of the results and parameters
   return(list(df_with_computed_metrics = df_with_computed_metrics,
               results_by_day = results_by_day, 
               axis_sed_chosen_name = axis_sed_chosen_name, 
               sed_cutpoint_chosen = sed_cutpoint_chosen, 
               axis_mvpa_chosen_name = axis_mvpa_chosen_name,
               mpa_cutpoint_chosen = mpa_cutpoint_chosen,
               vpa_cutpoint_chosen = vpa_cutpoint_chosen))
    
  })
    
  # Plotting data with intensity categories
    output$graph_int <- renderPlot({
      plot_data_with_intensity(data = results_list()$df_with_computed_metrics, metric = input$Metric2)
    }, height = function(){nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 90}, res = 120)
    
  
  # Showing results by day in a table
  output$results_by_day <- reactable::renderReactable({
    Sys.sleep(0.5)
    reactable::reactable(results_list()$results_by_day,  
              striped = TRUE,
              list(total_counts_axis1 = reactable::colDef(minWidth = 150),
                   total_counts_vm = reactable::colDef(minWidth = 150),
                   minutes_SED = reactable::colDef(minWidth = 120),
                   minutes_LPA = reactable::colDef(minWidth = 120),
                   minutes_MPA = reactable::colDef(minWidth = 120),
                   minutes_VPA = reactable::colDef(minWidth = 120),
                   minutes_MVPA = reactable::colDef(minWidth = 120),
                   percent_SED = reactable::colDef(minWidth = 120),
                   percent_LPA = reactable::colDef(minWidth = 120),
                   percent_MPA = reactable::colDef(minWidth = 120),
                   percent_VPA = reactable::colDef(minWidth = 120),
                   percent_MVPA = reactable::colDef(minWidth = 120),
                   ratio_mvpa_sed = reactable::colDef(minWidth = 125),
                   total_kcal_wear_time = reactable::colDef(minWidth = 160),
                   mets_hours_mvpa = reactable::colDef(minWidth = 160)))
  })
  
  
  # Getting results averaged on valid days
  results_summary <- eventReactive(input$Run, {
    
    
      # Computing results averaged on valid days
      results_list()$results_by_day %>%
        average_results(minimum_wear_time = input$minimum_wear_time_for_analysis)
    })
  
  # Showing results averaged on valid days in a table
  output$results_summary <- reactable::renderReactable({
    
    reactable::reactable(
      results_summary(), 
      list(valid_days = reactable::colDef(minWidth = 90),
           wear_time = reactable::colDef(minWidth = 90),
           total_counts_axis1 = reactable::colDef(minWidth = 150),
           total_counts_vm = reactable::colDef(minWidth = 150),
           minutes_SED = reactable::colDef(minWidth = 120),
           minutes_LPA = reactable::colDef(minWidth = 120),
           minutes_MPA = reactable::colDef(minWidth = 120),
           minutes_VPA = reactable::colDef(minWidth = 120),
           minutes_MVPA = reactable::colDef(minWidth = 120),
           percent_SED = reactable::colDef(minWidth = 120),
           percent_LPA = reactable::colDef(minWidth = 120),
           percent_MPA = reactable::colDef(minWidth = 120),
           percent_VPA = reactable::colDef(minWidth = 120),
           percent_MVPA = reactable::colDef(minWidth = 120),
           ratio_mvpa_sed = reactable::colDef(minWidth = 125),
           total_kcal_wear_time = reactable::colDef(minWidth = 160),
           mets_hours_mvpa = reactable::colDef(minWidth = 160)),
      striped = TRUE
    )
    
  })
  
  ##############################
  # Hide / show Download buttons ----
  ##############################
  
  observe({
      shinyjs::hide("ExpDataset")
      shinyjs::hide("ExpResultsByDays")
      shinyjs::hide("ExpDailySummary")
      shinyjs::hide("report_en")
      shinyjs::hide("report_fr")
      
    if(nrow(results_list()$df_with_computed_metrics) >=1) {
      
      shinyjs::show("ExpDataset")
      shinyjs::show("ExpResultsByDays")
      shinyjs::show("ExpDailySummary")
      shinyjs::show("report_en")
      shinyjs::show("report_fr")
    }
  })
  
  
  ###################
  # Exporting results ----
  ###################
  
  # Exporting marked dataset
  output$ExpDataset <- downloadHandler(
    filename = function() {
      paste0(input$upload, "_MarkedDataset.csv")
    },
    content = function(file) {
      utils::write.csv2(results_list()$df_with_computed_metrics, file, row.names = FALSE)
    }
  )
  
  # Exporting results by day 
  output$ExpResultsByDays <- downloadHandler(
    filename = function() {
      paste0(input$upload, "_ResultsByDay.csv")
    },
    content = function(file) {
      utils::write.csv2(results_list()$results_by_day, file, row.names = FALSE)
    }
  )
  
  # Exporting daily summary  
  output$ExpDailySummary <- downloadHandler(
    filename = function() {
      paste0(input$upload, "_DailySummary.csv")
    },
    content = function(file) {
      utils::write.csv2(results_summary(), file, row.names = FALSE)
    }
  )
  
  #################
  # Generate report ----
  #################
  
  # Generating report EN
  output$report_en <- downloadHandler(
    
    
    filename = "report.pdf",
    content = function(file) {
      
      
      withProgress(message = 'Please wait...', {
        
        report <- system.file("report", "report_en.Rmd", package = "activAnalyzer")

        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
        tempReport <- file.path(tempdir(), "report_en.Rmd")
        file.copy(report, tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          assessor_name = input$assessor_name,
          assessor_surname = input$assessor_surname,
          patient_name = input$patient_name,
          patient_surname = input$patient_surname,
          sex = input$sex,
          age = input$age,
          weight = input$weight,
          start_date = attributes(file())$startdatetime,
          end_date = attributes(file())$stopdatetime,
          device = attributes(file())$devicename,
          position = input$position,
          side = input$side,
          sampling_rate = attributes(file())$`original sample rate`,
          filter = attributes(file())$filter,
          start_day_analysis = input$start_day_analysis,
          end_day_analysis = input$end_day_analysis,
          axis_weartime = input$axis_weartime,
          frame_size = input$frame_size,
          allowanceFrame_size = input$allowanceFrame_size,
          streamFrame_size = input$streamFrame_size,
          equation_mets = input$equation_mets,
          bmr_kcal_d = bmr_kcal_d(),
          axis_sed = results_list()$axis_sed_chosen_name,
          axis_mvpa = results_list()$axis_mvpa_chosen_name,
          sed_cutpoint = results_list()$sed_cutpoint_chosen,
          mpa_cutpoint = results_list()$mpa_cutpoint_chosen,
          vpa_cutpoint = results_list()$vpa_cutpoint_chosen,
          minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis,
          results_by_day = results_list()$results_by_day,
          results_summary =  results_summary(),
          
          # Loading some data used in figures
          mvpa_lines = mvpa_lines,
          sed_lines = sed_lines,
          ratio_lines = ratio_lines,
          
          rendered_by_shiny = TRUE
        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
        out <- rmarkdown::render(tempReport,
                                 params = params,
                                 envir = new.env(parent = globalenv())
        )
        out <- file.rename(out, file)
        
      })
      
    }
  )
  
  # Generating report FR
  output$report_fr <- downloadHandler(
    
    
    filename = "rapport.pdf",
    content = function(file) {
      
      
      withProgress(message = 'Please wait...', {
        
        report <- system.file("report", "report_fr.Rmd", package = "activAnalyzer")
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
        tempReport <- file.path(tempdir(), "report_fr.Rmd")
        file.copy(report, tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          assessor_name = input$assessor_name,
          assessor_surname = input$assessor_surname,
          patient_name = input$patient_name,
          patient_surname = input$patient_surname,
          sex = input$sex,
          age = input$age,
          weight = input$weight,
          start_date = attributes(file())$startdatetime,
          end_date = attributes(file())$stopdatetime,
          device = attributes(file())$devicename,
          position = input$position,
          side = input$side,
          sampling_rate = attributes(file())$`original sample rate`,
          filter = attributes(file())$filter,
          start_day_analysis = input$start_day_analysis,
          end_day_analysis = input$end_day_analysis,
          axis_weartime = input$axis_weartime,
          frame_size = input$frame_size,
          allowanceFrame_size = input$allowanceFrame_size,
          streamFrame_size = input$streamFrame_size,
          equation_mets = input$equation_mets,
          bmr_kcal_d = bmr_kcal_d(),
          axis_sed = results_list()$axis_sed_chosen_name,
          axis_mvpa = results_list()$axis_mvpa_chosen_name,
          sed_cutpoint = results_list()$sed_cutpoint_chosen,
          mpa_cutpoint = results_list()$mpa_cutpoint_chosen,
          vpa_cutpoint = results_list()$vpa_cutpoint_chosen,
          minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis,
          results_by_day = results_list()$results_by_day,
          results_summary =  results_summary(),
          
          # Loading some data used in figures
          mvpa_lines = mvpa_lines,
          sed_lines = sed_lines,
          ratio_lines = ratio_lines,
          
          rendered_by_shiny = TRUE
        )
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
        out <- rmarkdown::render(tempReport,
                                 params = params,
                                 envir = new.env(parent = globalenv())
        )
        out <- file.rename(out, file)
        
      })
      
    }
  )
  
  
  
  ########### 
  # Reset app ----
  ########### 
  
  observeEvent(input$reset, {
    
    modal_confirm <- modalDialog(
      "Are you sure you want to reset the app?",
      title = "Reset app",
      footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("ok", "Reset", class = "btn btn-danger", style="color: #fff; background-color: #F8766D; border-color: #FC717F")
      )
    )
    
    showModal(modal_confirm)
  })
  
  observeEvent(input$ok, {
    aggg_result = -1
    if(aggg_result == -1)
    {
      session$reload()
      return()
    }
  })
  
  observeEvent(input$cancel, {
    removeModal()
  })
  
  ####################### 
  # Download user's guide ----
  #######################
  
  # English
  output$user_guide_en <- downloadHandler(
    filename = function() {
      paste0(input$upload, "activanalyzer_manual.pdf")
    },
    content = function(file) {
      file.copy(system.file("guide", "user_guide_en.pdf", package = "activAnalyzer"), file)
    }
  )
  
  # French
  output$user_guide_fr <- downloadHandler(
    filename = function() {
      paste0(input$upload, "activanalyzer_manuel.pdf")
    },
    content = function(file) {
      file.copy(system.file("guide", "user_guide_fr.pdf", package = "activAnalyzer"), file)
    }
  )
  
  #################################################
  # Exporting values/objects (required for testing) ----
  #################################################
  
  # Exporting dataframe marked for wear time
    observeEvent(input$validate, {
      shiny::exportTestValues(df = df())
    })
  
  # Exporting dataframe marked for wear time
    observeEvent(input$Run, {
      shiny::exportTestValues(results_by_day = results_list()$results_by_day)
    })
    
  # Exporting dataframe marked for wear time
    observeEvent(input$Run, {
      shiny::exportTestValues(results_summary = results_summary())
    })
  
}


