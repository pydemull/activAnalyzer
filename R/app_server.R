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
    
  # Controlling appearance of the "Validate configuration" button
    observe({
      shinyjs::hide("validate")
      if(nrow(data()) >=1) {
      shinyjs::show("validate")
        
      }
    })
    
    
    
  ################
  # Days selection ----
  ################
    
    output$select_days <- renderUI({
      dates <- attributes(as.factor(df()$date))$levels
      checkboxGroupInput("selected_days", h3("Select the days to keep for analysis"), dates, selected = dates, inline = TRUE)
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
                is.numeric(input$streamFrame_size) & 
                input$streamFrame_size >= 0
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
                           allowanceFrame = input$allowanceFrame_size,
                           streamFrame = input$streamFrame_size)  
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
  }, 
  width = "auto", 
  height = function(){
    height <- dplyr::case_when(
      nlevels(as.factor(df()$date)) >= 8 ~ nlevels(as.factor(df()$date)) * 95,
      nlevels(as.factor(df()$date)) == 7 ~ nlevels(as.factor(df()$date)) * 105,
      nlevels(as.factor(df()$date)) == 6 ~ nlevels(as.factor(df()$date)) * 107,
      nlevels(as.factor(df()$date)) == 5 ~ nlevels(as.factor(df()$date)) * 110,
      nlevels(as.factor(df()$date)) == 4 ~ nlevels(as.factor(df()$date)) * 115,
      nlevels(as.factor(df()$date)) == 3 ~ nlevels(as.factor(df()$date)) * 130,
      nlevels(as.factor(df()$date)) == 2 ~ nlevels(as.factor(df()$date)) * 150,
      nlevels(as.factor(df()$date)) == 1 ~ nlevels(as.factor(df()$date)) * 205
    )
   return(height)
    }, 
  res = 120)
    
  
  ###################################################
  # Getting results when clicking on the "Run" button ----
  ###################################################
    
  # Controlling the appearance of the "Run analysis" button
    observe({
      shinyjs::hide("Run")
      if(nrow(df()) >=1) {
        shinyjs::show("Run")
        
      }
    })
    
  
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
                      (
                          (!input$sex %in% c("male", "female", "undefined") | 
                          !is.numeric(input$age) |
                          input$age <= 0 | 
                          !is.numeric(input$weight) |
                          input$weight <= 0) | 
                          input$equation_mets == "..."),
                      "Please provide valid values for the inputs shown in Patient's information section and choose a MET equation."
                    )
        )

         
      # SED cut-point
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "sed_cutpoint", 
                       (
                         (input$sed_cutpoint == "...")
                         | # OR
                           (
                             (input$mvpa_cutpoint == "Freedson et al. (1998) [Adults]" | (input$mvpa_cutpoint == "Personalized..." & input$perso_mvpa_axis == "vertical axis")) & 
                               (input$sed_cutpoint == "Aguilar-Farias et al. (2014) [Older adults]" | (input$sed_cutpoint == "Personalized..." & input$perso_sed_axis == "vector magnitude"))
                           ) 
                         | # OR
                           (
                             (input$mvpa_cutpoint %in% c("Santos-Lozano et al. (2013) [Adults]",
                                                         "Santos-Lozano et al. (2013) [Older adults]",
                                                         "Sasaki et al. (2011) [Adults]") | (input$mvpa_cutpoint == "Personalized..." & input$perso_mvpa_axis == "vector magnitude")) & 
                               (input$sed_cutpoint == "Personalized..." & input$perso_sed_axis == "vertical axis")
                           )
                         | # OR
                           (
                             input$mvpa_cutpoint == "Personalized..." &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= input$perso_mpa_cutpoint
                           )
                         | # OR
                           (
                             input$sed_cutpoint == "Personalized..." & !is.numeric(input$perso_sed_cutpoint)
                           )
                         | # OR
                           (
                             input$sed_cutpoint == "Aguilar-Farias et al. (2014) [Older adults]" &  input$mvpa_cutpoint == "Personalized..." & input$perso_mpa_cutpoint <= 200
                           )
                         | # OR
                           (
                             input$mvpa_cutpoint == "Freedson et al. (1998) [Adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 1952
                           )
                         | # OR
                           (
                             input$mvpa_cutpoint == "Santos-Lozano et al. (2013) [Adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 3208
                           )
                         | # OR
                           (
                             input$mvpa_cutpoint == "Santos-Lozano et al. (2013) [Older adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 2751
                           )
                         | # OR
                           (
                             input$mvpa_cutpoint == "Sasaki et al. (2011) [Adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 2690
                           )
                       ),
                       "Please choose a SED cut-point that uses the same axis as for the MVPA cut-points. The value must be as follows: SED < MPA < VPA."
                     
                     )
        )
        
  
        
      # MVPA cut-points
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "mvpa_cutpoint", 
                       (    
                           (input$mvpa_cutpoint == "...")
                           | # OR
                             (
                               (input$mvpa_cutpoint == "Freedson et al. (1998) [Adults]" | (input$mvpa_cutpoint == "Personalized..." & input$perso_mvpa_axis == "vertical axis")) & 
                                 (input$sed_cutpoint == "Aguilar-Farias et al. (2014) [Older adults]" | (input$sed_cutpoint == "Personalized..." & input$perso_sed_axis == "vector magnitude"))
                             ) 
                           | # OR
                             (
                               (input$mvpa_cutpoint %in% c("Santos-Lozano et al. (2013) [Adults]",
                                                           "Santos-Lozano et al. (2013) [Older adults]",
                                                           "Sasaki et al. (2011) [Adults]") | (input$mvpa_cutpoint == "Personalized..." & input$perso_mvpa_axis == "vector magnitude")) & 
                               (input$sed_cutpoint == "Personalized..." & input$perso_sed_axis == "vertical axis")
                             )
                           | # OR
                            (
                              input$mvpa_cutpoint == "Personalized..." &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= input$perso_mpa_cutpoint
                            )
                           | # OR
                             (
                               input$mvpa_cutpoint == "Personalized..." & ((!is.numeric(input$perso_mpa_cutpoint)) | (!is.numeric(input$perso_vpa_cutpoint)))
                             )
                           | # OR
                            (
                              input$sed_cutpoint == "Aguilar-Farias et al. (2014) [Older adults]" &  input$mvpa_cutpoint == "Personalized..." & input$perso_mpa_cutpoint <= 200
                            )
                           | # OR
                            (
                              input$mvpa_cutpoint == "Freedson et al. (1998) [Adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 1952
                            )
                           | # OR
                            (
                              input$mvpa_cutpoint == "Santos-Lozano et al. (2013) [Adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 3208
                            )
                           | # OR
                            (
                              input$mvpa_cutpoint == "Santos-Lozano et al. (2013) [Older adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 2751
                            )
                           | # OR
                            (
                               input$mvpa_cutpoint == "Sasaki et al. (2011) [Adults]" &  input$sed_cutpoint == "Personalized..." & input$perso_sed_cutpoint >= 2690
                             )
                           | # OR
                            (
                              input$mvpa_cutpoint == "Personalized..." & input$perso_mpa_cutpoint >= input$perso_vpa_cutpoint
                            )
                       ),
                       "Please choose MVPA cut-points that use the same axis as for the SED cut-point. The values must be as follows: SED < MPA < VPA."
                       
                     )
        )
        
      # Minimum daily wear time
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "minimum_wear_time_for_analysis", 
                       (!is.numeric(input$minimum_wear_time_for_analysis)) | (input$minimum_wear_time_for_analysis < 0),
                     "Please choose a number >= 0"
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
    
    output$table_mvpa_cutpoints_freedson_adults <- reactable::renderReactable({
      
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
    
  
  
  # Setting PROactive default values to valid a day based on wear time
    observeEvent(input$pro_active_period, {
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(60*60*7))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*22))
      updateNumericInput(inputId = "minimum_wear_time_for_analysis", value = 8)
    })
    

  # Controlling PROactive settings
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
    
  # Returning to default settings for the minimum wear time duration
    observeEvent(input$reset_period, {
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(60*60*0))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*23+60*59))
      updateNumericInput(inputId = "minimum_wear_time_for_analysis", value = 10)
    })
          
  # Getting BMR (kcal/d)
    bmr_kcal_d <- eventReactive(input$Run, {
     
     compute_bmr(age = input$age, sex = input$sex, weight = input$weight)
     
    })
  
    
  # Getting list of results: dataset with metrics; results by day corresponding 
  # to valid wear time  (except for total kcal that also uses nonwear time with
  # attribution of bmr to nonwear epochs); selected equations and cut-points

  results_list <- eventReactive(input$Run, {
    
      req(
        input$sex %in% c("male", "female", "undefined") &
        is.numeric(input$age) & 
        input$age > 0 &
        is.numeric(input$weight) &
        input$weight > 0 &
        is.numeric(input$minimum_wear_time_for_analysis) &
        input$minimum_wear_time_for_analysis >= 0
        )
     
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
        axis_mvpa_chosen_name <- "vector magnitude"
        mpa_cutpoint_chosen <- 3208 
        vpa_cutpoint_chosen <- 8565 
      } else if (input$mvpa_cutpoint == "Freedson et al. (1998) [Adults]"){
        axis_mvpa_chosen <- "axis1"
        axis_mvpa_chosen_name <- "vertical axis"
        mpa_cutpoint_chosen <- 1952 
        vpa_cutpoint_chosen <- 5725 
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
    
    # Waiting for valid inputs
    
      if (input$equation_mets == "...") {
        validate("Please choose a MET equation.")
      }
      
      if (input$sed_cutpoint == "..." | input$mvpa_cutpoint == "...") {
        validate("Please provide values for the cut-points.")
      }
      
      if (input$sed_cutpoint == "Personalized..." & !is.numeric(input$perso_sed_cutpoint)) {
        validate("Please provide a numeric value for the cut-point.")
      }
    
      if (input$mvpa_cutpoint == "Personalized..." & ((!is.numeric(input$perso_mpa_cutpoint)) | (!is.numeric(input$perso_vpa_cutpoint)))) {
        validate("Please provide numeric values for the cut-points.")
      }
          
      if (axis_mvpa_chosen != axis_sed_chosen) {
        validate("Please use the same axis for both SED and MVPA cut-points.")
      }
      
      if (sed_cutpoint_chosen >= mpa_cutpoint_chosen) {
        validate("Please choose a SED cut-point that is strictly lower than the MVPA cut-point.")
      }
      
      if (mpa_cutpoint_chosen >= vpa_cutpoint_chosen) {
        validate("Please choose a MPA cut-point that is strictly lower than the VPA cut-point.")
      }
    
      
    
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
                       sex = input$sex,
                       dates = input$selected_days)
      
   # Creating a dataframe with results by day and corresponding to valid wear time only  
     results_by_day <-
       df_with_computed_metrics %>%
       recap_by_day(
         age = input$age, 
         weight = input$weight, 
         sex = input$sex,
         valid_wear_time_start = input$start_day_analysis,
         valid_wear_time_end = input$end_day_analysis
       )
     
   # Returning a list of the results and parameters
     return(list(df_with_computed_metrics = df_with_computed_metrics,
                 results_by_day = results_by_day, 
                 axis_sed_chosen_name = axis_sed_chosen_name, 
                 sed_cutpoint_chosen = sed_cutpoint_chosen, 
                 axis_mvpa_chosen_name = axis_mvpa_chosen_name,
                 mpa_cutpoint_chosen = mpa_cutpoint_chosen,
                 vpa_cutpoint_chosen = vpa_cutpoint_chosen))
    })
      
      
  # Creating reactive time filters used for the plot with intensity metrics
    analysis_filters <- eventReactive(input$Run, {
      list(start_day_analysis = input$start_day_analysis,
           end_day_analysis = input$end_day_analysis
           )
      })
    
  # Plotting data with intensity categories
    output$graph_int <- renderPlot({
      plot_data_with_intensity(data = results_list()$df_with_computed_metrics, 
                               metric = input$Metric2,
                               valid_wear_time_start = analysis_filters()$start_day_analysis,
                               valid_wear_time_end = analysis_filters()$end_day_analysis)
    }, 
    width = "auto", 
    height = function(){
      height <- dplyr::case_when(
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) >= 8 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 95,
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) == 7 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 105,
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) == 6 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 107,
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) == 5 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 110,
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) == 4 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 115,
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) == 3 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 130,
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) == 2 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 150,
        nlevels(as.factor(results_list()$df_with_computed_metrics$date)) == 1 ~ nlevels(as.factor(results_list()$df_with_computed_metrics$date)) * 205
      )
      return(height)
    }, 
    res = 120)
    

    
  
  # Showing results by day in a table
    output$results_by_day <- reactable::renderReactable({
      Sys.sleep(0.5)
      reactable::reactable(results_list()$results_by_day,  
                striped = TRUE,
                list(
                     total_counts_axis1 = reactable::colDef(minWidth = 150),
                     total_counts_vm = reactable::colDef(minWidth = 150),
                     axis1_per_min = reactable::colDef(minWidth = 150),
                     vm_per_min = reactable::colDef(minWidth = 150),
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
                     total_kcal = reactable::colDef(minWidth = 120),
                     mets_hours_mvpa = reactable::colDef(minWidth = 160)))
    })
    
  
  # Getting results summarized over valid days (means)
    results_summary_means <- eventReactive(input$Run, {
        results_list()$results_by_day %>%
          average_results(minimum_wear_time = input$minimum_wear_time_for_analysis, fun = "mean")
      })
  
  # Getting results summarized over valid days (medians)
    results_summary_medians <- eventReactive(input$Run, {
      results_list()$results_by_day %>%
        average_results(minimum_wear_time = input$minimum_wear_time_for_analysis, fun = "median")
    })
  
  # Showing results summarized over valid days in a table (means)
    output$results_summary_means <- reactable::renderReactable({
      
      reactable::reactable(
        results_summary_means(), 
        list(valid_days = reactable::colDef(minWidth = 90),
             wear_time = reactable::colDef(minWidth = 90),
             total_counts_axis1 = reactable::colDef(minWidth = 150),
             total_counts_vm = reactable::colDef(minWidth = 150),
             axis1_per_min = reactable::colDef(minWidth = 150),
             vm_per_min = reactable::colDef(minWidth = 150),
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
             total_kcal = reactable::colDef(minWidth = 120),
             mets_hours_mvpa = reactable::colDef(minWidth = 160)),
        striped = TRUE
      )
      
    })
    
  # Showing results summarized over valid days in a table (medians)
    output$results_summary_medians <- reactable::renderReactable({
      
      reactable::reactable(
        results_summary_medians(), 
        list(valid_days = reactable::colDef(minWidth = 90),
             wear_time = reactable::colDef(minWidth = 90),
             total_counts_axis1 = reactable::colDef(minWidth = 150),
             total_counts_vm = reactable::colDef(minWidth = 150),
             axis1_per_min = reactable::colDef(minWidth = 150),
             vm_per_min = reactable::colDef(minWidth = 150),
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
             total_kcal = reactable::colDef(minWidth = 120),
             mets_hours_mvpa = reactable::colDef(minWidth = 160)),
        striped = TRUE
      )
      
    })
    
  
  # Showing PROactive scores
    
    # C-PPAC: Based on medians
    output$PROactive_scores_cppac_medians <- reactable::renderReactable({
      
      steps_score <- compute_pro_actigraph_score(results_summary_medians()[["total_steps"]], metric = "steps", fun = "median")
      vmu_score <- compute_pro_actigraph_score(results_summary_medians()[["vm_per_min"]], metric = "vm", fun = "median")
      
      reactable::reactable(
        tibble::tribble(
          ~Metric,              ~Score,
          "Daily steps score",   paste(steps_score),
          "Daily VMU score",     paste(vmu_score)
        ),
        striped = TRUE,
        list(Score = reactable::colDef(align = "center"))
      )
    
      
    })
    
    # C-PPAC: Based on means
    output$PROactive_scores_cppac_means <- reactable::renderReactable({
      
      steps_score <- compute_pro_actigraph_score(results_summary_means()[["total_steps"]], metric = "steps", fun = "mean")
      vmu_score <- compute_pro_actigraph_score(results_summary_means()[["vm_per_min"]], metric = "vm", fun = "mean")
      
      reactable::reactable(
        tibble::tribble(
          ~Metric,              ~Score,
          "Daily steps score",   paste(steps_score),
          "Daily VMU score",     paste(vmu_score)
        ),
        striped = TRUE,
        list(Score = reactable::colDef(align = "center"))
      )
      
      
    })
    
    # D-PPAC
    output$PROactive_scores_dppac <-  reactable::renderReactable({
      reactable::reactable(
      results_list()$results_by_day %>%
        dplyr::mutate(Date = date,
                      "Daily steps score" = compute_pro_actigraph_score(
                                               x = .data$total_steps,
                                               quest = "D-PPAC",
                                               metric = "steps"
                                              ),
                      "Daily VMU score" = compute_pro_actigraph_score(
                                               x = .data$vm_per_min,
                                               quest = "D-PPAC",
                                               metric = "vmu"
                                             ),
                      ) %>%
        dplyr::select(Date, "Daily steps score", "Daily VMU score" )
      ),
      striped = TRUE,
      list(Score = reactable::colDef(align = "center"))
      )
    })
    
  
  #########################
  # Hiding / showing boxes ----
  #########################
  
  # Box for graph with wear time
    observe({
      shinyjs::hide("myBox")
      shinyjs::hide("Metric")
      shinyjs::hide("graph")
      
      if(nrow(df()) >=1) {
        
        shinyjs::show("myBox")
        shinyjs::show("Metric")
        shinyjs::show("graph")
        
      }
    })
  
  # Boxes for graph with PA categories and results
    observe({
      shinyjs::hide("myBox2")
      shinyjs::hide("Metric2")
      shinyjs::hide("graph_int")
      shinyjs::hide("BoxResByDay")
      shinyjs::hide("BoxResMeans")
      shinyjs::hide("BoxResMedians")
      shinyjs::hide("CPPAC_PROactive_medians")
      shinyjs::hide("CPPAC_PROactive_means")
      shinyjs::hide("DPPAC_PROactive")

      
      if(nrow(results_list()$df_with_computed_metrics) >=1) {
        
      shinyjs::show("myBox2")
      shinyjs::show("Metric2")
      shinyjs::show("graph_int")
      shinyjs::show("BoxResByDay")
      shinyjs::show("BoxResMeans")
      shinyjs::show("BoxResMedians")
      shinyjs::show("CPPAC_PROactive_medians")
      shinyjs::show("CPPAC_PROactive_means")
      shinyjs::show("DPPAC_PROactive")

      }
    })
    
  
  ##################################
  # Hiding/ showing Download buttons ----
  ##################################
  
  observe({
      shinyjs::hide("ExpDataset")
      shinyjs::hide("ExpResultsByDays")
      shinyjs::hide("ExpDailySummaryMeans")
      shinyjs::hide("ExpDailySummaryMedians")
      shinyjs::hide("report_en")
      shinyjs::hide("report_fr")
      shinyjs::hide("reset")
      
    if(nrow(results_list()$df_with_computed_metrics) >=1) {
      shinyjs::show("ExpDataset")
      shinyjs::show("ExpResultsByDays")
      shinyjs::show("ExpDailySummaryMeans")
      shinyjs::show("ExpDailySummaryMedians")
      shinyjs::show("reset")
    }
      
    if(results_summary_means()$valid_days >=1 | results_summary_medians()$valid_days >=1) {
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
        utils::write.csv2(results_list()$df_with_computed_metrics %>%
                            dplyr::select(-col_time_stamp, -timestamp), 
                          file, row.names = FALSE)
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
  
  # Exporting daily summary (means)
    output$ExpDailySummaryMeans <- downloadHandler(
      filename = function() {
        paste0(input$upload, "_DailySummaryMeans.csv")
      },
      content = function(file) {
        utils::write.csv2(results_summary_means(), file, row.names = FALSE)
      }
    )
  
  # Exporting daily summary (medians)
    output$ExpDailySummaryMedians <- downloadHandler(
      filename = function() {
        paste0(input$upload, "_DailySummaryMedians.csv")
      },
      content = function(file) {
        utils::write.csv2(results_summary_medians(), file, row.names = FALSE)
      }
    )
  
  ###################
  # Generating report ----
  ###################
  
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
          results_summary_means =  results_summary_means(),
          results_summary_medians =  results_summary_medians(),
          
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
          results_summary_means =  results_summary_means(),
          results_summary_medians =  results_summary_medians(),
          
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
  
  
  
  ############### 
  # Resetting app ----
  ###############
  
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
  
  ########################## 
  # Downloading user's guide ----
  ##########################
  
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
  
  # Exporting dataframe for the results by day
    observeEvent(input$Run, {
      shiny::exportTestValues(results_by_day = results_list()$results_by_day)
    })
    
  # Exporting dataframe for the daily means
    observeEvent(input$Run, {
      shiny::exportTestValues(results_summary_means = results_summary_means())
    })
    
    # Exporting dataframe for the daily medians
    observeEvent(input$Run, {
      shiny::exportTestValues(results_summary_medians = results_summary_medians())
    })
    
    # Exporting plot showing nonwear/wear time
    observeEvent(input$Run, {
      shiny::exportTestValues(gg_plot_data = plot_data(data = df(), metric = input$Metric))
    })
    
    # Exporting inputs after setting proactive inputs for data analysis
      observeEvent(input$pro_active_period, {
        shiny::exportTestValues(start_day_analysis = input$start_day_analysis)
        shiny::exportTestValues(end_day_analysis = input$end_day_analysis)
      })
      
    # Exporting inputs after resetting inputs for nonwear/wear time analysis
      observeEvent(input$validate, {
        shiny::exportTestValues(axis_weartime = input$axis_weartime)
        shiny::exportTestValues(frame_size = input$frame_size)
        shiny::exportTestValues(allowanceFrame_size = input$allowanceFrame_size)
        shiny::exportTestValues(streamFrame_size = input$streamFrame_size)
        shiny::exportTestValues(start_day_analysis = input$start_day_analysis)
        shiny::exportTestValues(end_day_analysis = input$end_day_analysis)
      })
    
    # Exporting BMR
      observeEvent(input$Run, {
        shiny::exportTestValues(BMR = bmr_kcal_d())
      })
      
  
}


