#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  
  ######################
  # Managing app closure ----
  ######################
  
  # Increasing users count when starting new session
    isolate({users$count <- users$count + 1 
    })
    
  # Decreasing users count when closing session
  # Stopping app when count is 0 AND when Reset button has not been clicked
    session$onSessionEnded(function() {
      isolate({
        users$count = users$count - 1
        if (users$count == 0 && is.null(input$ok)) stopApp()
      })
    })
  
           # The following comments were helpful to build the code: 
           # https://stackoverflow.com/questions/47728208/how-many-users-are-connected-to-my-shiny-application
  
  
  ##############################
  # Uploading and preparing data ----
  ##############################

  # Getting data file without modification (required for extracting device attributes 
  # information when generating the report)
     file <- reactive({
      
      # Waiting for required conditions 
        req(input$upload)
      
      # Reading file
        read_agd(input$upload$datapath)
      
    })

  # Getting reactive dataset
    data <- reactive({
      
      # Waiting for required conditions 
        req(input$upload)
      
      # Formatting file
        prepare_dataset(data = input$upload$datapath)
      
      })
    
  # Controlling appearance of the "Validate configuration" button
    observe({
     shinyjs::hide("validate")
     if((tools::file_ext(input$upload$name) == "agd") && nrow(data()) >= 1) {
     shinyjs::show("validate")
     } else {
       shinyjs::hide("validate")
     }
    })
    
  # Controlling file extension
    observeEvent(input$upload,
                shinyFeedback::feedbackWarning(
                  "upload", 
                  ((tools::file_ext(input$upload$name) == "agd") == FALSE),
                  "Invalid file format. Please choose an .agd file."
                )
    )
      
    
  ################
  # Days selection ----
  ################
    
  # Selecting days required for analysis
    output$select_days <- renderUI({
      dates <- attributes(as.factor(df()$date))$levels
      checkboxGroupInput("selected_days", h3("Select the days to keep for analyzis (please only select the 7 appropriate days if your analyzis is related to PROactive questionnaire framework)"), dates, selected = dates, inline = TRUE)
    })  
    
  
  ###########################################################################################
  # Getting dataframe with marks for wear/nonwear time when clicking on the "Validate" button ----
  ###########################################################################################
  
  # Controlling for correct inputs
  
    # File epoch length
      observeEvent(input$validate,
                  shinyFeedback::feedbackWarning(
                    "to_epoch", 
                    (!is.numeric(as.numeric(input$to_epoch)) | 
                     as.numeric(input$to_epoch) < 1 | 
                     as.numeric(input$to_epoch) > 60 |
                     as.numeric(input$to_epoch) < as.numeric(hms::as_hms(data()$TimeStamp[2] - data()$TimeStamp[1])) |
                     ((as.numeric(input$to_epoch) / as.numeric(hms::as_hms(data()$TimeStamp[2] - data()$TimeStamp[1]))) - floor(as.numeric(input$to_epoch) / as.numeric(hms::as_hms(data()$TimeStamp[2] - data()$TimeStamp[1])))) != 0
                     ),
                    "Please choose a number between 1 and 60 and that is greater or equal to the duration of the file epochs. 
                    The ratio between the desired epoch and the current epoch must be an integer."
                  )
      )

      output$warning_epoch <- renderText({
      "With such a short epoch, the app will have to deal with a large dataset, and processing times will certainly be long. 
      As the figures provided by the app are quite complex, rendering them with this epoch to vizualize a week of measurement could take several minutes. 
      For this reason, the figure will not be created with this epoch to save your time. Please use a longer epoch if you want to benefit from the figures of the app."
      })
    
      observe({
      req(is.numeric(as.numeric(input$to_epoch)))
      shinyjs::hide("warning_epoch")
      if(as.numeric(input$to_epoch) < 10) {
        shinyjs::show("warning_epoch")
        
      }
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
                is.numeric(as.numeric(input$to_epoch)) & 
                as.numeric(input$to_epoch) >= 1 &
                as.numeric(input$to_epoch) <= 60 &
                as.numeric(input$to_epoch) >= as.numeric(hms::as_hms(data()$TimeStamp[2] - data()$TimeStamp[1])) & 
                ((as.numeric(input$to_epoch) / as.numeric(hms::as_hms(data()$TimeStamp[2] - data()$TimeStamp[1]))) - floor(as.numeric(input$to_epoch) / as.numeric(hms::as_hms(data()$TimeStamp[2] - data()$TimeStamp[1])))) == 0 & 
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
        
        # Creating reactive dataframe with wear/nonwear marks
          df <- 
            mark_wear_time(
              dataset = data(),
              to_epoch = as.numeric(input$to_epoch),
              cts = cts, 
              frame = input$frame_size, 
              allowanceFrame = input$allowanceFrame_size,
              streamFrame = input$streamFrame_size
              ) 
          
          return(df)

         })
        

  # Returning to default values for the wear time detection algorithm
    observeEvent(input$reset_nonwear, {
      axis_weartime <- c("vector magnitude", "vertical axis")
      updateNumericInput(inputId = "to_epoch", value = 60)
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
    
    if (as.numeric(df()$time[2] - df()$time[1]) < 10) { 
      ggplot2::ggplot() + ggplot2::geom_text(
        ggplot2::aes(
          x = 1, 
          y  = 1,
          label = "Sorry, below 10-s epochs, we prefer \nnot to build the plot to save your time..."),
        size = 10
        ) +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()
        )
    } else {
    plot_data(data = df(), metric = input$Metric)
    }
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
        
        
      # Controlling PROactive settings
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "start_day_analysis", 
                       hms::as_hms(input$end_day_analysis) <= hms::as_hms(input$start_day_analysis),
                       "End time should be superior to start time."
                     )
        )
        
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "end_day_analysis", 
                       hms::as_hms(input$end_day_analysis) <= hms::as_hms(input$start_day_analysis),
                       "End time should be superior to start time."
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
    
    
  # Returning to default settings for the minimum wear time duration
    observeEvent(input$reset_period, {
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(60*60*0))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*23+60*59+59))
      updateNumericInput(inputId = "minimum_wear_time_for_analysis", value = 10)
    })
          
  # Getting BMR (kcal/d)
    bmr_kcal_d <- eventReactive(input$Run, {
     
     # Computing BMR
       compute_bmr(age = input$age, sex = input$sex, weight = input$weight)
     
    })
  
    
  # Getting list of results: dataset with metrics; results by day corresponding 
  # to valid wear time  (except for total kcal that also uses nonwear time with
  # attribution of BMR to nonwear epochs); selected equations and cut-points
  
    
    # Setting axis and cut-points to compute SED and MVPA times
    
      cut_points <- eventReactive(input$Run, {
        
        # Waiting for required conditions 
          req(
            (isTruthy(input$sed_cutpoint != "...") | isTruthy(input$sed_cutpoint == "Personalized..." & is.numeric(input$perso_sed_cutpoint))) &
                (isTruthy(input$mvpa_cutpoint != "...") | isTruthy(input$mvpa_cutpoint == "Personalized..." & ((is.numeric(input$perso_mpa_cutpoint)) & (is.numeric(input$perso_vpa_cutpoint)))))
            )
        
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
          
          list <- list(
            axis_sed_chosen = axis_sed_chosen, 
            axis_mvpa_chosen = axis_mvpa_chosen, 
            axis_sed_chosen_name = axis_sed_chosen_name, 
            axis_mvpa_chosen_name = axis_mvpa_chosen_name, 
            sed_cutpoint_chosen = sed_cutpoint_chosen, 
            mpa_cutpoint_chosen = mpa_cutpoint_chosen, 
            vpa_cutpoint_chosen = vpa_cutpoint_chosen
            )
          
        return(list)
        
      })
   
      # Building the list  
        results_list <- eventReactive(input$Run, {
          
            # Waiting for required conditions 
              req(
                input$sex %in% c("male", "female", "undefined") &
                is.numeric(input$age) & 
                input$age > 0 &
                is.numeric(input$weight) &
                input$weight > 0 &
                input$equation_mets != "..." &
                (isTruthy(input$sed_cutpoint != "...") | isTruthy(input$sed_cutpoint == "Personalized..." & is.numeric(input$perso_sed_cutpoint))) &
                (isTruthy(input$mvpa_cutpoint != "...") | isTruthy(input$mvpa_cutpoint == "Personalized..." & ((is.numeric(input$perso_mpa_cutpoint)) & (is.numeric(input$perso_vpa_cutpoint))))) &
                cut_points()$axis_mvpa_chosen == cut_points()$axis_sed_chosen &
                cut_points()$sed_cutpoint_chosen < cut_points()$mpa_cutpoint_chosen &
                cut_points()$mpa_cutpoint_chosen < cut_points()$vpa_cutpoint_chosen & 
                is.numeric(input$minimum_wear_time_for_analysis) &
                input$minimum_wear_time_for_analysis >= 0 &
                hms::as_hms(input$end_day_analysis) > hms::as_hms(input$start_day_analysis)
                )
       

            # Building the dataframe with intensity marks
              df_with_computed_metrics <-
                df() %>%
                mark_intensity(
                  col_axis = cut_points()$axis_mvpa_chosen, 
                  sed_cutpoint = cut_points()$sed_cutpoint_chosen, 
                  mpa_cutpoint = cut_points()$mpa_cutpoint_chosen, 
                  vpa_cutpoint = cut_points()$vpa_cutpoint_chosen,
                  equation = input$equation_mets,
                  age = input$age,
                  weight = input$weight,
                  sex = input$sex,
                  dates = input$selected_days
                  )
    
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
    
     
             # Returning the list of the results and chosen parameters
               return(list(df_with_computed_metrics = df_with_computed_metrics,
                           results_by_day = results_by_day, 
                           axis_sed_chosen_name = cut_points()$axis_sed_chosen_name, 
                           sed_cutpoint_chosen = cut_points()$sed_cutpoint_chosen, 
                           axis_mvpa_chosen_name = cut_points()$axis_mvpa_chosen_name,
                           mpa_cutpoint_chosen = cut_points()$mpa_cutpoint_chosen,
                           vpa_cutpoint_chosen = cut_points()$vpa_cutpoint_chosen))
    })
 
      
      
  # Creating reactive time filters used for the plot with intensity metrics
    analysis_filters <- eventReactive(input$Run, {
      list(start_day_analysis = input$start_day_analysis,
           end_day_analysis = input$end_day_analysis
           )
      })
    
  # Plotting data with intensity categories
    output$graph_int <- renderPlot({
      if (as.numeric(results_list()$df_with_computed_metrics$time[2] - results_list()$df_with_computed_metrics$time[1]) < 10) { 
        ggplot2::ggplot() + ggplot2::geom_text(
          ggplot2::aes(
            x = 1,
            y  = 1,
            label = "Sorry, below 10-s epochs, we prefer \nnot to build the plot to save your time..."),
          size = 10
          ) +
          ggplot2::theme(
            axis.title = ggplot2::element_blank(),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank()
            )
      } else {
      plot_data_with_intensity(data = results_list()$df_with_computed_metrics, 
                               metric = input$Metric2,
                               valid_wear_time_start = analysis_filters()$start_day_analysis,
                               valid_wear_time_end = analysis_filters()$end_day_analysis)
      }
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
    
  
  #########################################
  # Getting PROACTIVE questionnaire results ----
  #########################################
    
  # Computing PROactive accelerometer scores for C-PPAC
      
      # Score for steps count based on daily median of valid days
        steps_score_cppac_median <- reactive({
         req(nrow(results_summary_medians()) == 1)
         compute_pro_actigraph_score(results_summary_medians()[["total_steps"]], quest = "C-PPAC", metric = "steps", fun = "median")
        })
      
      # Score for VMU based on daily median of valid days
        vmu_score_cppac_median <- reactive({
         req(nrow(results_summary_medians()) == 1)
         compute_pro_actigraph_score(results_summary_medians()[["vm_per_min"]], quest = "C-PPAC", metric = "vm", fun = "median")
        })
      
      
      # Score for steps count based on daily mean of valid days
        steps_score_cppac_mean <- reactive({
         req(nrow(results_summary_means()) == 1)
         compute_pro_actigraph_score(results_summary_means()[["total_steps"]], quest = "C-PPAC", metric = "steps", fun = "mean")
        })
      
      # Score for VMU based on daily mean of valid days
        vmu_score_cppac_mean <- reactive({
         req(nrow(results_summary_means()) == 1)
         compute_pro_actigraph_score(results_summary_means()[["vm_per_min"]], quest = "C-PPAC", metric = "vm", fun = "mean")
        })
      
  # Showing PROactive accelerometer scores in tables for the week (C-PPAC) or for each day (D-PPAC)
    
        # C-PPAC (EN) / Steps: Based on the MEDIAN of valid days
          output$table_cppac_en_steps_med <- reactable::renderReactable({
          
          # Default table    
            table <- tibble::tribble(
               ~Score,     ~Range,
               0,          "<=1000",
               1,          "1001-2000",
               2,          "2001-4000",
               3,          "4001-6000",                             
               4,          ">6000"
             )
            
          # Information to show depending on the available data
            if(is.na(steps_score_cppac_median())) {
              reactable::reactable(
                tibble::tribble(
                  ~"Sorry, no results is available due to absence of any valid day.", 
                  ""      
                )
              )
            } else {
                reactable::reactable(
                  table,
                  defaultColDef = reactable::colDef(align = "center"),
                  columns = list(
                    Score = reactable::colDef(
                      style = function(value, index) {
                        if (table$Score[index] == steps_score_cppac_median()) {
                          color_bg <- "#337ab7"
                          color_text <- "white"
                        } else {
                          color_bg <- ""
                          color_text <- "black"
                        }
                        list(background = color_bg, color = color_text)
                      }
                     ),
                    Range = reactable::colDef(
                      style = function(value, index) {
                        if (table$Score[index] == steps_score_cppac_median()) {
                          color_bg <- "#337ab7"
                          color_text <- "white"
                        } else {
                          color_bg <- ""
                          color_text <- "black"
                        }
                        list(background = color_bg, color = color_text)
                      }
                    )
                    ),
                  striped = TRUE,
                )
   
                 }
              })
           
        
        # C-PPAC (EN) / VMU: Based on the MEDIAN of valid days
          output$table_cppac_en_vmu_med <- reactable::renderReactable({
            
            # Default table
              table <- tibble::tribble(
                ~Score,      ~Range,
                0,           "<=100",
                1,           "101-200",
                2,           "201-300",
                3,           "301-500",                             
                4,           ">500"
              )
            # Information to show depending on the available data
              if(is.na(vmu_score_cppac_median())) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to absence of any valid day.", 
                    ""      
                  )
                )
              } else {
              
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score_cppac_median()) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score_cppac_median()) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              }
            })
          
        
        # C-PPAC (EN) / Steps: Based on the MEAN of valid days
          output$table_cppac_en_steps_mean <- reactable::renderReactable({
            
            # Default table
              table <- tibble::tribble(
                ~Score,       ~Range,
                0,            "<=1300",
                1,            "1301-2200",
                2,            "2201-4000",
                3,            "4001-5700",                             
                4,            ">5700"
              )
              
           # Information to show depending on the available data
             if(is.na(steps_score_cppac_mean())) {
               reactable::reactable(
                 tibble::tribble(
                   ~"Sorry, no results is available due to absence of any valid day.", 
                   ""      
                 )
               )
             } else {
               
              reactable::reactable(
                 table,
                 defaultColDef = reactable::colDef(align = "center"),
                 columns = list(
                   Score = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score_cppac_mean()) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   ),
                   Range = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score_cppac_mean()) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   )
                 ),
                 striped = TRUE,
               )

             }
        })
    
        # C-PPAC (EN) / VMU: Based on the MEAN of valid days
          output$table_cppac_en_vmu_mean <- reactable::renderReactable({
            
           # Default table
             table <- tibble::tribble(
               ~Score,   ~Range,
               0,        "<=180",
               1,        "181-260",
               2,        "261-350",
               3,        "351-490",                             
               4,        ">490"
             )
            
           # Information to show depending on the available data
             if(is.na(vmu_score_cppac_mean())) {
               reactable::reactable(
                 tibble::tribble(
                   ~"Sorry, no results is available due to absence of any valid day.", 
                   ""      
                 )
               )
             } else {
               
             reactable::reactable(
               table,
               defaultColDef = reactable::colDef(align = "center"),
               columns = list(
                 Score = reactable::colDef(
                   style = function(value, index) {
                     if (table$Score[index] == vmu_score_cppac_mean()) {
                       color_bg <- "#337ab7"
                       color_text <- "white"
                     } else {
                       color_bg <- ""
                       color_text <- "black"
                     }
                     list(background = color_bg, color = color_text)
                   }
                 ),
                 Range = reactable::colDef(
                   style = function(value, index) {
                     if (table$Score[index] == vmu_score_cppac_mean()) {
                       color_bg <- "#337ab7"
                       color_text <- "white"
                     } else {
                       color_bg <- ""
                       color_text <- "black"
                     }
                     list(background = color_bg, color = color_text)
                   }
                 )
               ),
               striped = TRUE,
             )
             
             }
          })
          
    # ============================================================================================================================================================
          
        # C-PPAC (FR) / Steps: Based on the MEDIAN of valid days
          output$table_cppac_en_steps_med_fr <- reactable::renderReactable({
           
            # Default table
              table <- tibble::tribble(
                ~Score,              ~Intervalle,
                0,                   "<=1000",
                1,                   "1001-2000",
                2,                   "2001-4000",
                3,                   "4001-6000",                             
                4,                   ">6000"
              )
            
           # Information to show depending on the available data
             if(is.na(steps_score_cppac_median())) {
               reactable::reactable(
                 tibble::tribble(
                   ~"Désolé, aucun score n'est disponible en raison de l'absence de jour valide.", 
                   ""      
                 )
               )
             } else {
               
             reactable::reactable(
                 table,
                 defaultColDef = reactable::colDef(align = "center"),
                 columns = list(
                   Score = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score_cppac_median()) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   ),
                   Intervalle = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score_cppac_median()) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   )
                 ),
                 striped = TRUE,
               )
             }
          })
        
        
        # C-PPAC (FR) / VMU: Based on the MEDIAN of valid days
          output$table_cppac_en_vmu_med_fr <- reactable::renderReactable({
            
            # Default table
              table <- tibble::tribble(
                ~Score,       ~Intervalle,
                0,            "<=100",
                1,            "101-200",
                2,            "201-300",
                3,            "301-500",                             
                4,            ">500"
              )
            
            # Information to show depending on the available data
              if(is.na(vmu_score_cppac_median())) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun score n'est disponible en raison de l'absence de jour valide.", 
                    ""      
                  )
                )
              } else {
                
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score_cppac_median()) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score_cppac_median()) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              }
          })
          
        
        # C-PPAC (FR) / Steps: Based on the MEAN of valid days
          output$table_cppac_en_steps_mean_fr <- reactable::renderReactable({
            
            # Default table
              table <- tibble::tribble(
                ~Score,       ~Intervalle,
                0,            "<=1300",
                1,            "1301-2200",
                2,            "2201-4000",
                3,            "4001-5700",                             
                4,            ">5700"
              )
             
            # Information to show depending on the available data
              if(is.na(steps_score_cppac_mean())) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun score n'est disponible en raison de l'absence de jour valide.", 
                    ""      
                  )
                )
              } else {
                
              reactable::reactable(
                  table,
                  defaultColDef = reactable::colDef(align = "center"),
                  columns = list(
                    Score = reactable::colDef(
                      style = function(value, index) {
                        if (table$Score[index] == steps_score_cppac_mean()) {
                          color_bg <- "#337ab7"
                          color_text <- "white"
                        } else {
                          color_bg <- ""
                          color_text <- "black"
                        }
                        list(background = color_bg, color = color_text)
                      }
                    ),
                    Intervalle = reactable::colDef(
                      style = function(value, index) {
                        if (table$Score[index] == steps_score_cppac_mean()) {
                          color_bg <- "#337ab7"
                          color_text <- "white"
                        } else {
                          color_bg <- ""
                          color_text <- "black"
                        }
                        list(background = color_bg, color = color_text)
                      }
                    )
                  ),
                  striped = TRUE,
                )
              }
            })
         
        # C-PPAC (FR) / VMU: Based on the MEAN of valid days
          output$table_cppac_en_vmu_mean_fr <- reactable::renderReactable({
            
            # Default table
              table <- tibble::tribble(
                ~Score,       ~Intervalle,
                0,            "<=180",
                1,            "181-260",
                2,            "261-350",
                3,            "351-490",                             
                4,            ">490"
              )
            
            # Information to show depending on the available data
              if(is.na(vmu_score_cppac_mean())) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun score n'est disponible en raison de l'absence de jour valide.", 
                    ""      
                  )
                )
              } else {
              
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score_cppac_mean()) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score_cppac_mean()) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              }
          })
          
    # ============================================================================================================================================================
        
        # D-PPAC (EN) DAY 1 / Steps
          output$table_dppac_en_d1_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[1, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,        ~Range,
                0,             "<=1000",
                1,             "1001-3000",
                2,             "3001-5000",
                3,             "5001-7000",                             
                4,             ">7000"
              )
            
            # Information to show depending on the available data
             if(nrow(results_list()$results_by_day) < 1 | results_list()$results_by_day[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
               reactable::reactable(
                 tibble::tribble(
                   ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                   ""      
                 )
               )
             } else {
               
          
             reactable::reactable(
                 table,
                 defaultColDef = reactable::colDef(align = "center"),
                 columns = list(
                   Score = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   ),
                   Range = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   )
                 ),
                 striped = TRUE,
               )
         
             }
           })
        
        # D-PPAC (EN) DAY 1 / VMU
          output$table_dppac_en_d1_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[1, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
              # Default table
                table <- tibble::tribble(
                  ~Score,        ~Range,
                  0,             "<=100",
                  1,             "101-200",
                  2,             "201-300",
                  3,             "301-400",
                  4,             "401-600",                             
                  5,             ">600"
                )
            
              # Information to show depending on the available data
                if(nrow(results_list()$results_by_day) < 1 | results_list()$results_by_day[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                  reactable::reactable(
                    tibble::tribble(
                      ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                      ""      
                    )
                  )
                } else {
                reactable::reactable(
                  table,
                  defaultColDef = reactable::colDef(align = "center"),
                  columns = list(
                    Score = reactable::colDef(
                      style = function(value, index) {
                        if (table$Score[index] == vmu_score) {
                          color_bg <- "#337ab7"
                          color_text <- "white"
                        } else {
                          color_bg <- ""
                          color_text <- "black"
                        }
                        list(background = color_bg, color = color_text)
                      }
                    ),
                    Range = reactable::colDef(
                      style = function(value, index) {
                        if (table$Score[index] == vmu_score) {
                          color_bg <- "#337ab7"
                          color_text <- "white"
                        } else {
                          color_bg <- ""
                          color_text <- "black"
                        }
                        list(background = color_bg, color = color_text)
                      }
                    )
                  ),
                  striped = TRUE,
                )
                  
                }
   
          })
          
          
        # D-PPAC (EN) DAY 2 / Steps
          output$table_dppac_en_d2_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[2, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,          ~Range,
                0,               "<=1000",
                1,               "1001-3000",
                2,               "3001-5000",
                3,               "5001-7000",                             
                4,               ">7000"
              )
            
            # Information to show depending on the available data
              if(nrow(results_list()$results_by_day) < 2 | results_list()$results_by_day[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
                
              }
              
          })
 
        # D-PPAC (EN) DAY 2 / VMU
          output$table_dppac_en_d2_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[2, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,              ~Range,
                0,                   "<=100",
                1,                   "101-200",
                2,                   "201-300",
                3,                   "301-400",
                4,                   "401-600",                             
                5,                   ">600"
              )
            
            # Information to show depending on the available data
              if(nrow(results_list()$results_by_day) < 2 | results_list()$results_by_day[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
                
              }
              
          })
        
          
        # D-PPAC (EN) DAY 3 / Steps
          output$table_dppac_en_d3_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[3, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,          ~Range,
                0,               "<=1000",
                1,               "1001-3000",
                2,               "3001-5000",
                3,               "5001-7000",                             
                4,               ">7000"
              )
            
            # Information to show depending on the available data
              if(nrow(results_list()$results_by_day) < 3 | results_list()$results_by_day[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
          })

        # D-PPAC (EN) DAY 3 / VMU
          output$table_dppac_en_d3_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[3, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,         ~Range,
                0,              "<=100",
                1,              "101-200",
                2,              "201-300",
                3,              "301-400",
                4,              "401-600",                             
                5,              ">600"
              )
            
            # Information to show depending on the available data
              if(nrow(results_list()$results_by_day) < 3 | results_list()$results_by_day[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.",  
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              }
              
          })
        
        
        # D-PPAC (EN) DAY 4 / Steps
          output$table_dppac_en_d4_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[4, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,    ~Range,
                0,         "<=1000",
                1,         "1001-3000",
                2,         "3001-5000",
                3,         "5001-7000",                             
                4,         ">7000"
              )
            
            # Information to show depending on the available data
              if(nrow(results_list()$results_by_day) < 4 | results_list()$results_by_day[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
            
          })
          
        
        # D-PPAC (EN) DAY 4 / VMU
        output$table_dppac_en_d4_vmu <- reactable::renderReactable({
          
          # Computing daily PROactive VMU score
            vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[4, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
         
          # Default table
            table <- tibble::tribble(
              ~Score,      ~Range,
              0,           "<=100",
              1,           "101-200",
              2,           "201-300",
              3,           "301-400",
              4,           "401-600",                             
              5,           ">600"
            )
          
          # Information to show depending on the available data
            if(nrow(results_list()$results_by_day) < 4 | results_list()$results_by_day[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
              reactable::reactable(
                tibble::tribble(
                  ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                  ""      
                )
              )
            } else {
            reactable::reactable(
              table,
              defaultColDef = reactable::colDef(align = "center"),
              columns = list(
                Score = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == vmu_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                ),
                Range = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == vmu_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                )
              ),
              striped = TRUE,
            )
            
            }
            
        })
        
        
        # D-PPAC (EN) DAY 5 / Steps
          output$table_dppac_en_d5_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[5, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,          ~Range,
                0,               "<=1000",
                1,               "1001-3000",
                2,               "3001-5000",
                3,               "5001-7000",                             
                4,               ">7000"
              )
            
            # Information to show depending on the available data
              if(nrow(results_list()$results_by_day) < 5 | results_list()$results_by_day[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
            
          })
          
        # D-PPAC (EN) DAY 5 / VMU
          output$table_dppac_en_d5_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[5, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,           ~Range,
                0,                "<=100",
                1,                "101-200",
                2,                "201-300",
                3,                "301-400",
                4,                "401-600",                             
                5,                ">600"
              )
            
            # Information to show depending on the available data
              if(nrow(results_list()$results_by_day) < 5 | results_list()$results_by_day[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
          })
        
        
        # D-PPAC (EN) DAY 6 / Steps
          output$table_dppac_en_d6_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[6, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,             ~Range,
                0,                  "<=1000",
                1,                  "1001-3000",
                2,                  "3001-5000",
                3,                  "5001-7000",                             
                4,                  ">7000"
              )
            
            # Information to show depending on the available data  
              if(nrow(results_list()$results_by_day) < 6 | results_list()$results_by_day[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
                
              }
              
          })
        
        # D-PPAC (EN) DAY 6 / VMU
          output$table_dppac_en_d6_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[6, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,            ~Range,
                0,                 "<=100",
                1,                 "101-200",
                2,                 "201-300",
                3,                 "301-400",
                4,                 "401-600",                             
                5,                 ">600"
              )
            
            # Information to show depending on the available data  
              if(nrow(results_list()$results_by_day) < 6 | results_list()$results_by_day[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
                
              }
            
          })
          
        
        # D-PPAC (EN) DAY 7 / Steps
          output$table_dppac_en_d7_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[7, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,          ~Range,
                0,               "<=1000",
                1,               "1001-3000",
                2,               "3001-5000",
                3,               "5001-7000",                             
                4,               ">7000"
              )
            
            # Information to show depending on the available data  
              if(nrow(results_list()$results_by_day) < 7 | results_list()$results_by_day[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              }
              
          })
          
        # D-PPAC (EN) DAY 7 / VMU
          output$table_dppac_en_d7_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[7, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table 
              table <- tibble::tribble(
                ~Score,          ~Range,
                0,               "<=100",
                1,               "101-200",
                2,               "201-300",
                3,               "301-400",
                4,               "401-600",                             
                5,               ">600"
              )
            
            # Information to show depending on the available data   
              if(nrow(results_list()$results_by_day) < 7 | results_list()$results_by_day[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Sorry, no results is available due to an insufficient number of selected days or due to insufficient wear time for this day.",  
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Range = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
          })
        
    # ============================================================================================================================================================
          
        # D-PPAC (FR) DAY 1 / Steps
          output$table_dppac_fr_d1_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[1, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,         ~Intervalle,
                0,              "<=1000",
                1,              "1001-3000",
                2,              "3001-5000",
                3,              "5001-7000",                             
                4,              ">7000"
              )
            
            # Information to show depending on the available data  
              if(nrow(results_list()$results_by_day) < 1 | results_list()$results_by_day[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.",  
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
            
          })
  
        # D-PPAC (FR) DAY 1 / VMU
          output$table_dppac_fr_d1_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[1, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,                              ~Intervalle,
                0,                                   "<=100",
                1,                                   "101-200",
                2,                                   "201-300",
                3,                                   "301-400",
                4,                                   "401-600",                             
                5,                                   ">600"
              )
            
           # Information to show depending on the available data 
             if(nrow(results_list()$results_by_day) < 1 | results_list()$results_by_day[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
               reactable::reactable(
                 tibble::tribble(
                   ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                   ""      
                 )
               )
             } else {
             reactable::reactable(
               table,
               defaultColDef = reactable::colDef(align = "center"),
               columns = list(
                 Score = reactable::colDef(
                   style = function(value, index) {
                     if (table$Score[index] == vmu_score) {
                       color_bg <- "#337ab7"
                       color_text <- "white"
                     } else {
                       color_bg <- ""
                       color_text <- "black"
                     }
                     list(background = color_bg, color = color_text)
                   }
                 ),
                 Intervalle = reactable::colDef(
                   style = function(value, index) {
                     if (table$Score[index] == vmu_score) {
                       color_bg <- "#337ab7"
                       color_text <- "white"
                     } else {
                       color_bg <- ""
                       color_text <- "black"
                     }
                     list(background = color_bg, color = color_text)
                   }
                 )
               ),
               striped = TRUE,
             )
             
             }
             
           })
          
           
        # D-PPAC (FR) DAY 2 / Steps
        output$table_dppac_fr_d2_steps <- reactable::renderReactable({
          
          # Computing daily PROactive steps score
            steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[2, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
          
          # Default table
            table <- tibble::tribble(
              ~Score,            ~Intervalle,
              0,                 "<=1000",
              1,                 "1001-3000",
              2,                 "3001-5000",
              3,                 "5001-7000",                             
              4,                 ">7000"
            )
          
          # Information to show depending on the available data 
            if(nrow(results_list()$results_by_day) < 2 | results_list()$results_by_day[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
              reactable::reactable(
                tibble::tribble(
                  ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                  ""      
                )
              )
            } else {
            reactable::reactable(
              table,
              defaultColDef = reactable::colDef(align = "center"),
              columns = list(
                Score = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == steps_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                ),
                Intervalle = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == steps_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                )
              ),
              striped = TRUE,
            )
            
            }
            
          })

        # D-PPAC (FR) DAY 2 / VMU
        output$table_dppac_fr_d2_vmu <- reactable::renderReactable({
          
          # Computing daily PROactive VMU score
            vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[2, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
          
          # Default table
            table <- tibble::tribble(
              ~Score,          ~Intervalle,
              0,               "<=100",
              1,               "101-200",
              2,               "201-300",
              3,               "301-400",
              4,               "401-600",                             
              5,               ">600"
            )
          
          # Information to show depending on the available data 
            if(nrow(results_list()$results_by_day) < 2 | results_list()$results_by_day[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
              reactable::reactable(
                tibble::tribble(
                  ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                  ""      
                )
              )
            } else {
            reactable::reactable(
              table,
              defaultColDef = reactable::colDef(align = "center"),
              columns = list(
                Score = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == vmu_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                ),
                Intervalle = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == vmu_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                )
              ),
              striped = TRUE,
            )
            
            }
            
        })
        
        
        # D-PPAC (FR) DAY 3 / Steps
        output$table_dppac_fr_d3_steps <- reactable::renderReactable({
          
          # Computing daily PROactive steps score
            steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[3, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
          
          # Default table
            table <- tibble::tribble(
              ~Score,          ~Intervalle,
              0,               "<=1000",
              1,               "1001-3000",
              2,               "3001-5000",
              3,               "5001-7000",                             
              4,               ">7000"
            )
          
          # Information to show depending on the available data 
            if(nrow(results_list()$results_by_day) < 3 | results_list()$results_by_day[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
              reactable::reactable(
                tibble::tribble(
                  ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                  ""      
                )
              )
            } else {
            reactable::reactable(
              table,
              defaultColDef = reactable::colDef(align = "center"),
              columns = list(
                Score = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == steps_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                ),
                Intervalle = reactable::colDef(
                  style = function(value, index) {
                    if (table$Score[index] == steps_score) {
                      color_bg <- "#337ab7"
                      color_text <- "white"
                    } else {
                      color_bg <- ""
                      color_text <- "black"
                    }
                    list(background = color_bg, color = color_text)
                  }
                )
              ),
              striped = TRUE,
            )
              
            }
          
        })
      
        # D-PPAC (FR) DAY 3 / VMU
          output$table_dppac_fr_d3_vmu <- reactable::renderReactable({
          
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[3, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
          
            # Default table
              table <- tibble::tribble(
                ~Score,        ~Intervalle,
                0,             "<=100",
                1,             "101-200",
                2,             "201-300",
                3,             "301-400",
                4,             "401-600",                             
                5,             ">600"
              )
          
            # Information to show depending on the available data 
              if(nrow(results_list()$results_by_day) < 3 | results_list()$results_by_day[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
        })
        
        
        # D-PPAC (FR) DAY 4 / Steps
          output$table_dppac_fr_d4_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[4, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,                              ~Intervalle,
                0,                                   "<=1000",
                1,                                   "1001-3000",
                2,                                   "3001-5000",
                3,                                   "5001-7000",                             
                4,                                   ">7000"
              )
            
            # Information to show depending on the available data     
              if(nrow(results_list()$results_by_day) < 4 | results_list()$results_by_day[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
            
          })
 
        # D-PPAC (FR) DAY 4 / VMU
          output$table_dppac_fr_d4_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[4, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,     ~Intervalle,
                0,          "<=100",
                1,          "101-200",
                2,          "201-300",
                3,          "301-400",
                4,          "401-600",                             
                5,          ">600"
              )
            
            # Information to show depending on the available data  
              if(nrow(results_list()$results_by_day) < 4 | results_list()$results_by_day[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.",  
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
          })
          
        
        # D-PPAC (FR) DAY 5 / Steps
          output$table_dppac_fr_d5_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[5, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,      ~Intervalle,
                0,           "<=1000",
                1,           "1001-3000",
                2,           "3001-5000",
                3,           "5001-7000",                             
                4,           ">7000"
              )
            
            # Information to show depending on the available data  
              if(nrow(results_list()$results_by_day) < 5 | results_list()$results_by_day[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
          })
          
        # D-PPAC (FR) DAY 5 / VMU
          output$table_dppac_fr_d5_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[5, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,         ~Intervalle,
                0,              "<=100",
                1,              "101-200",
                2,              "201-300",
                3,              "301-400",
                4,              "401-600",                             
                5,              ">600"
              )
              
            # Information to show depending on the available data 
              if(nrow(results_list()$results_by_day) < 5 | results_list()$results_by_day[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
            })
            
          
        # D-PPAC (FR) DAY 6 / Steps
          output$table_dppac_fr_d6_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[6, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,      ~Intervalle,
                0,           "<=1000",
                1,           "1001-3000",
                2,           "3001-5000",
                3,           "5001-7000",                             
                4,           ">7000"
              )
            
             # Information to show depending on the available data 
               if(nrow(results_list()$results_by_day) < 6 | results_list()$results_by_day[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                 reactable::reactable(
                   tibble::tribble(
                     ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                     ""      
                   )
                 )
               } else {
               reactable::reactable(
                 table,
                 defaultColDef = reactable::colDef(align = "center"),
                 columns = list(
                   Score = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   ),
                   Intervalle = reactable::colDef(
                     style = function(value, index) {
                       if (table$Score[index] == steps_score) {
                         color_bg <- "#337ab7"
                         color_text <- "white"
                       } else {
                         color_bg <- ""
                         color_text <- "black"
                       }
                       list(background = color_bg, color = color_text)
                     }
                   )
                 ),
                 striped = TRUE,
               )
               
               }
               
          })

        # D-PPAC (FR) DAY 6 / VMU
          output$table_dppac_fr_d6_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[6, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table  
              table <- tibble::tribble(
                ~Score,         ~Intervalle,
                0,              "<=100",
                1,              "101-200",
                2,              "201-300",
                3,              "301-400",
                4,              "401-600",                             
                5,              ">600"
              )
            
            # Information to show depending on the available data 
              if(nrow(results_list()$results_by_day) < 6 | results_list()$results_by_day[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
          })
        
          
        # D-PPAC (FR) DAY 7 / Steps
          output$table_dppac_fr_d7_steps <- reactable::renderReactable({
            
            # Computing daily PROactive steps score
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day[7, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
            # Default table
              table <- tibble::tribble(
                ~Score,          ~Intervalle,
                0,               "<=1000",
                1,               "1001-3000",
                2,               "3001-5000",
                3,               "5001-7000",                             
                4,               ">7000"
              )
            
            
            # Information to show depending on the available data 
              if(nrow(results_list()$results_by_day) < 7 | results_list()$results_by_day[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == steps_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
          })
          
        # D-PPAC (FR) DAY 7 / VMU
          output$table_dppac_fr_d7_vmu <- reactable::renderReactable({
            
            # Computing daily PROactive VMU score
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day[7, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
            # Default table
              table <- tibble::tribble(
                ~Score,                              ~Intervalle,
                0,                                   "<=100",
                1,                                   "101-200",
                2,                                   "201-300",
                3,                                   "301-400",
                4,                                   "401-600",                             
                5,                                   ">600"
              )
            
            # Information to show depending on the available data 
              if(nrow(results_list()$results_by_day) < 7 | results_list()$results_by_day[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"Désolé, aucun résultat n'est disponible en raison d'un nombre insuffisant de jours sélectionnés ou d'un temps de port insuffisant pour ce jour.", 
                    ""      
                  )
                )
              } else {
              reactable::reactable(
                table,
                defaultColDef = reactable::colDef(align = "center"),
                columns = list(
                  Score = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  ),
                  Intervalle = reactable::colDef(
                    style = function(value, index) {
                      if (table$Score[index] == vmu_score) {
                        color_bg <- "#337ab7"
                        color_text <- "white"
                      } else {
                        color_bg <- ""
                        color_text <- "black"
                      }
                      list(background = color_bg, color = color_text)
                    }
                  )
                ),
                striped = TRUE,
              )
              
              }
              
          })
          
   # ===================================================================================================================================================
          
        # Getting PROactive final results: C-PPAC (EN) ----
        
            # Setting steps score
              chosen_proactive_cppac_steps_score_en <- eventReactive(input$get_cppac_summary_en, {
                
              # Waiting for required conditions
                req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
              
              # Choosing score
                if (input$cppac_EN_summary_metric == "Scores based on MEDIANS") {
                  score <- steps_score_cppac_median()
                } else {
                  score <- steps_score_cppac_mean()
                }
              
              return(score)
            })
            
            # Setting vmu score
              chosen_proactive_cppac_vmu_score_en <- eventReactive(input$get_cppac_summary_en, {
              
              # Waiting for required conditions
                req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
              
              # Choosing score
                if (input$cppac_EN_summary_metric == "Scores based on MEDIANS") {
                  score <- vmu_score_cppac_median()
                } else {
                  score <- vmu_score_cppac_mean()
                }
                
              return(score)
            })
        
            # Building table of results
              tab_cppac_summary_en <- eventReactive(input$get_cppac_summary_en, {
                
                # Waiting for required conditions
                  req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
              
                # Building table
                  table <- 
                    tibble::tribble(
                      ~Item                                                                                                                              ,~"Difficulty score"                                              ,~"Amount score",
                      "In the past 7 days, how much walking did you do outside?"                                                                             , NA                                                              ,  compute_pro_score_cppac(input$cppac_EN_q1, question = "q1"),
                      "In the past 7 days, how many chores did you do outside the house?"                                                                    , NA                                                              ,  compute_pro_score_cppac(input$cppac_EN_q2, question = "q2"),
                      "In the past 7 days, how much difficulty did you have getting dressed?"                                                                , compute_pro_score_cppac(input$cppac_EN_q3, question = "q3")     ,  NA,
                      "In the past 7 days, how much difficulty did you have getting out and about?"                                                          , compute_pro_score_cppac(input$cppac_EN_q4, question = "q4")     ,  NA,
                      "In the past 7 days, how often did you avoid doing activities because of your lung problems?"                                          , compute_pro_score_cppac(input$cppac_EN_q5, question = "q5")     ,  NA,
                      "In the past 7 days, how breathless were you in general during your activities?"                                                       , compute_pro_score_cppac(input$cppac_EN_q6, question = "q6")     ,  NA,
                      "In the past 7 days, how often did you lack physical strength to do things because of your lung problems?"                             , compute_pro_score_cppac(input$cppac_EN_q7, question = "q7")     ,  NA,
                      "In the past 7 days, how tired were you in general during your activities?"                                                            , compute_pro_score_cppac(input$cppac_EN_q8, question = "q8")     ,  NA,
                      "In the past 7 days, how often did you have to take breaks during your physical activities?"                                           , compute_pro_score_cppac(input$cppac_EN_q9, question = "q9")     ,  NA,
                      "In the past 7 days, how breathless were you when walking on level ground indoors and outdoors?"                                       , compute_pro_score_cppac(input$cppac_EN_q10, question = "q10")   ,  NA,
                      "In the past 7 days, how much time did you need to recover from your physical activities?"                                             , compute_pro_score_cppac(input$cppac_EN_q11, question = "q11")   ,  NA,
                      "In the past 7 days, did you need to consider your lung problems when you planned your activities because of your lung problems?"      , compute_pro_score_cppac(input$cppac_EN_q12, question = "q12")   ,  NA,
                      "Weekly steps score"                                                                                                                   , NA                                                              ,  chosen_proactive_cppac_steps_score_en(),
                      "Weekly VMU score"                                                                                                                     , NA                                                              ,  chosen_proactive_cppac_vmu_score_en()
                    )
            
              return(table)
              
            })
            
            
            # Showing table of results
              output$PROactive_scores_cppac_summary_en <- reactable::renderReactable({
                
                # Waiting for required conditions
                  req(input$get_cppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Showing table
                  reactable::reactable(
                    tab_cppac_summary_en(),
                    defaultColDef = reactable::colDef(align = "center"),
                    columns = list(
                      Item = reactable::colDef(align = "left"),
                      "Difficulty score" = reactable::colDef(width = 80),
                      "Amount score" = reactable::colDef(width = 80)
                      ),
                    defaultPageSize = 17,
                    striped = TRUE
                  )
              })
            
            # Value Boxes
              
              # ****************
              # Difficulty (raw)
              # ****************
                output$infoBox_cppac_en_total_diff <- shinydashboard::renderValueBox({
                  
                  # Waiting for required conditions
                    req(input$get_cppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                  
                  # Box
                    shinydashboard::valueBox(
                      tags$h3(paste0(sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE), " / 40"), style = "color: white;"),
                      "Difficulty score (raw)", icon = NULL,
                      color = "aqua",
                      width = 4
                    )
                })
            
              # ************
              # Amount (raw)
              # ************
                output$infoBox_cppac_en_total_amount <- shinydashboard::renderValueBox({
                  
                  # Waiting for required conditions
                    req(input$get_cppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                  
                  # Box
                    shinydashboard::valueBox(
                      tags$h3(paste0(sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), " / 15"), style = "color: white;"), 
                      "Amount score (raw)", icon = NULL,
                      color = "teal",
                      width = 4
                    )
                })
            
                
              # ************
              # Total (raw)
              # ************
                output$infoBox_cppac_en_total_all <- shinydashboard::renderValueBox({
                  
                  # Waiting for required conditions
                    req(input$get_cppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                  
                  # Box
                    shinydashboard::valueBox(
                      tags$h3(paste0(sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE) + sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), " / 55"),  style = "color: white;"), 
                      "Total score (raw)",  icon = NULL,
                      color = "purple",
                      width = 4
                    )
                })
            
              # ******************
              # Difficulty (rasch)
              # ******************
                output$infoBox_cppac_en_total_diff_rasch <- shinydashboard::renderValueBox({
                  
                  # Waiting for required conditions
                    req(input$get_cppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                  
                  # Box
                    shinydashboard::valueBox(
                      tags$h3(paste0(rasch_transform(
                        x = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE),
                        quest = "C-PPAC",
                        score = "difficulty"), " / 100"), style = "color: white;"),
                      "Difficulty score (Rasch)", icon = NULL,
                      color = "aqua",
                      width = 4
                    )
                })
            
              # ************
              # Amount (raw)
              # ************                
                output$infoBox_cppac_en_total_amount_rasch <- shinydashboard::renderValueBox({
                  
                  # Waiting for required conditions
                    req(input$get_cppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                  
                  # Box
                    shinydashboard::valueBox(
                      tags$h3(paste0(rasch_transform(
                        x = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE),
                        quest = "C-PPAC",
                        score = "quantity"), " / 100"), style = "color: white;"), 
                      "Amount score (Rasch)", icon = NULL,
                      color = "teal",
                      width = 4
                    )
                })
            
             # ************
             # Total (raw)
             # ************
               output$infoBox_cppac_en_total_all_rasch <- shinydashboard::renderValueBox({
                 
                 # Waiting for required conditions
                   req(input$get_cppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                 
                 # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(
                      round((rasch_transform(x = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                        rasch_transform(x = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1),
                      " / 100"),  style = "color: white;"), 
                    "Total score (Rasch)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
               })
            
            
     # Getting PROactive final results: D-PPAC (EN) ----
            
            # Table with monitor PROactive scores for each day
              table_gt3x_results_en <- eventReactive(input$get_dppac_summary_en, {
                results_list()$results_by_day %>%
                  dplyr::mutate(
                    Selected_Day_ID = seq_along(date),
                    Date = date,
                    Steps_score = compute_pro_actigraph_score(
                      x = .data$total_steps,
                      quest = "D-PPAC",
                      metric = "steps"
                    ),
                    VMU_score = compute_pro_actigraph_score(
                      x = .data$vm_per_min,
                      quest = "D-PPAC",
                      metric = "vmu"
                    ),
                    Validity = ifelse(wear_time >= input$minimum_wear_time_for_analysis * 60, "Valid", "Non-valid")
                  ) %>%
                  dplyr::select(Selected_Day_ID, Date, Steps_score, VMU_score, Validity) %>%
                  dplyr::filter(Selected_Day_ID <= 7)
                
              })
            
            # Table with all results (questionnaire items and monitors scores)
              tab_dppac_summary_en <- eventReactive(input$get_dppac_summary_en, {
                
                # Waiting for required conditions
                  req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) 
                
                # Making 7 tables (each for one assessment day)
                  table_day1 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 1", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 1", "How much walking did you do outside today?"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d1_q1, question = "q1", language = "en"),
                      "Day 1", "How many chores did you do outside the house today? "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d1_q2, question = "q2", language = "en"),
                      "Day 1", "How much difficulty did you have getting dressed today?"                                 , compute_pro_score_dppac(input$dppac_EN_d1_q3, question = "q3", language = "en")     ,  NA,
                      "Day 1", "How often did you avoid doing activities because of your lung problems today?"           , compute_pro_score_dppac(input$dppac_EN_d1_q4, question = "q4", language = "en")     ,  NA,
                      "Day 1", "How breathless were you in general during your activities today?"                        , compute_pro_score_dppac(input$dppac_EN_d1_q5, question = "q5", language = "en")     ,  NA,
                      "Day 1", "How tired were you in general during your activities today?"                             , compute_pro_score_dppac(input$dppac_EN_d1_q6, question = "q6", language = "en")     ,  NA,
                      "Day 1", "How often did you have to take breaks during your physical activities today?"            , compute_pro_score_dppac(input$dppac_EN_d1_q7, question = "q7", language = "en")     ,  NA,
                      "Day 1", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[1] == "Valid", table_gt3x_results_en()$Steps_score[1], NA),
                      "Day 1", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[1] == "Valid", table_gt3x_results_en()$VMU_score[1], NA)
                    )  
                  
                  table_day2 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 2", ""                                                                                        , NA                                                                                ,   NA,              
                      "Day 2", "How much walking did you do outside today?"                                              , NA                                                                                ,  compute_pro_score_dppac(input$dppac_EN_d2_q1, question = "q1", language = "en"),
                      "Day 2", "How many chores did you do outside the house today? "                                    , NA                                                                                ,  compute_pro_score_dppac(input$dppac_EN_d2_q2, question = "q2", language = "en"),
                      "Day 2", "How much difficulty did you have getting dressed today?"                                 , compute_pro_score_dppac(input$dppac_EN_d2_q3, question = "q3", language = "en")   ,  NA,
                      "Day 2", "How often did you avoid doing activities because of your lung problems today?"           , compute_pro_score_dppac(input$dppac_EN_d2_q4, question = "q4", language = "en")   ,  NA,
                      "Day 2", "How breathless were you in general during your activities today?"                        , compute_pro_score_dppac(input$dppac_EN_d2_q5, question = "q5", language = "en")   ,  NA,
                      "Day 2", "How tired were you in general during your activities today?"                             , compute_pro_score_dppac(input$dppac_EN_d2_q6, question = "q6", language = "en")   ,  NA,
                      "Day 2", "How often did you have to take breaks during your physical activities today?"            , compute_pro_score_dppac(input$dppac_EN_d2_q7, question = "q7", language = "en")   ,  NA,
                      "Day 2", "Weekly steps score"                                                                      , NA                                                                                ,  ifelse(table_gt3x_results_en()$Validity[2] == "Valid", table_gt3x_results_en()$Steps_score[2], NA),
                      "Day 2", "Weekly VMU score"                                                                        , NA                                                                                ,  ifelse(table_gt3x_results_en()$Validity[2] == "Valid", table_gt3x_results_en()$VMU_score[2], NA)
                    ) 
                  
                  table_day3 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 3", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 3", "How much walking did you do outside today?"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d3_q1, question = "q1", language = "en"),
                      "Day 3", "How many chores did you do outside the house today? "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d3_q2, question = "q2", language = "en"),
                      "Day 3", "How much difficulty did you have getting dressed today?"                                 , compute_pro_score_dppac(input$dppac_EN_d3_q3, question = "q3", language = "en")     ,  NA,
                      "Day 3", "How often did you avoid doing activities because of your lung problems today?"           , compute_pro_score_dppac(input$dppac_EN_d3_q4, question = "q4", language = "en")     ,  NA,
                      "Day 3", "How breathless were you in general during your activities today?"                        , compute_pro_score_dppac(input$dppac_EN_d3_q5, question = "q5", language = "en")     ,  NA,
                      "Day 3", "How tired were you in general during your activities today?"                             , compute_pro_score_dppac(input$dppac_EN_d3_q6, question = "q6", language = "en")     ,  NA,
                      "Day 3", "How often did you have to take breaks during your physical activities today?"            , compute_pro_score_dppac(input$dppac_EN_d3_q7, question = "q7", language = "en")     ,  NA,
                      "Day 3", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[3] == "Valid", table_gt3x_results_en()$Steps_score[3], NA),
                      "Day 3", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[3] == "Valid", table_gt3x_results_en()$VMU_score[3], NA)
                    ) 
                  
                  table_day4 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 4", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 4", "How much walking did you do outside today?"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d4_q1, question = "q1", language = "en"),
                      "Day 4", "How many chores did you do outside the house today? "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d4_q2, question = "q2", language = "en"),
                      "Day 4", "How much difficulty did you have getting dressed today?"                                 , compute_pro_score_dppac(input$dppac_EN_d4_q3, question = "q3", language = "en")     ,  NA,
                      "Day 4", "How often did you avoid doing activities because of your lung problems today?"           , compute_pro_score_dppac(input$dppac_EN_d4_q4, question = "q4", language = "en")     ,  NA,
                      "Day 4", "How breathless were you in general during your activities today?"                        , compute_pro_score_dppac(input$dppac_EN_d4_q5, question = "q5", language = "en")     ,  NA,
                      "Day 4", "How tired were you in general during your activities today?"                             , compute_pro_score_dppac(input$dppac_EN_d4_q6, question = "q6", language = "en")     ,  NA,
                      "Day 4", "How often did you have to take breaks during your physical activities today?"            , compute_pro_score_dppac(input$dppac_EN_d4_q7, question = "q7", language = "en")     ,  NA,
                      "Day 4", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[4] == "Valid", table_gt3x_results_en()$Steps_score[4], NA),
                      "Day 4", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[4] == "Valid", table_gt3x_results_en()$VMU_score[4], NA)
                    ) 
                  table_day5 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 5", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 5", "How much walking did you do outside today?"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d5_q1, question = "q1", language = "en"),
                      "Day 5", "How many chores did you do outside the house today? "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d5_q2, question = "q2", language = "en"),
                      "Day 5", "How much difficulty did you have getting dressed today?"                                 , compute_pro_score_dppac(input$dppac_EN_d5_q3, question = "q3", language = "en")     ,  NA,
                      "Day 5", "How often did you avoid doing activities because of your lung problems today?"           , compute_pro_score_dppac(input$dppac_EN_d5_q4, question = "q4", language = "en")     ,  NA,
                      "Day 5", "How breathless were you in general during your activities today?"                        , compute_pro_score_dppac(input$dppac_EN_d5_q5, question = "q5", language = "en")     ,  NA,
                      "Day 5", "How tired were you in general during your activities today?"                             , compute_pro_score_dppac(input$dppac_EN_d5_q6, question = "q6", language = "en")     ,  NA,
                      "Day 5", "How often did you have to take breaks during your physical activities today?"            , compute_pro_score_dppac(input$dppac_EN_d5_q7, question = "q7", language = "en")     ,  NA,
                      "Day 5", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[5] == "Valid", table_gt3x_results_en()$Steps_score[5], NA),
                      "Day 5", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[5] == "Valid", table_gt3x_results_en()$VMU_score[5], NA)
                    )               
                  table_day6 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 6", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 6", "How much walking did you do outside today?"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d6_q1, question = "q1", language = "en"),
                      "Day 6", "How many chores did you do outside the house today? "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d6_q2, question = "q2", language = "en"),
                      "Day 6", "How much difficulty did you have getting dressed today?"                                 , compute_pro_score_dppac(input$dppac_EN_d6_q3, question = "q3", language = "en")     ,  NA,
                      "Day 6", "How often did you avoid doing activities because of your lung problems today?"           , compute_pro_score_dppac(input$dppac_EN_d6_q4, question = "q4", language = "en")     ,  NA,
                      "Day 6", "How breathless were you in general during your activities today?"                        , compute_pro_score_dppac(input$dppac_EN_d6_q5, question = "q5", language = "en")     ,  NA,
                      "Day 6", "How tired were you in general during your activities today?"                             , compute_pro_score_dppac(input$dppac_EN_d6_q6, question = "q6", language = "en")     ,  NA,
                      "Day 6", "How often did you have to take breaks during your physical activities today?"            , compute_pro_score_dppac(input$dppac_EN_d6_q7, question = "q7", language = "en")     ,  NA,
                      "Day 6", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[6] == "Valid", table_gt3x_results_en()$Steps_score[6], NA),
                      "Day 6", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[6] == "Valid", table_gt3x_results_en()$VMU_score[6], NA)
                    ) 
                  
                  table_day7 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 7", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 7", "How much walking did you do outside today?"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d7_q1, question = "q1", language = "en"),
                      "Day 7", "How many chores did you do outside the house today? "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d7_q2, question = "q2", language = "en"),
                      "Day 7", "How much difficulty did you have getting dressed today?"                                 , compute_pro_score_dppac(input$dppac_EN_d7_q3, question = "q3", language = "en")     ,  NA,
                      "Day 7", "How often did you avoid doing activities because of your lung problems today?"           , compute_pro_score_dppac(input$dppac_EN_d7_q4, question = "q4", language = "en")     ,  NA,
                      "Day 7", "How breathless were you in general during your activities today?"                        , compute_pro_score_dppac(input$dppac_EN_d7_q5, question = "q5", language = "en")     ,  NA,
                      "Day 7", "How tired were you in general during your activities today?"                             , compute_pro_score_dppac(input$dppac_EN_d7_q6, question = "q6", language = "en")     ,  NA,
                      "Day 7", "How often did you have to take breaks during your physical activities today?"            , compute_pro_score_dppac(input$dppac_EN_d7_q7, question = "q7", language = "en")     ,  NA,
                      "Day 7", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[7] == "Valid", table_gt3x_results_en()$Steps_score[7], NA),
                      "Day 7", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[7] == "Valid", table_gt3x_results_en()$VMU_score[7], NA)
                    ) 
                
                # Combining all tables
                   table_all_days <- 
                     dplyr::bind_rows(table_day1, table_day2, table_day3, table_day4, table_day5, table_day6, table_day7) 
                
                return(table_all_days)
                
              })
            
            # Final table of results
              output$PROactive_scores_dppac_summary_en <-  reactable::renderReactable({
                
                # Waiting for required conditions
                  req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) 
                
                # Showing table
                  reactable::reactable(
                    tab_dppac_summary_en(),
                    rowStyle = function(index) {
                      
                      if ((tab_dppac_summary_en()[index, "Item"] %in% c("Weekly steps score", "Weekly VMU score")) && is.na(tab_dppac_summary_en()[index, "Amount score"])) {
                        list(background = "#FF6666")
                      } else if (tab_dppac_summary_en()[index, "Item"] == "") {
                        list(background = "grey", color = "white", fontWeight = "bold")
                      } else {
                        NULL
                      }
                    },
                    defaultColDef = reactable::colDef(align = "center"),
                    columns = list(
                      Item = reactable::colDef(align = "left"),
                      "Day" = reactable::colDef(width = 60),
                      "Difficulty score" = reactable::colDef(width = 80),
                      "Amount score" = reactable::colDef(width = 80)
                      
                    ),
                    defaultPageSize = 70,
                    striped = TRUE
                  )
                  
              })
            
            # Value Boxes (means of valid days)

              # Making summary table
                recap_ddpac_en <- eventReactive(input$get_dppac_summary_en, {
                  
                  # Waiting for required conditions
                    req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) 
                
                  # Making table
                    tab_dppac_summary_en() %>%
                      dplyr::mutate(score_type = rep(c("", "quant", "quant", "diff", "diff", "diff", "diff", "diff", "quant", "quant"), 7)) %>%
                      dplyr::group_by(Day, score_type) %>%
                      dplyr::filter(Item != "") %>%
                      dplyr::summarise(difficulty_score_raw = sum(.data[["Difficulty score"]], na.rm = TRUE),
                                       amount_score_raw = sum(.data[["Amount score"]])
                      ) %>%
                      tidyr::pivot_wider(values_from = c(difficulty_score_raw, amount_score_raw), names_from = score_type) %>%
                      dplyr::filter(!is.na(amount_score_raw_quant)) %>%
                      dplyr::rename(difficulty_score_raw = difficulty_score_raw_diff, amount_score_raw = amount_score_raw_quant) %>%
                      dplyr::mutate(
                        total_score_raw = difficulty_score_raw + amount_score_raw,
                        difficulty_score_rasch = rasch_transform(x = difficulty_score_raw, quest = "D-PPAC", score = "difficulty"),
                        amount_score_rasch = rasch_transform(x = amount_score_raw, quest = "D-PPAC", score = "quantity"),
                        total_score_rasch = (difficulty_score_rasch + amount_score_rasch) / 2
                      ) %>%
                      dplyr::ungroup() %>%
                      dplyr::summarise(
                        mean_difficulty_score_raw = mean(difficulty_score_raw),
                        mean_amount_score_raw = mean(amount_score_raw),
                        mean_total_score_raw = mean(total_score_raw),
                        mean_difficulty_score_rasch = mean(difficulty_score_rasch),
                        mean_amount_score_rasch = mean(amount_score_rasch),
                        mean_total_score_rasch = mean(total_score_rasch)
                      )
                
              })
              
                
              #****************
              #Difficulty (raw)
              #****************
              output$infoBox_dppac_en_total_diff <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_en()$mean_difficulty_score_raw[1], 1), " / 20"), style = "color: white;"),
                    "Mean difficulty score (raw)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #************
              #Amount (raw)
              #************
              output$infoBox_dppac_en_total_amount <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_en()$mean_amount_score_raw[1], 1), " / 17"), style = "color: white;"), 
                    "Mean amount score (raw)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              #***********
              #Total (raw)
              #***********
              output$infoBox_dppac_en_total_all <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_en()$mean_total_score_raw[1], 1), " / 37"),  style = "color: white;"), 
                    "Mean total score (raw)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })
              
              
              #******************
              #Difficulty (rasch)
              #******************
              output$infoBox_dppac_en_total_diff_rasch <- shinydashboard::renderValueBox({
                req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Waiting for required conditions
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_en()$mean_difficulty_score_rasch[1], 1), " / 100"), style = "color: white;"),
                    "Mean difficulty score (Rasch)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #**************
              #Amount (rasch)
              #**************
              output$infoBox_dppac_en_total_amount_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_en()$mean_amount_score_rasch[1], 1), " / 100"), style = "color: white;"), 
                    "Mean amount score (Rasch)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              
              #*************
              #Total (rasch)
              #*************
              output$infoBox_dppac_en_total_all_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_en()$mean_total_score_rasch[1], 1),
                                   " / 100"),  style = "color: white;"), 
                    "Mean total score (Rasch)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })    
              
      # Getting PROactive final results: C-PPAC (FR) ----
            
            # Setting steps score
              chosen_proactive_cppac_steps_score_fr <- eventReactive(input$get_cppac_summary_fr, {
                
                # Waiting for required conditions
                  req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Choosing score
                  if (input$cppac_FR_summary_metric == "Scores calculés à partir des MÉDIANES") {
                    score <- steps_score_cppac_median()
                  } else {
                    score <- steps_score_cppac_mean()
                  }
                
                return(score)
              })
            
            # Setting vmu score
              chosen_proactive_cppac_vmu_score_fr <- eventReactive(input$get_cppac_summary_fr, {
                
                # Waiting for required conditions
                  req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Choosing score
                  if (input$cppac_FR_summary_metric == "Scores calculés à partir des MÉDIANES") {
                    score <- vmu_score_cppac_median()
                  } else {
                    score <- vmu_score_cppac_mean()
                  }
                
                return(score)
              })
            
            # Building table of results
              tab_cppac_summary_fr <- eventReactive(input$get_cppac_summary_fr, {
              
                # Waiting for required conditions
                  req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Building table
                  table <- 
                    tibble::tribble(
                      ~Item                                                                                                                                             ,~"Score de difficulté"                                                            ,~"Score de quantité",
                      "Au cours des 7 derniers jours, avez-vous marché à l’extérieur ?"                                                                                 , NA                                                                               ,  compute_pro_score_cppac(input$cppac_FR_q1, question = "q1", language = "fr"),
                      "Au cours des 7 derniers jours, avez-vous effectué des tâches à l’extérieur ?"                                                                    , NA                                                                               ,  compute_pro_score_cppac(input$cppac_FR_q2, question = "q2", language = "fr"),
                      "Au cours des 7 derniers jours, avez-vous eu des difficultés pour vous habiller ?"                                                                , compute_pro_score_cppac(input$cppac_FR_q3, question = "q3", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous eu des difficultés pour sortir de chez vous ?"                                                          , compute_pro_score_cppac(input$cppac_FR_q4, question = "q4", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous évité des activités à cause de vos problèmes respiratoires ?"                                           , compute_pro_score_cppac(input$cppac_FR_q5, question = "q5", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, étiez-vous essoufflé(e) en général durant vos activités ?"                                                        , compute_pro_score_cppac(input$cppac_FR_q6, question = "q6", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous manqué de force pour effectuer des tâches à cause de vos problèmes respiratoires ?"                     , compute_pro_score_cppac(input$cppac_FR_q7, question = "q7", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, étiez-vous fatigué(e) en général durant vos activités ?"                                                          , compute_pro_score_cppac(input$cppac_FR_q8, question = "q8", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous dû faire des pauses pendant vos activités physiques ?"                                                  , compute_pro_score_cppac(input$cppac_FR_q9, question = "q9", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, étiez-vous essoufflé(e) lors de la marche sur terrain plat, à l’intérieur et à l’extérieur ?"                     , compute_pro_score_cppac(input$cppac_FR_q10, question = "q10", language = "fr")   ,  NA,
                      "Au cours des 7 derniers jours, combien de temps vous a-t-il fallu pour récupérer de vos activités physiques ?"                                   , compute_pro_score_cppac(input$cppac_FR_q11, question = "q11", language = "fr")   ,  NA,
                      "Au cours des 7 derniers jours, avez-vous eu besoin de prendre en compte vos problèmes respiratoires lorsque vous avez planifié vos activités ?"  , compute_pro_score_cppac(input$cppac_FR_q12, question = "q12", language = "fr")   ,  NA,
                      "Score de pas hebdomadaire"                                                                                                                       , NA                                                                               ,  chosen_proactive_cppac_steps_score_fr(),
                      "Score VMU hebdomadaire"                                                                                                                          , NA                                                                               ,  chosen_proactive_cppac_vmu_score_fr()
                    )
 
              return(table)
              
            })
            
            
          # Showing table of results
            output$PROactive_scores_cppac_summary_fr <- reactable::renderReactable({
              
              # Waiting for required conditions
                req(input$get_cppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
              
              # Showing table
                reactable::reactable(
                  tab_cppac_summary_fr(),
                  defaultColDef = reactable::colDef(align = "center"),
                  columns = list(
                    Item = reactable::colDef(align = "left"),
                    "Score de difficulté" = reactable::colDef(width = 80),
                    "Score de quantité" = reactable::colDef(width = 80)
                  ),
                  defaultPageSize = 17,
                  striped = TRUE
                )
              })
            
            # Value Boxes
            
              #*****************
              # Difficulty (raw)
              #*****************
              output$infoBox_cppac_fr_total_diff <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_cppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(sum(tab_cppac_summary_fr()$"Score de difficulté", na.rm = TRUE), " / 40"), style = "color: white;"),
                    "Score de difficulté (brut)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #*************
              # Amount (raw)
              #*************
              output$infoBox_cppac_fr_total_amount <- shinydashboard::renderValueBox({
              
                 # Waiting for required conditions
                   req(input$get_cppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                 
                 # Box
                 shinydashboard::valueBox(
                   tags$h3(paste0(sum(tab_cppac_summary_fr()$"Score de quantité", na.rm = TRUE), " / 15"), style = "color: white;"), 
                   "Score de quantité (brut)", icon = NULL,
                   color = "teal",
                   width = 4
                 )
            })
            
              #************
              # Total (raw)
              #************
              output$infoBox_cppac_fr_total_all <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_cppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(sum(tab_cppac_summary_fr()$"Score de difficulté", na.rm = TRUE) + sum(tab_cppac_summary_fr()$"Score de quantité", na.rm = TRUE), " / 55"),  style = "color: white;"), 
                    "Score total (brut)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })
            
              #*******************
              # Difficulty (rasch)
              #*******************
              output$infoBox_cppac_fr_total_diff_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_cppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(rasch_transform(
                      x = sum(tab_cppac_summary_fr()$"Score de difficulté", na.rm = TRUE),
                      quest = "C-PPAC",
                      score = "difficulty"), " / 100"), style = "color: white;"),
                    "Score de difficulté (Rasch)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #***************
              # Amount (rasch)
              #***************
              output$infoBox_cppac_fr_total_amount_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_cppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box 
                 shinydashboard::valueBox(
                   tags$h3(paste0(rasch_transform(
                     x = sum(tab_cppac_summary_fr()$"Score de quantité", na.rm = TRUE),
                     quest = "C-PPAC",
                     score = "quantity"), " / 100"), style = "color: white;"), 
                   "Score de quantité (Rasch)", icon = NULL,
                   color = "teal",
                   width = 4
                 )
             })
            
              
              #**************
              # Total (rasch)
              #**************
              output$infoBox_cppac_fr_total_all_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_cppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box 
                  shinydashboard::valueBox(
                    tags$h3(paste0(
                      round((rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de difficulté", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                               rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de quantité", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1),
                      " / 100"),  style = "color: white;"), 
                    "Score total (Rasch)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })
            
            
        # Getting PROactive final results: D-PPAC (FR) ----
            
               # Table with monitor PROactive scores for each day
                 table_gt3x_results_fr <- eventReactive(input$get_dppac_summary_fr, {
                     results_list()$results_by_day %>%
                     dplyr::mutate(
                       Selected_Day_ID = seq_along(date),
                       Date = date,
                       Steps_score = compute_pro_actigraph_score(
                         x = .data$total_steps,
                         quest = "D-PPAC",
                         metric = "steps"
                       ),
                       VMU_score = compute_pro_actigraph_score(
                         x = .data$vm_per_min,
                         quest = "D-PPAC",
                         metric = "vmu"
                       ),
                       Validity = ifelse(wear_time >= input$minimum_wear_time_for_analysis * 60, "Valid", "Non-valid")
                     ) %>%
                     dplyr::select(Selected_Day_ID, Date, Steps_score, VMU_score, Validity) %>%
                     dplyr::filter(Selected_Day_ID <= 7)
                 
                 })
               
               # Table with all results (questionnaire items and monitors scores)
                 tab_dppac_summary_fr <- eventReactive(input$get_dppac_summary_fr, {
                   
                   # Waiting for required conditions
                     req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) 
                   
                   # Making 7 tables (each for one assessment day)
                     table_day1 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                      ,~"Score de difficulté"                                                            ,~"Score de quantité",
                         "Jour 1", ""                                                                                         , NA                                                                              ,   NA,              
                         "Jour 1", "Avez-vous marché à l’extérieur aujourd’hui ?"                                             , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d1_q1, question = "q1", language = "fr"),
                         "Jour 1", "Avez-vous effectué des tâches à l’extérieur aujourd’hui ?"                                , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d1_q2, question = "q2", language = "fr"),
                         "Jour 1", "Avez-vous eu des difficultés pour vous habiller aujourd’hui ?"                            , compute_pro_score_dppac(input$dppac_FR_d1_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 1", "Avez-vous évité des activités à cause de vos problèmes respiratoires aujourd’hui ?"       , compute_pro_score_dppac(input$dppac_FR_d1_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 1", "Etiez-vous essoufflé(e) en général durant vos activités aujourd’hui ?"                    , compute_pro_score_dppac(input$dppac_FR_d1_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 1", "Etiez-vous fatigué(e) en général durant vos activités aujourd’hui ?"                      , compute_pro_score_dppac(input$dppac_FR_d1_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 1", "Avez-vous du faire des pauses pendant vos activités physiques aujourd’hui ?"              , compute_pro_score_dppac(input$dppac_FR_d1_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 1", "Score de pas hebdomadaire"                                                                , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[1] == "Valid", table_gt3x_results_fr()$Steps_score[1], NA),
                         "Jour 1", "Score VMU hebdomadaire"                                                                   , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[1] == "Valid", table_gt3x_results_fr()$VMU_score[1], NA)
                       )  
                     
                     table_day2 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                      ,~"Score de difficulté"                                                            ,~"Score de quantité",
                         "Jour 2", ""                                                                                         , NA                                                                              ,   NA,              
                         "Jour 2", "Avez-vous marché à l’extérieur aujourd’hui ?"                                             , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d2_q1, question = "q1", language = "fr"),
                         "Jour 2", "Avez-vous effectué des tâches à l’extérieur aujourd’hui ?"                                , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d2_q2, question = "q2", language = "fr"),
                         "Jour 2", "Avez-vous eu des difficultés pour vous habiller aujourd’hui ?"                            , compute_pro_score_dppac(input$dppac_FR_d2_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 2", "Avez-vous évité des activités à cause de vos problèmes respiratoires aujourd’hui ?"       , compute_pro_score_dppac(input$dppac_FR_d2_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 2", "Etiez-vous essoufflé(e) en général durant vos activités aujourd’hui ?"                    , compute_pro_score_dppac(input$dppac_FR_d2_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 2", "Etiez-vous fatigué(e) en général durant vos activités aujourd’hui ?"                      , compute_pro_score_dppac(input$dppac_FR_d2_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 2", "Avez-vous du faire des pauses pendant vos activités physiques aujourd’hui ?"              , compute_pro_score_dppac(input$dppac_FR_d2_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 2", "Score de pas hebdomadaire"                                                                , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[2] == "Valid", table_gt3x_results_fr()$Steps_score[2], NA),
                         "Jour 2", "Score VMU hebdomadaire"                                                                   , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[2] == "Valid", table_gt3x_results_fr()$VMU_score[2], NA)
                       ) 
                     
                     table_day3 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                      ,~"Score de difficulté"                                                            ,~"Score de quantité",
                         "Jour 3", ""                                                                                         , NA                                                                              ,   NA,              
                         "Jour 3", "Avez-vous marché à l’extérieur aujourd’hui ?"                                             , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d3_q1, question = "q1", language = "fr"),
                         "Jour 3", "Avez-vous effectué des tâches à l’extérieur aujourd’hui ?"                                , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d3_q2, question = "q2", language = "fr"),
                         "Jour 3", "Avez-vous eu des difficultés pour vous habiller aujourd’hui ?"                            , compute_pro_score_dppac(input$dppac_FR_d3_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 3", "Avez-vous évité des activités à cause de vos problèmes respiratoires aujourd’hui ?"       , compute_pro_score_dppac(input$dppac_FR_d3_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 3", "Etiez-vous essoufflé(e) en général durant vos activités aujourd’hui ?"                    , compute_pro_score_dppac(input$dppac_FR_d3_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 3", "Etiez-vous fatigué(e) en général durant vos activités aujourd’hui ?"                      , compute_pro_score_dppac(input$dppac_FR_d3_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 3", "Avez-vous du faire des pauses pendant vos activités physiques aujourd’hui ?"              , compute_pro_score_dppac(input$dppac_FR_d3_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 3", "Score de pas hebdomadaire"                                                                , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[3] == "Valid", table_gt3x_results_fr()$Steps_score[3], NA),
                         "Jour 3", "Score VMU hebdomadaire"                                                                   , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[3] == "Valid", table_gt3x_results_fr()$VMU_score[3], NA)
                       ) 
                     
                     table_day4 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                      ,~"Score de difficulté"                                                            ,~"Score de quantité",
                         "Jour 4", ""                                                                                         , NA                                                                              ,   NA,              
                         "Jour 4", "Avez-vous marché à l’extérieur aujourd’hui ?"                                             , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d4_q1, question = "q1", language = "fr"),
                         "Jour 4", "Avez-vous effectué des tâches à l’extérieur aujourd’hui ?"                                , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d4_q2, question = "q2", language = "fr"),
                         "Jour 4", "Avez-vous eu des difficultés pour vous habiller aujourd’hui ?"                            , compute_pro_score_dppac(input$dppac_FR_d4_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 4", "Avez-vous évité des activités à cause de vos problèmes respiratoires aujourd’hui ?"       , compute_pro_score_dppac(input$dppac_FR_d4_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 4", "Etiez-vous essoufflé(e) en général durant vos activités aujourd’hui ?"                    , compute_pro_score_dppac(input$dppac_FR_d4_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 4", "Etiez-vous fatigué(e) en général durant vos activités aujourd’hui ?"                      , compute_pro_score_dppac(input$dppac_FR_d4_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 4", "Avez-vous du faire des pauses pendant vos activités physiques aujourd’hui ?"              , compute_pro_score_dppac(input$dppac_FR_d4_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 4", "Score de pas hebdomadaire"                                                                , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[4] == "Valid", table_gt3x_results_fr()$Steps_score[4], NA),
                         "Jour 4", "Score VMU hebdomadaire"                                                                   , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[4] == "Valid", table_gt3x_results_fr()$VMU_score[4], NA)
                       ) 
                     
                     table_day5 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                      ,~"Score de difficulté"                                                            ,~"Score de quantité",
                         "Jour 5", ""                                                                                         , NA                                                                              ,   NA,              
                         "Jour 5", "Avez-vous marché à l’extérieur aujourd’hui ?"                                             , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d5_q1, question = "q1", language = "fr"),
                         "Jour 5", "Avez-vous effectué des tâches à l’extérieur aujourd’hui ?"                                , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d5_q2, question = "q2", language = "fr"),
                         "Jour 5", "Avez-vous eu des difficultés pour vous habiller aujourd’hui ?"                            , compute_pro_score_dppac(input$dppac_FR_d5_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 5", "Avez-vous évité des activités à cause de vos problèmes respiratoires aujourd’hui ?"       , compute_pro_score_dppac(input$dppac_FR_d5_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 5", "Etiez-vous essoufflé(e) en général durant vos activités aujourd’hui ?"                    , compute_pro_score_dppac(input$dppac_FR_d5_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 5", "Etiez-vous fatigué(e) en général durant vos activités aujourd’hui ?"                      , compute_pro_score_dppac(input$dppac_FR_d5_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 5", "Avez-vous du faire des pauses pendant vos activités physiques aujourd’hui ?"              , compute_pro_score_dppac(input$dppac_FR_d5_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 5", "Score de pas hebdomadaire"                                                                , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[5] == "Valid", table_gt3x_results_fr()$Steps_score[5], NA),
                         "Jour 5", "Score VMU hebdomadaire"                                                                   , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[5] == "Valid", table_gt3x_results_fr()$VMU_score[5], NA)
                       ) 
                     
                     table_day6 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                      ,~"Score de difficulté"                                                            ,~"Score de quantité",
                         "Jour 6", ""                                                                                         , NA                                                                              ,   NA,              
                         "Jour 6", "Avez-vous marché à l’extérieur aujourd’hui ?"                                             , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d6_q1, question = "q1", language = "fr"),
                         "Jour 6", "Avez-vous effectué des tâches à l’extérieur aujourd’hui ?"                                , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d6_q2, question = "q2", language = "fr"),
                         "Jour 6", "Avez-vous eu des difficultés pour vous habiller aujourd’hui ?"                            , compute_pro_score_dppac(input$dppac_FR_d6_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 6", "Avez-vous évité des activités à cause de vos problèmes respiratoires aujourd’hui ?"       , compute_pro_score_dppac(input$dppac_FR_d6_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 6", "Etiez-vous essoufflé(e) en général durant vos activités aujourd’hui ?"                    , compute_pro_score_dppac(input$dppac_FR_d6_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 6", "Etiez-vous fatigué(e) en général durant vos activités aujourd’hui ?"                      , compute_pro_score_dppac(input$dppac_FR_d6_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 6", "Avez-vous du faire des pauses pendant vos activités physiques aujourd’hui ?"              , compute_pro_score_dppac(input$dppac_FR_d6_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 6", "Score de pas hebdomadaire"                                                                , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[6] == "Valid", table_gt3x_results_fr()$Steps_score[6], NA),
                         "Jour 6", "Score VMU hebdomadaire"                                                                   , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[6] == "Valid", table_gt3x_results_fr()$VMU_score[6], NA)
                       ) 
                     
                     table_day7 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                      ,~"Score de difficulté"                                                            ,~"Score de quantité",
                         "Jour 7", ""                                                                                         , NA                                                                              ,   NA,              
                         "Jour 7", "Avez-vous marché à l’extérieur aujourd’hui ?"                                             , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d7_q1, question = "q1", language = "fr"),
                         "Jour 7", "Avez-vous effectué des tâches à l’extérieur aujourd’hui ?"                                , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d7_q2, question = "q2", language = "fr"),
                         "Jour 7", "Avez-vous eu des difficultés pour vous habiller aujourd’hui ?"                            , compute_pro_score_dppac(input$dppac_FR_d7_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 7", "Avez-vous évité des activités à cause de vos problèmes respiratoires aujourd’hui ?"       , compute_pro_score_dppac(input$dppac_FR_d7_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 7", "Etiez-vous essoufflé(e) en général durant vos activités aujourd’hui ?"                    , compute_pro_score_dppac(input$dppac_FR_d7_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 7", "Etiez-vous fatigué(e) en général durant vos activités aujourd’hui ?"                      , compute_pro_score_dppac(input$dppac_FR_d7_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 7", "Avez-vous du faire des pauses pendant vos activités physiques aujourd’hui ?"              , compute_pro_score_dppac(input$dppac_FR_d7_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 7", "Score de pas hebdomadaire"                                                                , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[7] == "Valid", table_gt3x_results_fr()$Steps_score[7], NA),
                         "Jour 7", "Score VMU hebdomadaire"                                                                   , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[7] == "Valid", table_gt3x_results_fr()$VMU_score[7], NA)
                       ) 
                 # Combining all tables  
                   table_all_days <- 
                     dplyr::bind_rows(table_day1, table_day2, table_day3, table_day4, table_day5, table_day6, table_day7) 
                 
                 return(table_all_days)
                 
               })
            
            # Final table of results
              output$PROactive_scores_dppac_summary_fr <-  reactable::renderReactable({
                
                # Waiting for required conditions
                  req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) 
                
                # Showing table
                  reactable::reactable(
                    tab_dppac_summary_fr(),
                    rowStyle = function(index) {
                      
                      if ((tab_dppac_summary_fr()[index, "Item"] %in% c("Score de pas hebdomadaire", "Score VMU hebdomadaire")) && is.na(tab_dppac_summary_fr()[index, "Score de quantité"])) {
                        list(background = "#FF6666")
                      } else if (tab_dppac_summary_fr()[index, "Item"] == "") {
                        list(background = "grey", color = "white", fontWeight = "bold")
                      } else {
                        NULL
                      }
                    },
                    defaultColDef = reactable::colDef(align = "center"),
                    columns = list(
                      Item = reactable::colDef(align = "left"),
                      "Jour" = reactable::colDef(width = 60),
                      "Score de difficulté" = reactable::colDef(width = 80),
                      "Score de quantité" = reactable::colDef(width = 80)
                      
                    ),
                    defaultPageSize = 70,
                    striped = TRUE
                  )
         
              })
            
            # Value Boxes
              
              # Making summary table
                recap_ddpac_fr <- eventReactive(input$get_dppac_summary_fr, {
                
                  # Waiting for required conditions
                    req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) 
                
                  # Making table
                    tab_dppac_summary_fr() %>%
                     dplyr::mutate(score_type = rep(c("", "quant", "quant", "diff", "diff", "diff", "diff", "diff", "quant", "quant"), 7)) %>%
                     dplyr::group_by(Jour, score_type) %>%
                     dplyr::filter(Item != "") %>%
                     dplyr::summarise(difficulty_score_raw = sum(.data[["Score de difficulté"]], na.rm = TRUE),
                                      amount_score_raw = sum(.data[["Score de quantité"]])
                                      ) %>%
                     tidyr::pivot_wider(values_from = c(difficulty_score_raw, amount_score_raw), names_from = score_type ) %>%
                     dplyr::filter(!is.na(amount_score_raw_quant)) %>%
                     dplyr::rename(difficulty_score_raw = difficulty_score_raw_diff, amount_score_raw = amount_score_raw_quant) %>%
                     dplyr::mutate(
                       total_score_raw = difficulty_score_raw + amount_score_raw,
                       difficulty_score_rasch = rasch_transform(x = difficulty_score_raw, quest = "D-PPAC", score = "difficulty"),
                       amount_score_rasch = rasch_transform(x = amount_score_raw, quest = "D-PPAC", score = "quantity"),
                       total_score_rasch = (difficulty_score_rasch + amount_score_rasch) / 2
                     ) %>%
                     dplyr::ungroup() %>%
                     dplyr::summarise(
                       mean_difficulty_score_raw = mean(difficulty_score_raw),
                       mean_amount_score_raw = mean(amount_score_raw),
                       mean_total_score_raw = mean(total_score_raw),
                       mean_difficulty_score_rasch = mean(difficulty_score_rasch),
                       mean_amount_score_rasch = mean(amount_score_rasch),
                       mean_total_score_rasch = mean(total_score_rasch)
                     )
                
                })
                
              #****************
              #Difficulty (raw)
              #****************
              output$infoBox_dppac_fr_total_diff <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_fr()$mean_difficulty_score_raw[1], 1), " / 20"), style = "color: white;"),
                    "Score de difficulté moyen (brut)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #************
              #Amount (raw)
              #************
              output$infoBox_dppac_fr_total_amount <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_fr()$mean_amount_score_raw[1], 1), " / 17"), style = "color: white;"), 
                    "Score de quantité moyen (brut)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              #***********
              #Total (raw)
              #***********
              output$infoBox_dppac_fr_total_all <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_fr()$mean_total_score_raw[1], 1), " / 37"),  style = "color: white;"), 
                    "Score total moyen (brut)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })
              
              
              #******************
              #Difficulty (rasch)
              #******************
              output$infoBox_dppac_fr_total_diff_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_fr()$mean_difficulty_score_rasch[1], 1), " / 100"), style = "color: white;"),
                    "Score de difficulté moyen (Rasch)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              
              #**************
              #Amount (rasch)
              #**************
              output$infoBox_dppac_fr_total_amount_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_ddpac_fr()$mean_amount_score_rasch[1], 1), " / 100"), style = "color: white;"), 
                    "Score de quantité moyen (Rasch)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              
              #*************
              #Total (rasch
              #*************
              output$infoBox_dppac_fr_total_all_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                 shinydashboard::valueBox(
                   tags$h3(paste0(round(recap_ddpac_fr()$mean_total_score_rasch[1], 1),
                     " / 100"),  style = "color: white;"), 
                   "Score total moyen (Rasch)",  icon = NULL,
                   color = "purple",
                   width = 4
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

      if(nrow(results_list()$df_with_computed_metrics) >=1) {
        
      shinyjs::show("myBox2")
      shinyjs::show("Metric2")
      shinyjs::show("graph_int")
      shinyjs::show("BoxResByDay")
      shinyjs::show("BoxResMeans")
      shinyjs::show("BoxResMedians")
      }
    })
    
  
  ###################################
  # Hiding / showing Download buttons ----
  ###################################
  
  observe({
      shinyjs::hide("ExpDataset")
      shinyjs::hide("ExpResultsByDays")
      shinyjs::hide("ExpDailySummaryMeans")
      shinyjs::hide("ExpDailySummaryMedians")
      shinyjs::hide("report_en")
      shinyjs::hide("report_fr")
      shinyjs::hide("go_to_proactive_q")
      shinyjs::hide("get_cppac_summary_en")
      shinyjs::hide("get_cppac_summary_fr")
      shinyjs::hide("get_dppac_summary_en")
      shinyjs::hide("get_dppac_summary_fr")

    if(nrow(results_list()$df_with_computed_metrics) >=1) {
      shinyjs::show("ExpDataset")
      shinyjs::show("ExpResultsByDays")
      shinyjs::show("ExpDailySummaryMeans")
      shinyjs::show("ExpDailySummaryMedians")
    }
      
    if(results_summary_means()$valid_days >=1 | results_summary_medians()$valid_days >=1) {
      shinyjs::show("report_en")
      shinyjs::show("report_fr") 
      shinyjs::show("go_to_proactive_q")
    }
      
    if(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) {
      shinyjs::show("get_cppac_summary_en")
      shinyjs::show("get_cppac_summary_fr")
      shinyjs::show("get_dppac_summary_en")
      shinyjs::show("get_dppac_summary_fr")
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
                            dplyr::select(-timestamp), 
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
    
  # Warning message to indicate that no questionnaire results will be provided in case of absence of any valid days
    output$warning_no_valid_days <- renderText({
      
      req(results_summary_medians()$valid_days == 0 & results_summary_means()$valid_days == 0)
      "Unfortunately, there is no valid days. As a consequence, no report will be generated. 
      Moreover, despite the fact that the app allows to fill a PROactive questionnaire, no summary results will be provided."
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
          epoch = as.numeric(input$to_epoch),
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
          epoch = as.numeric(input$to_epoch),
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
  
  #######################################################
  # Switching from accelerometer panel to PROactive panel
  #######################################################
    
    observeEvent(input$go_to_proactive_q, {
      newtab <- switch(input$tabs,
                       "app" = "proactive"
      )
      shinydashboard::updateTabItems(session, "tabs", newtab)
      
      # Thanks to Stackoverflow insights for writing this piece of code: 
      # https://stackoverflow.com/questions/32971921/navigate-to-particular-sidebar-menu-item-in-shinydashboard
    })
  
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
      shiny::exportTestValues(to_epoch = as.numeric(input$to_epoch))
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


