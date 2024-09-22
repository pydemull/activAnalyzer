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
  
  #############################
  # Filling patient information ----
  #############################
  
  # Automatically filling patient information inputs by defaut values
  observeEvent(input$auto_fill_char, {
    updateTextInput(inputId = "assessor_name", value = "Doe")
    updateTextInput(inputId = "assessor_surname", value = "John")
    updateTextInput(inputId = "patient_name", value = "Doe")
    updateTextInput(inputId = "patient_surname", value = "Jane")
    updateSelectInput(inputId = "sex", selected = "female")
    updateNumericInput(inputId = "age", value = 67)
    updateNumericInput(inputId = "weight", value = 86)
    updateSelectInput(inputId = "side", selected = "right")
  })
    
  # Warning regarding non-complete patient information if any
    
    output$warning_auto_fill_char <- renderText({
      "While you are ready to analyze wear time, you are not ready to get all 
      the final results that will be displayed in Section 4 because you have not
      fullfilled all the required inputs in Section 1 (see the inputs with a red star). 
      If you decide to ignore this message, you will cannot get the final results. 
      If you are using the app only for testing it, you can just click on the 
      \"Auto-fill\" button at the end of Section 1."
    })
    
    shinyjs::hide("box-auto_fill_char")
    shinyjs::hide("warning_auto_fill_char")
    observe({
      if (!is.null(init$file) &&
          tools::file_ext(init$name) == "agd" &&
          "axis1" %in% names(init$file) &&
          "axis2" %in% names(init$file) &&
          "axis3" %in% names(init$file) &&
          "steps" %in% names(init$file) && 
          (
            !(input$sex %in% c("female", "male", "intersex", "undefined", "prefer not to say")) |
            !(input$age > 0 & is.numeric(input$age)) |
            !(input$weight > 0 & is.numeric(input$weight))
          )
      ) {
        shinyjs::show("box-auto_fill_char")
        shinyjs::show("warning_auto_fill_char")
      } else {
        shinyjs::hide("box-auto_fill_char")
        shinyjs::hide("warning_auto_fill_char")
      }
    })

    
  ##############################
  # Uploading and preparing data ----
  ##############################
  
  # Initializing reactive values for data inputs
    init <- reactiveValues(file = NULL, data = NULL, name = NULL)

  # Getting data supplied by the user if any
    observeEvent(input$upload, {
      init$name <- input$upload$name
      req(tools::file_ext(input$upload$name) == "agd")
      init$file <- read_agd(input$upload$datapath)
      req(
        "axis1" %in% names(init$file) & 
        "axis2" %in% names(init$file) & 
        "axis3" %in% names(init$file) &
        "steps" %in% names(init$file)
          )
      init$data <- prepare_dataset(input$upload$datapath)
    })
    
  # Showing file features
    output$warning_features <- renderText({
      
      if (attributes(file())$devicename == "GT3X") {
        sampling_rate <- "XX"
      } else {
        sampling_rate <- paste(attributes(file())$`original sample rate`, " Hz")
      }
      
      paste0(
        "Start date: ", attributes(file())$startdatetime,
        " | End date: ", attributes(file())$stopdatetime,
        " | Device: ", attributes(file())$devicename,
        " | Sampling rate: ", sampling_rate,
        " | Filter: ", attributes(file())$filter
      )
    })
    
    shinyjs::hide("box-features")
    shinyjs::hide("warning_features")
    observe({
      req(!is.null(init$name))
      init$name 
      if(nrow(data()) >= 1) {
        shinyjs::show("box-features")
        shinyjs::show("warning_features")
      } else {
        shinyjs::hide("box-features")
        shinyjs::hide("warning_features")
      }
    })
    
    
  # Getting demo data if any
    observeEvent(input$demo, {
      withProgress(message = 'Upload Demo File...', {
      shiny::setProgress(1)
      init$file <- read_agd(system.file("extdata", "acc.agd", package = "activAnalyzer"))
      init$data <- prepare_dataset(system.file("extdata", "acc.agd", package = "activAnalyzer"))
      init$name <- system.file("extdata", "acc.agd", package = "activAnalyzer")
      shiny::setProgress(1)
      })
    })
   
  # Building the reactive datasets that will be used for subsequent analysis
    
    # Dataset containing device information
    file <- reactive({
      req(
        !is.null(init$file) & 
        tools::file_ext(init$name) == "agd" &
        "axis1" %in% names(init$file) & 
        "axis2" %in% names(init$file) & 
        "axis3" %in% names(init$file) &
        "steps" %in% names(init$file)
        )
      isolate(init$file)
      })
    
    # Dataset prepared for analysis
    data <- reactive({
      req(
        !is.null(init$file) & 
          tools::file_ext(init$name) == "agd" &
          "axis1" %in% names(init$file) & 
          "axis2" %in% names(init$file) & 
          "axis3" %in% names(init$file) &
          "steps" %in% names(init$file)
      )
      isolate(init$data)
      })
    
  # Controlling appearance of the "Validate configuration" button
    shinyjs::hide("validate")
    observe({
     if (!is.null(init$file) &&
        tools::file_ext(init$name) == "agd" &&
        "axis1" %in% names(init$file) &&
        "axis2" %in% names(init$file) &&
        "axis3" %in% names(init$file) &&
        "steps" %in% names(init$file)
        ){
     shinyjs::show("validate")
     } else {
       shinyjs::hide("validate")
     }
    })
    
  # Controlling file extension and content
    observeEvent(input$upload,
                shinyFeedback::feedbackWarning(
                  "upload", 
                  (!(tools::file_ext(init$name) == "agd") | !(
                    "axis1" %in% names(init$file) & 
                    "axis2" %in% names(init$file) & 
                    "axis3" %in% names(init$file) &
                    "steps" %in% names(init$file)
                  )),
                  "Invalid file. Choose an appropriate .agd file (cf. guide)."
                )
    )
    
   # Getting default settings to configure activity intensity analysis
    observeEvent(input$auto_fill_intensity, {
      updateSelectInput(inputId = "equation_mets", selected = "Santos-Lozano et al. (2013) [Older adults]")
      updateSelectInput(inputId = "sed_cutpoint", selected = "Aguilar-Farias et al. (2014) [Older adults]")
      updateSelectInput(inputId = "mvpa_cutpoint", selected = "Santos-Lozano et al. (2013) [Older adults]")
    })
    
  ################
  # Days selection ----
  ################
    
  # Selecting days required for analysis
    output$select_days <- renderUI({
      dates <- paste0(attributes(as.factor(df()$date))$levels, " (", weekdays(as.Date(attributes(as.factor(df()$date))$levels)), ")")
      checkboxGroupInput("selected_days", h3("Select the days to keep for analysis (please only select the 7 appropriate days if your analysis is related to PROactive framework)"), dates, selected = dates, inline = TRUE)
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
                    paste("Please choose a number between 1 and 60 and that is greater or equal to the duration of the file epochs. 
                    The ratio between the desired epoch and the current epoch must be an integer. The epoch length of the file currently
                          used is", as.numeric(hms::as_hms(data()$TimeStamp[2] - data()$TimeStamp[1])), "s.")
                  )
      )

      output$warning_epoch <- renderText({
      "With such a short epoch, the app will have to deal with a large dataset, and processing times will certainly be long. 
      As the figures provided by the app are quite complex, rendering them with this epoch to vizualize a week of measurement could take several minutes. 
      For this reason, the figure will not be created with this epoch to save your time. Please use a longer epoch if you want to benefit from the figures of the app."
      })
    
      shinyjs::hide("box-epoch")
      shinyjs::hide("warning_epoch")
      observe({
      req(is.numeric(as.numeric(input$to_epoch)))
      if(as.numeric(input$to_epoch) < 10) {
        shinyjs::show("box-epoch")
        shinyjs::show("warning_epoch")
      } else {
        shinyjs::hide("box-epoch")
        shinyjs::hide("warning_epoch")
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
      
    # Alert message when working with the Demo file
      output$warning_demo <- renderText({
        "This is just a reminder that you are now working on demo data, not your own data !"
      })
      
      shinyjs::hide("box-demo")
      shinyjs::hide("warning_demo")
      observe({
        req(!is.null(init$name))
        init$name 
        if(init$name == system.file("extdata", "acc.agd", package = "activAnalyzer")) {
          shinyjs::show("box-demo")
          shinyjs::show("warning_demo")
        } else {
          shinyjs::hide("box-demo")
          shinyjs::hide("warning_demo")
        }
      })
      
  # Building reactive dataframe marked for nonwear/wear time
  
    df <- eventReactive(input$validate, {
      
        # Waiting for required conditions 
          req(tools::file_ext(init$name) == "agd" & 
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
          withProgress(message = 'Please wait...', {
          shiny::setProgress(0.1)
            
              df <- 
                  mark_wear_time(
                    dataset = data(),
                    to_epoch = as.numeric(input$to_epoch),
                    cts = cts, 
                    frame = input$frame_size, 
                    allowanceFrame = input$allowanceFrame_size,
                    streamFrame = input$streamFrame_size
                  ) 
           shiny::setProgress(1)
           
                })
          
          return(df)

         })
        

  # Returning to default values for the wear time detection algorithm
    observeEvent(input$reset_nonwear, {
      updateNumericInput(inputId = "to_epoch", value = 60)
      updateSelectInput(inputId = "axis_weartime", selected = "vector magnitude")
      updateNumericInput(inputId = "frame_size", value = 90)
      updateNumericInput(inputId = "allowanceFrame_size", value = 2)
      updateNumericInput(inputId = "streamFrame_size", value = 30)
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(0))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*23+60*59+59))
    })
    
  
  ########################################
  # Visualizing all data with nonwear time ----
  ########################################
  
  # Creating/updating reactive values for zooming in on the plot with nonwear time
    zoom_param <- reactiveValues(
      metric = "axis1", 
      zoom_from_weartime = "00:00:00", 
      zoom_to_weartime = "23:59:59"
    )
    
    observeEvent(input$validate, {
      zoom_param$metric <- "axis1"
      zoom_param$zoom_from_weartime <- "00:00:00"
      zoom_param$zoom_to_weartime <- "23:59:59"
      updateSelectInput(inputId = "Metric", selected = "axis1")
      updateSelectInput(inputId = "zoom_from_weartime", selected = hms::as_hms(0))
      updateSelectInput(inputId = "zoom_to_weartime", selected = hms::as_hms(60*60*23+60*59+59))
    })
    
    observeEvent(input$update_graphic, {
      zoom_param$metric <- input$Metric
      zoom_param$zoom_from_weartime <- input$zoom_from_weartime
      zoom_param$zoom_to_weartime <- input$zoom_to_weartime
    })
    
    # Controlling for correct inputs when updating the plot with nonwear time marks
    observeEvent(input$update_graphic,
                 shinyFeedback::feedbackWarning(
                   "zoom_from_weartime", 
                   hms::as_hms(input$zoom_to_weartime) <= hms::as_hms(input$zoom_from_weartime),
                   "Start time should be inferior to end time."
                 )
    )
    
    observeEvent(input$update_graphic,
                 shinyFeedback::feedbackWarning(
                   "zoom_to_weartime", 
                   hms::as_hms(input$zoom_to_weartime) <= hms::as_hms(input$zoom_from_weartime),
                   "End time should be superior to start time."
                 )
    )
  
  output$graph <- renderPlot({
    
    # Waiting for correct inputs
      req(zoom_param$zoom_from_weartime < zoom_param$zoom_to_weartime)
    
    # Making the plot
    
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
    plot_data(
      data = df(), 
      metric = zoom_param$metric, 
      zoom_from = zoom_param$zoom_from_weartime,
      zoom_to = zoom_param$zoom_to_weartime
      )
    }
  }, 
  width = "auto", 
  height = function(){
    
    n_levels = length(unique(df()$date))
    
    height <- dplyr::case_when(
      n_levels >= 8 ~ n_levels * 95,
      n_levels == 7 ~ n_levels * 105,
      n_levels == 6 ~ n_levels * 107,
      n_levels == 5 ~ n_levels * 110,
      n_levels == 4 ~ n_levels * 115,
      n_levels == 3 ~ n_levels * 130,
      n_levels == 2 ~ n_levels * 150,
      n_levels == 1 ~ n_levels * 205
    )
    return(height)
  }, 
  res = 120)
  
  #####################################################
  # Adding missing physical activity information if any ----
  #####################################################
  
  # Initializing reactive values for measurement dates
  dates_inputs <- reactiveValues(dates = NULL)
  
  # Updating reactive values for measurement dates when clicking on the "Validate
  # configuration" button
  observeEvent(input$validate, {
    dates_inputs$dates <- attributes(as.factor(df()$date))$levels
  })
  
  # Setting reactive buttons
    period_buttons_1 <- mod_control_pa_period_view_server("period_1")
    period_buttons_2 <- mod_control_pa_period_view_server("period_2", add_period_btn = period_buttons_1$add_period_btn)
    period_buttons_3 <- mod_control_pa_period_view_server("period_3", add_period_btn = period_buttons_2$add_period_btn)
    period_buttons_4 <- mod_control_pa_period_view_server("period_4", add_period_btn = period_buttons_3$add_period_btn)
    period_buttons_5 <- mod_control_pa_period_view_server("period_5", add_period_btn = period_buttons_4$add_period_btn)
    period_buttons_6 <- mod_control_pa_period_view_server("period_6", add_period_btn = period_buttons_5$add_period_btn)
    period_buttons_7 <- mod_control_pa_period_view_server("period_7", add_period_btn = period_buttons_6$add_period_btn)
    period_buttons_8 <- mod_control_pa_period_view_server("period_8", add_period_btn = period_buttons_7$add_period_btn)
    period_buttons_9 <- mod_control_pa_period_view_server("period_9", add_period_btn = period_buttons_8$add_period_btn)
    period_buttons_10 <- mod_control_pa_period_view_server("period_10", add_period_btn = period_buttons_9$add_period_btn)
    period_buttons_11 <- mod_control_pa_period_view_server("period_11", add_period_btn = period_buttons_10$add_period_btn)
    period_buttons_12 <- mod_control_pa_period_view_server("period_12", add_period_btn = period_buttons_11$add_period_btn)
    period_buttons_13 <- mod_control_pa_period_view_server("period_13", add_period_btn = period_buttons_12$add_period_btn)
    period_buttons_14 <- mod_control_pa_period_view_server("period_14", add_period_btn = period_buttons_13$add_period_btn)
    period_buttons_15 <- mod_control_pa_period_view_server("period_15", add_period_btn = period_buttons_14$add_period_btn)

  
  # Control row of inputs
    # Row 1
    period_info_1 <- mod_report_pa_period_server("period_1", 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_1", 
                                      # Below the input is set to allow keeping visible the "Add"
                                      # button of the first row after removing the 2nde row
                                      remove_period_btn = period_buttons_2$remove_period_btn)
    
    # Row 2
    period_info_2 <- mod_report_pa_period_server("period_2",
                                add_period_btn = period_buttons_1$add_period_btn, 
                                remove_period_btn = period_buttons_2$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_2", 
                                      add_period_btn = period_buttons_1$add_period_btn, 
                                      remove_period_btn = period_buttons_3$remove_period_btn)
    
    # Row 3
    period_info_3 <- mod_report_pa_period_server("period_3", 
                                add_period_btn = period_buttons_2$add_period_btn, 
                                remove_period_btn = period_buttons_3$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_3", 
                                      add_period_btn = period_buttons_2$add_period_btn, 
                                      remove_period_btn = period_buttons_4$remove_period_btn)
    
    # Row 4
    period_info_4 <- mod_report_pa_period_server("period_4", 
                                add_period_btn = period_buttons_3$add_period_btn, 
                                remove_period_btn = period_buttons_4$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_4", 
                                      add_period_btn = period_buttons_3$add_period_btn, 
                                      remove_period_btn = period_buttons_5$remove_period_btn)
    
    # Row 5
    period_info_5 <- mod_report_pa_period_server("period_5", 
                                add_period_btn = period_buttons_4$add_period_btn, 
                                remove_period_btn = period_buttons_5$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_5", 
                                      add_period_btn = period_buttons_4$add_period_btn, 
                                      remove_period_btn = period_buttons_6$remove_period_btn)
    
    # Row 6
    period_info_6 <- mod_report_pa_period_server("period_6", 
                                add_period_btn = period_buttons_5$add_period_btn, 
                                remove_period_btn = period_buttons_6$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_6", 
                                      add_period_btn = period_buttons_5$add_period_btn, 
                                      remove_period_btn = period_buttons_7$remove_period_btn)
    
    # Row 7
    period_info_7 <- mod_report_pa_period_server("period_7", 
                                add_period_btn = period_buttons_6$add_period_btn, 
                                remove_period_btn = period_buttons_7$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_7", 
                                      add_period_btn = period_buttons_6$add_period_btn, 
                                      remove_period_btn = period_buttons_8$remove_period_btn)
    
    # Row 8
    period_info_8 <- mod_report_pa_period_server("period_8", 
                                add_period_btn = period_buttons_7$add_period_btn, 
                                remove_period_btn = period_buttons_8$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_8", 
                                      add_period_btn = period_buttons_7$add_period_btn, 
                                      remove_period_btn = period_buttons_9$remove_period_btn)
    
    # Row 9
    period_info_9 <- mod_report_pa_period_server("period_9", 
                                add_period_btn = period_buttons_8$add_period_btn, 
                                remove_period_btn = period_buttons_9$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_9", 
                                      add_period_btn = period_buttons_8$add_period_btn, 
                                      remove_period_btn = period_buttons_10$remove_period_btn)
    
    # Row 10
    period_info_10 <- mod_report_pa_period_server("period_10", 
                                add_period_btn = period_buttons_9$add_period_btn, 
                                remove_period_btn = period_buttons_10$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_10", 
                                      add_period_btn = period_buttons_9$add_period_btn, 
                                      remove_period_btn = period_buttons_11$remove_period_btn)
    
    # Row 11
    period_info_11 <- mod_report_pa_period_server("period_11", 
                                add_period_btn = period_buttons_10$add_period_btn, 
                                remove_period_btn = period_buttons_11$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_11", 
                                      add_period_btn = period_buttons_10$add_period_btn, 
                                      remove_period_btn = period_buttons_12$remove_period_btn)
    
    # Row 12
    period_info_12 <- mod_report_pa_period_server("period_12", 
                                add_period_btn = period_buttons_11$add_period_btn, 
                                remove_period_btn = period_buttons_12$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_12", 
                                      add_period_btn = period_buttons_11$add_period_btn, 
                                      remove_period_btn = period_buttons_13$remove_period_btn)
    
    # Row 13
    period_info_13 <- mod_report_pa_period_server("period_13", 
                                add_period_btn = period_buttons_12$add_period_btn, 
                                remove_period_btn = period_buttons_13$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_13", 
                                      add_period_btn = period_buttons_12$add_period_btn, 
                                      remove_period_btn = period_buttons_14$remove_period_btn)
    
    # Row 14
    period_info_14 <- mod_report_pa_period_server("period_14", 
                                add_period_btn = period_buttons_13$add_period_btn, 
                                remove_period_btn = period_buttons_14$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_14", 
                                      add_period_btn = period_buttons_13$add_period_btn, 
                                      remove_period_btn = period_buttons_15$remove_period_btn)
    
    # Row 15
    period_info_15 <- mod_report_pa_period_server("period_15", 
                                add_period_btn = period_buttons_14$add_period_btn, 
                                remove_period_btn = period_buttons_15$remove_period_btn, 
                                dates_inputs = dates_inputs)
    mod_control_pa_period_view_server("period_15", 
                                      add_period_btn = period_buttons_14$add_period_btn)
    
   
    # Making a dataframe (based on the inputs set to indicate relevant missing PA periods)
    # when clicking on the Run button ; this dataframe will be used to replace data from the
    # initial accelerometer dataset by the data of the present dataframe for the corresponding 
    # epochs
    
        # Getting inputs
        recap_pa_perdiods <- eventReactive(input$Run, {
           df_1  <- get_pa_period_info(period = period_info_1)
           df_2  <- get_pa_period_info(period = period_info_2)
           df_3  <- get_pa_period_info(period = period_info_3)
           df_4  <- get_pa_period_info(period = period_info_4)
           df_5  <- get_pa_period_info(period = period_info_5)
           df_6  <- get_pa_period_info(period = period_info_6)
           df_7  <- get_pa_period_info(period = period_info_7)
           df_8  <- get_pa_period_info(period = period_info_8)
           df_9  <- get_pa_period_info(period = period_info_9)
           df_10 <- get_pa_period_info(period = period_info_10)
           df_11 <- get_pa_period_info(period = period_info_11)
           df_12 <- get_pa_period_info(period = period_info_12)
           df_13 <- get_pa_period_info(period = period_info_13)
           df_14 <- get_pa_period_info(period = period_info_14)
           df_15 <- get_pa_period_info(period = period_info_15)
          
        # Making dataframe
        recap <-
          dplyr::bind_rows(
            df_1, df_2, df_3, df_4, df_5,
            df_6, df_7, df_8, df_9, df_10,
            df_11, df_12, df_13, df_14, df_15
            ) %>%
          dplyr::filter(date != "...") 
        
        # Returning dataframe
        return(recap)
        
    }) 
    

  ###################################################
  # Getting results when clicking on the "Run" button ----
  ###################################################
    
  # Controlling the appearance of the "Run analysis" button
    shinyjs::hide("Run")
    observe({
      if(nrow(df()) >=1) {
        shinyjs::show("Run")
      } else {
        shinyjs::hide("Run")
      }
    })
    
  # Controlling for correct inputs
     
      # Sex
        observeEvent(input$Run,
                    shinyFeedback::feedbackWarning(
                      "sex", 
                      (input$sex %in% c("male", "female", "intersex", "undefined", "prefer not to say")) == FALSE,
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
                      input$equation_mets == "..."),
                      "Please choose a MET equation."
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
        
       
      # Intensity bins parameters
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "start_first_bin", 
                       ((is.numeric(input$start_first_bin) == FALSE | input$start_first_bin < 0)),
                       "Please provide a value >=0."
                     )
        )
        
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "start_last_bin", 
                       ((is.numeric(input$start_last_bin) == FALSE | input$start_last_bin <= 0 | input$start_last_bin <= input$start_first_bin)),
                       "Please provide a value > Start first bin."
                     )
        )
      
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "bin_width", 
                       ((is.numeric(input$bin_width) == FALSE | input$bin_width <= 0 | input$bin_width <= input$start_first_bin | input$bin_width >= input$start_last_bin)),
                       "Please provide a value > Start first bin and < Start last bin."
                     )
        )
           
      # Warning regarding non-complete/incorrect values for estimating intensity
        
        output$warning_intensity_inputs <- renderText({
          "It seems the inputs required for getting final results are not all 
          correctly configured. Please look at Section 1 and Section 3 to solve this
          issue."
        })
        
        shinyjs::hide("box-intensity_inputs")
        shinyjs::hide("warning_intensity_inputs")
        observeEvent(input$Run, {
          if (
              (input$sex %in% c("male", "female", "intersex", "undefined", "prefer not to say")) == FALSE |
              (is.numeric(input$age) == FALSE | input$age <= 0) |
              (is.numeric(input$weight) == FALSE | input$weight <= 0) |
              (
                (!input$sex %in% c("male", "female", "intersex", "undefined", "prefer not to say") | 
                 !is.numeric(input$age) |
                 input$age <= 0 | 
                 !is.numeric(input$weight) |
                 input$weight <= 0) | 
                input$equation_mets == "..."
                ) |
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
              ) |
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
              )
              
              ) {
            shinyjs::show("box-intensity_inputs")
            shinyjs::show("warning_intensity_inputs")
          } else {
            shinyjs::hide("box-intensity_inputs")
            shinyjs::hide("warning_intensity_inputs")
          }
        })
        
      
      # Warning regarding missing PA period information when inappropriate inputs are provided
      
        output$warning_pa_periods_inputs <- renderText({
            "For each period, the end time should be superior or equal to the start time (with values >= 0
            for both), and the METs value should be >= 0."
          })
          
          shinyjs::hide("box-pa-periods-inputs")
          shinyjs::hide("warning_pa_periods_inputs")
          
        observeEvent(input$Run, {
          if (
            period_info_1$corr_start_time_hh() < 0 | 
            period_info_2$corr_start_time_hh() < 0 | 
            period_info_3$corr_start_time_hh() < 0 | 
            period_info_4$corr_start_time_hh() < 0 | 
            period_info_5$corr_start_time_hh() < 0 | 
            period_info_6$corr_start_time_hh() < 0 | 
            period_info_7$corr_start_time_hh() < 0 | 
            period_info_8$corr_start_time_hh() < 0 | 
            period_info_9$corr_start_time_hh() < 0 | 
            period_info_10$corr_start_time_hh() < 0 | 
            period_info_11$corr_start_time_hh() < 0 | 
            period_info_12$corr_start_time_hh() < 0 | 
            period_info_13$corr_start_time_hh() < 0 | 
            period_info_14$corr_start_time_hh() < 0 | 
            period_info_15$corr_start_time_hh() < 0 | 
            period_info_1$corr_start_time_mm() < 0 | 
            period_info_2$corr_start_time_mm() < 0 | 
            period_info_3$corr_start_time_mm() < 0 | 
            period_info_4$corr_start_time_mm() < 0 | 
            period_info_5$corr_start_time_mm() < 0 | 
            period_info_6$corr_start_time_mm() < 0 | 
            period_info_7$corr_start_time_mm() < 0 | 
            period_info_8$corr_start_time_mm() < 0 | 
            period_info_9$corr_start_time_mm() < 0 | 
            period_info_10$corr_start_time_mm() < 0 | 
            period_info_11$corr_start_time_mm() < 0 | 
            period_info_12$corr_start_time_mm() < 0 | 
            period_info_13$corr_start_time_mm() < 0 | 
            period_info_14$corr_start_time_mm() < 0 | 
            period_info_15$corr_start_time_mm() < 0 | 
            period_info_1$corr_end_time_hh() < 0 | 
            period_info_2$corr_end_time_hh() < 0 | 
            period_info_3$corr_end_time_hh() < 0 | 
            period_info_4$corr_end_time_hh() < 0 | 
            period_info_5$corr_end_time_hh() < 0 | 
            period_info_6$corr_end_time_hh() < 0 | 
            period_info_7$corr_end_time_hh() < 0 | 
            period_info_8$corr_end_time_hh() < 0 | 
            period_info_9$corr_end_time_hh() < 0 | 
            period_info_10$corr_end_time_hh() < 0 | 
            period_info_11$corr_end_time_hh() < 0 | 
            period_info_12$corr_end_time_hh() < 0 | 
            period_info_13$corr_end_time_hh() < 0 | 
            period_info_14$corr_end_time_hh() < 0 | 
            period_info_15$corr_end_time_hh() < 0 | 
            period_info_1$corr_end_time_mm() < 0 | 
            period_info_2$corr_end_time_mm() < 0 | 
            period_info_3$corr_end_time_mm() < 0 | 
            period_info_4$corr_end_time_mm() < 0 | 
            period_info_5$corr_end_time_mm() < 0 | 
            period_info_6$corr_end_time_mm() < 0 | 
            period_info_7$corr_end_time_mm() < 0 | 
            period_info_8$corr_end_time_mm() < 0 | 
            period_info_9$corr_end_time_mm() < 0 | 
            period_info_10$corr_end_time_mm() < 0 | 
            period_info_11$corr_end_time_mm() < 0 | 
            period_info_12$corr_end_time_mm() < 0 | 
            period_info_13$corr_end_time_mm() < 0 | 
            period_info_14$corr_end_time_mm() < 0 | 
            period_info_15$corr_end_time_mm() < 0 |
            hms::as_hms(period_info_1$corr_start_time_hh()*3600 + period_info_1$corr_start_time_mm()*60) >
              hms::as_hms(period_info_1$corr_end_time_hh()*3600 + period_info_1$corr_end_time_mm()*60) |
            hms::as_hms(period_info_2$corr_start_time_hh()*3600 + period_info_2$corr_start_time_mm()*60) >
              hms::as_hms(period_info_2$corr_end_time_hh()*3600 + period_info_2$corr_end_time_mm()*60) | 
            hms::as_hms(period_info_3$corr_start_time_hh()*3600 + period_info_3$corr_start_time_mm()*60) >
              hms::as_hms(period_info_3$corr_end_time_hh()*3600 + period_info_3$corr_end_time_mm()*60) |
            hms::as_hms(period_info_4$corr_start_time_hh()*3600 + period_info_4$corr_start_time_mm()*60) >
              hms::as_hms(period_info_4$corr_end_time_hh()*3600 + period_info_4$corr_end_time_mm()*60) |
            hms::as_hms(period_info_5$corr_start_time_hh()*3600 + period_info_5$corr_start_time_mm()*60) >
              hms::as_hms(period_info_5$corr_end_time_hh()*3600 + period_info_5$corr_end_time_mm()*60) |
            hms::as_hms(period_info_6$corr_start_time_hh()*3600 + period_info_6$corr_start_time_mm()*60) >
              hms::as_hms(period_info_6$corr_end_time_hh()*3600 + period_info_6$corr_end_time_mm()*60) |
            hms::as_hms(period_info_7$corr_start_time_hh()*3600 + period_info_7$corr_start_time_mm()*60) >
              hms::as_hms(period_info_7$corr_end_time_hh()*3600 + period_info_7$corr_end_time_mm()*60) | 
            hms::as_hms(period_info_8$corr_start_time_hh()*3600 + period_info_8$corr_start_time_mm()*60) >
              hms::as_hms(period_info_8$corr_end_time_hh()*3600 + period_info_8$corr_end_time_mm()*60) |
            hms::as_hms(period_info_9$corr_start_time_hh()*3600 + period_info_9$corr_start_time_mm()*60) >
              hms::as_hms(period_info_9$corr_end_time_hh()*3600 + period_info_9$corr_end_time_mm()*60) |
            hms::as_hms(period_info_10$corr_start_time_hh()*3600 + period_info_10$corr_start_time_mm()*60) >
              hms::as_hms(period_info_10$corr_end_time_hh()*3600 + period_info_10$corr_end_time_mm()*60) |
            hms::as_hms(period_info_11$corr_start_time_hh()*3600 + period_info_11$corr_start_time_mm()*60) >
              hms::as_hms(period_info_11$corr_end_time_hh()*3600 + period_info_11$corr_end_time_mm()*60) |
            hms::as_hms(period_info_12$corr_start_time_hh()*3600 + period_info_12$corr_start_time_mm()*60) >
              hms::as_hms(period_info_12$corr_end_time_hh()*3600 + period_info_12$corr_end_time_mm()*60) | 
            hms::as_hms(period_info_13$corr_start_time_hh()*3600 + period_info_13$corr_start_time_mm()*60) >
              hms::as_hms(period_info_13$corr_end_time_hh()*3600 + period_info_13$corr_end_time_mm()*60) |
            hms::as_hms(period_info_14$corr_start_time_hh()*3600 + period_info_14$corr_start_time_mm()*60) >
              hms::as_hms(period_info_14$corr_end_time_hh()*3600 + period_info_14$corr_end_time_mm()*60) |
            hms::as_hms(period_info_15$corr_start_time_hh()*3600 + period_info_15$corr_start_time_mm()*60) >
            hms::as_hms(period_info_15$corr_end_time_hh()*3600 + period_info_15$corr_end_time_mm()*60) |
            period_info_1$corr_mets() < 0 |
            period_info_2$corr_mets() < 0 |
            period_info_3$corr_mets() < 0 |
            period_info_4$corr_mets() < 0 |
            period_info_5$corr_mets() < 0 |
            period_info_6$corr_mets() < 0 |
            period_info_7$corr_mets() < 0 |
            period_info_8$corr_mets() < 0 |
            period_info_9$corr_mets() < 0 |
            period_info_10$corr_mets() < 0 |
            period_info_11$corr_mets() < 0 |
            period_info_12$corr_mets() < 0 |
            period_info_13$corr_mets() < 0 |
            period_info_14$corr_mets() < 0 |
            period_info_15$corr_mets() < 0
          ) {
          shinyjs::show("box-pa-periods-inputs")
          shinyjs::show("warning_pa_periods_inputs")
          } else {
          shinyjs::hide("box-pa-periods-inputs")
          shinyjs::hide("warning_pa_periods_inputs")
          }
        })
           
      # Controlling PROactive settings
        observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "start_day_analysis", 
                       hms::as_hms(input$end_day_analysis) <= hms::as_hms(input$start_day_analysis),
                       "Start time should be inferior to end time."
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
          reactable::reactable(striped = TRUE, bordered = TRUE,
                    list('Study' = reactable::colDef(minWidth = 150),
                         'Population' = reactable::colDef(minWidth = 150),
                         'Activities performed' = reactable::colDef(minWidth = 150),
                         'Device used' = reactable::colDef(minWidth = 80),
                         'Axis used' = reactable::colDef(minWidth = 150),
                         'Filter enabled' = reactable::colDef(minWidth = 150),
                         Equation = reactable::colDef(minWidth = 150)
                    )
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
         reactable::reactable(striped = TRUE, bordered = TRUE,
                   list('Study' = reactable::colDef(minWidth = 150),
                        'Population' = reactable::colDef(minWidth = 150),
                        'Activities performed' = reactable::colDef(minWidth = 150),
                        'Device used' = reactable::colDef(minWidth = 80),
                        'Axis used' = reactable::colDef(minWidth = 150),
                        'Filter enabled' = reactable::colDef(minWidth = 150),
                        'SED cut-point in counts/min' = reactable::colDef(minWidth = 150))
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
          reactable::reactable(striped = TRUE, bordered = TRUE,
                    list('Study' = reactable::colDef(minWidth = 150),
                         'Population' = reactable::colDef(minWidth = 150),
                         'Activities performed' = reactable::colDef(minWidth = 150),
                         'Device used' = reactable::colDef(minWidth = 80),
                         'Axis used' = reactable::colDef(minWidth = 150),
                         'Filter enabled' = reactable::colDef(minWidth = 150),
                         'MPA cut-point (3 METs) in counts/min' = reactable::colDef(minWidth = 150),
                         'VPA cut-point (6 METs) in counts/min' = reactable::colDef(minWidth = 150))
          )
      }
      
    })
    
    output$table_mvpa_cutpoints_santos_adults <- reactable::renderReactable({
      
      if(input$mvpa_cutpoint == "...") {NULL
      } else {
        mvpa_cutpoints %>% 
          dplyr::filter(Study == input$mvpa_cutpoint) %>%
          reactable::reactable(striped = TRUE, bordered = TRUE,
                    list('Study' = reactable::colDef(minWidth = 150),
                         'Population' = reactable::colDef(minWidth = 150),
                         'Activities performed' = reactable::colDef(minWidth = 150),
                         'Device used' = reactable::colDef(minWidth = 80),
                         'Axis used' = reactable::colDef(minWidth = 150),
                         'Filter enabled' = reactable::colDef(minWidth = 150),
                         'MPA cut-point (3 METs) in counts/min' = reactable::colDef(minWidth = 150),
                         'VPA cut-point (6 METs) in counts/min' = reactable::colDef(minWidth = 150))
          )
      }
      
    })
    
    output$table_mvpa_cutpoints_freedson_adults <- reactable::renderReactable({
      
      if(input$mvpa_cutpoint == "...") {NULL
      } else {
        mvpa_cutpoints %>% 
          dplyr::filter(Study == input$mvpa_cutpoint) %>%
          reactable::reactable(striped = TRUE, bordered = TRUE,
                               list('Study' = reactable::colDef(minWidth = 150),
                                    'Population' = reactable::colDef(minWidth = 150),
                                    'Activities performed' = reactable::colDef(minWidth = 150),
                                    'Device used' = reactable::colDef(minWidth = 80),
                                    'Axis used' = reactable::colDef(minWidth = 150),
                                    'Filter enabled' = reactable::colDef(minWidth = 150),
                                    'MPA cut-point (3 METs) in counts/min' = reactable::colDef(minWidth = 150),
                                    'VPA cut-point (6 METs) in counts/min' = reactable::colDef(minWidth = 150))
          )
      }
      
    })
    
    output$table_mvpa_cutpoints_santos_older <- reactable::renderReactable({
      
      if(input$mvpa_cutpoint == "...") {NULL
      } else {
        mvpa_cutpoints %>% 
          dplyr::filter(Study == input$mvpa_cutpoint) %>%
          reactable::reactable(striped = TRUE, bordered = TRUE,
                    list('Study' = reactable::colDef(minWidth = 150),
                         'Population' = reactable::colDef(minWidth = 150),
                         'Activities performed' = reactable::colDef(minWidth = 150),
                         'Device used' = reactable::colDef(minWidth = 80),
                         'Axis used' = reactable::colDef(minWidth = 150),
                         'Filter enabled' = reactable::colDef(minWidth = 150),
                         'MPA cut-point (3 METs) in counts/min' = reactable::colDef(minWidth = 150),
                         'VPA cut-point (6 METs) in counts/min' = reactable::colDef(minWidth = 150))
          )
      }
      
    })
    
  
    # Returning to default settings for the minimum wear time duration
    observeEvent(input$reset_period, {
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(60*60*0))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*23+60*59+59))
      updateNumericInput(inputId = "minimum_wear_time_for_analysis", value = 10)
    })
    
    
    # Setting PROactive default values to valid a day based on wear time: non-sleep wearing protocol
    observeEvent(input$pro_active_period_non_sleep, {
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(60*60*0))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*23+60*59+59))
      updateNumericInput(inputId = "minimum_wear_time_for_analysis", value = 8)
    })
    
  # Setting PROactive default values to valid a day based on wear time: 24-h wearing protocol
    observeEvent(input$pro_active_period_24h, {
      updateSelectInput(inputId = "start_day_analysis", selected = hms::as_hms(60*60*7))
      updateSelectInput(inputId = "end_day_analysis", selected = hms::as_hms(60*60*22))
      updateNumericInput(inputId = "minimum_wear_time_for_analysis", value = 8)
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
          
          withProgress(message = 'Please wait...', {
          
            # Waiting for required conditions 
              req(
                # Patient's characteristics
                input$sex %in% c("male", "female", "intersex", "undefined", "prefer not to say") &
                is.numeric(input$age) & 
                input$age > 0 &
                is.numeric(input$weight) &
                input$weight > 0 &
                  
                is.numeric(input$start_first_bin) &
                input$start_first_bin >= 0 &
                  
                is.numeric(input$start_last_bin) &
                input$start_last_bin > input$start_first_bin &
                  
                is.numeric(input$bin_width) &
                input$bin_width > input$start_first_bin &
                input$bin_width < input$start_last_bin &
                  
                  
                # Analysis settings
                input$equation_mets != "..." &
                (isTruthy(input$sed_cutpoint != "...") | isTruthy(input$sed_cutpoint == "Personalized..." & is.numeric(input$perso_sed_cutpoint))) &
                (isTruthy(input$mvpa_cutpoint != "...") | isTruthy(input$mvpa_cutpoint == "Personalized..." & ((is.numeric(input$perso_mpa_cutpoint)) & (is.numeric(input$perso_vpa_cutpoint))))) &
                cut_points()$axis_mvpa_chosen == cut_points()$axis_sed_chosen &
                cut_points()$sed_cutpoint_chosen < cut_points()$mpa_cutpoint_chosen &
                cut_points()$mpa_cutpoint_chosen < cut_points()$vpa_cutpoint_chosen & 
                is.numeric(input$minimum_wear_time_for_analysis) &
                input$minimum_wear_time_for_analysis >= 0 &
                hms::as_hms(input$end_day_analysis) > hms::as_hms(input$start_day_analysis) &
                
                # Missing PA period information
                period_info_1$corr_start_time_hh() >= 0 & 
                period_info_2$corr_start_time_hh() >= 0 & 
                period_info_3$corr_start_time_hh() >= 0 & 
                period_info_4$corr_start_time_hh() >= 0 & 
                period_info_5$corr_start_time_hh() >= 0 & 
                period_info_6$corr_start_time_hh() >= 0 & 
                period_info_7$corr_start_time_hh() >= 0 & 
                period_info_8$corr_start_time_hh() >= 0 & 
                period_info_9$corr_start_time_hh() >= 0 & 
                period_info_10$corr_start_time_hh() >= 0 & 
                period_info_11$corr_start_time_hh() >= 0 & 
                period_info_12$corr_start_time_hh() >= 0 & 
                period_info_13$corr_start_time_hh() >= 0 & 
                period_info_14$corr_start_time_hh() >= 0 & 
                period_info_15$corr_start_time_hh() >= 0 & 
                period_info_1$corr_start_time_mm() >= 0 & 
                period_info_2$corr_start_time_mm() >= 0 & 
                period_info_3$corr_start_time_mm() >= 0 & 
                period_info_4$corr_start_time_mm() >= 0 & 
                period_info_5$corr_start_time_mm() >= 0 & 
                period_info_6$corr_start_time_mm() >= 0 & 
                period_info_7$corr_start_time_mm() >= 0 & 
                period_info_8$corr_start_time_mm() >= 0 & 
                period_info_9$corr_start_time_mm() >= 0 & 
                period_info_10$corr_start_time_mm() >= 0 & 
                period_info_11$corr_start_time_mm() >= 0 & 
                period_info_12$corr_start_time_mm() >= 0 & 
                period_info_13$corr_start_time_mm() >= 0 & 
                period_info_14$corr_start_time_mm() >= 0 & 
                period_info_15$corr_start_time_mm() >= 0 & 
                period_info_1$corr_end_time_hh() >= 0 & 
                period_info_2$corr_end_time_hh() >= 0 & 
                period_info_3$corr_end_time_hh() >= 0 & 
                period_info_4$corr_end_time_hh() >= 0 & 
                period_info_5$corr_end_time_hh() >= 0 & 
                period_info_6$corr_end_time_hh() >= 0 & 
                period_info_7$corr_end_time_hh() >= 0 & 
                period_info_8$corr_end_time_hh() >= 0 & 
                period_info_9$corr_end_time_hh() >= 0 & 
                period_info_10$corr_end_time_hh() >= 0 & 
                period_info_11$corr_end_time_hh() >= 0 & 
                period_info_12$corr_end_time_hh() >= 0 & 
                period_info_13$corr_end_time_hh() >= 0 & 
                period_info_14$corr_end_time_hh() >= 0 & 
                period_info_15$corr_end_time_hh() >= 0 & 
                period_info_1$corr_end_time_mm() >= 0 & 
                period_info_2$corr_end_time_mm() >= 0 & 
                period_info_3$corr_end_time_mm() >= 0 & 
                period_info_4$corr_end_time_mm() >= 0 & 
                period_info_5$corr_end_time_mm() >= 0 & 
                period_info_6$corr_end_time_mm() >= 0 & 
                period_info_7$corr_end_time_mm() >= 0 & 
                period_info_8$corr_end_time_mm() >= 0 & 
                period_info_9$corr_end_time_mm() >= 0 & 
                period_info_10$corr_end_time_mm() >= 0 & 
                period_info_11$corr_end_time_mm() >= 0 & 
                period_info_12$corr_end_time_mm() >= 0 & 
                period_info_13$corr_end_time_mm() >= 0 & 
                period_info_14$corr_end_time_mm() >= 0 & 
                period_info_15$corr_end_time_mm() >= 0 &
                hms::as_hms(period_info_1$corr_start_time_hh()*3600 + period_info_1$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_1$corr_end_time_hh()*3600 + period_info_1$corr_end_time_mm()*60) &
                hms::as_hms(period_info_2$corr_start_time_hh()*3600 + period_info_2$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_2$corr_end_time_hh()*3600 + period_info_2$corr_end_time_mm()*60) & 
                hms::as_hms(period_info_3$corr_start_time_hh()*3600 + period_info_3$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_3$corr_end_time_hh()*3600 + period_info_3$corr_end_time_mm()*60) &
                hms::as_hms(period_info_4$corr_start_time_hh()*3600 + period_info_4$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_4$corr_end_time_hh()*3600 + period_info_4$corr_end_time_mm()*60) &
                hms::as_hms(period_info_5$corr_start_time_hh()*3600 + period_info_5$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_5$corr_end_time_hh()*3600 + period_info_5$corr_end_time_mm()*60) &
                hms::as_hms(period_info_6$corr_start_time_hh()*3600 + period_info_6$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_6$corr_end_time_hh()*3600 + period_info_6$corr_end_time_mm()*60) &
                hms::as_hms(period_info_7$corr_start_time_hh()*3600 + period_info_7$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_7$corr_end_time_hh()*3600 + period_info_7$corr_end_time_mm()*60) & 
                hms::as_hms(period_info_8$corr_start_time_hh()*3600 + period_info_8$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_8$corr_end_time_hh()*3600 + period_info_8$corr_end_time_mm()*60) &
                hms::as_hms(period_info_9$corr_start_time_hh()*3600 + period_info_9$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_9$corr_end_time_hh()*3600 + period_info_9$corr_end_time_mm()*60) &
                hms::as_hms(period_info_10$corr_start_time_hh()*3600 + period_info_10$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_10$corr_end_time_hh()*3600 + period_info_10$corr_end_time_mm()*60) &
                hms::as_hms(period_info_11$corr_start_time_hh()*3600 + period_info_11$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_11$corr_end_time_hh()*3600 + period_info_11$corr_end_time_mm()*60) &
                hms::as_hms(period_info_12$corr_start_time_hh()*3600 + period_info_12$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_12$corr_end_time_hh()*3600 + period_info_12$corr_end_time_mm()*60) & 
                hms::as_hms(period_info_13$corr_start_time_hh()*3600 + period_info_13$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_13$corr_end_time_hh()*3600 + period_info_13$corr_end_time_mm()*60) &
                hms::as_hms(period_info_14$corr_start_time_hh()*3600 + period_info_14$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_14$corr_end_time_hh()*3600 + period_info_14$corr_end_time_mm()*60) &
                hms::as_hms(period_info_15$corr_start_time_hh()*3600 + period_info_15$corr_start_time_mm()*60) <=
                  hms::as_hms(period_info_15$corr_end_time_hh()*3600 + period_info_15$corr_end_time_mm()*60) &
                period_info_1$corr_mets() >= 0,
                period_info_2$corr_mets() >= 0,
                period_info_3$corr_mets() >= 0,
                period_info_4$corr_mets() >= 0,
                period_info_5$corr_mets() >= 0,
                period_info_6$corr_mets() >= 0,
                period_info_7$corr_mets() >= 0,
                period_info_8$corr_mets() >= 0,
                period_info_9$corr_mets() >= 0,
                period_info_10$corr_mets() >= 0,
                period_info_11$corr_mets() >= 0,
                period_info_12$corr_mets() >= 0,
                period_info_13$corr_mets() >= 0,
                period_info_14$corr_mets() >= 0,
                period_info_15$corr_mets() >= 0
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
              
             shiny::setProgress(0.5)  # set progress to 50%
              
            # Modifying the dataset based on the PA periods reported by the user if any
             
              if (nrow(recap_pa_perdiods()) >= 1) {
                
                    # Setting correction factor for determining kcal and MET-hrs per epoch
                    cor_factor = 60 / (as.numeric(df()$time[2] - df()$time[1]))
                    
                    # Computing basal metabolic rate
                    bmr_kcal_min <- suppressMessages(
                      compute_bmr(age = input$age, sex = input$sex, weight = input$weight) / (24*60)
                    )
                    
                    # Correcting wearing, METs, SED, LPA, MPA, VPA, kcal and MET-hr columns
                    for (i in 1:nrow(recap_pa_perdiods())) {
                      
                      df_with_computed_metrics$wearing <- dplyr::if_else(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"],
                        factor(c("w"), levels = c("nw", "w")), df_with_computed_metrics$wearing)
                      
                      df_with_computed_metrics$non_wearing_count <- ifelse(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"],
                        0, df_with_computed_metrics$non_wearing_count)
                     
                      df_with_computed_metrics$wearing_count <- ifelse(
                       df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                         df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                         df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"],
                       1, df_with_computed_metrics$wearing_count)
                      
                      df_with_computed_metrics$METS <- ifelse(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"],
                        recap_pa_perdiods()[i, "METS"], df_with_computed_metrics$METS)
                      
                      df_with_computed_metrics$SED <- ifelse(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                          recap_pa_perdiods()[i, "METS"] <= 1.5, 1, 
                        ifelse(
                          df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                            df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                            df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                            recap_pa_perdiods()[i, "METS"] > 1.5, 0, df_with_computed_metrics$SED)
                      )
                      
                      df_with_computed_metrics$LPA <- ifelse(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                          recap_pa_perdiods()[i, "METS"] > 1.5 & recap_pa_perdiods()[i, "METS"] < 3, 1,
                        ifelse(
                          df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                            df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                            df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                            (recap_pa_perdiods()[i, "METS"] <= 1.5 | recap_pa_perdiods()[i, "METS"] >= 3), 0,
                             df_with_computed_metrics$LPA)
                        )
                      
                      df_with_computed_metrics$MPA <- ifelse(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                          recap_pa_perdiods()[i, "METS"] >= 3 & recap_pa_perdiods()[i, "METS"] < 6, 1,
                        ifelse(
                          df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                            df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                            df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                            (recap_pa_perdiods()[i, "METS"] < 3 | recap_pa_perdiods()[i, "METS"] >= 6), 0,
                             df_with_computed_metrics$MPA)
                        )
                      
                      df_with_computed_metrics$VPA <- ifelse(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                          recap_pa_perdiods()[i, "METS"] >= 6, 1,
                        ifelse(
                          df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                            df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                            df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                            recap_pa_perdiods()[i, "METS"] < 6, 0, df_with_computed_metrics$VPA)
                      )
                      
                      df_with_computed_metrics$kcal <- ifelse(
                        df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                          df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                          df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                          recap_pa_perdiods()[i, "METS"] <= 1.5, bmr_kcal_min / cor_factor, 
                               ifelse(
                                 df_with_computed_metrics$date == recap_pa_perdiods()[i, "date"] &
                                   df_with_computed_metrics$time >= recap_pa_perdiods()[i, "start"] & 
                                   df_with_computed_metrics$time <= recap_pa_perdiods()[i, "end"] & 
                                   recap_pa_perdiods()[i, "METS"] > 1.5, df_with_computed_metrics$METS * bmr_kcal_min / cor_factor,
                                 df_with_computed_metrics$kcal)
                      )
                      
                      df_with_computed_metrics$mets_hours_mvpa <- ifelse(df_with_computed_metrics$METS >= 3, df_with_computed_metrics$METS * 1/60 / cor_factor, 0)
    
                  } 
                    
                      # Marking the bouts based on intensity categories
                      df_with_computed_metrics$intensity_category <- 
                        ifelse(df_with_computed_metrics$non_wearing_count == 1, "Nonwear", 
                               ifelse(df_with_computed_metrics$SED == 1, "SED", 
                                      ifelse(df_with_computed_metrics$LPA == 1, "LPA", "MVPA")))
                      
                      # Thanks to https://stackoverflow.com/questions/29661269/increment-by-1-for-every-change-in-column 
                      # for the code block below
                      df_with_computed_metrics$intensity_category <- as.factor(df_with_computed_metrics$intensity_category)
                      df_with_computed_metrics$intensity_category_num <- as.numeric(as.character(forcats::fct_recode(df_with_computed_metrics$intensity_category, "0" = "Nonwear", "1" = "SED", "2" = "LPA", "3" = "MVPA")))
                      df_with_computed_metrics$bout <- cumsum(c(1, as.numeric(diff(df_with_computed_metrics$intensity_category_num))!= 0))
              }
              
              
              
             # Creating a list of the results by day and corresponding to valid wear time only  
               results_by_day <-
                 df_with_computed_metrics %>%
                 recap_by_day(
                   age = input$age, 
                   weight = input$weight, 
                   sex = input$sex,
                   valid_wear_time_start = input$start_day_analysis,
                   valid_wear_time_end = input$end_day_analysis,
                   start_first_bin = input$start_first_bin,
                   start_last_bin = input$start_last_bin,
                   bin_width = input$bin_width
                 )
    
           shiny::setProgress(1)  # set progress to 100%
               
             # Returning the list of the results and chosen parameters
               return(
                 list(
                   df_with_computed_metrics = df_with_computed_metrics,
                   results_by_day = results_by_day, 
                   axis_sed_chosen_name = cut_points()$axis_sed_chosen_name, 
                   sed_cutpoint_chosen = cut_points()$sed_cutpoint_chosen, 
                   axis_mvpa_chosen_name = cut_points()$axis_mvpa_chosen_name,
                   mpa_cutpoint_chosen = cut_points()$mpa_cutpoint_chosen,
                   vpa_cutpoint_chosen = cut_points()$vpa_cutpoint_chosen
                   )
                 )
          })
    })
 
      
      
  # Creating reactive time filters used for the plot with intensity metrics
    analysis_filters <- eventReactive(input$Run, {
      list(start_day_analysis = input$start_day_analysis,
           end_day_analysis = input$end_day_analysis
           )
      })
    
  # Creating/updating reactive values for zooming in on the plot with intensity metrics
    zoom_param2 <- reactiveValues(
      metric = "axis1", 
      zoom_from_analysis = "00:00:00", 
      zoom_to_analysis = "23:59:59"
      )
    
    observeEvent(input$Run, {
      zoom_param2$metric <- "axis1"
      zoom_param2$zoom_from_analysis <- "00:00:00"
      zoom_param2$zoom_to_analysis <- "23:59:59"
      updateSelectInput(inputId = "Metric2", selected = "axis1")
      updateSelectInput(inputId = "zoom_from_analysis", selected = hms::as_hms(0))
      updateSelectInput(inputId = "zoom_to_analysis", selected = hms::as_hms(60*60*23+60*59+59))
    })
    
    observeEvent(input$update_graphic2, {
      zoom_param2$metric <- input$Metric2
      zoom_param2$zoom_from_analysis <- input$zoom_from_analysis
      zoom_param2$zoom_to_analysis <- input$zoom_to_analysis
    })
    
  # Controlling for correct inputs when updating the plot with intensity marks
    observeEvent(input$update_graphic2,
                 shinyFeedback::feedbackWarning(
                   "zoom_from_analysis", 
                   hms::as_hms(input$zoom_to_analysis) <= hms::as_hms(input$zoom_from_analysis),
                   "Start time should be inferior to end time."
                 )
    )
    
    observeEvent(input$update_graphic2,
                 shinyFeedback::feedbackWarning(
                   "zoom_to_analysis", 
                   hms::as_hms(input$zoom_to_analysis) <= hms::as_hms(input$zoom_from_analysis),
                   "End time should be superior to start time."
                 )
    )
    
  # Getting results summarized over valid days (means)
  results_summary_means <- eventReactive(input$Run, {
    results_list()$results_by_day$df_all_metrics %>%
      average_results(minimum_wear_time = input$minimum_wear_time_for_analysis, fun = "mean")
  })
  
  # Getting results summarized over valid days (medians)
  results_summary_medians <- eventReactive(input$Run, {
    results_list()$results_by_day$df_all_metrics %>%
      average_results(minimum_wear_time = input$minimum_wear_time_for_analysis, fun = "median")
  })
    
  # Activity volume metrics panels ===========================================================================================
  
  # Showing section title for activity volume metrics
    output$title_activity_volume_metrics <- renderUI({
      
      
      # Showing title
      h3("Activity volume metrics")
    })  
    
  # Plotting data with intensity categories
    output$graph_int <- renderPlot({
      
      # Waiting for correct inputs
        req(zoom_param2$zoom_from_analysis < zoom_param2$zoom_to_analysis)
      
      # Making the plot
      
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
      plot_data_with_intensity(
        data = results_list()$df_with_computed_metrics, 
        metric = zoom_param2$metric,
        valid_wear_time_start = analysis_filters()$start_day_analysis,
        valid_wear_time_end = analysis_filters()$end_day_analysis,
        zoom_from =  zoom_param2$zoom_from_analysis,
        zoom_to =  zoom_param2$zoom_to_analysis
        )
      }
    }, 
    width = "auto", 
    height = function(){
      
      n_levels = length(unique(results_list()$df_with_computed_metrics$date))
      
      height <- dplyr::case_when(
        n_levels >= 8 ~ n_levels * 95,
        n_levels == 7 ~ n_levels * 105,
        n_levels == 6 ~ n_levels * 107,
        n_levels == 5 ~ n_levels * 110,
        n_levels == 4 ~ n_levels * 115,
        n_levels == 3 ~ n_levels * 130,
        n_levels == 2 ~ n_levels * 150,
        n_levels == 1 ~ n_levels * 205
      )
      return(height)
    }, 
    res = 120)
    
    
  # Showing results by day in a table
    output$results_by_day_vol_tab <- reactable::renderReactable({
      Sys.sleep(0.5)
      reactable::reactable(results_list()$results_by_day$df_all_metrics %>% dplyr::select(date, wear_time:total_steps),  
                striped = TRUE,
                list(
                     date = reactable::colDef(
                       style = list(position = "sticky", left = 0, background = "#CCCCCC", zIndex = 1),
                       headerStyle = list(position = "sticky", left = 0,  background = "#fff", zIndex = 1)
                     ),
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
    
  
  # Plotting activity volume metrics by day
    output$results_by_day_vol_fig <- renderPlot({
      
      create_fig_res_by_day(
       data = results_list()$results_by_day$df_all_metrics, 
       minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis, 
       start_day_analysis = analysis_filters()$start_day_analysis,
       end_day_analysis = analysis_filters()$end_day_analysis,
       metrics = "volume"
      )
    }, width = "auto", height = 1000, res = 100)
      
  
  
  # Showing results summarized over valid days in a table (means)
    output$results_summary_vol_means <- reactable::renderReactable({
      
      reactable::reactable(
        results_summary_means() %>% dplyr::select(valid_days:total_steps), 
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
    output$results_summary_vol_medians <- reactable::renderReactable({
      
      reactable::reactable(
        results_summary_medians() %>% dplyr::select(valid_days:total_steps), 
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
    
    
    # Plotting activity volume metrics with comparisons with norms and recommendations
    output$compa_norms_fig <- renderPlot({
      
      # PAL
      g_pal <- create_fig_pal(score = results_summary_means()[["pal"]], "en") + theme(plot.margin = margin(2, 1, 0.5, 1, "cm"))
      
      # Steps
      g_steps <- create_fig_steps(score = results_summary_means()[["total_steps"]], "en") + theme(plot.margin = margin(0, 1, 0.5, 1, "cm"))
      
      # MVPA
      g_mvpa <- create_fig_mvpa(score = results_summary_means()[["minutes_MVPA"]], "en") + theme(plot.margin = margin(0, 1, 0, 1, "cm"))
      
      # SED
      g_sed <- create_fig_sed(score = results_summary_means()[["minutes_SED"]], "en") + theme(plot.margin = margin(0, 1, 0, 1, "cm"))
      
      # MVPA/SED ratio
      g_ratio <- create_fig_ratio_mvpa_sed(score = results_summary_means()[["ratio_mvpa_sed"]], "en") + theme(plot.margin = margin(0, 1, 0, 1, "cm"))
      
      # Whole figure
      g_pal / g_steps / (g_mvpa | g_sed | g_ratio) + 
        patchwork::plot_layout(heights = c(0.6, 0.6, 1.5)) & theme(legend.justification = "center")
    }, width = "auto", height = 1000, res = 100)
    
    
  # Step accumulation metrics =========================================================================================
  
  # Showing section title for intstep accumulation metrics
  output$title_step_acc_metrics <- renderUI({
    
    # Showing title
    h3("Step accumulation metrics")
  }) 
    
    # Showing results by day in a table
    output$results_by_day_step_tab <- reactable::renderReactable({
      Sys.sleep(0.5)
      reactable::reactable(results_list()$results_by_day$df_all_metrics %>% dplyr::select(date, max_steps_60min:peak_steps_1min),  
                           striped = TRUE,
                           list(
                             date = reactable::colDef(
                               style = list(position = "sticky", left = 0, background = "#CCCCCC", zIndex = 1),
                               headerStyle = list(position = "sticky", left = 0,  background = "#fff", zIndex = 1)
                             )
                             )
                           )
    })
    
  # Plotting step accumulation metrics by day
  output$results_by_day_step_fig <- renderPlot({
    
    create_fig_res_by_day(
      data = results_list()$results_by_day$df_all_metrics, 
      minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis, 
      start_day_analysis = analysis_filters()$start_day_analysis,
      end_day_analysis = analysis_filters()$end_day_analysis,
      metrics = "step_acc"
    )
  }, width = "auto", height = 600, res = 100)
 
  # Showing results summarized over valid days in a table (means)
  output$results_summary_step_means <- reactable::renderReactable({
    
    reactable::reactable(
      results_summary_means() %>% dplyr::select(valid_days, max_steps_60min:peak_steps_1min), 
      list(valid_days = reactable::colDef(minWidth = 90)),
      striped = TRUE
    )
    
  })
  
  # Showing results summarized over valid days in a table (medians)
  output$results_summary_step_medians <- reactable::renderReactable({
    
    reactable::reactable(
      results_summary_medians() %>% dplyr::select(valid_days, max_steps_60min:peak_steps_1min), 
      list(valid_days = reactable::colDef(minWidth = 90)),
      striped = TRUE
    )
    
  })
    
  # Distribution of intensity metrics ====================================================================================
  
  # Showing section title for intensity ditribution metrics
  output$title_int_distri_metrics <- renderUI({
    
  # Showing title
  h3("Intensity distribution metrics")
  }) 
  
  # Plotting intensity distribution analysis (1)
  output$int_dist_analysis_fig1 <- renderPlot({
    
    results_list()$results_by_day$p_band
  }, width = "auto", height = function(){round(nlevels(as.factor(results_list()$df_with_computed_metrics$date))/3, 0) * 370}, res = 100)
  
  # Plotting intensity distribution analysis (2)
  output$int_dist_analysis_fig1bis <- renderPlot({
    
    results_list()$results_by_day$p_log
  }, width = "auto", height = function(){round(nlevels(as.factor(results_list()$df_with_computed_metrics$date))/3, 0) * 370}, res = 100)
  
  # Showing results summarized over valid days in a table (means)
  output$results_summary_int_dist_means <- reactable::renderReactable({
    
    reactable::reactable(
      results_summary_means() %>% dplyr::select(valid_days, ig:M5), 
      list(valid_days = reactable::colDef(minWidth = 90)),
      striped = TRUE
    )
    
  })
  
  # Showing results by day in a table
  output$results_by_day_int_dist_tab <- reactable::renderReactable({
    Sys.sleep(0.5)
    reactable::reactable(results_list()$results_by_day$df_all_metrics %>% dplyr::select(date, ig:M5),  
                         striped = TRUE,
                         list(
                           date = reactable::colDef(
                             style = list(position = "sticky", left = 0, background = "#CCCCCC", zIndex = 1),
                             headerStyle = list(position = "sticky", left = 0,  background = "#fff", zIndex = 1)
                           )
                         )
    )
  })
  
  # Plotting intensity distribution metrics by day
  output$results_by_day_int_dist_fig <- renderPlot({
    
    create_fig_res_by_day(
      data = results_list()$results_by_day$df_all_metrics, 
      minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis, 
      start_day_analysis = analysis_filters()$start_day_analysis,
      end_day_analysis = analysis_filters()$end_day_analysis,
      metrics = "int_distri",
      epoch_label = paste0(input$to_epoch, "s")
    ) + theme(axis.text.x = element_text(angle = 90))
  }, width = "auto", height = 800, res = 100)
  
  # Plotting intensity distribution metrics summary
  output$results_summary_int_dist_fig <- renderPlot({
    
    # Set correction factor for showing counts/min-intensity thresholds in correspondance to the chosen epoch
    cor_factor <- 60 / as.numeric(input$to_epoch)
    
    create_fig_mx_summary(
    data = results_summary_means(),
    labels = NULL,
    mpa_cutpoint = results_list()$mpa_cutpoint_chosen / cor_factor, 
    vpa_cutpoint = results_list()$vpa_cutpoint_chosen / cor_factor
    )
      
  }, width = "auto", height = 800, res = 100)
  
  # Showing results summarized over valid days in a table (means)
  output$results_summary_int_dist_means <- reactable::renderReactable({
    
    reactable::reactable(
      results_summary_means() %>% dplyr::select(valid_days, ig:M5), 
      list(valid_days = reactable::colDef(minWidth = 90)),
      striped = TRUE
    )
    
  })
  
  # Showing results summarized over valid days in a table (medians)
  output$results_summary_int_dist_medians <- reactable::renderReactable({
    
    reactable::reactable(
      results_summary_medians() %>% dplyr::select(valid_days, ig:M5), 
      list(valid_days = reactable::colDef(minWidth = 90)),
      striped = TRUE
    )
    
  })
    
    
  # Sedentary behaviour accumulation metrics panels =======================================================================
    
  # Showing section title for pattern of accumulation of sedentary behaviour
    output$title_SB_accum_metrics <- renderUI({
      
      # Title  will be shown only if dataset has been processed using 60-s epochs
      req(
        results_summary_means()$valid_days >=1 &
        (as.numeric(results_list()$df_with_computed_metrics$time[2] - results_list()$df_with_computed_metrics$time[1]) == 60)
      )
      
      # Showing title
      h3("Sedentary behaviour accumulation metrics computed using valid days")
      
    })  

  # Computing metrics for pattern of accumulation of sedentary behaviour
    metrics_accum_sed <- eventReactive(input$Run, {
      
      # Metrics will be computed only if dataset has been processed using 60-s epochs
      req(
        results_summary_means()$valid_days >=1 &
          (as.numeric(results_list()$df_with_computed_metrics$time[2] - results_list()$df_with_computed_metrics$time[1]) == 60)
      )
      
      # Getting metrics
      compute_accumulation_metrics(
        data = results_list()$df_with_computed_metrics,
        behaviour = "sed",
        dates = attributes(as.factor(
          (results_list()$results_by_day$df_all_metrics %>% 
            dplyr::filter(wear_time >= input$minimum_wear_time_for_analysis * 60))$date)
          )$levels,
        valid_wear_time_start = input$start_day_analysis,
        valid_wear_time_end = input$end_day_analysis,
        zoom_from = "00:00:00",
        zoom_to = "23:59:59"
      )
    
    })
    
  # Plotting temporal distribution of sedentary bouts
    output$graph_breaks_SB <- renderPlot({
  
       metrics_accum_sed()$p_breaks
       }, 
       width = "auto", 
       height = function(){
         
         n_levels = length(unique(metrics_accum_sed()$recap_bouts_by_day$date))
         
         height <- dplyr::case_when(
           n_levels >= 8 ~ n_levels * 95,
           n_levels == 7 ~ n_levels * 105,
           n_levels == 6 ~ n_levels * 107,
           n_levels == 5 ~ n_levels * 110,
           n_levels == 4 ~ n_levels * 115,
           n_levels == 3 ~ n_levels * 130,
           n_levels == 2 ~ n_levels * 150,
           n_levels == 1 ~ n_levels * 205
         )
         return(height)
       }, 
       res = 120)
  
  # Plotting alpha for sedentary bouts
    output$graph_alpha_SB <- renderPlot({
      metrics_accum_sed()$p_alpha
    }, width = "auto", height = 400, res = 80)
    
  # Plotting MBD for sedentary bouts
     output$graph_mbd_SB <- renderPlot({
       metrics_accum_sed()$p_MBD
     }, width = "auto", height = 400, res = 80)
     
  # Plotting UBD for sedentary bouts
    output$graph_ubd_SB <- renderPlot({
      metrics_accum_sed()$p_UBD
    }, width = "auto", height = 400, res = 80)
  
  # Plotting Gini index for sedentary bouts
    output$graph_gini_SB <- renderPlot({
      metrics_accum_sed()$p_gini
    }, width = "auto", height = 400, res = 80)
  
    
  # Physical activity accumulation metrics panels =======================================================================
  
  # Showing section title for pattern of accumulation of physical activity
    output$title_PA_accum_metrics <- renderUI({
      
      # Title  will be shown only if dataset has been processed using 60-s epochs
      req(
        results_summary_means()$valid_days >=1 &
          (as.numeric(results_list()$df_with_computed_metrics$time[2] - results_list()$df_with_computed_metrics$time[1]) == 60)
      )
      
      # Showing title
      h3("Physical activity accumulation metrics computed using valid days")
    })
    
  # Computing metrics for pattern of accumulation of physical activity
    metrics_accum_pa <- eventReactive(input$Run, {
     
     # Metrics will be computed only if dataset has been processed using 60-s epochs
      req(
        results_summary_means()$valid_days >=1 &
          (as.numeric(results_list()$df_with_computed_metrics$time[2] - results_list()$df_with_computed_metrics$time[1]) == 60)
      )
      
    # Getting metrics
     compute_accumulation_metrics(
       data = results_list()$df_with_computed_metrics,
       behaviour = "pa",
       dates = attributes(as.factor(
         (results_list()$results_by_day$df_all_metrics %>% 
            dplyr::filter(wear_time >= input$minimum_wear_time_for_analysis * 60))$date)
       )$levels,
       valid_wear_time_start = input$start_day_analysis,
       valid_wear_time_end = input$end_day_analysis,
       zoom_from = "00:00:00",
       zoom_to = "23:59:59"
     )
     
   })
  
  # Plotting temporal distribution of physical activity bouts
    output$graph_breaks_PA <- renderPlot({
      
      metrics_accum_pa()$p_breaks
    }, 
    width = "auto", 
    height = function(){
      
      n_levels = length(unique(metrics_accum_pa()$recap_bouts_by_day$date))
      
      height <- dplyr::case_when(
        n_levels >= 8 ~ n_levels * 95,
        n_levels == 7 ~ n_levels * 105,
        n_levels == 6 ~ n_levels * 107,
        n_levels == 5 ~ n_levels * 110,
        n_levels == 4 ~ n_levels * 115,
        n_levels == 3 ~ n_levels * 130,
        n_levels == 2 ~ n_levels * 150,
        n_levels == 1 ~ n_levels * 205
      )
      return(height)
    }, 
    res = 120)
  
  # Plotting alpha for  physical activity bouts
    output$graph_alpha_PA <- renderPlot({
      metrics_accum_pa()$p_alpha
    }, width = "auto", height = 400, res = 80)
  
  # Plotting MBD for  physical activity bouts
    output$graph_mbd_PA <- renderPlot({
      metrics_accum_pa()$p_MBD
    }, width = "auto", height = 400, res = 80)
    
  # Plotting UBD for  physical activity bouts
    output$graph_ubd_PA <- renderPlot({
      metrics_accum_pa()$p_UBD
    }, width = "auto", height = 400, res = 80)
  
  # Plotting Gini index for  physical activity bouts
    output$graph_gini_PA <- renderPlot({
      metrics_accum_pa()$p_gini
    }, width = "auto", height = 400, res = 80)
  
  
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
                          color_bg <- "#3F51B5"
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
                          color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
                         color_bg <- "#3F51B5"
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
                         color_bg <- "#3F51B5"
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
                       color_bg <- "#3F51B5"
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
                       color_bg <- "#3F51B5"
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
                   ~"D\u00e9sol\u00e9, aucun score n'est disponible en raison de l'absence de jour valide.", 
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
                         color_bg <- "#3F51B5"
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
                         color_bg <- "#3F51B5"
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
                    ~"D\u00e9sol\u00e9, aucun score n'est disponible en raison de l'absence de jour valide.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
                    ~"D\u00e9sol\u00e9, aucun score n'est disponible en raison de l'absence de jour valide.", 
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
                          color_bg <- "#3F51B5"
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
                          color_bg <- "#3F51B5"
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
                    ~"D\u00e9sol\u00e9, aucun score n'est disponible en raison de l'absence de jour valide.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[1, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
             if(nrow(results_list()$results_by_day$df_all_metrics) < 1 || results_list()$results_by_day$df_all_metrics[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                         color_bg <- "#3F51B5"
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
                         color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[1, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
                if(nrow(results_list()$results_by_day$df_all_metrics) < 1 || results_list()$results_by_day$df_all_metrics[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                          color_bg <- "#3F51B5"
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
                          color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[2, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 2 || results_list()$results_by_day$df_all_metrics[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[2, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 2 || results_list()$results_by_day$df_all_metrics[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[3, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 3 || results_list()$results_by_day$df_all_metrics[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[3, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 3 || results_list()$results_by_day$df_all_metrics[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[4, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 4 || results_list()$results_by_day$df_all_metrics[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
            vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[4, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
         
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
            if(nrow(results_list()$results_by_day$df_all_metrics) < 4 || results_list()$results_by_day$df_all_metrics[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                      color_bg <- "#3F51B5"
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
                      color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[5, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 5 || results_list()$results_by_day$df_all_metrics[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[5, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 5 || results_list()$results_by_day$df_all_metrics[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[6, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 6 || results_list()$results_by_day$df_all_metrics[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[6, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 6 || results_list()$results_by_day$df_all_metrics[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[7, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 7 || results_list()$results_by_day$df_all_metrics[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[7, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 7 || results_list()$results_by_day$df_all_metrics[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[1, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 1 || results_list()$results_by_day$df_all_metrics[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.",  
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[1, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
             if(nrow(results_list()$results_by_day$df_all_metrics) < 1 || results_list()$results_by_day$df_all_metrics[1, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
               reactable::reactable(
                 tibble::tribble(
                   ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                       color_bg <- "#3F51B5"
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
                       color_bg <- "#3F51B5"
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
            steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[2, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
          
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
            if(nrow(results_list()$results_by_day$df_all_metrics) < 2 || results_list()$results_by_day$df_all_metrics[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
              reactable::reactable(
                tibble::tribble(
                  ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                      color_bg <- "#3F51B5"
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
                      color_bg <- "#3F51B5"
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
            vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[2, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
          
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
            if(nrow(results_list()$results_by_day$df_all_metrics) < 2 || results_list()$results_by_day$df_all_metrics[2, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
              reactable::reactable(
                tibble::tribble(
                  ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                      color_bg <- "#3F51B5"
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
                      color_bg <- "#3F51B5"
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
            steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[3, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
          
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
            if(nrow(results_list()$results_by_day$df_all_metrics) < 3 || results_list()$results_by_day$df_all_metrics[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
              reactable::reactable(
                tibble::tribble(
                  ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                      color_bg <- "#3F51B5"
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
                      color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[3, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
          
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 3 || results_list()$results_by_day$df_all_metrics[3, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[4, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 4 || results_list()$results_by_day$df_all_metrics[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[4, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 4 || results_list()$results_by_day$df_all_metrics[4, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.",  
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[5, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 5 || results_list()$results_by_day$df_all_metrics[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[5, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 5 || results_list()$results_by_day$df_all_metrics[5, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[6, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
               if(nrow(results_list()$results_by_day$df_all_metrics) < 6 || results_list()$results_by_day$df_all_metrics[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                 reactable::reactable(
                   tibble::tribble(
                     ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                         color_bg <- "#3F51B5"
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
                         color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[6, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 6 || results_list()$results_by_day$df_all_metrics[6, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              steps_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[7, ][["total_steps"]], quest = "D-PPAC", metric = "steps")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 7 || results_list()$results_by_day$df_all_metrics[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
              vmu_score <- compute_pro_actigraph_score(results_list()$results_by_day$df_all_metrics[7, ][["vm_per_min"]], quest = "D-PPAC", metric = "vm")
            
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
              if(nrow(results_list()$results_by_day$df_all_metrics) < 7 || results_list()$results_by_day$df_all_metrics[7, ][["wear_time"]] < input$minimum_wear_time_for_analysis * 60) {
                reactable::reactable(
                  tibble::tribble(
                    ~"D\u00e9sol\u00e9, aucun r\u00e9sultat n'est disponible en raison d'un nombre insuffisant de jours s\u00e9lectionn\u00e9s ou d'un temps de port insuffisant pour ce jour.", 
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
                        color_bg <- "#3F51B5"
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
                        color_bg <- "#3F51B5"
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
                      "In the past 7 days, how much walking did you do outside\u003f"                                                                             , NA                                                              ,  compute_pro_score_cppac(input$cppac_EN_q1, question = "q1"),
                      "In the past 7 days, how many chores did you do outside the house\u003f"                                                                    , NA                                                              ,  compute_pro_score_cppac(input$cppac_EN_q2, question = "q2"),
                      "In the past 7 days, how much difficulty did you have getting dressed\u003f"                                                                , compute_pro_score_cppac(input$cppac_EN_q3, question = "q3")     ,  NA,
                      "In the past 7 days, how much difficulty did you have getting out and about\u003f"                                                          , compute_pro_score_cppac(input$cppac_EN_q4, question = "q4")     ,  NA,
                      "In the past 7 days, how often did you avoid doing activities because of your lung problems\u003f"                                          , compute_pro_score_cppac(input$cppac_EN_q5, question = "q5")     ,  NA,
                      "In the past 7 days, how breathless were you in general during your activities\u003f"                                                       , compute_pro_score_cppac(input$cppac_EN_q6, question = "q6")     ,  NA,
                      "In the past 7 days, how often did you lack physical strength to do things because of your lung problems\u003f"                             , compute_pro_score_cppac(input$cppac_EN_q7, question = "q7")     ,  NA,
                      "In the past 7 days, how tired were you in general during your activities\u003f"                                                            , compute_pro_score_cppac(input$cppac_EN_q8, question = "q8")     ,  NA,
                      "In the past 7 days, how often did you have to take breaks during your physical activities\u003f"                                           , compute_pro_score_cppac(input$cppac_EN_q9, question = "q9")     ,  NA,
                      "In the past 7 days, how breathless were you when walking on level ground indoors and outdoors\u003f"                                       , compute_pro_score_cppac(input$cppac_EN_q10, question = "q10")   ,  NA,
                      "In the past 7 days, how much time did you need to recover from your physical activities\u003f"                                             , compute_pro_score_cppac(input$cppac_EN_q11, question = "q11")   ,  NA,
                      "In the past 7 days, did you need to consider your lung problems when you planned your activities because of your lung problems\u003f"      , compute_pro_score_cppac(input$cppac_EN_q12, question = "q12")   ,  NA,
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
            
              # **************
              # Amount (rasch)
              # **************                
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
            
             # *************
             # Total (rasch)
             # *************
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
                results_list()$results_by_day$df_all_metrics %>%
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
                      "Day 1", "How much walking did you do outside today\u003f"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d1_q1, question = "q1", language = "en"),
                      "Day 1", "How many chores did you do outside the house today\u003f "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d1_q2, question = "q2", language = "en"),
                      "Day 1", "How much difficulty did you have getting dressed today\u003f"                                 , compute_pro_score_dppac(input$dppac_EN_d1_q3, question = "q3", language = "en")     ,  NA,
                      "Day 1", "How often did you avoid doing activities because of your lung problems today\u003f"           , compute_pro_score_dppac(input$dppac_EN_d1_q4, question = "q4", language = "en")     ,  NA,
                      "Day 1", "How breathless were you in general during your activities today\u003f"                        , compute_pro_score_dppac(input$dppac_EN_d1_q5, question = "q5", language = "en")     ,  NA,
                      "Day 1", "How tired were you in general during your activities today\u003f"                             , compute_pro_score_dppac(input$dppac_EN_d1_q6, question = "q6", language = "en")     ,  NA,
                      "Day 1", "How often did you have to take breaks during your physical activities today\u003f"            , compute_pro_score_dppac(input$dppac_EN_d1_q7, question = "q7", language = "en")     ,  NA,
                      "Day 1", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[1] == "Valid", table_gt3x_results_en()$Steps_score[1], NA),
                      "Day 1", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[1] == "Valid", table_gt3x_results_en()$VMU_score[1], NA)
                    )  
                  
                  table_day2 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 2", ""                                                                                        , NA                                                                                ,   NA,              
                      "Day 2", "How much walking did you do outside today\u003f"                                              , NA                                                                                ,  compute_pro_score_dppac(input$dppac_EN_d2_q1, question = "q1", language = "en"),
                      "Day 2", "How many chores did you do outside the house today\u003f "                                    , NA                                                                                ,  compute_pro_score_dppac(input$dppac_EN_d2_q2, question = "q2", language = "en"),
                      "Day 2", "How much difficulty did you have getting dressed today\u003f"                                 , compute_pro_score_dppac(input$dppac_EN_d2_q3, question = "q3", language = "en")   ,  NA,
                      "Day 2", "How often did you avoid doing activities because of your lung problems today\u003f"           , compute_pro_score_dppac(input$dppac_EN_d2_q4, question = "q4", language = "en")   ,  NA,
                      "Day 2", "How breathless were you in general during your activities today\u003f"                        , compute_pro_score_dppac(input$dppac_EN_d2_q5, question = "q5", language = "en")   ,  NA,
                      "Day 2", "How tired were you in general during your activities today\u003f"                             , compute_pro_score_dppac(input$dppac_EN_d2_q6, question = "q6", language = "en")   ,  NA,
                      "Day 2", "How often did you have to take breaks during your physical activities today\u003f"            , compute_pro_score_dppac(input$dppac_EN_d2_q7, question = "q7", language = "en")   ,  NA,
                      "Day 2", "Weekly steps score"                                                                      , NA                                                                                ,  ifelse(table_gt3x_results_en()$Validity[2] == "Valid", table_gt3x_results_en()$Steps_score[2], NA),
                      "Day 2", "Weekly VMU score"                                                                        , NA                                                                                ,  ifelse(table_gt3x_results_en()$Validity[2] == "Valid", table_gt3x_results_en()$VMU_score[2], NA)
                    ) 
                  
                  table_day3 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 3", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 3", "How much walking did you do outside today\u003f"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d3_q1, question = "q1", language = "en"),
                      "Day 3", "How many chores did you do outside the house today\u003f "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d3_q2, question = "q2", language = "en"),
                      "Day 3", "How much difficulty did you have getting dressed today\u003f"                                 , compute_pro_score_dppac(input$dppac_EN_d3_q3, question = "q3", language = "en")     ,  NA,
                      "Day 3", "How often did you avoid doing activities because of your lung problems today\u003f"           , compute_pro_score_dppac(input$dppac_EN_d3_q4, question = "q4", language = "en")     ,  NA,
                      "Day 3", "How breathless were you in general during your activities today\u003f"                        , compute_pro_score_dppac(input$dppac_EN_d3_q5, question = "q5", language = "en")     ,  NA,
                      "Day 3", "How tired were you in general during your activities today\u003f"                             , compute_pro_score_dppac(input$dppac_EN_d3_q6, question = "q6", language = "en")     ,  NA,
                      "Day 3", "How often did you have to take breaks during your physical activities today\u003f"            , compute_pro_score_dppac(input$dppac_EN_d3_q7, question = "q7", language = "en")     ,  NA,
                      "Day 3", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[3] == "Valid", table_gt3x_results_en()$Steps_score[3], NA),
                      "Day 3", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[3] == "Valid", table_gt3x_results_en()$VMU_score[3], NA)
                    ) 
                  
                  table_day4 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 4", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 4", "How much walking did you do outside today\u003f"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d4_q1, question = "q1", language = "en"),
                      "Day 4", "How many chores did you do outside the house today\u003f "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d4_q2, question = "q2", language = "en"),
                      "Day 4", "How much difficulty did you have getting dressed today\u003f"                                 , compute_pro_score_dppac(input$dppac_EN_d4_q3, question = "q3", language = "en")     ,  NA,
                      "Day 4", "How often did you avoid doing activities because of your lung problems today\u003f"           , compute_pro_score_dppac(input$dppac_EN_d4_q4, question = "q4", language = "en")     ,  NA,
                      "Day 4", "How breathless were you in general during your activities today\u003f"                        , compute_pro_score_dppac(input$dppac_EN_d4_q5, question = "q5", language = "en")     ,  NA,
                      "Day 4", "How tired were you in general during your activities today\u003f"                             , compute_pro_score_dppac(input$dppac_EN_d4_q6, question = "q6", language = "en")     ,  NA,
                      "Day 4", "How often did you have to take breaks during your physical activities today\u003f"            , compute_pro_score_dppac(input$dppac_EN_d4_q7, question = "q7", language = "en")     ,  NA,
                      "Day 4", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[4] == "Valid", table_gt3x_results_en()$Steps_score[4], NA),
                      "Day 4", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[4] == "Valid", table_gt3x_results_en()$VMU_score[4], NA)
                    ) 
                  table_day5 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 5", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 5", "How much walking did you do outside today\u003f"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d5_q1, question = "q1", language = "en"),
                      "Day 5", "How many chores did you do outside the house today\u003f "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d5_q2, question = "q2", language = "en"),
                      "Day 5", "How much difficulty did you have getting dressed today\u003f"                                 , compute_pro_score_dppac(input$dppac_EN_d5_q3, question = "q3", language = "en")     ,  NA,
                      "Day 5", "How often did you avoid doing activities because of your lung problems today\u003f"           , compute_pro_score_dppac(input$dppac_EN_d5_q4, question = "q4", language = "en")     ,  NA,
                      "Day 5", "How breathless were you in general during your activities today\u003f"                        , compute_pro_score_dppac(input$dppac_EN_d5_q5, question = "q5", language = "en")     ,  NA,
                      "Day 5", "How tired were you in general during your activities today\u003f"                             , compute_pro_score_dppac(input$dppac_EN_d5_q6, question = "q6", language = "en")     ,  NA,
                      "Day 5", "How often did you have to take breaks during your physical activities today\u003f"            , compute_pro_score_dppac(input$dppac_EN_d5_q7, question = "q7", language = "en")     ,  NA,
                      "Day 5", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[5] == "Valid", table_gt3x_results_en()$Steps_score[5], NA),
                      "Day 5", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[5] == "Valid", table_gt3x_results_en()$VMU_score[5], NA)
                    )               
                  table_day6 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 6", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 6", "How much walking did you do outside today\u003f"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d6_q1, question = "q1", language = "en"),
                      "Day 6", "How many chores did you do outside the house today\u003f "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d6_q2, question = "q2", language = "en"),
                      "Day 6", "How much difficulty did you have getting dressed today\u003f"                                 , compute_pro_score_dppac(input$dppac_EN_d6_q3, question = "q3", language = "en")     ,  NA,
                      "Day 6", "How often did you avoid doing activities because of your lung problems today\u003f"           , compute_pro_score_dppac(input$dppac_EN_d6_q4, question = "q4", language = "en")     ,  NA,
                      "Day 6", "How breathless were you in general during your activities today\u003f"                        , compute_pro_score_dppac(input$dppac_EN_d6_q5, question = "q5", language = "en")     ,  NA,
                      "Day 6", "How tired were you in general during your activities today\u003f"                             , compute_pro_score_dppac(input$dppac_EN_d6_q6, question = "q6", language = "en")     ,  NA,
                      "Day 6", "How often did you have to take breaks during your physical activities today\u003f"            , compute_pro_score_dppac(input$dppac_EN_d6_q7, question = "q7", language = "en")     ,  NA,
                      "Day 6", "Weekly steps score"                                                                      , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[6] == "Valid", table_gt3x_results_en()$Steps_score[6], NA),
                      "Day 6", "Weekly VMU score"                                                                        , NA                                                                               ,  ifelse(table_gt3x_results_en()$Validity[6] == "Valid", table_gt3x_results_en()$VMU_score[6], NA)
                    ) 
                  
                  table_day7 <- 
                    tibble::tribble(
                      ~Day,   ~Item                                                                                      ,~"Difficulty score"                                                            ,~"Amount score",
                      "Day 7", ""                                                                                        , NA                                                                              ,   NA,              
                      "Day 7", "How much walking did you do outside today\u003f"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d7_q1, question = "q1", language = "en"),
                      "Day 7", "How many chores did you do outside the house today\u003f "                                    , NA                                                                               ,  compute_pro_score_dppac(input$dppac_EN_d7_q2, question = "q2", language = "en"),
                      "Day 7", "How much difficulty did you have getting dressed today\u003f"                                 , compute_pro_score_dppac(input$dppac_EN_d7_q3, question = "q3", language = "en")     ,  NA,
                      "Day 7", "How often did you avoid doing activities because of your lung problems today\u003f"           , compute_pro_score_dppac(input$dppac_EN_d7_q4, question = "q4", language = "en")     ,  NA,
                      "Day 7", "How breathless were you in general during your activities today\u003f"                        , compute_pro_score_dppac(input$dppac_EN_d7_q5, question = "q5", language = "en")     ,  NA,
                      "Day 7", "How tired were you in general during your activities today\u003f"                             , compute_pro_score_dppac(input$dppac_EN_d7_q6, question = "q6", language = "en")     ,  NA,
                      "Day 7", "How often did you have to take breaks during your physical activities today\u003f"            , compute_pro_score_dppac(input$dppac_EN_d7_q7, question = "q7", language = "en")     ,  NA,
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
                        list(background = "#3F51B5", color = "white", fontWeight = "bold")
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
                recap_dppac_en <- eventReactive(input$get_dppac_summary_en, {
                  
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
              
                
              #*****************
              # Difficulty (raw)
              #*****************
              output$infoBox_dppac_en_total_diff <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_en()$mean_difficulty_score_raw[1], 1), " / 20"), style = "color: white;"),
                    "Mean difficulty score (raw)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #*************
              # Amount (raw)
              #*************
              output$infoBox_dppac_en_total_amount <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_en()$mean_amount_score_raw[1], 1), " / 17"), style = "color: white;"), 
                    "Mean amount score (raw)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              #************
              # Total (raw)
              #************
              output$infoBox_dppac_en_total_all <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_en()$mean_total_score_raw[1], 1), " / 37"),  style = "color: white;"), 
                    "Mean total score (raw)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })
              
              
              #*******************
              # Difficulty (rasch)
              #*******************
              output$infoBox_dppac_en_total_diff_rasch <- shinydashboard::renderValueBox({
                req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Waiting for required conditions
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_en()$mean_difficulty_score_rasch[1], 1), " / 100"), style = "color: white;"),
                    "Mean difficulty score (Rasch)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #***************
              # Amount (rasch)
              #***************
              output$infoBox_dppac_en_total_amount_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_en()$mean_amount_score_rasch[1], 1), " / 100"), style = "color: white;"), 
                    "Mean amount score (Rasch)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              
              #**************
              # Total (rasch)
              #**************
              output$infoBox_dppac_en_total_all_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_en & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_en()$mean_total_score_rasch[1], 1),
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
                  if (input$cppac_FR_summary_metric == "Scores calcul\u00e9s \u00e0 partir des M\u00c9DIANES") {
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
                  if (input$cppac_FR_summary_metric == "Scores calcul\u00e9s \u00e0 partir des M\u00c9DIANES") {
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
                      ~Item                                                                                                                                                                      ,~"Score de difficult\u00e9"                                                       ,~"Score de quantit\u00e9",
                      "Au cours des 7 derniers jours, avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur \u003f"                                                                                 , NA                                                                               ,  compute_pro_score_cppac(input$cppac_FR_q1, question = "q1", language = "fr"),
                      "Au cours des 7 derniers jours, avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur \u003f"                                                               , NA                                                                               ,  compute_pro_score_cppac(input$cppac_FR_q2, question = "q2", language = "fr"),
                      "Au cours des 7 derniers jours, avez-vous eu des difficult\u00e9s pour vous habiller \u003f"                                                                               , compute_pro_score_cppac(input$cppac_FR_q3, question = "q3", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous eu des difficult\u00e9s pour sortir de chez vous \u003f"                                                                         , compute_pro_score_cppac(input$cppac_FR_q4, question = "q4", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires \u003f"                                      , compute_pro_score_cppac(input$cppac_FR_q5, question = "q5", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, \u00e9tiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s \u003f"                                                   , compute_pro_score_cppac(input$cppac_FR_q6, question = "q6", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous manqu\u00e9 de force pour effectuer des t\u00e2ches \u00e0 cause de vos probl\u00e8mes respiratoires \u003f"                     , compute_pro_score_cppac(input$cppac_FR_q7, question = "q7", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, \u00e9tiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s \u003f"                                                     , compute_pro_score_cppac(input$cppac_FR_q8, question = "q8", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, avez-vous d\u00fb faire des pauses pendant vos activit\u00e9s physiques \u003f"                                                            , compute_pro_score_cppac(input$cppac_FR_q9, question = "q9", language = "fr")     ,  NA,
                      "Au cours des 7 derniers jours, \u00e9tiez-vous essouffl\u00e9(e) lors de la marche sur terrain plat, \u00e0 l\u2019int\u00e9rieur et \u00e0 l\u2019ext\u00e9rieur \u003f" , compute_pro_score_cppac(input$cppac_FR_q10, question = "q10", language = "fr")   ,  NA,
                      "Au cours des 7 derniers jours, combien de temps vous a-t-il fallu pour r\u00e9cup\u00e9rer de vos activit\u00e9s physiques \u003f"                                        , compute_pro_score_cppac(input$cppac_FR_q11, question = "q11", language = "fr")   ,  NA,
                      "Au cours des 7 derniers jours, avez-vous eu besoin de prendre en compte vos probl\u00e8mes respiratoires lorsque vous avez planifi\u00e9 vos activit\u00e9s \u003f"       , compute_pro_score_cppac(input$cppac_FR_q12, question = "q12", language = "fr")   ,  NA,
                      "Score de pas hebdomadaire"                                                                                                                                                , NA                                                                               ,  chosen_proactive_cppac_steps_score_fr(),
                      "Score VMU hebdomadaire"                                                                                                                                                   , NA                                                                               ,  chosen_proactive_cppac_vmu_score_fr()
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
                    "Score de difficult\xc3\xa9" = reactable::colDef(width = 80),
                    "Score de quantit\xc3\xa9" = reactable::colDef(width = 80)
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
                    tags$h3(paste0(sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE), " / 40"), style = "color: white;"),
                    "Score de difficult\u00e9 (brut)", icon = NULL,
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
                   tags$h3(paste0(sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), " / 15"), style = "color: white;"), 
                   "Score de quantit\u00e9 (brut)", icon = NULL,
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
                    tags$h3(paste0(sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE) + sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), " / 55"),  style = "color: white;"), 
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
                      x = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE),
                      quest = "C-PPAC",
                      score = "difficulty"), " / 100"), style = "color: white;"),
                    "Score de difficult\u00e9 (Rasch)", icon = NULL,
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
                     x = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE),
                     quest = "C-PPAC",
                     score = "quantity"), " / 100"), style = "color: white;"), 
                   "Score de quantit\u00e9 (Rasch)", icon = NULL,
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
                      round((rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                               rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1),
                      " / 100"),  style = "color: white;"), 
                    "Score total (Rasch)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })
            
            
        # Getting PROactive final results: D-PPAC (FR) ----
            
               # Table with monitor PROactive scores for each day
                 table_gt3x_results_fr <- eventReactive(input$get_dppac_summary_fr, {
                     results_list()$results_by_day$df_all_metrics %>%
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
                         ~Jour,   ~Item                                                                                                                      ,~"Score de difficult\u00e9"                                                       ,~"Score de quantit\u00e9",
                         "Jour 1", ""                                                                                                                        , NA                                                                               ,   NA,              
                         "Jour 1", "Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                                              , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d1_q1, question = "q1", language = "fr"),
                         "Jour 1", "Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                            , NA                                                                               ,  compute_pro_score_dppac(input$dppac_FR_d1_q2, question = "q2", language = "fr"),
                         "Jour 1", "Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f"                                            , compute_pro_score_dppac(input$dppac_FR_d1_q3, question = "q3", language = "fr")  ,  NA,
                         "Jour 1", "Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f"   , compute_pro_score_dppac(input$dppac_FR_d1_q4, question = "q4", language = "fr")  ,  NA,
                         "Jour 1", "Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                     , compute_pro_score_dppac(input$dppac_FR_d1_q5, question = "q5", language = "fr")  ,  NA,
                         "Jour 1", "Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                       , compute_pro_score_dppac(input$dppac_FR_d1_q6, question = "q6", language = "fr")  ,  NA,
                         "Jour 1", "Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f"                              , compute_pro_score_dppac(input$dppac_FR_d1_q7, question = "q7", language = "fr")  ,  NA,
                         "Jour 1", "Score de pas hebdomadaire"                                                                                               , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[1] == "Valid", table_gt3x_results_fr()$Steps_score[1], NA),
                         "Jour 1", "Score VMU hebdomadaire"                                                                                                  , NA                                                                               ,  ifelse(table_gt3x_results_fr()$Validity[1] == "Valid", table_gt3x_results_fr()$VMU_score[1], NA)
                       )  
                     
                     table_day2 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                                                        ,~"Score de difficult\u00e9"                                                          ,~"Score de quantit\u00e9",
                         "Jour 2", ""                                                                                                                          , NA                                                                                  ,   NA,              
                         "Jour 2", "Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                                                , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d2_q1, question = "q1", language = "fr"),
                         "Jour 2", "Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                              , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d2_q2, question = "q2", language = "fr"),
                         "Jour 2", "Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f"                                              , compute_pro_score_dppac(input$dppac_FR_d2_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 2", "Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f"     , compute_pro_score_dppac(input$dppac_FR_d2_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 2", "Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                       , compute_pro_score_dppac(input$dppac_FR_d2_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 2", "Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                         , compute_pro_score_dppac(input$dppac_FR_d2_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 2", "Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f"                                , compute_pro_score_dppac(input$dppac_FR_d2_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 2", "Score de pas hebdomadaire"                                                                                                 , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[2] == "Valid", table_gt3x_results_fr()$Steps_score[2], NA),
                         "Jour 2", "Score VMU hebdomadaire"                                                                                                    , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[2] == "Valid", table_gt3x_results_fr()$VMU_score[2], NA)
                       ) 
                     
                     table_day3 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                                                      ,~"Score de difficult\u00e9"                                                          ,~"Score de quantit\u00e9",
                         "Jour 3", ""                                                                                                                        , NA                                                                                  ,   NA,              
                         "Jour 3", "Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                                              , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d3_q1, question = "q1", language = "fr"),
                         "Jour 3", "Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                            , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d3_q2, question = "q2", language = "fr"),
                         "Jour 3", "Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f"                                            , compute_pro_score_dppac(input$dppac_FR_d3_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 3", "Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f"   , compute_pro_score_dppac(input$dppac_FR_d3_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 3", "Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                     , compute_pro_score_dppac(input$dppac_FR_d3_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 3", "Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                       , compute_pro_score_dppac(input$dppac_FR_d3_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 3", "Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f"                              , compute_pro_score_dppac(input$dppac_FR_d3_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 3", "Score de pas hebdomadaire"                                                                                               , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[3] == "Valid", table_gt3x_results_fr()$Steps_score[3], NA),
                         "Jour 3", "Score VMU hebdomadaire"                                                                                                  , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[3] == "Valid", table_gt3x_results_fr()$VMU_score[3], NA)
                       ) 
                     
                     table_day4 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                                                     ,~"Score de difficult\u00e9"                                                          ,~"Score de quantit\u00e9",
                         "Jour 4", ""                                                                                                                       , NA                                                                                  ,   NA,              
                         "Jour 4", "Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                                             , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d4_q1, question = "q1", language = "fr"),
                         "Jour 4", "Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                           , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d4_q2, question = "q2", language = "fr"),
                         "Jour 4", "Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f"                                           , compute_pro_score_dppac(input$dppac_FR_d4_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 4", "Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f"  , compute_pro_score_dppac(input$dppac_FR_d4_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 4", "Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                    , compute_pro_score_dppac(input$dppac_FR_d4_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 4", "Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                      , compute_pro_score_dppac(input$dppac_FR_d4_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 4", "Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f"                             , compute_pro_score_dppac(input$dppac_FR_d4_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 4", "Score de pas hebdomadaire"                                                                                              , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[4] == "Valid", table_gt3x_results_fr()$Steps_score[4], NA),
                         "Jour 4", "Score VMU hebdomadaire"                                                                                                 , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[4] == "Valid", table_gt3x_results_fr()$VMU_score[4], NA)
                       ) 
                     
                     table_day5 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                                                      ,~"Score de difficult\u00e9"                                                          ,~"Score de quantit\u00e9",
                         "Jour 5", ""                                                                                                                        , NA                                                                                  ,   NA,              
                         "Jour 5", "Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                                              , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d5_q1, question = "q1", language = "fr"),
                         "Jour 5", "Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                            , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d5_q2, question = "q2", language = "fr"),
                         "Jour 5", "Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f"                                            , compute_pro_score_dppac(input$dppac_FR_d5_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 5", "Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f"   , compute_pro_score_dppac(input$dppac_FR_d5_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 5", "Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                     , compute_pro_score_dppac(input$dppac_FR_d5_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 5", "Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                       , compute_pro_score_dppac(input$dppac_FR_d5_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 5", "Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f"                              , compute_pro_score_dppac(input$dppac_FR_d5_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 5", "Score de pas hebdomadaire"                                                                                               , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[5] == "Valid", table_gt3x_results_fr()$Steps_score[5], NA),
                         "Jour 5", "Score VMU hebdomadaire"                                                                                                  , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[5] == "Valid", table_gt3x_results_fr()$VMU_score[5], NA)
                       ) 
                     
                     table_day6 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                                                       ,~"Score de difficult\u00e9"                                                          ,~"Score de quantit\u00e9",
                         "Jour 6", ""                                                                                                                         , NA                                                                                  ,   NA,              
                         "Jour 6", "Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                                               , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d6_q1, question = "q1", language = "fr"),
                         "Jour 6", "Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                             , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d6_q2, question = "q2", language = "fr"),
                         "Jour 6", "Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f"                                             , compute_pro_score_dppac(input$dppac_FR_d6_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 6", "Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f"    , compute_pro_score_dppac(input$dppac_FR_d6_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 6", "Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                      , compute_pro_score_dppac(input$dppac_FR_d6_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 6", "Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                        , compute_pro_score_dppac(input$dppac_FR_d6_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 6", "Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f"                               , compute_pro_score_dppac(input$dppac_FR_d6_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 6", "Score de pas hebdomadaire"                                                                                                , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[6] == "Valid", table_gt3x_results_fr()$Steps_score[6], NA),
                         "Jour 6", "Score VMU hebdomadaire"                                                                                                   , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[6] == "Valid", table_gt3x_results_fr()$VMU_score[6], NA)
                       ) 
                     
                     table_day7 <- 
                       tibble::tribble(
                         ~Jour,   ~Item                                                                                                                       ,~"Score de difficult\u00e9"                                                          ,~"Score de quantit\u00e9",
                         "Jour 7", ""                                                                                                                         , NA                                                                                  ,  NA,              
                         "Jour 7", "Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                                               , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d7_q1, question = "q1", language = "fr"),
                         "Jour 7", "Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f"                             , NA                                                                                  ,  compute_pro_score_dppac(input$dppac_FR_d7_q2, question = "q2", language = "fr"),
                         "Jour 7", "Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f"                                             , compute_pro_score_dppac(input$dppac_FR_d7_q3, question = "q3", language = "fr")     ,  NA,
                         "Jour 7", "Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f"    , compute_pro_score_dppac(input$dppac_FR_d7_q4, question = "q4", language = "fr")     ,  NA,
                         "Jour 7", "Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                      , compute_pro_score_dppac(input$dppac_FR_d7_q5, question = "q5", language = "fr")     ,  NA,
                         "Jour 7", "Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f"                        , compute_pro_score_dppac(input$dppac_FR_d7_q6, question = "q6", language = "fr")     ,  NA,
                         "Jour 7", "Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f"                               , compute_pro_score_dppac(input$dppac_FR_d7_q7, question = "q7", language = "fr")     ,  NA,
                         "Jour 7", "Score de pas hebdomadaire"                                                                                                , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[7] == "Valid", table_gt3x_results_fr()$Steps_score[7], NA),
                         "Jour 7", "Score VMU hebdomadaire"                                                                                                   , NA                                                                                  ,  ifelse(table_gt3x_results_fr()$Validity[7] == "Valid", table_gt3x_results_fr()$VMU_score[7], NA)
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
                      
                      if ((tab_dppac_summary_fr()[index, "Item"] %in% c("Score de pas hebdomadaire", "Score VMU hebdomadaire")) && is.na(tab_dppac_summary_fr()[index, "Score de quantit\u00e9"])) {
                        list(background = "#FF6666")
                      } else if (tab_dppac_summary_fr()[index, "Item"] == "") {
                        list(background = "#3F51B5", color = "white", fontWeight = "bold")
                      } else {
                        NULL
                      }
                    },
                    defaultColDef = reactable::colDef(align = "center"),
                    columns = list(
                      Item = reactable::colDef(align = "left"),
                      "Jour" = reactable::colDef(width = 60),
                      "Score de difficult\xc3\xa9" = reactable::colDef(width = 80),
                      "Score de quantit\xc3\xa9" = reactable::colDef(width = 80)
                      
                    ),
                    defaultPageSize = 70,
                    striped = TRUE
                  )
         
              })
            
            # Value Boxes
              
              # Making summary table
                recap_dppac_fr <- eventReactive(input$get_dppac_summary_fr, {
                
                  # Waiting for required conditions
                    req(!is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median())) 
                
                  # Making table
                    tab_dppac_summary_fr() %>%
                     dplyr::mutate(score_type = rep(c("", "quant", "quant", "diff", "diff", "diff", "diff", "diff", "quant", "quant"), 7)) %>%
                     dplyr::group_by(Jour, score_type) %>%
                     dplyr::filter(Item != "") %>%
                     dplyr::summarise(difficulty_score_raw = sum(.data[["Score de difficult\u00e9"]], na.rm = TRUE),
                                      amount_score_raw = sum(.data[["Score de quantit\u00e9"]])
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
              # Difficulty (raw)
              #****************
              output$infoBox_dppac_fr_total_diff <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_fr()$mean_difficulty_score_raw[1], 1), " / 20"), style = "color: white;"),
                    "Score de difficult\u00e9 moyen (brut)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              #************
              # Amount (raw)
              #************
              output$infoBox_dppac_fr_total_amount <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_fr()$mean_amount_score_raw[1], 1), " / 17"), style = "color: white;"), 
                    "Score de quantit\u00e9 moyen (brut)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              #***********
              # Total (raw)
              #***********
              output$infoBox_dppac_fr_total_all <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_fr()$mean_total_score_raw[1], 1), " / 37"),  style = "color: white;"), 
                    "Score total moyen (brut)",  icon = NULL,
                    color = "purple",
                    width = 4
                  )
              })
              
              
              #******************
              # Difficulty (rasch)
              #******************
              output$infoBox_dppac_fr_total_diff_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_fr()$mean_difficulty_score_rasch[1], 1), " / 100"), style = "color: white;"),
                    "Score de difficult\u00e9 moyen (Rasch)", icon = NULL,
                    color = "aqua",
                    width = 4
                  )
              })
              
              
              #**************
              # Amount (rasch)
              #**************
              output$infoBox_dppac_fr_total_amount_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                  shinydashboard::valueBox(
                    tags$h3(paste0(round(recap_dppac_fr()$mean_amount_score_rasch[1], 1), " / 100"), style = "color: white;"), 
                    "Score de quantit\u00e9 moyen (Rasch)", icon = NULL,
                    color = "teal",
                    width = 4
                  )
              })
              
              
              #*************
              # Total (rasch
              #*************
              output$infoBox_dppac_fr_total_all_rasch <- shinydashboard::renderValueBox({
                
                # Waiting for required conditions
                  req(input$get_dppac_summary_fr & !is.na(steps_score_cppac_median()) & !is.na(vmu_score_cppac_median()))
                
                # Box
                 shinydashboard::valueBox(
                   tags$h3(paste0(round(recap_dppac_fr()$mean_total_score_rasch[1], 1),
                     " / 100"),  style = "color: white;"), 
                   "Score total moyen (Rasch)",  icon = NULL,
                   color = "purple",
                   width = 4
                 )
              })
       
             
  #########################
  # Hidding / showing boxes ----
  #########################
  
  # Box for graph with wear time
    shinyjs::hide("myBox")
    shinyjs::hide("Metric")
    shinyjs::hide("zoom_from_weartime")
    shinyjs::hide("zoom_to_weartime")
    shinyjs::hide("update_graphic")
    shinyjs::hide("graph")
    observe({
      if(nrow(df()) >=1) {
        shinyjs::show("myBox")
        shinyjs::show("Metric")
        shinyjs::show("zoom_from_weartime")
        shinyjs::show("zoom_to_weartime")
        shinyjs::show("update_graphic")
        shinyjs::show("graph")
      } else {
        shinyjs::hide("myBox")
        shinyjs::hide("Metric")
        shinyjs::hide("zoom_from_weartime")
        shinyjs::hide("zoom_to_weartime")
        shinyjs::hide("update_graphic")
        shinyjs::hide("graph")
      }
    })
  
  # Boxes for graph with PA categories and results
    shinyjs::hide("myBox2")
    shinyjs::hide("Metric2")
    shinyjs::hide("zoom_from_analysis")
    shinyjs::hide("zoom_to_analysis")
    shinyjs::hide("update_graphic2")
    shinyjs::hide("graph_int")
    shinyjs::hide("BoxResByDayVolTab")
    shinyjs::hide("BoxResVolMeans")
    shinyjs::hide("BoxResVolMedians")
    shinyjs::hide("BoxResByDayStepTab")
    shinyjs::hide("BoxResStepMeans")
    shinyjs::hide("BoxResStepMedians")
    shinyjs::hide("BoxResByDayIntDistTab")
    shinyjs::hide("BoxResIntDistMeans")
    shinyjs::hide("BoxResIntDistMedians")
    shinyjs::hide("title_activity_volume_metrics")
    shinyjs::hide("title_step_acc_metrics")
    shinyjs::hide("title_int_distri_metrics")
    observe({
      if(nrow(results_list()$df_with_computed_metrics) >=1) {
      shinyjs::show("myBox2")
      shinyjs::show("Metric2")
      shinyjs::show("zoom_from_analysis")
      shinyjs::show("zoom_to_analysis")
      shinyjs::show("update_graphic2")
      shinyjs::show("graph_int")
      shinyjs::show("BoxResByDayVolTab")
      shinyjs::show("BoxResVolMeans")
      shinyjs::show("BoxResVolMedians")
      shinyjs::show("BoxResByDayStepTab")
      shinyjs::show("BoxResStepMeans")
      shinyjs::show("BoxResStepMedians")
      shinyjs::show("BoxResByDayIntDistTab")
      shinyjs::show("BoxResIntDistMeans")
      shinyjs::show("BoxResIntDistMedians")
      shinyjs::show("title_activity_volume_metrics")
      shinyjs::show("title_step_acc_metrics")
      shinyjs::show("title_int_distri_metrics")
      } else {
      shinyjs::hide("myBox2")
      shinyjs::hide("Metric2")
      shinyjs::hide("zoom_from_analysis")
      shinyjs::hide("zoom_to_analysis")
      shinyjs::hide("update_graphic2")
      shinyjs::hide("graph_int")
      shinyjs::hide("BoxResByDayVolTab")
      shinyjs::hide("BoxResVolMeans")
      shinyjs::hide("BoxResVolMedians")
      shinyjs::hide("BoxResByDayStepTab")
      shinyjs::hide("BoxResStepMeans")
      shinyjs::hide("BoxResStepMedians")
      shinyjs::hide("BoxResByDayIntDistTab")
      shinyjs::hide("BoxResIntDistMeans")
      shinyjs::hide("BoxResIntDistMedians")
      shinyjs::hide("title_activity_volume_metrics")
      shinyjs::hide("title_step_acc_metrics")
      shinyjs::hide("title_int_distri_metrics")
      }
    })
    
    # Boxes for graphs showing accumulation of behaviour over time
    shinyjs::hide("BoxBreaksSB")
    shinyjs::hide("BoxAlphaSB")
    shinyjs::hide("BoxMBDSB")
    shinyjs::hide("BoxUBDSB")
    shinyjs::hide("BoxGiniSB")
    shinyjs::hide("BoxBreaksPA")
    shinyjs::hide("BoxAlphaPA")
    shinyjs::hide("BoxMBDPA")
    shinyjs::hide("BoxUBDPA")
    shinyjs::hide("BoxGiniPA")
    
       # Boxes will be shown only if dataset has been processed using 60-s epochs
    observe({
      if(
        results_summary_means()$valid_days >=1 &&
         (as.numeric(results_list()$df_with_computed_metrics$time[2] - results_list()$df_with_computed_metrics$time[1]) == 60)
        ) {
        shinyjs::show("BoxBreaksSB")
        shinyjs::show("BoxAlphaSB")
        shinyjs::show("BoxMBDSB")
        shinyjs::show("BoxUBDSB")
        shinyjs::show("BoxGiniSB")
        shinyjs::show("BoxBreaksPA")
        shinyjs::show("BoxAlphaPA")
        shinyjs::show("BoxMBDPA")
        shinyjs::show("BoxUBDPA")
        shinyjs::show("BoxGiniPA")
      } else {
        shinyjs::hide("BoxBreaksSB")
        shinyjs::hide("BoxAlphaSB")
        shinyjs::hide("BoxMBDSB")
        shinyjs::hide("BoxUBDSB")
        shinyjs::hide("BoxGiniSB")
        shinyjs::hide("BoxBreaksPA")
        shinyjs::hide("BoxAlphaPA")
        shinyjs::hide("BoxMBDPA")
        shinyjs::hide("BoxUBDPA")
        shinyjs::hide("BoxGiniPA")
      }
    })
    
  
  ####################################
  # Hidding / showing Download buttons ----
  ####################################
  
  # Hidding main buttons
    shinyjs::hide("ExpDataset")
    shinyjs::hide("ExpResultsByDays")
    shinyjs::hide("ExpDailySummaryMeans")
    shinyjs::hide("ExpDailySummaryMedians")
    shinyjs::hide("report_en_long")
    shinyjs::hide("report_fr_long")
    shinyjs::hide("report_en_short")
    shinyjs::hide("report_fr_short")
    shinyjs::hide("go_to_proactive_q")
   
  # Showing main buttons
   observe({
    if(nrow(results_list()$df_with_computed_metrics) >=1) {
      shinyjs::show("ExpDataset")
      shinyjs::show("ExpResultsByDays")
      shinyjs::show("ExpDailySummaryMeans")
      shinyjs::show("ExpDailySummaryMedians")
    } else {
      shinyjs::hide("ExpDataset")
      shinyjs::hide("ExpResultsByDays")
      shinyjs::hide("ExpDailySummaryMeans")
      shinyjs::hide("ExpDailySummaryMedians")
    }
     if(results_summary_means()$valid_days >=1 || results_summary_medians()$valid_days >=1) {
       shinyjs::show("report_en_long")
       shinyjs::show("report_fr_long")
       shinyjs::show("report_en_short")
       shinyjs::show("report_fr_short") 
       shinyjs::show("go_to_proactive_q")
     } else {
       shinyjs::hide("report_en_long")
       shinyjs::hide("report_fr_long")
       shinyjs::hide("report_en_short")
       shinyjs::hide("report_fr_short") 
       shinyjs::hide("go_to_proactive_q")
     }
   })
   ######################################
   # Hidding / showing PROactive elements ----
   ######################################
    
   # Hidding CPPAC-EN elements
     shinyjs::hide("get_cppac_summary_en")
     shinyjs::hide("PROactive_scores_cppac_summary_en")
     shinyjs::hide("infoBox_cppac_en_total_diff")
     shinyjs::hide("infoBox_cppac_en_total_amount")
     shinyjs::hide("infoBox_cppac_en_total_all")
     shinyjs::hide("infoBox_cppac_en_total_diff_rasch")
     shinyjs::hide("infoBox_cppac_en_total_amount_rasch")
     shinyjs::hide("infoBox_cppac_en_total_all_rasch")
     shinyjs::hide("report_en_cppac_html")
     shinyjs::hide("report_en_cppac_pdf")
     
   # Hidding CPPAC-FR elements
     shinyjs::hide("get_cppac_summary_fr")
     shinyjs::hide("PROactive_scores_cppac_summary_fr")
     shinyjs::hide("infoBox_cppac_fr_total_diff")
     shinyjs::hide("infoBox_cppac_fr_total_amount")
     shinyjs::hide("infoBox_cppac_fr_total_all")
     shinyjs::hide("infoBox_cppac_fr_total_diff_rasch")
     shinyjs::hide("infoBox_cppac_fr_total_amount_rasch")
     shinyjs::hide("infoBox_cppac_fr_total_all_rasch")
     shinyjs::hide("report_fr_cppac_html")
     shinyjs::hide("report_fr_cppac_pdf")
     
   # Hidding DPPAC-EN elements
     shinyjs::hide("get_dppac_summary_en")
     shinyjs::hide("PROactive_scores_dppac_summary_en")
     shinyjs::hide("infoBox_dppac_en_total_diff")
     shinyjs::hide("infoBox_dppac_en_total_amount")
     shinyjs::hide("infoBox_dppac_en_total_all")
     shinyjs::hide("infoBox_dppac_en_total_diff_rasch")
     shinyjs::hide("infoBox_dppac_en_total_amount_rasch")
     shinyjs::hide("infoBox_dppac_en_total_all_rasch")
     shinyjs::hide("report_en_dppac_html")
     shinyjs::hide("report_en_dppac_pdf")
     
   # Hidding DPPAC-FR elements
     shinyjs::hide("get_dppac_summary_fr")
     shinyjs::hide("PROactive_scores_dppac_summary_fr")
     shinyjs::hide("infoBox_dppac_fr_total_diff")
     shinyjs::hide("infoBox_dppac_fr_total_amount")
     shinyjs::hide("infoBox_dppac_fr_total_all")
     shinyjs::hide("infoBox_dppac_fr_total_diff_rasch")
     shinyjs::hide("infoBox_dppac_fr_total_amount_rasch")
     shinyjs::hide("infoBox_dppac_fr_total_all_rasch")
     shinyjs::hide("report_fr_dppac_html") 
     shinyjs::hide("report_fr_dppac_pdf") 
   

   # Showing the button to be clicked for getting results summary
     observe({
      if(!is.na(steps_score_cppac_median()) && !is.na(vmu_score_cppac_median())) {
        shinyjs::show("get_cppac_summary_en")
        shinyjs::show("get_cppac_summary_fr")
        shinyjs::show("get_dppac_summary_en")
        shinyjs::show("get_dppac_summary_fr")
      } else {
        shinyjs::hide("get_cppac_summary_en")
        shinyjs::hide("get_cppac_summary_fr")
        shinyjs::hide("get_dppac_summary_en")
        shinyjs::hide("get_dppac_summary_fr")
      }
    })
    
   # Showing CPPAC-EN results
     observeEvent(input$get_cppac_summary_en, {
       shinyjs::show("PROactive_scores_cppac_summary_en")
       shinyjs::show("infoBox_cppac_en_total_diff")
       shinyjs::show("infoBox_cppac_en_total_amount")
       shinyjs::show("infoBox_cppac_en_total_all")
       shinyjs::show("infoBox_cppac_en_total_diff_rasch")
       shinyjs::show("infoBox_cppac_en_total_amount_rasch")
       shinyjs::show("infoBox_cppac_en_total_all_rasch")
       shinyjs::show("report_en_cppac_html")
       shinyjs::show("report_en_cppac_pdf")
     })
     
     
   # Showing CPPAC-FR results
     observeEvent(input$get_cppac_summary_fr, {
       shinyjs::show("PROactive_scores_cppac_summary_fr")
       shinyjs::show("infoBox_cppac_fr_total_diff")
       shinyjs::show("infoBox_cppac_fr_total_amount")
       shinyjs::show("infoBox_cppac_fr_total_all")
       shinyjs::show("infoBox_cppac_fr_total_diff_rasch")
       shinyjs::show("infoBox_cppac_fr_total_amount_rasch")
       shinyjs::show("infoBox_cppac_fr_total_all_rasch")
       shinyjs::show("report_fr_cppac_html")
       shinyjs::show("report_fr_cppac_pdf")
     })
     
   # Showing DPPAC-EN results
     observeEvent(input$get_dppac_summary_en, {
       shinyjs::show("PROactive_scores_dppac_summary_en")
       shinyjs::show("infoBox_dppac_en_total_diff")
       shinyjs::show("infoBox_dppac_en_total_amount")
       shinyjs::show("infoBox_dppac_en_total_all")
       shinyjs::show("infoBox_dppac_en_total_diff_rasch")
       shinyjs::show("infoBox_dppac_en_total_amount_rasch")
       shinyjs::show("infoBox_dppac_en_total_all_rasch")
       shinyjs::show("report_en_dppac_html")
       shinyjs::show("report_en_dppac_pdf")
     })
   

   # Showing DPPAC-FR results
     observeEvent(input$get_dppac_summary_fr, {
       shinyjs::show("PROactive_scores_dppac_summary_fr")
       shinyjs::show("infoBox_dppac_fr_total_diff")
       shinyjs::show("infoBox_dppac_fr_total_amount")
       shinyjs::show("infoBox_dppac_fr_total_all")
       shinyjs::show("infoBox_dppac_fr_total_diff_rasch")
       shinyjs::show("infoBox_dppac_fr_total_amount_rasch")
       shinyjs::show("infoBox_dppac_fr_total_all_rasch")
       shinyjs::show("report_fr_dppac_html") 
       shinyjs::show("report_fr_dppac_pdf") 
     })
     
   # Hidding again all PROactive results
     observeEvent(input$Run,{
       if(is.na(steps_score_cppac_median()) || is.na(vmu_score_cppac_median())) {
       
       # Hidding CPPAC-EN elements
       shinyjs::hide("get_cppac_summary_en")
       shinyjs::hide("PROactive_scores_cppac_summary_en")
       shinyjs::hide("infoBox_cppac_en_total_diff")
       shinyjs::hide("infoBox_cppac_en_total_amount")
       shinyjs::hide("infoBox_cppac_en_total_all")
       shinyjs::hide("infoBox_cppac_en_total_diff_rasch")
       shinyjs::hide("infoBox_cppac_en_total_amount_rasch")
       shinyjs::hide("infoBox_cppac_en_total_all_rasch")
       shinyjs::hide("report_en_cppac_html")
       shinyjs::hide("report_en_cppac_pdf")
       
       # Hidding CPPAC-FR elements
       shinyjs::hide("get_cppac_summary_fr")
       shinyjs::hide("PROactive_scores_cppac_summary_fr")
       shinyjs::hide("infoBox_cppac_fr_total_diff")
       shinyjs::hide("infoBox_cppac_fr_total_amount")
       shinyjs::hide("infoBox_cppac_fr_total_all")
       shinyjs::hide("infoBox_cppac_fr_total_diff_rasch")
       shinyjs::hide("infoBox_cppac_fr_total_amount_rasch")
       shinyjs::hide("infoBox_cppac_fr_total_all_rasch")
       shinyjs::hide("report_fr_cppac_html")
       shinyjs::hide("report_fr_cppac_pdf")
       
       # Hidding DPPAC-EN elements
       shinyjs::hide("get_dppac_summary_en")
       shinyjs::hide("PROactive_scores_dppac_summary_en")
       shinyjs::hide("infoBox_dppac_en_total_diff")
       shinyjs::hide("infoBox_dppac_en_total_amount")
       shinyjs::hide("infoBox_dppac_en_total_all")
       shinyjs::hide("infoBox_dppac_en_total_diff_rasch")
       shinyjs::hide("infoBox_dppac_en_total_amount_rasch")
       shinyjs::hide("infoBox_dppac_en_total_all_rasch")
       shinyjs::hide("report_en_dppac_html")
       shinyjs::hide("report_en_dppac_pdf")
       
       # Hidding DPPAC-FR elements
       shinyjs::hide("get_dppac_summary_fr")
       shinyjs::hide("PROactive_scores_dppac_summary_fr")
       shinyjs::hide("infoBox_dppac_fr_total_diff")
       shinyjs::hide("infoBox_dppac_fr_total_amount")
       shinyjs::hide("infoBox_dppac_fr_total_all")
       shinyjs::hide("infoBox_dppac_fr_total_diff_rasch")
       shinyjs::hide("infoBox_dppac_fr_total_amount_rasch")
       shinyjs::hide("infoBox_dppac_fr_total_all_rasch")
       shinyjs::hide("report_fr_dppac_html") 
       shinyjs::hide("report_fr_dppac_pdf") 
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
        utils::write.csv2(results_list()$results_by_day$df_all_metrics, file, row.names = FALSE)
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
      Moreover, despite the fact that the app allows to fill a PROactive questionnaire, no results summaries will be provided."
    }
    )
    
    shinyjs::hide("box-no-valid-days")
    observe({
      if(results_summary_medians()$valid_days == 0 && results_summary_means()$valid_days == 0) {
        shinyjs::show("box-no-valid-days")
      } else {
        shinyjs::hide("box-no-valid-days")
      }
    })
  
  ####################
  # Generating reports ----
  ####################
  
    # Initializing the list of accumulation metrics for the long reports
      list_accum_metrics <- reactiveValues(
        mean_breaks_sed = NA,
        p_breaks_sed = NA,
        p_alpha_sed = NA,
        p_MBD_sed = NA,
        p_UBD_sed = NA,
        p_gini_sed = NA,
        mean_breaks_pa = NA,
        p_breaks_pa = NA,
        p_alpha_pa = NA,
        p_MBD_pa = NA,
        p_UBD_pa = NA,
        p_gini_pa = NA
      )
    
    # Updating the list of accumulation metrics for the long reports (NA is returned for non 60-s epoch analysis)
      observeEvent(results_summary_means(), {
        if(results_summary_means()$valid_days >=1 && (as.numeric(results_list()$df_with_computed_metrics$time[2] - results_list()$df_with_computed_metrics$time[1]) == 60)) {
        list_accum_metrics$mean_breaks_sed <- metrics_accum_sed()$metrics$mean_breaks
        list_accum_metrics$p_breaks_sed <- metrics_accum_sed()$p_breaks
        list_accum_metrics$p_alpha_sed <- metrics_accum_sed()$p_alpha
        list_accum_metrics$p_MBD_sed <- metrics_accum_sed()$p_MBD
        list_accum_metrics$p_UBD_sed <- metrics_accum_sed()$p_UBD
        list_accum_metrics$p_gini_sed <- metrics_accum_sed()$p_gini
        
        list_accum_metrics$mean_breaks_pa <- metrics_accum_pa()$metrics$mean_breaks
        list_accum_metrics$p_breaks_pa <- metrics_accum_pa()$p_breaks
        list_accum_metrics$p_alpha_pa <- metrics_accum_pa()$p_alpha
        list_accum_metrics$p_MBD_pa <- metrics_accum_pa()$p_MBD
        list_accum_metrics$p_UBD_pa <- metrics_accum_pa()$p_UBD
        list_accum_metrics$p_gini_pa <- metrics_accum_pa()$p_gini
        } else {
        list_accum_metrics$mean_breaks_sed <- NA
        list_accum_metrics$p_breaks_sed <- NA
        list_accum_metrics$p_alpha_sed <- NA
        list_accum_metrics$p_MBD_sed <- NA
        list_accum_metrics$p_UBD_sed <- NA
        list_accum_metrics$p_gini_sed <- NA
        list_accum_metrics$mean_breaks_pa <- NA
        list_accum_metrics$p_breaks_pa <- NA
        list_accum_metrics$p_alpha_pa <- NA
        list_accum_metrics$p_MBD_pa <-NA
        list_accum_metrics$p_UBD_pa <- NA
        list_accum_metrics$p_gini_pa <- NA
        }
      })
    
    # Generating general report EN LONG (HTML) ======================================================================================
    output$report_en_long <- downloadHandler(
      
      
      filename = "long_report.html",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_en_long.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_en_long.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            graph_int_distri_bands = results_list()$results_by_day$p_band,
            graph_int_distri_models = results_list()$results_by_day$p_log,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            df_with_computed_metrics = results_list()$df_with_computed_metrics,
            
            mean_breaks_sed = list_accum_metrics$mean_breaks_sed,
            p_breaks_sed    = list_accum_metrics$p_breaks_sed,
            p_alpha_sed     = list_accum_metrics$p_alpha_sed,
            p_MBD_sed       = list_accum_metrics$p_MBD_sed,
            p_UBD_sed       = list_accum_metrics$p_UBD_sed,
            p_gini_sed      = list_accum_metrics$p_gini_sed,
            mean_breaks_pa = list_accum_metrics$mean_breaks_pa,
            p_breaks_pa     = list_accum_metrics$p_breaks_pa,
            p_alpha_pa      = list_accum_metrics$p_alpha_pa,
            p_MBD_pa        = list_accum_metrics$p_MBD_pa,
            p_UBD_pa        = list_accum_metrics$p_UBD_pa,
            p_gini_pa       = list_accum_metrics$p_gini_pa,
 
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
    
  # Generating general report EN SHORT (PDF) ======================================================================================
    output$report_en_short <- downloadHandler(
    
    
    filename = "short_report.pdf",
    content = function(file) {
      
      
      withProgress(message = 'Please wait...', {
        
        report <- system.file("report", "report_en_short.Rmd", package = "activAnalyzer")

        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
        tempReport <- file.path(tempdir(), "report_en_short.Rmd")
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
          results_by_day = results_list()$results_by_day$df_all_metrics,
          graph_int_distri_bands = results_list()$results_by_day$p_band,
          graph_int_distri_models = results_list()$results_by_day$p_log,
          results_summary_means =  results_summary_means(),
          results_summary_medians =  results_summary_medians(),
          df_with_computed_metrics = results_list()$df_with_computed_metrics,
          
          mean_breaks_sed = list_accum_metrics$mean_breaks_sed,
          p_breaks_sed    = list_accum_metrics$p_breaks_sed,
          p_alpha_sed     = list_accum_metrics$p_alpha_sed,
          p_MBD_sed       = list_accum_metrics$p_MBD_sed,
          p_UBD_sed       = list_accum_metrics$p_UBD_sed,
          p_gini_sed      = list_accum_metrics$p_gini_sed,
          mean_breaks_pa = list_accum_metrics$mean_breaks_pa,
          p_breaks_pa     = list_accum_metrics$p_breaks_pa,
          p_alpha_pa      = list_accum_metrics$p_alpha_pa,
          p_MBD_pa        = list_accum_metrics$p_MBD_pa,
          p_UBD_pa        = list_accum_metrics$p_UBD_pa,
          p_gini_pa       = list_accum_metrics$p_gini_pa,
          
          
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
  
    # Generating general report FR LONG (HTML) ======================================================================================
    output$report_fr_long <- downloadHandler(
      
      
      filename = "long_report.html",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_fr_long.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_fr_long.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            graph_int_distri_bands = results_list()$results_by_day$p_band,
            graph_int_distri_models = results_list()$results_by_day$p_log,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            df_with_computed_metrics = results_list()$df_with_computed_metrics,
            
            mean_breaks_sed = list_accum_metrics$mean_breaks_sed,
            p_breaks_sed    = list_accum_metrics$p_breaks_sed,
            p_alpha_sed     = list_accum_metrics$p_alpha_sed,
            p_MBD_sed       = list_accum_metrics$p_MBD_sed,
            p_UBD_sed       = list_accum_metrics$p_UBD_sed,
            p_gini_sed      = list_accum_metrics$p_gini_sed,
            mean_breaks_pa = list_accum_metrics$mean_breaks_pa,
            p_breaks_pa     = list_accum_metrics$p_breaks_pa,
            p_alpha_pa      = list_accum_metrics$p_alpha_pa,
            p_MBD_pa        = list_accum_metrics$p_MBD_pa,
            p_UBD_pa        = list_accum_metrics$p_UBD_pa,
            p_gini_pa       = list_accum_metrics$p_gini_pa,
            
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
    
  # Generating general report FR SHORT (PDF) ======================================================================================
    output$report_fr_short <- downloadHandler(
    
      
    filename = "short_report.pdf",
    content = function(file) {
      
      
      withProgress(message = 'Please wait...', {
        
        report <- system.file("report", "report_fr_short.Rmd", package = "activAnalyzer")
        
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
        tempReport <- file.path(tempdir(), "report_fr_short.Rmd")
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
          results_by_day = results_list()$results_by_day$df_all_metrics,
          graph_int_distri_bands = results_list()$results_by_day$p_band,
          graph_int_distri_models = results_list()$results_by_day$p_log,
          results_summary_means =  results_summary_means(),
          results_summary_medians =  results_summary_medians(),
          df_with_computed_metrics = results_list()$df_with_computed_metrics,
          
          mean_breaks_sed = list_accum_metrics$mean_breaks_sed,
          p_breaks_sed    = list_accum_metrics$p_breaks_sed,
          p_alpha_sed     = list_accum_metrics$p_alpha_sed,
          p_MBD_sed       = list_accum_metrics$p_MBD_sed,
          p_UBD_sed       = list_accum_metrics$p_UBD_sed,
          p_gini_sed      = list_accum_metrics$p_gini_sed,
          mean_breaks_pa = list_accum_metrics$mean_breaks_pa,
          p_breaks_pa     = list_accum_metrics$p_breaks_pa,
          p_alpha_pa      = list_accum_metrics$p_alpha_pa,
          p_MBD_pa        = list_accum_metrics$p_MBD_pa,
          p_UBD_pa        = list_accum_metrics$p_UBD_pa,
          p_gini_pa       = list_accum_metrics$p_gini_pa,
          
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
    
  # Generating C-PPAC report EN ======================================================================================
    
      output$report_en_cppac_html <- downloadHandler(
        
        
        filename = "report_cppac.html",
        content = function(file) {
          
          
          withProgress(message = 'Please wait...', {
            
            report <- system.file("report", "report_en_cppac_html.Rmd", package = "activAnalyzer")
            
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
            tempReport <- file.path(tempdir(), "report_en_cppac_html.Rmd")
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
              results_by_day = results_list()$results_by_day$df_all_metrics,
              results_summary_means =  results_summary_means(),
              results_summary_medians =  results_summary_medians(),
              cppac_table = tab_cppac_summary_en(),
              cppac_diff_raw = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE),
              cppac_amount_raw = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE),
              cppac_total_raw = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE) + sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE),
              cppac_diff_rasch = rasch_transform(x = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE), quest = "C-PPAC", score = "difficulty"),
              cppac_amount_rasch = rasch_transform(x = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), quest = "C-PPAC", score = "quantity"),
              cppac_total_rasch = round((rasch_transform(x = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                                           rasch_transform(x = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1),
              
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
      
      
    output$report_en_cppac_pdf <- downloadHandler(
      
      
      filename = "report_cppac.pdf",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_en_cppac_pdf.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_en_cppac_pdf.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            cppac_table = tab_cppac_summary_en(),
            cppac_diff_raw = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE),
            cppac_amount_raw = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE),
            cppac_total_raw = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE) + sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE),
            cppac_diff_rasch = rasch_transform(x = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE), quest = "C-PPAC", score = "difficulty"),
            cppac_amount_rasch = rasch_transform(x = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), quest = "C-PPAC", score = "quantity"),
            cppac_total_rasch = round((rasch_transform(x = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
            rasch_transform(x = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1),
            
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
    
    # Generating C-PPAC report FR ======================================================================================
    
    output$report_fr_cppac_html <- downloadHandler(
      
      
      filename = "rapport_cppac.html",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_fr_cppac_html.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_fr_cppac_html.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            cppac_table = tab_cppac_summary_fr(),
            cppac_diff_raw = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE),
            cppac_amount_raw = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE),
            cppac_total_raw = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE) + sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE),
            cppac_diff_rasch = rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "difficulty"),
            cppac_amount_rasch = rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "quantity"),
            cppac_total_rasch = round((rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                                         rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1),
            
            
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
    
    
    output$report_fr_cppac_pdf <- downloadHandler(
      
      
      filename = "rapport_cppac.pdf",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_fr_cppac_pdf.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_fr_cppac_pdf.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            cppac_table = tab_cppac_summary_fr(),
            cppac_diff_raw = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE),
            cppac_amount_raw = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE),
            cppac_total_raw = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE) + sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE),
            cppac_diff_rasch = rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "difficulty"),
            cppac_amount_rasch = rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "quantity"),
            cppac_total_rasch = round((rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                                         rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1),
          
            
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
  
    
    # Generating D-PPAC report EN ======================================================================================
    
    output$report_en_dppac_html <- downloadHandler(
      
      
      filename = "report_dppac.html",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_en_dppac_html.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_en_dppac_html.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            dppac_table = tab_dppac_summary_en(),
            dppac_diff_raw = round(recap_dppac_en()$mean_difficulty_score_raw[1], 1),
            dppac_amount_raw =round(recap_dppac_en()$mean_amount_score_raw[1], 1),
            dppac_total_raw = round(recap_dppac_en()$mean_total_score_raw[1], 1),
            dppac_diff_rasch = round(recap_dppac_en()$mean_difficulty_score_rasch[1], 1),
            dppac_amount_rasch = round(recap_dppac_en()$mean_amount_score_rasch[1], 1),
            dppac_total_rasch = round(recap_dppac_en()$mean_total_score_rasch[1], 1),
            
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
    
    
    output$report_en_dppac_pdf <- downloadHandler(
      
      
      filename = "report_dppac.pdf",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_en_dppac_pdf.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_en_dppac_pdf.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            dppac_table = tab_dppac_summary_en(),
            dppac_diff_raw = round(recap_dppac_en()$mean_difficulty_score_raw[1], 1),
            dppac_amount_raw =round(recap_dppac_en()$mean_amount_score_raw[1], 1),
            dppac_total_raw = round(recap_dppac_en()$mean_total_score_raw[1], 1),
            dppac_diff_rasch = round(recap_dppac_en()$mean_difficulty_score_rasch[1], 1),
            dppac_amount_rasch = round(recap_dppac_en()$mean_amount_score_rasch[1], 1),
            dppac_total_rasch = round(recap_dppac_en()$mean_total_score_rasch[1], 1),
            
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
    
    # Generating D-PPAC report FR ======================================================================================
    
    output$report_fr_dppac_html <- downloadHandler(
      
      
      filename = "rapport_dppac.html",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_fr_dppac_html.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_fr_dppac_html.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            dppac_table = tab_dppac_summary_fr(),
            dppac_diff_raw = round(recap_dppac_fr()$mean_difficulty_score_raw[1], 1),
            dppac_amount_raw =round(recap_dppac_fr()$mean_amount_score_raw[1], 1),
            dppac_total_raw = round(recap_dppac_fr()$mean_total_score_raw[1], 1),
            dppac_diff_rasch = round(recap_dppac_fr()$mean_difficulty_score_rasch[1], 1),
            dppac_amount_rasch = round(recap_dppac_fr()$mean_amount_score_rasch[1], 1),
            dppac_total_rasch = round(recap_dppac_fr()$mean_total_score_rasch[1], 1),
            
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
    
    
    output$report_fr_dppac_pdf <- downloadHandler(
      
      
      filename = "rapport_dppac.pdf",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          report <- system.file("report", "report_fr_dppac_pdf.Rmd", package = "activAnalyzer")
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed). Code retrieved from https://shiny.rstudio.com/articles/generating-reports.html.
          tempReport <- file.path(tempdir(), "report_fr_dppac_pdf.Rmd")
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
            results_by_day = results_list()$results_by_day$df_all_metrics,
            results_summary_means =  results_summary_means(),
            results_summary_medians =  results_summary_medians(),
            dppac_table = tab_dppac_summary_fr(),
            dppac_diff_raw = round(recap_dppac_fr()$mean_difficulty_score_raw[1], 1),
            dppac_amount_raw =round(recap_dppac_fr()$mean_amount_score_raw[1], 1),
            dppac_total_raw = round(recap_dppac_fr()$mean_total_score_raw[1], 1),
            dppac_diff_rasch = round(recap_dppac_fr()$mean_difficulty_score_rasch[1], 1),
            dppac_amount_rasch = round(recap_dppac_fr()$mean_amount_score_rasch[1], 1),
            dppac_total_rasch = round(recap_dppac_fr()$mean_total_score_rasch[1], 1),
            
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
      "Are you sure you want to reset the app\u003f",
      title = "Reset app",
      footer = tagList(
        actionButton("cancel", "Cancel"),
        actionButton("ok", "Reset", class = "btn btn-danger", style="color: #fff; background-color: red; border-color: red")
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
    
  # Exporting inputs for patient information
    observeEvent(input$auto_fill_char, {
      shiny::exportTestValues(assessor_name = input$assessor_name)
      shiny::exportTestValues(assessor_surname = input$assessor_surname)
      shiny::exportTestValues(patient_name = input$patient_name)
      shiny::exportTestValues(patient_surname = input$patient_surname)
      shiny::exportTestValues(age = input$age)
      shiny::exportTestValues(sex = input$sex)
      shiny::exportTestValues(weight = input$weight)
      shiny::exportTestValues(side = input$side)
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

  # Exporting dataframe marked for wear time
    observeEvent(input$validate, {
      shiny::exportTestValues(df = df())
    })
    
  # Exporting plot showing nonwear/wear time ("Validate configuration" button)
    observeEvent(input$validate, {
      shiny::exportTestValues(gg_plot_data_init = plot_data(
        data = df(), 
        metric = input$Metric,
        zoom_from = input$zoom_from_weartime,
        zoom_to = input$zoom_to_weartime
        )
      )
    })
    
  # Exporting plot showing nonwear/wear time ("Update graphic" button)
    observeEvent(input$update_graphic, {
      shiny::exportTestValues(gg_plot_data_update = plot_data(
        data = df(), 
        metric = input$Metric,
        zoom_from = input$zoom_from_weartime,
        zoom_to = input$zoom_to_weartime
        )
    )
    })
    
  # Exporting inputs for missing PA infos ("Run analysis" button)
    observeEvent(input$Run, {
     shiny::exportTestValues(
       recap_pa_perdiods = recap_pa_perdiods()
     )
   })
    
  # Exporting plot showing physical activity intensity marks ("Run" button)
    observeEvent(input$Run, {
      shiny::exportTestValues(gg_plot_data_int_init = plot_data_with_intensity(
        data = results_list()$df_with_computed_metrics, 
        metric = input$Metric2,
        zoom_from = input$zoom_from_analysis,
        zoom_to = input$zoom_to_analysis
       )
      )
    })
    
   # Exporting plot showing physical activity intensity marks ("Update graphic" button)
    observeEvent(input$update_graphic2, {
      shiny::exportTestValues(gg_plot_data_int_update = plot_data_with_intensity(
        data = results_list()$df_with_computed_metrics, 
        metric = input$Metric2,
        zoom_from = input$zoom_from_analysis,
        zoom_to = input$zoom_to_analysis
      )
      )
    })
    
  # Exporting dataframe for the results by day
    observeEvent(input$Run, {
      shiny::exportTestValues(results_by_day = results_list()$results_by_day$df_all_metrics)
    })
    
  # Exporting dataframe for the daily means
    observeEvent(input$Run, {
      shiny::exportTestValues(results_summary_means = results_summary_means())
    })
    
  # Exporting dataframe for the daily medians
    observeEvent(input$Run, {
      shiny::exportTestValues(results_summary_medians = results_summary_medians())
    })

  # Exporting inputs after setting activity intensity analysis
    observeEvent(input$auto_fill_intensity, {
      shiny::exportTestValues(equation_mets = input$equation_mets)
      shiny::exportTestValues(sed_cutpoint = input$sed_cutpoint)
      shiny::exportTestValues(mvpa_cutpoint = input$mvpa_cutpoint)
    })
    
  # Exporting inputs after setting default configuration to validate a day (period and number of wear time hours)
    observeEvent(input$reset_period, {
      shiny::exportTestValues(start_day_analysis = input$start_day_analysis)
      shiny::exportTestValues(end_day_analysis = input$end_day_analysis)
      shiny::exportTestValues(minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis)
    })
    
  # Exporting inputs after setting proactive (non-sleep wearing protocol) inputs for data analysis
    observeEvent(input$pro_active_period_non_sleep, {
      shiny::exportTestValues(start_day_analysis = input$start_day_analysis)
      shiny::exportTestValues(end_day_analysis = input$end_day_analysis)
      shiny::exportTestValues(minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis)
    })
    
  # Exporting inputs after setting proactive (24-h wearing protocol) inputs for data analysis
    observeEvent(input$pro_active_period_24h, {
      shiny::exportTestValues(start_day_analysis = input$start_day_analysis)
      shiny::exportTestValues(end_day_analysis = input$end_day_analysis)
      shiny::exportTestValues(minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis)
    })

  # Exporting BMR
    observeEvent(input$Run, {
      shiny::exportTestValues(BMR = bmr_kcal_d())
    })
    
  # Exporting PROactive accelerometer scores for C-PPAC
    observeEvent(input$get_cppac_summary_en, {
      shiny::exportTestValues(score_cppac_diff_en = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE))
      shiny::exportTestValues(score_cppac_quant_en = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE))
      shiny::exportTestValues(score_cppac_tot_rasch_en = round((rasch_transform(x = sum(tab_cppac_summary_en()$"Difficulty score", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                                                               rasch_transform(x = sum(tab_cppac_summary_en()$"Amount score", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1))    })
    
    observeEvent(input$get_cppac_summary_fr, {
      shiny::exportTestValues(score_cppac_diff_fr = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE))
      shiny::exportTestValues(score_cppac_quant_fr = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE))
      shiny::exportTestValues(score_cppac_tot_rasch_fr = round((rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de difficult\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "difficulty") +
                                                                  rasch_transform(x = sum(tab_cppac_summary_fr()$"Score de quantit\u00e9", na.rm = TRUE), quest = "C-PPAC", score = "quantity")) / 2, 1))                         
      })
    
  # Exporting PROactive accelerometer scores for D-PPAC
    observeEvent(input$get_dppac_summary_en, {
      shiny::exportTestValues(score_dppac_diff_en = recap_dppac_en()$mean_difficulty_score_raw[1])
      shiny::exportTestValues(score_dppac_quant_en = recap_dppac_en()$mean_amount_score_raw[1])
      shiny::exportTestValues(score_dppac_tot_rasch_en = round(recap_dppac_en()$mean_total_score_rasch[1], 1))
      })
    
    observeEvent(input$get_dppac_summary_fr, {
      shiny::exportTestValues(score_dppac_diff_fr = recap_dppac_fr()$mean_difficulty_score_raw[1])
      shiny::exportTestValues(score_dppac_quant_fr = recap_dppac_fr()$mean_amount_score_raw[1])
      shiny::exportTestValues(score_dppac_tot_rasch_fr = round(recap_dppac_fr()$mean_total_score_rasch[1], 1))
    })
    
      
}


