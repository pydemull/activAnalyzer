options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  
  ##############################
  # Uploading and preparing data
  ##############################
  
  # Getting data file without modification (required for extracting attributes information when generating the report)
    file <- reactive({
      
      req(input$upload)
      read_agd(input$upload$datapath)
      
    })
  
  # Getting reactive dataset
    data <- reactive({
        
        req(input$upload)
        prepare_dataset(data = input$upload$datapath)

      })

  ###########################################################################################
  # Getting dataframe with marks for wear/nonwear time when clicking on the "Validate" button
  ###########################################################################################
    
  # Controlling for correct inputs
  
    # File input
      observeEvent(input$validate,
                 shinyFeedback::feedbackWarning(
                   "upload", 
                   ((tools::file_ext(input$upload$name) == "agd") == FALSE),
                   "Invalid file format. Please choose a .agd file."
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
    
    # Building reactive dataframe marked for nonwear/wear time
      
        df <- eventReactive(input$validate, {
    
        # Waiting for required conditions 
          req(tools::file_ext(input$upload$name) == "agd" & 
              is.numeric(input$frame_size) & 
              input$frame_size >= 0 & 
              is.numeric(input$allowanceFrame_size) & 
              input$allowanceFrame_size >= 0)
         
        # Setting the axis to be used for detecting nonwear time
          if (input$axis_weartime == "vector magnitude") {  
            cts <- "vm"
          } else {
            cts <- "axis1"
          }
        
        # Creating reactive dataframe
          df <- mark_weartime(dataset = data(),
                              cts = cts, 
                              frame = input$frame_size, 
                              allowanceFrame = input$allowanceFrame_size)
          return(df)
         
         })
  
  # Returning to default values for the wear time detection algorithm
    observeEvent(input$reset_nonwear, {
      axis_weartime <- c("vector magnitude", "vertical axis")
      updateSelectInput(inputId = "axis_weartime", choices = axis_weartime)
      updateNumericInput(inputId = "frame_size", value = 90)
      updateNumericInput(inputId = "allowanceFrame_size", value = 2)
    })
  
 
  ########################################
  # Visualizing all data with nonwear time
  ########################################
  
  output$graph <- renderPlot({
     
    # Setting metric to visualize  
      max_metric <- max(df()[input$Metric], na.rm = TRUE)
      
    # Plotting data
      ggplot(data = df(), aes(x = time)) +
        geom_ribbon(aes(ymin = 0, ymax = non_wearing_count / non_wearing_count * max_metric, fill = "Nonwear time"), alpha = 0.5) +
        geom_line(aes_string(y = input$Metric)) +
        scale_x_time(breaks = hms::hms(seq(0, 24*3600, 6*3600))) +
        scale_fill_manual(values = "red") +
        labs(x = "Time (hh:mm:ss)", y = paste(input$Metric), fill = "") +
        theme_bw() +
        theme(legend.position = "bottom") + 
        facet_wrap(.~ date)
    
  })
  
  ###################################################
  # Getting results when clicking on the "Run" button
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
                       (input$sed_cutpoints == "..."),
                       "Please choose a value for the SED cut-point."
                     )
        )
        
        # SED cut-points
          observeEvent(input$Run,
                     shinyFeedback::feedbackWarning(
                       "mvpa_cutpoints", 
                       (input$sed_cutpoints == "..."),
                        "Please choose values for the MPVA cut-points."
                       )
                     )

        
        

  # Showing the table presenting the studies that validated METs equations
    output$table_equations <- renderReactable({
      
      if(input$equation_mets == "...") {NULL
        } else {
        read_csv2("data/equations_mets.csv") %>% 
        filter(Study == input$equation_mets) %>%
        reactable(striped = TRUE,
                  list(Study = colDef(minWidth = 80),
                       'Population' = colDef(minWidth = 60),
                       'Activities performed' = colDef(minWidth = 60),
                       'Device used' = colDef(minWidth = 40),
                       'Axis used' = colDef(minWidth = 40),
                       'Filter enabled' = colDef(minWidth = 40))
        )
        }
      
    })
    
  # Switching to show appropriate choices for SED cut-points
    observeEvent(input$sed_cutpoints, {
      updateTabsetPanel(inputId = "switcher_sed", selected = input$sed_cutpoints)
    })
    
    
  # Showing the table presenting the studies that validated SED cut-points
    output$table_sed_cutpoints <- renderReactable({
      
      if(input$sed_cutpoints == "...") {NULL
      } else {
        read_csv2("data/sed_cutpoints.csv") %>% 
          filter(Study == input$sed_cutpoints) %>%
          reactable(striped = TRUE,
                    list(Study = colDef(minWidth = 80),
                         'Population' = colDef(minWidth = 70),
                         'Activities performed' = colDef(minWidth = 60),
                         'Device used' = colDef(minWidth = 40),
                         'Axis used' = colDef(minWidth = 30),
                         'Filter enabled' = colDef(minWidth = 40),
                         'SED cut-point in counts/min' = colDef(minWidth = 60))
          )
      }
      
    })
    
  # Switching to show appropriate choices for MVPA cut-points
    observeEvent(input$mvpa_cutpoints, {
      updateTabsetPanel(inputId = "switcher_mvpa", selected = input$mvpa_cutpoints)
    })
    
    
  # Showing the table presenting the studies that validated MVPA cut-points
    output$table_mvpa_cutpoints_sasaki <- renderReactable({
      
      if(input$mvpa_cutpoints == "...") {NULL
      } else {
        read_csv2("data/mvpa_cutpoints.csv") %>% 
          filter(Study == input$mvpa_cutpoints) %>%
          reactable(striped = TRUE,
                    list(Study = colDef(minWidth = 80),
                         'Population' = colDef(minWidth = 70),
                         'Activities performed' = colDef(minWidth = 60),
                         'Device used' = colDef(minWidth = 40),
                         'Axis used' = colDef(minWidth = 30),
                         'Filter enabled' = colDef(minWidth = 40),
                         'MPA cut-point (3 METs) in counts/min' = colDef(minWidth = 60),
                         'VPA cut-point (6 METs) in counts/min' = colDef(minWidth = 60))
          )
      }
      
    })
    
    output$table_mvpa_cutpoints_santos_adults <- renderReactable({
      
      if(input$mvpa_cutpoints == "...") {NULL
      } else {
        read_csv2("data/mvpa_cutpoints.csv") %>% 
          filter(Study == input$mvpa_cutpoints) %>%
          reactable(striped = TRUE,
                    list(Study = colDef(minWidth = 80),
                         'Population' = colDef(minWidth = 70),
                         'Activities performed' = colDef(minWidth = 60),
                         'Device used' = colDef(minWidth = 40),
                         'Axis used' = colDef(minWidth = 30),
                         'Filter enabled' = colDef(minWidth = 40),
                         'MPA cut-point (3 METs) in counts/min' = colDef(minWidth = 60),
                         'VPA cut-point (6 METs) in counts/min' = colDef(minWidth = 60))
          )
      }
      
    })
    
    output$table_mvpa_cutpoints_santos_older <- renderReactable({
      
      if(input$mvpa_cutpoints == "...") {NULL
      } else {
        read_csv2("data/mvpa_cutpoints.csv") %>% 
          filter(Study == input$mvpa_cutpoints) %>%
          reactable(striped = TRUE,
                    list(Study = colDef(minWidth = 80),
                         'Population' = colDef(minWidth = 70),
                         'Activities performed' = colDef(minWidth = 60),
                         'Device used' = colDef(minWidth = 40),
                         'Axis used' = colDef(minWidth = 30),
                         'Filter enabled' = colDef(minWidth = 40),
                         'MPA cut-point (3 METs) in counts/min' = colDef(minWidth = 60),
                         'VPA cut-point (6 METs) in counts/min' = colDef(minWidth = 60))
          )
      }
      
    })
      
  
    # Getting BMR (kcal/d)
      bmr_kcal_d <- eventReactive(input$Run, {
        
        compute_bmr(age = input$age, sex = input$sex, weight = input$weight)
        
      })
      
    
  # Getting results by day corresponding to valid wear time  (except for total kcal 
  # that also uses nonwear time with attribution of bmr to nonwear epochs)
    
    results_by_day <- eventReactive(input$Run, {
      
      # Waiting for valid inputs

        if (!input$sex %in% c("male", "female", "undefined") | input$age <= 0 | input$weight <= 0) {
          validate("Please provide valid values for the inputs shown in Patient's information section.")
        }
      
        if (input$equation_mets == "...") {
          validate("Please choose a MET equation.")
        }
      
        if (input$sed_cutpoints == "..." | input$mvpa_cutpoints == "...") {
          validate("Please provide values for the cut-points.")
        }
        
        if (input$perso_sed_axis != input$perso_mvpa_axis) {
          validate("Please use the same axis for both SED and MVPA cut-points.")
        }
      
        if (input$sed_cutpoints == "Aguilar-Farias et al. (2014) [Older adults]" && 
            input$perso_mvpa_axis == "vertical axis") {
          validate("Please use the same axis for both SED and MVPA cut-points.")
        }
      
        if (input$perso_sed_axis == "vertical axis" &&
            input$mvpa_cutpoints %in% c("Sasaki et al. (2011) [Adults]", 
                                        "Santos-Lozano et al. (2013) [Adults]", 
                                        "Santos-Lozano et al. (2013) [Older adults]")) {
          validate("Please use the same axis for both SED and MVPA cut-points.")
        }
      
      

      # Setting axis and cut-points to compute SED and MVPA times
      
        # SED
          if(input$sed_cutpoints == "Aguilar-Farias et al. (2014) [Older adults]") { 
            axis_sed_chosen <- df()$vm
            axis_sed_chosen_name <<- "vector magnitude"
            sed_cutpoint_chosen <<- 200
          } else if (input$sed_cutpoints == "Personalized...") {
               if(input$perso_sed_axis == "vector magnitude") {
                 axis_sed_chosen <- df()$vm
                 axis_sed_chosen_name <<- "vector magnitude"
                 sed_cutpoint_chosen <<- input$perso_sed_cutpoint
               } else {
                 axis_sed_chosen <- df()$axis1 
                 axis_sed_chosen_name <<- "vertical axis"
                 sed_cutpoint_chosen <<- input$perso_sed_cutpoint
               }
          } else {
            NULL}
      
        # MVPA
          if(input$mvpa_cutpoints == "Sasaki et al. (2011) [Adults]") { 
            axis_mvpa_chosen <- df()$vm
            axis_mvpa_chosen_name <<- "vector magnitude"
            mpa_cutpoint_chosen <<- 2690
            vpa_cutpoint_chosen <<- 6167
          } else if (input$mvpa_cutpoints == "Santos-Lozano et al. (2013) [Adults]"){
            axis_mvpa_chosen <- df()$vm
            axis_mvpa_chosen_name <<- "vector magnitude"
            mpa_cutpoint_chosen <<- 3208 
            vpa_cutpoint_chosen <<- 8565 
          } else if (input$mvpa_cutpoints == "Santos-Lozano et al. (2013) [Older adults]"){
            axis_mvpa_chosen <- df()$vm
            axis_mvpa_chosen_name <<- "vector magnitude"
            mpa_cutpoint_chosen <<- 2751 
            vpa_cutpoint_chosen <<- 9359  
          } else if (input$mvpa_cutpoints == "Personalized...") {
            if(input$perso_mvpa_axis == "vector magnitude") {
              axis_mvpa_chosen <- df()$vm
              axis_mvpa_chosen_name <<- "vector magnitude"
              mpa_cutpoint_chosen <<- input$perso_mpa_cutpoint
              vpa_cutpoint_chosen <<- input$perso_vpa_cutpoint
            } else {
              axis_mvpa_chosen <- df()$axis1
              axis_mvpa_chosen_name <<- "vertical axis"
              mpa_cutpoint_chosen <<- input$perso_mpa_cutpoint
              vpa_cutpoint_chosen <<- input$perso_vpa_cutpoint
            }
          } else {
            NULL}

      
      # Adding variables of interest to the initial dataframe
        df_with_computed_metrics <-
          df() %>%
          mark_intensity(axis = axis_mvpa_chosen, 
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
         
         return(results_by_day)
       
       })

       # Showing results by day in a table
         output$results_by_day <- renderReactable({
           Sys.sleep(0.5)
           reactable(results_by_day(),  
                     striped = TRUE,
                     list(total_counts_axis1 = colDef(minWidth = 150),
                          total_counts_vm = colDef(minWidth = 150),
                          minutes_SED = colDef(minWidth = 120),
                          minutes_LPA = colDef(minWidth = 120),
                          minutes_MPA = colDef(minWidth = 120),
                          minutes_VPA = colDef(minWidth = 120),
                          minutes_MVPA = colDef(minWidth = 120),
                          percent_SED = colDef(minWidth = 120),
                          percent_LPA = colDef(minWidth = 120),
                          percent_MPA = colDef(minWidth = 120),
                          percent_VPA = colDef(minWidth = 120),
                          percent_MVPA = colDef(minWidth = 120),
                          ratio_mvpa_sed = colDef(minWidth = 125),
                          total_kcal_wear_time = colDef(minWidth = 160),
                          mets_hours_mvpa = colDef(minWidth = 160)))
         })
       
  
  # Getting results averaged on valid days
    results_summary <- eventReactive(input$Run, {
      

      # Computing results averaged on valid days
        results_by_day() %>%
          average_results(minimum_wear_time = input$minimum_wear_time_for_analysis)
        })
    
      # Showing results averaged on valid days in a table
        output$results_summary <- renderReactable({
          
          reactable(
            results_summary(), 
            list(valid_days = colDef(minWidth = 90),
                 wear_time = colDef(minWidth = 90),
                 total_counts_axis1 = colDef(minWidth = 150),
                 total_counts_vm = colDef(minWidth = 150),
                 minutes_SED = colDef(minWidth = 120),
                 minutes_LPA = colDef(minWidth = 120),
                 minutes_MPA = colDef(minWidth = 120),
                 minutes_VPA = colDef(minWidth = 120),
                 minutes_MVPA = colDef(minWidth = 120),
                 percent_SED = colDef(minWidth = 120),
                 percent_LPA = colDef(minWidth = 120),
                 percent_MPA = colDef(minWidth = 120),
                 percent_VPA = colDef(minWidth = 120),
                 percent_MVPA = colDef(minWidth = 120),
                 ratio_mvpa_sed = colDef(minWidth = 125),
                 total_kcal_wear_time = colDef(minWidth = 160),
                 mets_hours_mvpa = colDef(minWidth = 160)),
            striped = TRUE
          )
          
        })
    
  ###################
  # Exporting results
  ###################
    
  # Exporting results by day 
    output$ExpResultsByDays <- downloadHandler(
      filename = function() {
        paste0(input$upload, "_ResultsByDay.csv")
      },
      content = function(file) {
        write_csv2(results_by_day(), file)
      }
    )
  
  # Exporting daily summary  
    output$ExpDailySummary <- downloadHandler(
      filename = function() {
        paste0(input$upload, "_DailySummary.csv")
      },
      content = function(file) {
        write_csv2(results_summary(), file)
      }
    )
  
  #################
  # Generate report
  #################

    # Generating report EN
      output$report_en <- downloadHandler(
        
      
        filename = "report.pdf",
        content = function(file) {
          

          withProgress(message = 'Please wait...', {
            
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report_en.Rmd")
          file.copy("report_en.Rmd", tempReport, overwrite = TRUE)
          
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
            axis_weartime = input$axis_weartime,
            frame_size = input$frame_size,
            allowanceFrame_size = input$allowanceFrame_size,
            equation_mets = input$equation_mets,
            bmr_kcal_d = bmr_kcal_d(),
            axis_sed = axis_sed_chosen_name,
            axis_mvpa = axis_mvpa_chosen_name,
            sed_cutpoint = sed_cutpoint_chosen,
            mpa_cutpoint = mpa_cutpoint_chosen,
            vpa_cutpoint = vpa_cutpoint_chosen,
            minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis,
            results_by_day = results_by_day(),
            results_summary =  results_summary(),
            
            # Loading some data used in figures
            mvpa_lines = read_csv2("data/mvpa_lines.csv"),
            sed_lines = read_csv2("data/sed_lines.csv"),
            ratio_lines = read_csv2("data/ratio_lines.csv"),
            
            rendered_by_shiny = TRUE
          )
    
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          out <- rmarkdown::render(tempReport,
                                   params = params,
                                   envir = new.env(parent = globalenv())
          )
          out <- file.rename(out, file)
          
          })

      }
    )
    
    # Generating report EN
    output$report_fr <- downloadHandler(
      
      
      filename = "rapport.pdf",
      content = function(file) {
        
        
        withProgress(message = 'Please wait...', {
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), "report_fr.Rmd")
          file.copy("report_fr.Rmd", tempReport, overwrite = TRUE)
          
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
            axis_weartime = input$axis_weartime,
            frame_size = input$frame_size,
            allowanceFrame_size = input$allowanceFrame_size,
            equation_mets = input$equation_mets,
            bmr_kcal_d = bmr_kcal_d(),
            axis_sed = axis_sed_chosen_name,
            axis_mvpa = axis_mvpa_chosen_name,
            sed_cutpoint = sed_cutpoint_chosen,
            mpa_cutpoint = mpa_cutpoint_chosen,
            vpa_cutpoint = vpa_cutpoint_chosen,
            minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis,
            results_by_day = results_by_day(),
            results_summary =  results_summary(),
            
            # Loading some data used in figures
            mvpa_lines = read_csv2("data/mvpa_lines.csv"),
            sed_lines = read_csv2("data/sed_lines.csv"),
            ratio_lines = read_csv2("data/ratio_lines.csv"),
            
            rendered_by_shiny = TRUE
          )
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          out <- rmarkdown::render(tempReport,
                                   params = params,
                                   envir = new.env(parent = globalenv())
          )
          out <- file.rename(out, file)
          
        })
        
      }
    )
    

      
 ########### 
 # Reset app
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
  # Download user's guide
  #######################
    
    # English
      output$user_guide_en <- downloadHandler(
        filename = function() {
          paste0(input$upload, "activanalyzer_manual.pdf")
        },
        content = function(file) {
          file.copy("guide/user_guide_en.pdf", file)
        }
      )
    
    # French
      output$user_guide_fr <- downloadHandler(
        filename = function() {
          paste0(input$upload, "activanalyzer_manuel.pdf")
        },
        content = function(file) {
          file.copy("guide/user_guide_fr.pdf", file)
        }
      )
    
    
}
    


