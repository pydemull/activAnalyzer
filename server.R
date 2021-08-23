options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  
  ################
  # Uploading data
  ################
  
  # Getting reactive data
    data <- reactive({
      
      req(input$upload)
      prepare_dataset(data = input$upload$datapath)
      
    })
    
  ####################################################
  # Getting dataframe with marks for wear/nonwear time
  ####################################################
  
  df <- eventReactive(input$validate, {
    
     # Waiting for required conditions 
       req(tools::file_ext(input$upload$name) == "agd" & 
           is.numeric(input$frame_size) & 
           input$frame_size >= 0 & 
           is.numeric(input$allowanceFrame_size) & 
           input$allowanceFrame_size >= 0)
     
     # Creating reactive dataframe
       if (input$axis_weartime == "vector magnitude") {  
         
         df <- wearingMarking(dataset = data(), 
                              TS = "TimeStamp", 
                              cts = "vm", 
                              frame = input$frame_size, 
                              allowanceFrame = input$allowanceFrame_size) %>%
           mutate(non_wearing_count = ifelse(wearing == "nw", 1, 0),
                  wearing_count = ifelse(wearing == "w", 1, 0)) 
       } else {
         
         df <- wearingMarking(dataset = data(), 
                              TS = "TimeStamp", 
                              cts = "axis1", 
                              frame = input$frame_size, 
                              allowanceFrame = input$allowanceFrame_size) %>%
           mutate(non_wearing_count = ifelse(wearing == "nw", 1, 0),
                  wearing_count = ifelse(wearing == "w", 1, 0)) 
         
       }
       
       return(df)
      
      })
  
  
  # Controlling for correct inputs
  
    # File input
      observeEvent(input$validate,
                 shinyFeedback::feedbackWarning(
                   "upload", 
                   ((tools::file_ext(input$upload$name) == "agd") == FALSE),
                   "Invalid file format. Please choose a .agd file"
                 )
    )
      
    # Frame size
      observeEvent(input$validate,
                   shinyFeedback::feedbackWarning(
                     "frame_size", 
                     (is.numeric(input$frame_size) == FALSE | input$frame_size < 0),
                     "Please choose a number >= 0"
                   )
      )
    
    # Allowance frame size
      observeEvent(input$validate,
                   shinyFeedback::feedbackWarning(
                     "allowanceFrame_size", 
                     (is.numeric(input$allowanceFrame_size) == FALSE | input$allowanceFrame_size < 0),
                     "Please choose a number >= 0"
                   )
      )
      
      
 
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
  
  #########################################
  # Table showing metrics summarized by day
  #########################################
    
    # Controlling for correct inputs
     
      # Sex
        observeEvent(input$Run,
        shinyFeedback::feedbackWarning(
          "sex", 
          (input$sex %in% c("male", "female", "undefined")) == FALSE,
          "Please provide a value for sex"
          )
        )
      
      # Age
        observeEvent(input$Run,
        shinyFeedback::feedbackWarning(
          "age", 
          ((is.numeric(input$age) == FALSE | input$age <= 0)),
          "Please provide a value >0 for age"
          )
        )
      
      # Weight
        observeEvent(input$Run,
        shinyFeedback::feedbackWarning(
          "weight", 
          ((is.numeric(input$weight) == FALSE | input$weight <= 0)),
          "Please provide a value >0 for weight"
          )
        )
  
      
  # Getting BMR
    bmr_kcal_d <- eventReactive(input$Run, {
      
      # Computing BMR
        compute_bmr(age = input$age, sex = input$sex, weight = input$weight)
        
        })
  
  
  # Getting results by day corresponding to valid wear time  (except for total kcal 
  # that also uses nonwear time with attribution of bmr to nonwear epochs)
    
    results_by_day <- eventReactive(input$Run, ({
      
      if (!input$sex %in% c("male", "female", "undefined") | input$age <= 0 | input$weight <= 0) {
        validate("Please provide valid values for the inputs shown in Patient's information section.")
      }
      
      
      # Setting axis to compute METs
        if(input$axis == "vector magnitude") { axis_chosen <- df()$vm
        } else { axis_chosen <- df()$axis1 }
      
      # Computing BMR in kcal/min
        bmr_kcal_min <- bmr_kcal_d() / (24*60)
      
      # Adding variables of interest to the dataset
        df_with_computed_metrics <-
          df() %>%
          mutate(
            METS = compute_mets(data = .data, equation = input$equation_mets, weight = input$weight, gender = input$sex),
            kcal = METS * bmr_kcal_min,
            SED = ifelse(axis_chosen < input$sed_cutpoint, 1, 0),
            LPA = ifelse(axis_chosen >= input$sed_cutpoint & axis_chosen < input$mpa_cutpoint, 1, 0),
            MPA = ifelse(axis_chosen >= input$mpa_cutpoint & axis_chosen < input$vpa_cutpoint, 1, 0), 
            VPA = ifelse(axis_chosen >= input$vpa_cutpoint, 1, 0),
            mets_hours_mvpa = ifelse(METS >=3, 1/60 * METS, 0))
    
    # Creating a dataframe with results by day  
      results_by_day <-
        df_with_computed_metrics %>%
        group_by(date, .drop = FALSE) %>%
        filter(wearing == "w") %>%
        summarise(
          wear_time = sum(wearing_count),
          total_counts_axis1 = sum(axis1),
          total_counts_vm = sum(vm),
          total_steps = sum(steps),
          total_kcal_wear_time = round(sum(kcal), 2),
          minutes_SED = sum(SED),
          minutes_LPA = sum(LPA),
          minutes_MPA = sum(MPA),
          minutes_VPA = sum(VPA),
          minutes_MVPA = sum(MPA) + sum(VPA),
          percent_SED = round(minutes_SED / wear_time * 100, 2),
          percent_LPA = round(minutes_LPA / wear_time * 100, 2),
          percent_MPA = round(minutes_MPA / wear_time * 100, 2),
          percent_VPA = round(minutes_VPA / wear_time * 100, 2), 
          percent_MVPA = round(minutes_MVPA / wear_time * 100, 2),
          mets_hours_mvpa = round(sum(mets_hours_mvpa), 2),
          pal = round((total_kcal_wear_time + bmr_kcal_min * (24*60 - wear_time)) * 10/9 / bmr_kcal_d(), 2)) %>%
        ungroup()
      
      return(results_by_day)
    
    }))
  
  # Showing results in a table
    output$results_by_day <- renderReactable({
      reactable(results_by_day(),  
                striped = TRUE,
                list(total_counts_axis1 = colDef(minWidth = 150),
                     total_counts_vm = colDef(minWidth = 150),
                     minutes_MVPA = colDef(minWidth = 120),
                     percent_MVPA = colDef(minWidth = 120),
                     total_kcal_wear_time = colDef(minWidth = 160),
                     mets_hours_mvpa = colDef(minWidth = 160)))
    })
  
  
  #################################################
  # Table showing metrics averaged on a daily basis
  #################################################
  
  # Getting results averaged on valid days
    results_summary <- reactive({
      
      # Waiting for valid dataframe
        req(is.data.frame(results_by_day()))
    
      # Computing results averaged on valid days'
        results_by_day() %>%
          mutate(validity = ifelse(wear_time >= input$minimum_wear_time_for_analysis * 60, "valid", "invalid")) %>%
          filter(validity == "valid") %>%
          summarise(valid_days = n(),
                    wear_time = mean(wear_time),
                    total_counts_axis1 = round(mean(total_counts_axis1), 2),
                    total_counts_vm = round(mean(total_counts_vm), 2),
                    total_steps = round(mean(total_steps), 2),
                    total_kcal_wear_time = round(mean(total_kcal_wear_time), 2),
                    minutes_SED = round(mean(minutes_SED), 2),
                    minutes_LPA = round(mean(minutes_LPA), 2),
                    minutes_MPA = round(mean(minutes_MPA), 2),
                    minutes_VPA = round(mean(minutes_VPA), 2),
                    minutes_MVPA = round(mean(minutes_MVPA), 2),
                    percent_SED = round(mean(percent_SED), 2),
                    percent_LPA = round(mean(percent_LPA), 2),
                    percent_MPA = round(mean(percent_MPA), 2),
                    percent_VPA = round(mean(percent_VPA), 2),
                    percent_MVPA = round(mean(percent_MVPA), 2),
                    mets_hours_mvpa = round(mean(mets_hours_mvpa), 1),
                    pal = round(mean(pal), 2))
        })
    
  # Showing results in a table
    output$results_summary <- renderReactable({
      
      reactable(
        results_summary(), 
        list(valid_days = colDef(minWidth = 90),
             wear_time = colDef(minWidth = 90),
             total_counts_axis1 = colDef(minWidth = 150),
             total_counts_vm = colDef(minWidth = 150),
             minutes_MVPA = colDef(minWidth = 120),
             percent_MVPA = colDef(minWidth = 120),
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
  
  output$report <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        assessor_title = input$assessor_title,
        assessor_name = input$assessor_name,
        assessor_surname = input$assessor_surname,
        patient_title = input$patient_title,
        patient_name = input$patient_name,
        patient_surname = input$patient_surname,
        sex = input$sex,
        age = input$age,
        weight = input$weight,
        start_date = min(df()$date),
        end_date = max(df()$date),
        device = input$device,
        position = input$position,
        side = input$side,
        sampling_rate = input$sampling_rate,
        filter = input$filter,
        axis_weartime = input$axis_weartime,
        frame_size = input$frame_size,
        allowanceFrame_size = input$allowanceFrame_size,
        equation_mets = input$equation_mets,
        bmr_kcal_d = bmr_kcal_d(),
        axis = input$axis,
        sed_cutpoint = input$sed_cutpoint,
        mpa_cutpoint = input$mpa_cutpoint,
        vpa_cutpoint = input$vpa_cutpoint,
        minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis,
        results_by_day = results_by_day(),
        results_summary =  results_summary()
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      out <- rmarkdown::render(tempReport,
                               params = params,
                               envir = new.env(parent = globalenv())
      )
      out <- file.rename(out, file)
    }
  )
  
}

