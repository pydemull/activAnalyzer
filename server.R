
#####################################################################################################################################
# Server
#####################################################################################################################################

server <- function(input, output, session) {
  
  ##################################################
  # Getting and formating data and marking wear time
  ##################################################
  
  df <- reactive({
    
    req(input$upload)
    
    data <-
      readActigraph(input$upload$datapath) %>%
      dataCollapser(TS = "TimeStamp", by = 60) %>%
      mutate(TimeStamp = as.character(TimeStamp),
             TimeStamp2 = TimeStamp) %>%
      tidyr::separate("TimeStamp2", c("date", "time"), sep = " ") %>%
      mutate(time = hms::as_hms(time)) %>%
      select(TimeStamp, date, time, everything())
    
    data <- wearingMarking(data, TS = "TimeStamp", cts = "vm") %>%
      mutate(non_wearing_count = ifelse(wearing == "nw", 1, 0),
             wearing_count = ifelse(wearing == "w", 1, 0))
    
    return(data)
    
  })
  
    ########################################
    # Visualizing all data with nonwear time
    ########################################
  
    output$graph <- renderPlot({
      
      if (input$Metric == "...") {NULL
        } else {
      max_metric <- max(df()[input$Metric], na.rm = TRUE)
      
      ggplot(data = df(), aes(x = time)) +
        geom_ribbon(aes(ymin = 0, ymax = non_wearing_count / non_wearing_count * max_metric, fill = "Non-wear time (Choi et al., 2011)"), alpha = 0.5) +
        geom_line(aes_string(y = input$Metric)) +
        scale_x_time(breaks = hms::hms(seq(0, 24*3600, 6*3600))) +
        scale_fill_manual(values = "red") +
        labs(x = "Time (hh:mm:ss)", y = paste(input$Metric), fill = "") +
        theme_bw() +
        theme(legend.position = "bottom") + 
        facet_wrap(.~ date)
    }
      })
    
    ##########################################
    # Table showing metrics summarised per day
    ##########################################
    
    results_by_day <- eventReactive(input$Run, ({
      
      if(input$axis == "vertical axis") { axis_chosen <- df()$axis1
      } else { axis_chosen <- df()$vm }

      df_with_computed_metrics <-
      df() %>%
      mutate(
        METS = compute_mets(data = .data, equation = input$equation_mets, weight = input$weight, gender = input$sex),
        kcal = METS * input$weight / 60,
        SED = ifelse(axis_chosen < input$sed_cutpoint, 1, 0),
        LPA = ifelse(axis_chosen >= input$sed_cutpoint & axis_chosen < input$mpa_cutpoint, 1, 0),
        MPA = ifelse(axis_chosen >= input$mpa_cutpoint & axis_chosen < input$vpa_cutpoint, 1, 0), 
        VPA = ifelse(axis_chosen >= input$vpa_cutpoint, 1, 0))
    
    results_by_day <-
      df_with_computed_metrics %>%
      group_by(date, .drop = FALSE) %>%
      filter(wearing == "w") %>%
      summarise(
        wear_time = sum(wearing_count),
        total_counts_axis1 = sum(axis1),
        total_counts_vm = sum(vm),
        total_steps = sum(steps),
        xtotal_kcal = sum(kcal),
        minutes_SED = sum(SED),
        minutes_LPA = sum(LPA),
        minutes_MPA = sum(MPA),
        minutes_VPA = sum(VPA),
        minutes_MVPA = sum(MPA) + sum(VPA),
        percent_SED = round(minutes_SED / wear_time * 100, 2),
        percent_LPA = round(minutes_LPA / wear_time * 100, 2),
        percent_MPA = round(minutes_MPA / wear_time * 100, 2),
        percent_VPA = round(minutes_VPA / wear_time * 100, 2), 
        percent_MVPA = round(minutes_MVPA / wear_time * 100, 2)) %>%
      ungroup()
    
      mean_METS <-
        df_with_computed_metrics %>%
        group_by(date, .drop = FALSE) %>%
        filter(wearing == "w" & time >= hms::as_hms(input$period[1] * 3600) & time <= hms::as_hms(input$period[2] * 3600)) %>%
        summarise(
          mean_METS = mean(METS, na.rm = TRUE)) %>%
        ungroup()
      
      results_by_day$mean_METS <- round(mean_METS[["mean_METS"]], 2)
      
      return(results_by_day)
      
    }))
    
    output$results_by_day <- renderReactable({
        reactable(results_by_day(),  
                  striped = TRUE,
                  list(minutes_MVPA = colDef(minWidth = 120),
                                       percent_MVPA = colDef(minWidth = 120)))
      })
    
    
    #################################################
    # Table showing metrics averaged on a daily basis
    #################################################
    
    results_summary <- reactive({
      results_by_day() %>%
      mutate(validity = ifelse(wear_time >= input$minimum_wear_time_for_analysis * 60, "valid", "invalid")) %>%
      filter(validity == "valid") %>%
      summarise(valid_days = n(),
                wear_time = mean(wear_time),
                total_counts_axis1 = round(mean(total_counts_axis1), 2),
                total_counts_vm = round(mean(total_counts_vm), 2),
                total_steps = round(mean(total_steps), 2),
                tot_kcal = round(mean(total_kcal), 2),
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
                mean_METS = round(mean(mean_METS), 2)
      )
    })
    
     output$results_summary <- renderReactable({
        
       reactable(
         results_summary(), 
        list(valid_days = colDef(minWidth = 90),
             wear_time = colDef(minWidth = 90),
             total_counts_axis1 = colDef(minWidth = 140),
             total_counts_vm = colDef(minWidth = 120),
             minutes_MVPA = colDef(minWidth = 120),
             percent_MVPA = colDef(minWidth = 120)),
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
          height = input$height,
          weight = input$weight,
          start_date = min(df()$date),
          end_date = max(df()$date),
          device = input$device,
          position = input$position,
          side = input$side,
          sampling_rate = input$sampling_rate,
          filter = input$filter,
          equation_mets = input$equation_mets,
          axis = input$axis,
          sed_cutpoint = input$sed_cutpoint,
          mpa_cutpoint = input$mpa_cutpoint,
          vpa_cutpoint = input$vpa_cutpoint,
          minimum_wear_time_for_analysis = input$minimum_wear_time_for_analysis,
          mets_period = input$period,
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