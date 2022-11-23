#' report_pa_period UI Function
#'
#' @description A shiny Module to indicate relevant missing physical activity periods to be added to the accelerometer dataset.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_pa_period_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             fluidRow(
               
               # Date
               column(2,
                      selectInput(
                        ns("corr_date"), 
                        "Date (YYYY-MM-DD)",
                        choices = "..."
                      )
               ),
               
               # Start time
               column(1,
                      numericInput(
                        ns("corr_start_time_hh"), 
                        "Start time (hh:)",
                        value = 0,
                        min = 0,
                        max = 23,
                        step = 1
                      )
               ),
               column(1,
                      numericInput(
                        ns("corr_start_time_mm"), 
                        "Start time (mm:)",
                        value = 0,
                        min = 0,
                        max = 59,
                        step = 1
                      )
               ),
               
               # End time
               column(1,
                      numericInput(
                        ns("corr_end_time_hh"), 
                        "End time (hh:)",
                        value = 0,
                        min = 0,
                        max = 23,
                        step = 1
                      )
               ),
               column(1,
                      numericInput(
                        ns("corr_end_time_mm"), 
                        "End time (mm:)",
                        value = 0,
                        min = 0,
                        max = 59,
                        step = 1
                      )
               ),
               
               # METs
               column(1,
                      numericInput(
                        ns("corr_mets"), 
                        "Activity METs\n",
                        value = 0,
                        min = 0,
                        step = 0.1
                      )
               ),
             ),
      )
    )
  )
}
    
#' report_pa_period Server Functions
#'
#' @noRd 
mod_report_pa_period_server <- function(id, add_period_btn = NULL, remove_period_btn = NULL, dates_inputs = NULL){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Updating date inputs
   observe({
     updateSelectInput(session, "corr_date", choices = c("...", dates_inputs$dates))
     })

    # Hidding inputs when starting the app except the first row of inputs
    if (id != "period_1") {
      shinyjs::hide("corr_date")
      shinyjs::hide("corr_start_time_hh")
      shinyjs::hide("corr_start_time_mm")
      shinyjs::hide("corr_end_time_hh")
      shinyjs::hide("corr_end_time_mm")
      shinyjs::hide("corr_mets")
    }
    
    # Showing the row of inputs when clicking on the dedicated button
    if (!(is.null(add_period_btn))) {
      observeEvent(add_period_btn(), {
        shinyjs::show("corr_date")
        shinyjs::show("corr_start_time_hh")
        shinyjs::show("corr_start_time_mm")
        shinyjs::show("corr_end_time_hh")
        shinyjs::show("corr_end_time_mm")
        shinyjs::show("corr_mets")
      })
    }

    # Hidding the row of inputs when clicking on the dedicated button
    if (!(is.null(remove_period_btn))) {
      observeEvent(remove_period_btn(), {
        shinyjs::hide("corr_date")
        shinyjs::hide("corr_start_time_hh")
        shinyjs::hide("corr_start_time_mm")
        shinyjs::hide("corr_end_time_hh")
        shinyjs::hide("corr_end_time_mm")
        shinyjs::hide("corr_mets")
        updateNumericInput(session, "corr_mets", value = 0)
      })
    }

    
  })
}
    
## To be copied in the UI
# mod_report_pa_period_ui("report_pa_period_1")
    
## To be copied in the server
# mod_report_pa_period_server("report_pa_period_1")
