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
               column(3,
                      selectInput(
                        ns("corr_date"), 
                        "Date (YYYY-MM-DD)",
                        choices = "..."
                      )
               ),
               
               # Start time
               column(3,
                      selectizeInput(
                        ns("corr_start_time"),  
                        "Start time (hh:mm:ss)",
                        choices = "..."
                      )
               ),
               
               # End time
               column(3,
                      selectizeInput(
                        ns("corr_end_time"), 
                        "End time (hh:mm:ss)",
                        choices = "..."
                      )
               ),
               
               # METs
               column(3,
                      numericInput(
                        ns("corr_mets"), 
                        "Activity METs",
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
mod_report_pa_period_server <- function(id, add_period_btn = NULL, remove_period_btn = NULL){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Initializing PA periods inputs
    updateSelectizeInput(session, "corr_start_time", choices = c(hms::as_hms(seq(0, 60*(24*60-1), 60))), server = TRUE)
    updateSelectizeInput(session, "corr_end_time", choices = c(hms::as_hms(seq(0, 60*(24*60-1), 60))), server = TRUE)
    
    # Hidding inputs when starting the app except the first row of inputs
    if (id != "period_1") {
      shinyjs::hide("corr_date")
      shinyjs::hide("corr_start_time")
      shinyjs::hide("corr_end_time")
      shinyjs::hide("corr_mets")
    }
    
    # Showing the row of inputs when clicking on the dedicated button
    if (!(is.null(add_period_btn))) {
      observeEvent(add_period_btn(), {
        shinyjs::show("corr_date")
        shinyjs::show("corr_start_time")
        shinyjs::show("corr_end_time")
        shinyjs::show("corr_mets")
      })
    }

    # Hidding the row of inputs when clicking on the dedicated button
    if (!(is.null(remove_period_btn))) {
      observeEvent(remove_period_btn(), {
        shinyjs::hide("corr_date")
        shinyjs::hide("corr_start_time")
        shinyjs::hide("corr_end_time")
        shinyjs::hide("corr_mets")
      })
    }

    
  })
}
    
## To be copied in the UI
# mod_report_pa_period_ui("report_pa_period_1")
    
## To be copied in the server
# mod_report_pa_period_server("report_pa_period_1")
