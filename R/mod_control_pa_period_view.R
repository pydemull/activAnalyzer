#' control_pa_period_view UI Function
#'
#' @description A shiny Module to control the view of the inputs to be filled to indicate relevant missing physical activity periods
#'  to be added to the accelerometer dataset.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_control_pa_period_view_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  fluidRow(
    column(4,
           shiny::actionButton(ns("add_period_btn"), "+", class = "btn-control-add"),
           shiny::actionButton(ns("remove_period_btn"), "-", class = "btn-control-rem")
           )
  )
 
  )
}
    
#' control_pa_period_view Server Functions
#'
#' @noRd 
mod_control_pa_period_view_server <- function(id, add_period_btn = NULL, remove_period_btn = NULL){
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    
    # Hidding "Remove" button for period 1
    if (id == "period_1") {shinyjs::hide("remove_period_btn")}
    
    # Hidding both "Add" and "Remove" buttons when starting the app
    if (id != "period_1") { 
      shinyjs::hide("add_period_btn")
      shinyjs::hide("remove_period_btn")
    }
    
    # Showing the "Add" and "Remove" buttons when pressing the dedicated button
    if (!is.null(add_period_btn) && id != "period_15") {
       observeEvent(add_period_btn(), {
         shinyjs::show("add_period_btn")
         shinyjs::show("remove_period_btn")
       })
    } 
    
    if (!is.null(add_period_btn) && id == "period_15") {
      observeEvent(add_period_btn(), {
        shinyjs::show("remove_period_btn")
      })
    } 
    
    # Hidding buttons when a new row of inputs is created
    observeEvent(input$add_period_btn, {
      shinyjs::hide("add_period_btn")
      shinyjs::hide("remove_period_btn")
    })
    
    # Hidding the "Add" and "Remove" buttons when pressing the dedicated button
    observeEvent(input$remove_period_btn, {
      shinyjs::hide("add_period_btn")
      shinyjs::hide("remove_period_btn")
    })
    
    # Showing again the "Add" and "Remove" buttons after deleting the last row of inputs
    if (!is.null(remove_period_btn) && id == "period_1") {
      observeEvent(remove_period_btn(), {
      shinyjs::show("add_period_btn")
    })
    }
    
    if (!is.null(remove_period_btn) && id != "period_1") {
      observeEvent(remove_period_btn(), {
        shinyjs::show("add_period_btn")
        shinyjs::show("remove_period_btn")
      })
    }
   
    
    # Return reactive expressions for the "Add" and "Remove" buttons
    param <- list(add_period_btn = reactive(input$add_period_btn), remove_period_btn = reactive(input$remove_period_btn))
    return(param)
  

  })
}
    
## To be copied in the UI
# mod_control_pa_period_view_ui("control_pa_period_view_1")
    
## To be copied in the server
# mod_control_pa_period_view_server("control_pa_period_view_1")
