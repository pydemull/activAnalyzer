#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      shinydashboard::dashboardHeader(
        # Set height of dashboardHeader
        tags$li(class = "dropdown",
                tags$style(".main-header {vertical-align: middle;}"),
                tags$style(".main-header .logo {vertical-align: middle;}")
        ),
        title = span(img(src="www/favicon.png", width = 30), "activAnalyzer 0.2.9"), titleWidth = 235
                                      ),
      shinydashboardPlus::dashboardSidebar(
        tags$style(HTML("
      .main-sidebar{
        width: 235px;
      }
    ")),
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("App", tabName = "app", icon = icon("fas fa-tablet-alt")),
          shinydashboard::menuItem("User's guide", tabName = "guide", icon = icon("far fa-file-alt"))
        )),
      shinydashboard::dashboardBody(

        # Providing alert message when closing the web window  (code from 
        # https://stackoverflow.com/questions/56369796/adding-a-are-you-sure-you-want-to-leave-this-page-alert-message-when-exiting-a)
        tags$head(tags$script(htmltools::HTML("
             // Enable navigation prompt
             window.onbeforeunload = function() {
                return 'Your changes will be lost!';
             };
             "))),
        
        shinydashboard::tabItems(
          
          # $$$$$$$$$$$$$$$$$
          # First tab content ----
          # $$$$$$$$$$$$$$$$$
          
          shinydashboard::tabItem(tabName = "app",
                  
                  # Controlling appearance of warning feedbacks      
                  shinyFeedback::useShinyFeedback(),
                  tags$head(
                    tags$style(HTML("
            .shiny-output-error-validation {
              color: #ff0000;
              font-weight: bold;
            }
          "))
                  ),
                  
                  # Setting color for section titles
                  tags$head(tags$style('h2 {color:#337ab7;}')),
                  
                  ########################
                  # Section 1. Information ----
                  ########################
                  
                  fluidRow(
                    column(12,
                           h3("Section 1. Information", style="font-weight: bold; font-size: 30px; color: #337ab7;"),
                           tags$hr(style="border-color: #337ab7;"),
                    ),
                  ),
                  
                  #******************
                  # Assessor
                  #******************
                  
                  fluidRow(
                    column(6,                   
                           h2("Assessor"),
                    ),
                  ),
                  fluidRow(
                    column(2,
                           textInput("assessor_name", "Name", placeholder = "")
                    ),
                    column(2,
                           textInput("assessor_surname", "Surname", placeholder = "" )
                    ),
                  ),
                  
                  #******************
                  # Patient
                  #******************
                  
                  fluidRow(
                    column(6,                   
                           h2("Patient")
                    ),
                  ),
                  fluidRow(
                    column(2,
                           textInput("patient_name", "Name", placeholder = "")
                    ),
                    column(2,
                           textInput("patient_surname", "Surname", placeholder = "" )
                    ),
                  ), 
                  fluidRow(
                    column(2,
                           selectInput("sex", with_red_star("Sex"), sex)
                    ),
                    column(2,
                           numericInput("age", with_red_star("Age (yr)"), value = 0, min = 0)
                    ),
                    column(2,
                           numericInput("weight", with_red_star("Weight (kg)"), value = 0, min = 0)
                    ),
                  ),
                  
                  #******************
                  # Device
                  #******************
                  
                  fluidRow(
                    column(6,                   
                           h2("Device")
                    ),
                  ),
                  fluidRow(
                    column(2,
                           selectInput("position", "Position", position)
                    ),
                    column(2,
                           selectInput("side", "Side", side)
                    ),
                    
                  ),
                  
                  ###########################################################################
                  # Section 2. Data uploading, nonwear time detection, and data visualization ----
                  ###########################################################################
                  
                  #******************
                  # Inputs
                  #******************
                  
                  fluidRow(
                    column(12,
                           h3("Section 2. Data uploading, nonwear time detection, and data visualization", style="font-weight: bold; font-size: 30px; color: #337ab7;"),
                           tags$hr(style="border-color: #337ab7;")
                    ),
                  ),
                  fluidRow(
                    column(12,
                           fileInput("upload", "Upload file (please wait until seeing 'Upload complete' before continuing)", placeholder = ".agd")
                    ),
                  ),
                  fluidRow(
                    column(3,
                           selectInput("axis_weartime", "Axis to be considered to detect nonwear time", axis_weartime)
                    ),  
                    column(3,
                           numericInput("frame_size", "Time interval to be considered to detect nonwear time (min)", value = 90, min = 0)
                    ),
                    column(3,
                           numericInput("allowanceFrame_size", "Time interval with nonzero counts allowed during a nonwear period (min)", value = 2, min = 0)
                    ),
                    column(3,
                           numericInput("streamFrame_size", "Time interval with zero counts required around activity for nonwear time (min)", value = 30, min = 0)
                    ),
                  ),
                  fluidRow(
                    column(3,
                           shiny::actionButton("reset_nonwear", "Return to default values", style = "border-color: #2e6da4")
                    ),
                  ),
                  fluidRow(
                    column(3,
                           h3(""),
                           shiny::actionButton("validate", "Validate configuration", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    ),
                  ),
                  
                  #*************************
                  # Box showing monitor data
                  #*************************
                  
                  fluidRow(
                    h3(""),
                    shinydashboardPlus::box(id = "myBox", 
                                            shinycssloaders::withSpinner(plotOutput("graph", height = "auto")), 
                                            width = 12, 
                                            height = "auto")
                  ),
                  fluidRow(
                    align = "center",
                    selectInput("Metric", "Data to visualize", metrics),
                  ),
                  
                  
                  ##################################################
                  # Section 3. Configuration for metrics computation ----
                  ##################################################
                  
                  fluidRow(
                    column(12,
                           h3("Section 3. Configuration for metrics computation", style="font-weight: bold; font-size: 30px; color: #337ab7;"),
                           tags$hr(style="border-color: #337ab7;")
                    ),
                  ),    
                  
                  #******************
                  # Choosing Equation
                  #******************
                  
                  fluidRow(
                    column(10,
                           h3("Choose a MET equation"),
                           selectInput("equation_mets", with_red_star("Equation"), equations)
                    ),
                  ),
                  fluidRow(
                    column(12,
                           reactable::reactableOutput("table_equations")
                    )
                  ),
                  
                  #********************
                  # Choosing cut-points
                  #********************
                  
                  fluidRow(
                    column(12,
                           h3("Choose cut-points"),
                    )
                  ),
                  
                  #***************
                  # SED cut-points
                  #***************
                  
                  fluidRow(
                    column(12,
                           selectInput("sed_cutpoint", with_red_star("SED cut-point"), choices = sed_cutpoint)
                    )
                  ),
                  
                  fluidRow(
                    column(12,
                           tabsetPanel(
                             id = "switcher_sed",
                             type = "hidden",
                             tabPanelBody("...", ""),
                             tabPanelBody("Aguilar-Farias et al. (2014) [Older adults]", reactable::reactableOutput("table_sed_cutpoints")),
                             tabPanelBody("Personalized...", 
                                          fluidRow(
                                            column(3,
                                                   wellPanel(selectInput("perso_sed_axis", "Axis for SED categorization", perso_sed_axis),
                                                             numericInput("perso_sed_cutpoint", "SED cut-point in counts/min (<)", value = 200, min = 0))
                                            ),
                                          )
                             )
                           )
                    )
                  ),
                  
                  #****************
                  # MVPA cut-points
                  #****************
                  
                  fluidRow(
                    column(12,
                           h3(""),
                           selectInput("mvpa_cutpoint", with_red_star("MVPA cut-points"), choices = mvpa_cutpoint)
                    )
                  ),
                  fluidRow(
                    column(12,
                           tabsetPanel(
                             id = "switcher_mvpa",
                             type = "hidden",
                             tabPanelBody("...", ""),
                             tabPanelBody("Sasaki et al. (2011) [Adults]", reactable::reactableOutput("table_mvpa_cutpoints_sasaki")),
                             tabPanelBody("Santos-Lozano et al. (2013) [Adults]", reactable::reactableOutput("table_mvpa_cutpoints_santos_adults")),
                             tabPanelBody("Freedson et al. (1998) [Adults]", reactable::reactableOutput("table_mvpa_cutpoints_freedson_adults")),
                             tabPanelBody("Santos-Lozano et al. (2013) [Older adults]", reactable::reactableOutput("table_mvpa_cutpoints_santos_older")),
                             tabPanelBody("Personalized...", 
                                          fluidRow(
                                            column(3,
                                                   wellPanel(selectInput("perso_mvpa_axis", "Axis for MVPA categorization", perso_mvpa_axis),
                                                             numericInput("perso_mpa_cutpoint", "MPA cut-point in counts/min (>=)", value = 3208 , min = 0),
                                                             numericInput("perso_vpa_cutpoint", "VPA cut-point in counts/min (>=)", value = 8565 , min = 0))
                                            ),
                                          )
                             )
                           )
                    )
                  ),
                  
                  #********************************
                  # Minimum wear time for valid day
                  #********************************
                  
                  fluidRow(
                    column(12,
                           h3("Configure the inputs required to define the period of the day to be considered to count wear time and to validate a day based on wear time"),
                    ),
                  ),
                  fluidRow(
                    column(3,
                           selectInput("start_day_analysis", "Start of the period to consider each day", 
                                       choices = c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59)),
                                       selectize=FALSE
                           )
                    ),
                    column(3,
                           selectInput("end_day_analysis", "End of the period to consider each day", 
                                       choices = rev(c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59))),
                                       selectize=FALSE
                           )
                    ),
                    column(2,
                           numericInput("minimum_wear_time_for_analysis", "Minimum wear time to validate a day (hours)", value = 10)
                    ),
                    column(3,
                           shiny::actionButton("pro_active_period", "Set PROactive configuration for 24-h recording", style = "margin-top: 25px; background-color: #9933CC; color: white; border-color: #330066")
                    ),
                  ),
                  fluidRow(
                    column(3,
                           shiny::actionButton("reset_period", "Return to default values", style = "border-color: #2e6da4")
                    ),
                  ),
                  
                  #*****************
                  # Running analysis
                  #*****************
                  
                  fluidRow(
                    column(3,
                           h3(""),
                           shiny::actionButton("Run", "Run Analysis", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    ),
                  ),
                  
                  #######################################
                  # Section 4. Results, Export, and Reset ----
                  #######################################
                  
                  fluidRow(
                    column(12,
                           h3("Section 4. Results, Export, and Reset", style="font-weight: bold; font-size: 30px; color: #337ab7;"),
                           tags$hr(style="border-color: #337ab7;")
                    ),
                  ),  
                  
                  #********
                  # Results
                  #********
              
                       #*************************
                       # Box showing monitor data
                       #*************************
                       
                       fluidRow(
                         column(12,
                         h2("Results"),
                         h3("")
                         )
                         ),
                       fluidRow(
                         shinydashboardPlus::box(id = "myBox2", 
                                                 shinycssloaders::withSpinner(plotOutput("graph_int", height = "auto")), 
                                                 width = 12, 
                                                 height = "auto")
                       ),
                       fluidRow(
                         align = "center",
                         selectInput("Metric2", "Data to visualize", metrics),
                       ),
                      
                       #************************
                       # Table of results by day
                       #************************
                      
                       fluidRow(
                         column(12,
                                h3("Results by day"),
                                shinycssloaders::withSpinner(reactable::reactableOutput("results_by_day"))
                         ),
                       ),
                      
                       #****************************************************
                       # Table of results summarized over valid days (means)
                       #****************************************************
                       
                       fluidRow(
                         column(12,
                                h3("Results summarized over valid days (means)"),
                                shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_means"))
                         ),
                       ),
                       
                       #******************************************************
                       # Table of results summarized over valid days (medians)
                       #******************************************************
                       
                       fluidRow(
                         column(12,
                                h3("Results summarized over valid days (medians)"),
                                shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_medians"))
                         ),
                       ),
                       
                       #******************************************************
                       # Proactive scores
                       #******************************************************
                       
                       fluidRow(
                         column(2,
                                h3("PROactive scores"),
                                shinycssloaders::withSpinner(reactable::reactableOutput("PROactive_scores"))
                         ),
                       ),
                           
                  #******************
                  # Export
                  #******************
                  
                  fluidRow(
                    column(12,
                           h2(""),
                           h2("Export"),
                           downloadButton("ExpDataset", "Export marked dataset (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           downloadButton("ExpResultsByDays", "Export results by day (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           downloadButton("ExpDailySummaryMeans", "Export daily summary (means) (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           downloadButton("ExpDailySummaryMedians", "Export daily summary (medians) (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           downloadButton("report_en", "Generate report (EN) (.pdf)", style="border-color: #FF9900; color: black; background-color: #FFCC33"),
                           downloadButton("report_fr", "Generate report (FR) (.pdf)", style="border-color: #FF9900; color: black; background-color: #FFCC33")
                    ),
                    
                  ),
                  
                  #******************
                  # Reset app
                  #******************
                  
                  fluidRow(
                    column(4,
                           h2("Reset app"),
                           shiny::actionButton('reset',"Reset App",  style="color: #fff; background-color: #F8766D; border-color: #FC717F")
                    )
                  )
          ), # End first tab
          
          
          
          # $$$$$$$$$$$$$$$$$$
          # Second tab content ----
          # $$$$$$$$$$$$$$$$$$
          
          shinydashboard::tabItem(tabName = "guide",
                  h2("User's guide"),
                  
                  ################
                  # Notes to users ----
                  ################
                  
                  fluidRow(
                    column(12, 
                           h4("Welcome to the activAnalyzer app. activAnalyzer is a shinny app that was developed to analyze daily physical behavior data recorded at the hip 
                   in adults using an ActiGraph accelerometer (.agd files). Once analysis is completed,
                   the app allows to export results to .csv files and to generate a report of the measurement. All the configured inputs
                   relevant for interpreting the results are recorded into the report. Be sure that the inputs that are configured
                   when generating the report correspond to the analysis that was actually performed (in other words, avoid modifying the inputs
                   after generating satisfactory results). Please read the user's guide for details about how the app works."),
                    ),
                  ),
                  fluidRow(
                    column(4,
                           downloadButton("user_guide_en", "Download user's guide (EN) (.pdf)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           downloadButton("user_guide_fr", "Download user's guide (FR) (.pdf)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                    ),
                  ),
                  fluidRow(
                    column(12,
                           tags$hr(style="border-color: #337ab7;")
                    ),
                  ),
                  fluidRow(
                    column(12,
                           h2(""),
                           h2("Authors"),
                           h4("Pierre-Yves de M\u00fcllenheim, PhD", style = "font-weight: bold; font-size: 20px"),
                           h4("Associate professor"),
                           h4("Institut de formation en \u00e9ducation physique et en sport d\u0027Angers \u0028IFEPSA\u0029, UCO, France"),
                           h4("Email: pydemull@uco.fr"),
                           h2(""),
                           h4("Arnaud Chambellan, MD, PhD", style = "font-weight: bold; font-size: 20px"),
                           h4("Pulmonology Department, H\u00f4pital Saint Philibert, GHICL, France"),
                           h4("Professor of Physiology, Universit\u00e9 Catholique de Lille, France"),
                           h4("Email: chambellan.arnaud@ghicl.net")
                           
                    ),
                  ),
          ) # End second tab
          
        )
      ), # End dashboardBody 
      
      footer = shinydashboardPlus::dashboardFooter(
        left = "\u00a9 2021-2022 Conceived by Pierre-Yves de M\u00fcllenheim and Arnaud Chambellan. Developed by Pierre-Yves de M\u00fcllenheim - GNU General Public License Version 3.0",
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'activAnalyzer'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}



