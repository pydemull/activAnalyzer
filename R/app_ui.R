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
    # Script for Full Screen (https://stackoverflow.com/questions/42371164/how-to-run-r-shiny-app-in-full-sized-window)
    shinyjs::extendShinyjs(text = "shinyjs.toggleFullScreen = function() {
    var element = document.documentElement,
      enterFS = element.requestFullscreen || element.msRequestFullscreen || element.mozRequestFullScreen || element.webkitRequestFullscreen,
      exitFS = document.exitFullscreen || document.msExitFullscreen || document.mozCancelFullScreen || document.webkitExitFullscreen;
    if (!document.fullscreenElement && !document.msFullscreenElement && !document.mozFullScreenElement && !document.webkitFullscreenElement) {
      enterFS.call(element);
    } else {
      exitFS.call(document);
    }
  }", 
                           functions = c("shinyjs.toggleFullScreen")
                           ),

    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      shinydashboard::dashboardHeader(
        tags$li(class = "dropdown",
                tags$li(class = "dropdown",  shiny::actionButton('fs',"Full Screen",  class = "btn-fullscreen"), onclick = "shinyjs.toggleFullScreen();"),
                tags$li(class = "dropdown",  shiny::actionButton('reset',"Reset",  class = "btn-refresh")),
                tags$style(".main-header {vertical-align: middle;}"),
                tags$style(".main-header .logo {vertical-align: middle;}")
        ),
        title = span(img(src="www/favicon.png", width = 30), "activAnalyzer dev"), titleWidth = 237
                                      ),
      shinydashboardPlus::dashboardSidebar( 
        tags$style(HTML(".sidebar-menu li a {font-size: 17px;}")),
        tags$style(HTML(".main-sidebar{ width: 240px; }")),
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem("Accelerometer analysis", tabName = "app", icon = suppressMessages(icon("fa-solid fa-chart-column", class="icon-widget"))),
          shinydashboard::menuItem("PROactive questionnaire", tabName = "proactive", icon = suppressMessages(icon("fa-solid fa-file-circle-question",  class="icon-widget"))),
          shinydashboard::menuItem("User's guide", tabName = "guide", icon = suppressMessages(icon("fa-solid fa-file-lines",  class="icon-widget")))
        )),
      shinydashboard::dashboardBody(

        # Providing alert message when closing the web window  (code from 
        # https://stackoverflow.com/questions/56369796/adding-a-are-you-sure-you-want-to-leave-this-page-alert-message-when-exiting-a)
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
          tags$style(htmltools::HTML("hr {border-top: 1px solid #3F51B5;}")),
          tags$script(htmltools::HTML("
             // Enable navigation prompt
             window.onbeforeunload = function() {
                return 'Your changes will be lost!';
             };
             ")),
        # Keeping the top of the page when clicking on the "go_to_proactive_q" button
        # per https://stackoverflow.com/questions/44686681/r-shiny-tabpanel-does-not-automatically-got-to-top-of-page-when-clicked
        tags$script(" $(document).ready(function () {
         $('#go_to_proactive_q').on('click', function (e) {
          window.scrollTo(0, 0)
               });

               });"),
          ),
        
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
          ")),
                 tags$style(".checkbox-inline {margin: 0 !important;}")
                  ),

                  
                  ########################
                  # Section 1. Information ----
                  ########################
                  
                  fluidRow(
                    column(12,
                           h2("Section 1. Information"),
                    ),
                  ),
                  
                  #******************
                  # Assessor
                  #******************
                  
                  fluidRow(
                    column(12,                   
                           h3("Assessor"),
                           hr()
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
                    column(12,                   
                           h3("Patient"),
                           hr()
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
                    column(12,                   
                           h3("Device"),
                           hr()
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
                 
                 
                 #******************
                 # Auto-complete
                 #******************
                 
                 fluidRow(
                   column(2,
                          shiny::actionButton("auto_fill_char", "Auto-fill", class = "btn-return"),
                          )
                 ),
                  
                  ###########################################################################
                  # Section 2. Data uploading, nonwear time detection, and data visualization ----
                  ###########################################################################
                  
                  #******************
                  # Inputs
                  #******************
                  
                  fluidRow(
                    column(12,
                           h2("Section 2. Data uploading, nonwear time detection, and data visualization"),
                    ),
                  ),
                  fluidRow(
                    column(3,
                           h3(""),
                           fileInput("upload", "Upload file (please wait until seeing the 'Validate configuration' green button below)", placeholder = ".agd")
                    ),
                    column(2,
                           h3(""),
                           div("You can also load a demo file by clicking on the button below ! ", class = "control-label", style = "width: 100%;"),
                           shiny::actionButton('demo',"Load Demo File",  class = "btn-demo")
                    )
                  ),
                 fluidRow(
                   column(12,
                          shinydashboardPlus::box(id = "box-features",
                                                  title = "FILE INFO",
                                                  width = 12,
                                                  div(textOutput("warning_features"), class = "warn-message")
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          shinydashboardPlus::box(id = "box-demo",
                                                  title = "NOTE",
                                                  width = 12,
                                                  div(textOutput("warning_demo"), class = "warn-message")
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          selectInput("to_epoch", "Length of the epoch to use for analysis (s)", choices = c(60, 15, 10, 5, 1))
                   ),
                 ),
                 fluidRow(
                   column(12,
                          shinydashboardPlus::box(id = "box-epoch",
                                                  title = "NOTE",
                                                  width = 12,
                                                  div(textOutput("warning_epoch"), class = "warn-message")
                          ),
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
                           shiny::actionButton("reset_nonwear", "Default settings", class = "btn-return")
                    ),
                  ),
                 fluidRow(
                   column(12,
                          shinydashboardPlus::box(id = "box-auto_fill_char",
                                                  title = "NOTE",
                                                  width = 12,
                                                  div(textOutput("warning_auto_fill_char"), class = "warn-message")
                          ),                   ),
                 ),
                  fluidRow(
                    align = "center",                           
                    shiny::actionButton("validate", "Validate configuration", class = "btn-validate", style = "margin-bottom: 20px;"),
                  ),
                 
                  
                  #*************************
                  # Box showing monitor data
                  #*************************
                  
                  fluidRow(
                    shinydashboardPlus::box(id = "myBox", 
                                            title = "Wear time analysis",
                                            shinycssloaders::withSpinner(plotOutput("graph", height = "auto")), 
                                            width = 12, 
                                            height = NULL)
                  ),
                  fluidRow(
                    align = "center",
                    column(4, 
                           selectInput("Metric", "Data to visualize", metrics)
                    ),
                    column(4, 
                           selectInput("zoom_from_weartime", "Start of the period to visualize", 
                                       choices = c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59), hms::as_hms(60*60*23+60*59+59)),
                                       selectize=FALSE
                           )
                    ),
                    column(4,
                           selectInput("zoom_to_weartime", "End of the period to visualize", 
                                       choices = c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59), hms::as_hms(60*60*23+60*59+59)),
                                       selected = hms::as_hms(60*60*23+60*59+59),
                                       selectize=FALSE
                           )
                    ),
                  ),
                 fluidRow(
                   align = "center",
                   shiny::actionButton("update_graphic", "Update graphic", class = "btn-return")
                 ),
                  
                  
                  ##################################################
                  # Section 3. Configuration for metrics computation ----
                  ##################################################
                  
                  fluidRow(
                    column(12,
                           h2("Section 3. Configuration for metrics computation"),
                    ),
                  ),
                  
                  #***************************************
                  # Choosing days to consider for analysis
                  #***************************************

                  fluidRow(
                    column(12,
                           tags$div(align = "left",
                           uiOutput("select_days"),
                           )
                    ),
                  ),
                  
                  #*************************************
                  # Choosing MET equation and cut-points
                  #*************************************
                  
                  fluidRow(
                    column(12,
                           h3("Choose a MET equation and appropriate cut-points"),
                           hr(),
                           selectInput("equation_mets", with_red_star("Equation"), equations)
                    ),
                  ),
                  fluidRow(
                    column(12,
                           reactable::reactableOutput("table_equations")
                    )
                  ),
                  
                  #***************
                  # SED cut-points
                  #***************
                  
                  fluidRow(
                    column(12,
                           div(selectInput("sed_cutpoint", with_red_star("SED cut-point"), choices = sed_cutpoint), style = "margin-top: 20px; margin-bottom: 0px"),
                           div(tabsetPanel(
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
                           ), class = "tab_cutpoints")
                           )
                  ),
                  
                  #****************
                  # MVPA cut-points
                  #****************
                  
                  fluidRow(
                    column(12,
                           div(selectInput("mvpa_cutpoint", with_red_star("MVPA cut-points"), choices = mvpa_cutpoint), style = "margin-top: 20px; margin-bottom: 0px"),
                           div(tabsetPanel(
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
                           ), class = "tab_cutpoints")
                    )
                  ),
                 fluidRow(
                   column(3,
                          shiny::actionButton("auto_fill_intensity", "Default settings",  class = "btn-return", style = "margin-top: 10px"),
                   ),
                 ),
                 
                 #*************************************
                 # Choosing intensity bins
                 #*************************************
                 
                 fluidRow(
                   column(12,
                          h3("Choose parameters to determine bins of intensity"),
                          hr(),
                          ),
                 ),
                 fluidRow(
                   column(3,
                          numericInput("start_first_bin", "Start of the first bin (counts/chosen epoch duration)", value = 0, min = 0)
                   ),  
                   column(3,
                          numericInput("start_last_bin", "Start of the last bin (counts/chosen epoch duration)", value = 10000, min = 0)
                   ),
                   column(3,
                          numericInput("bin_width", "Width of the bins (counts/chosen epoch duration)", value = 500, min = 0)
                   )
                 ),
              
                 #************************************************************************
                 # Enter information related to relevant missing physical activity periods
                 #************************************************************************
                 
                 fluidRow(
                   column(12,
                          h3("Enter information relating to relevant physical activity periods that would be missing due to device removal (if any)"),
                          hr()
                   ),
                   column(12,
                          wellPanel(
                          h3("This part of the section is not mandatory. It allows you to manually modify the data contained in the accelerometer dataset. Each line for which 
                             a date is selected will be used to modify the dataset, whatever the MET value provided. Be careful to provide non-overlapping 
                             periods. If a period overlaps with a previous period, the data of the last period will be used.",
                             style = "line-height: 1.4em"),
                          style = "padding-top: 0px; padding-bottom: 10px; padding-right: 10px; padding-left: 10px"
                          )
                   ),
                 ),
                fluidRow(
                  column(12,
                         h4(""),
                         h4("")
                  ),
                 ),
                 fluidRow(
                   column(12,
                   mod_report_pa_period_ui("period_1"),
                   mod_control_pa_period_view_ui("period_1"),
                  
                   mod_report_pa_period_ui("period_2"),
                   mod_control_pa_period_view_ui("period_2"),
                   
                   mod_report_pa_period_ui("period_3"),
                   mod_control_pa_period_view_ui("period_3"),
                
                   mod_report_pa_period_ui("period_4"),
                   mod_control_pa_period_view_ui("period_4"),
                   
                   mod_report_pa_period_ui("period_5"),
                   mod_control_pa_period_view_ui("period_5"),
                   
                   mod_report_pa_period_ui("period_6"),
                   mod_control_pa_period_view_ui("period_6"),
                   
                   mod_report_pa_period_ui("period_7"),
                   mod_control_pa_period_view_ui("period_7"),
                   
                   mod_report_pa_period_ui("period_8"),
                   mod_control_pa_period_view_ui("period_8"),
                   
                   mod_report_pa_period_ui("period_9"),
                   mod_control_pa_period_view_ui("period_9"),
                   
                   mod_report_pa_period_ui("period_10"),
                   mod_control_pa_period_view_ui("period_10"),
                   
                   mod_report_pa_period_ui("period_11"),
                   mod_control_pa_period_view_ui("period_11"),
                   
                   mod_report_pa_period_ui("period_12"),
                   mod_control_pa_period_view_ui("period_12"),
                   
                   mod_report_pa_period_ui("period_13"),
                   mod_control_pa_period_view_ui("period_13"),
                   
                   mod_report_pa_period_ui("period_14"),
                   mod_control_pa_period_view_ui("period_14"),
                   
                   mod_report_pa_period_ui("period_15"),
                   mod_control_pa_period_view_ui("period_15"),
                  ),
                 ),
                 fluidRow(
                   column(12,
                          shinydashboardPlus::box(id = "box-pa-periods-inputs",
                                                  title = "NOTE",
                                                  width = 12,
                                                  div(textOutput("warning_pa_periods_inputs"), class = "warn-message")
                          ),                   ),
                 ),


                  #********************************
                  # Minimum wear time for valid day
                  #********************************
                  
                  fluidRow(
                    column(12,
                           h3("Configure the inputs required to define the period of the day to be considered to count wear time and to validate a day based on wear time"),
                           hr()
                    ),
                  ),
                  fluidRow(
                    column(3,
                           selectInput("start_day_analysis", "Start of the period to consider each day", 
                                       choices = c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59), hms::as_hms(60*60*23+60*59+59)),
                                       selectize=FALSE
                           )
                    ),
                    column(3,
                           selectInput("end_day_analysis", "End of the period to consider each day", 
                                       choices = c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59), hms::as_hms(60*60*23+60*59+59)),
                                       selected = hms::as_hms(60*60*23+60*59+59),
                                       selectize=FALSE
                           )
                    ),
                    column(3,
                           numericInput("minimum_wear_time_for_analysis", "Minimum wear time to validate a day (hours)", value = 10)
                    )
                  ),

                  fluidRow(
                    column(12,
                           shiny::actionButton("reset_period", "Default settings", class = "btn-return"),
                           shiny::actionButton("pro_active_period_non_sleep", "PROactive config. for non-sleep wearing protocol", class = "btn-proactive"),
                           shiny::actionButton("pro_active_period_24h", "PROactive config. for 24-h wearing protocol", class = "btn-proactive")
                           )
                  ),
                 fluidRow(
                   column(12,
                          shinydashboardPlus::box(id = "box-intensity_inputs",
                                                  title = "NOTE",
                                                  width = 12,
                                                  div(textOutput("warning_intensity_inputs"), class = "warn-message")
                          ),                   ),
                 ),
                  
                  #*****************
                  # Running analysis
                  #*****************
                  
                  fluidRow(
                    align = "center",
                    shiny::actionButton("Run", "Run analysis",  class = "btn-validate")
                    ),
                  
                  #############################
                  # Section 4. Results & Export ----
                  #############################
                  
                  fluidRow(
                    column(12,
                           h2("Section 4. Results & Export"),
                    ),
                  ),  
                  
                       #***********************************************
                       # Panels with metrics describing activity volume
                       #***********************************************
                
                       fluidRow(
                         column(12,
                                tags$div(align = "left",
                                         uiOutput("title_activity_volume_metrics"),
                                )
                         ),
                       ),
                       fluidRow(
                         shinydashboardPlus::box(id = "myBox2", 
                                                 title = "Temporal distribution of activity intensity",
                                                 shinycssloaders::withSpinner(plotOutput("graph_int", height = "auto")), 
                                                 width = 12, 
                                                 height = NULL)
                       ),
                       fluidRow(
                         align = "center",
                         column(4, 
                                selectInput("Metric2", "Data to visualize", metrics)
                                ),
                         column(4, 
                                selectInput("zoom_from_analysis", "Start of the period to visualize", 
                                            choices = c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59), hms::as_hms(60*60*23+60*59+59)),
                                            selectize=FALSE
                                            )
                         ),
                         column(4,
                                selectInput("zoom_to_analysis", "End of the period to visualize", 
                                                   choices = c(hms::as_hms(seq(0, 60*60*23, 60*30)), hms::as_hms(60*60*23+60*59), hms::as_hms(60*60*23+60*59+59)),
                                                   selected = hms::as_hms(60*60*23+60*59+59),
                                                   selectize=FALSE
                                            )
                                ),
                       ),
                       fluidRow(
                         align = "center",
                         shiny::actionButton("update_graphic2", "Update graphic", class = "btn-return")
                       ),
                      
                      fluidRow(
                        column(12,
                          shinydashboardPlus::box(
                            id = "BoxResByDayVolTab", 
                            title = "Results by day: Tabular view",
                            shinycssloaders::withSpinner(reactable::reactableOutput("results_by_day_vol_tab")),
                            width = NULL, 
                            height = NULL
                          )
                        )
                      ),
                        fluidRow(
                          column(12,
                          shinydashboardPlus::box(
                           id = "BoxResByDayVolFig", 
                           title = "Results by day: Graphical view",
                           shinycssloaders::withSpinner(plotOutput("results_by_day_vol_fig", height = "auto")), 
                           width = NULL, 
                           height = NULL
                         )
                          )
                       ),

                                 #****************************************************
                                 # Table of results summarized over valid days: Means
                                 #****************************************************
                       
                       fluidRow(
                         column(12,
                         shinydashboardPlus::box(
                           id = "BoxResVolMeans",
                           title = "Means computed using valid days: Tabular view",
                                shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_vol_means")),
                           width = 12, 
                           height = "auto")
                         ),
                       ),
                       
                                 #******************************************************
                                 # Table of results summarized over valid days: Medians
                                 #******************************************************
                       
                       fluidRow(
                         column(12,
                         shinydashboardPlus::box(
                           id = "BoxResVolMedians",
                           title = "Medians computed using valid days: Tabular view",
                                shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_vol_medians")),
                           width = 12, 
                           height = "auto")
                         ),
                       ),
                
                       fluidRow(
                         column(12,
                                shinydashboardPlus::box(
                                  id = "BoxCompaNormsFig", 
                                  title = "Comparisons with norms and recommendations",
                                  shinycssloaders::withSpinner(plotOutput("compa_norms_fig", height = "auto")), 
                                  width = NULL, 
                                  height = NULL
                                )
                         )
                       ),
                
                      #*********************************************************
                      # Panels with metrics describing step accumulation metrics
                      #*********************************************************
                      
                      fluidRow(
                        column(12,
                               tags$div(align = "left",
                                        uiOutput("title_step_acc_metrics"),
                               )
                        ),
                      ),
                      fluidRow(
                        column(12,
                               shinydashboardPlus::box(
                                 id = "BoxResByDayStepTab", 
                                 title = "Results by day: Tabular view",
                                 shinycssloaders::withSpinner(reactable::reactableOutput("results_by_day_step_tab")),
                                 width = NULL, 
                                 height = NULL
                               )
                        )
                      ),
                      fluidRow(
                        column(12,
                               shinydashboardPlus::box(
                                 id = "BoxResByDayStepFig", 
                                 title = "Results by day: Graphical view",
                                 shinycssloaders::withSpinner(plotOutput("results_by_day_step_fig", height = "auto")), 
                                 width = NULL, 
                                 height = NULL
                               )
                        )
                      ),
                                               #****************************************************
                                               # Table of results summarized over valid days: Means
                                               #****************************************************
                      
                      fluidRow(
                        column(12,
                               shinydashboardPlus::box(
                                 id = "BoxResStepMeans",
                                 title = "Means computed using valid days: Tabular view",
                                 shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_step_means")),
                                 width = 12, 
                                 height = "auto")
                        ),
                      ),
                      
                                                #******************************************************
                                                # Table of results summarized over valid days: Medians
                                                #******************************************************
                      
                      fluidRow(
                        column(12,
                               shinydashboardPlus::box(
                                 id = "BoxResStepMedians",
                                 title = "Medians computed using valid days: Tabular view",
                                 shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_step_medians")),
                                 width = 12, 
                                 height = "auto")
                        ),
                      ),
                      
                     #*******************************************************
                     # Panels with metrics describing intensity distribution
                     #*******************************************************
                     
                     fluidRow(
                       column(12,
                              tags$div(align = "left",
                                       uiOutput("title_int_distri_metrics"),
                              )
                       ),
                     ),
                
                     fluidRow(
                       column(6,
                              shinydashboardPlus::box(
                                id = "BoxResByDayIntDistFig1", 
                                title = "Intensity distribution analysis: Bins",
                                shinycssloaders::withSpinner(plotOutput("int_dist_analysis_fig1", height = "auto")), 
                                width = NULL, 
                                height = NULL
                              )
                       ),
                       column(6,
                              shinydashboardPlus::box(
                                id = "BoxResByDayIntDistFig1Bis", 
                                title = "Intensity distribution analysis: Log-log models",
                                shinycssloaders::withSpinner(plotOutput("int_dist_analysis_fig1bis", height = "auto")), 
                                width = NULL, 
                                height = NULL
                              )
                       ),
                       
                     ),
                     
                       fluidRow(
                         column(12,
                                shinydashboardPlus::box(
                                  id = "BoxResByDayIntDistTab", 
                                  title = "Results by day: Tabular view",
                                  shinycssloaders::withSpinner(reactable::reactableOutput("results_by_day_int_dist_tab")),
                                  width = NULL, 
                                  height = NULL
                                )
                         )
                       ),
                       fluidRow(
                         column(6,
                                shinydashboardPlus::box(
                                  id = "BoxResByDayIntDistFig2", 
                                  title = "Results by day: Graphical view",
                                  shinycssloaders::withSpinner(plotOutput("results_by_day_int_dist_fig", height = "auto")), 
                                  width = NULL, 
                                  height = NULL
                                )
                       ),
                         column(6,
                                shinydashboardPlus::box(
                                  id = "BoxSummaryIntDistFig", 
                                  title = "Means computed using valid days: Graphical view",
                                  shinycssloaders::withSpinner(plotOutput("results_summary_int_dist_fig", height = "auto")), 
                                  width = NULL, 
                                  height = NULL
                                )
                         )
                       ),
                                     
                                           #****************************************************
                                           # Table of results summarized over valid days: Means
                                           #****************************************************
                      
                      fluidRow(
                        column(12,
                               shinydashboardPlus::box(
                                 id = "BoxResIntDistMeans",
                                 title = "Means computed using valid days: Tabular view",
                                 shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_int_dist_means")),
                                 width = 12, 
                                 height = "auto")
                        ),
                      ),
                      
                                           #******************************************************
                                           # Table of results summarized over valid days: Medians
                                           #******************************************************
                      
                      fluidRow(
                        column(12,
                               shinydashboardPlus::box(
                                 id = "BoxResIntDistMedians",
                                 title = "Medians computed using valid days: Tabular view",
                                 shinycssloaders::withSpinner(reactable::reactableOutput("results_summary_int_dist_medians")),
                                 width = 12, 
                                 height = "auto")
                        ),
                      ),

                      #**********************************************************************************
                      # Panels with metrics describing the pattern of accumulation of sedentary behaviour
                      #**********************************************************************************

                      fluidRow(
                        column(12,
                               tags$div(align = "left",
                                        uiOutput("title_SB_accum_metrics"),
                               )
                        ),
                      ),
                      fluidRow(
                        shinydashboardPlus::box(
                          id = "BoxBreaksSB",
                          title = "Temporal distribution of sedentary bouts",
                          shinycssloaders::withSpinner(plotOutput("graph_breaks_SB", height = "auto")), 
                          width = 12, 
                          height = NULL
                          )
                     ),
                     fluidRow(
                       column(3,
                              shinydashboardPlus::box(
                                id = "BoxAlphaSB",
                                title = "Alpha coefficient",
                                shinycssloaders::withSpinner(plotOutput("graph_alpha_SB", height = "auto")), 
                                width = NULL, 
                                height = NULL
                              )
                              ),
                       column(3,
                              shinydashboardPlus::box(
                                id = "BoxMBDSB",
                                title = "Median Bout Duration (MBD)",
                                shinycssloaders::withSpinner(plotOutput("graph_mbd_SB", height = "auto")), 
                                width = NULL, 
                                height = NULL
                                )
                       ),
                       column(3,
                              shinydashboardPlus::box(
                                id = "BoxUBDSB",
                                title = "Usual Bout Duration (UBD)",
                                shinycssloaders::withSpinner(plotOutput("graph_ubd_SB", height = "auto")), 
                                width = NULL, 
                                height = NULL
                              )
                       ),
                       column(3,
                              shinydashboardPlus::box(
                                id = "BoxGiniSB",
                                title = "Gini index",
                                shinycssloaders::withSpinner(plotOutput("graph_gini_SB", height = "auto")), 
                                width = NULL, 
                                height = NULL
                              )
                       ),
                       
                     ),
                
                      #*******************************************************************************
                      # Panels with metrics describing the pattern of accumulation of physical activity
                      #*******************************************************************************
                      
                      fluidRow(
                        column(12,
                               tags$div(align = "left",
                                        uiOutput("title_PA_accum_metrics"),
                               )
                        ),
                      ),
                      fluidRow(
                        shinydashboardPlus::box(
                          id = "BoxBreaksPA",
                          title = "Temporal distribution of physical activity bouts",
                          shinycssloaders::withSpinner(plotOutput("graph_breaks_PA", height = "auto")), 
                          width = 12, 
                          height = NULL
                        )
                      ),
                      fluidRow(
                        column(3,
                               shinydashboardPlus::box(
                                 id = "BoxAlphaPA",
                                 title = "Alpha coefficient",
                                 shinycssloaders::withSpinner(plotOutput("graph_alpha_PA", height = "auto")), 
                                 width = NULL, 
                                 height = NULL
                               )
                        ),
                        column(3,
                               shinydashboardPlus::box(
                                 id = "BoxMBDPA",
                                 title = "Median Bout Duration (MBD)",
                                 shinycssloaders::withSpinner(plotOutput("graph_mbd_PA", height = "auto")), 
                                 width = NULL, 
                                 height = NULL
                               )
                        ),
                        column(3,
                               shinydashboardPlus::box(
                                 id = "BoxUBDPA",
                                 title = "Usual Bout Duration (UBD)",
                                 shinycssloaders::withSpinner(plotOutput("graph_ubd_PA", height = "auto")), 
                                 width = NULL, 
                                 height = NULL
                               )
                        ),
                        column(3,
                               shinydashboardPlus::box(
                                 id = "BoxGiniPA",
                                 title = "Gini index",
                                 shinycssloaders::withSpinner(plotOutput("graph_gini_PA", height = "auto")), 
                                 width = NULL, 
                                 height = NULL
                               )
                        ),
                        
                      ),
                             
      
                  #******************
                  # Export
                  #******************
                  fluidRow(
                    column(12,
                           shinydashboardPlus::box(id = "box-no-valid-days",
                                                   title = "NOTE",
                                                   width = 12,
                                                   div(textOutput("warning_no_valid_days"), class = "warn-message")
                           )
                    ),
                  ),
                  fluidRow(
                    column(12,
                           downloadButton("report_en_long", "Generate long report (EN) (.html)", class = "btn-report"),
                           downloadButton("report_fr_long", "Generate long report (FR) (.html)", class = "btn-report"),
                           downloadButton("report_en_short", "Generate short report (EN) (.pdf)", class = "btn-report"),
                           downloadButton("report_fr_short", "Generate short report (FR) (.pdf)", class = "btn-report"),
                           downloadButton("ExpDataset", "Export marked dataset (.csv)", class = "btn-secondary"),
                           downloadButton("ExpResultsByDays", "Export results by day (.csv)", class = "btn-secondary"),
                           downloadButton("ExpDailySummaryMeans", "Export daily summary (means) (.csv)", class = "btn-secondary"),
                           downloadButton("ExpDailySummaryMedians", "Export daily summary (medians) (.csv)",  class = "btn-secondary"),
                           actionButton("go_to_proactive_q", "Go to PROactive questionnaire", class = "btn-proactive")
                    ),
                    
                  )
                
          ), # End first tab
          
          
          # $$$$$$$$$$$$$$$$$$
          # Second tab content ----
          # $$$$$$$$$$$$$$$$$$
          
          shinydashboard::tabItem(tabName = "proactive",
                  
                 ####################
                 # Panel C-PPAC (EN) ----
                 ####################
                 
                 tabsetPanel(
                   id = "questionnaires", 
                    tabPanel("C-PPAC (EN)",
                    wellPanel(
                      fluidRow(
                        column(12,
                               "Reference: Gimeno-Santos et al. European Respiratory Journal 2015:46 988\u201310008 [reproduced according to the terms Creative Commons Attribution Non-Commercial Licence 4.0; https://creativecommons.org/licenses/by-nc/4.0]", style = "font-weight: bold;"
                        )
                      ),
                      fluidRow(
                        column(12,
                               h4("")
                        )
                      ),
                       fluidRow(
                         column(12,
                                 "INSTRUCTIONS TO PATIENTS:
                                  Patients with chronic lung disease like you often report that they have problems during physical activity. 
                                By physical activity, we mean all activities that require movement of your body. Examples are household activities, walking, going to work,
                                or getting dressed. However, please consider all activities you do, and not only these examples. We would like to know how you experienced 
                                your physical activity IN THE PAST 7 DAYS."
                                ),
                         ),
                       fluidRow(
                         column(12,
                                h4("")
                         )
                       ),
                       fluidRow(
                         column(12,
                                "Please select the box next to the response that best applies to you IN THE PAST 7 DAYS."
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h4("")
                         )
                       ),
                       fluidRow(
                         column(12,
                                "There are no wrong answers. We very much value your response."
                         ),
                       ),
                       ),      
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="cppac_EN_q1", label="In the past 7 days, how much walking did you do outside\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (about 10 minutes every day)",
                                                 "Some (about 30 minutes every day)",
                                                 "A lot (about 1 hour every day)",
                                                 "A great deal (more than 1 hour every day)"
                                               )
                                ),
                         ),
     
                        )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="cppac_EN_q2", label="In the past 7 days, how many chores did you do outside the house\u003f 
                                                   Some examples are gardening, taking the rubbish out, or doing small errands\u003f",
                                                   choices=c(
                                                     "None at all",
                                                     "A few",
                                                     "Some",
                                                     "A lot",
                                                     "A large amount"
                                                   )
                                ),
                         ),
                        ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="cppac_EN_q3", label="In the past 7 days, how much difficulty did you have getting dressed\u003f",
                                                   choices=c(
                                                    "None at all",
                                                    "A little bit",
                                                    "Some",
                                                    "A lot",
                                                    "A great deal"
                                                   )
                                ),
                         ),
                        ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="cppac_EN_q4", label="In the past 7 days, how much difficulty did you have getting out and about\u003f",
                                                   choices=c(
                                                    "None at all",
                                                     "A little bit",
                                                     "Some",
                                                     "A lot",
                                                     "A great deal"
                                                     
                                                   )
                                ),
                         ),
                        ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="cppac_EN_q5", label="In the past 7 days, how often did you avoid doing activities because of your lung problems\u003f",
                                                   choices=c(
                                                     "Not at all",
                                                     "Rarely",
                                                     "Sometimes",
                                                     "Frequently",
                                                     "All the time"
                                                   )
                                ),
                         ),
                        ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="cppac_EN_q6", label="In the past 7 days, how breathless were you in general during your activities\u003f",
                                                   choices=c(
                                                     "Not at all",
                                                     "A little bit",
                                                     "Moderately",
                                                     "Very",
                                                     "Extremely"
                                                     
                                                   )
                                ),
                         ),
                         
                       ),
                       
                     ), 
                     fluidRow(
                       column(12,
                              wellPanel(
                                radioButtons(inputId="cppac_EN_q7", label="In the past 7 days, how often did you lack physical strength to do things because of your lung problems\u003f",
                                                   choices=c(
                                                     "Not at all",
                                                     "Rarely",
                                                     "Sometimes",
                                                     "Frequently",
                                                     "All the time"
                                                     
                                                   )
                                ),
                              ),
                              
                       ),
                       
                     ),
                     fluidRow(
                       column(12,
                              wellPanel(
                                radioButtons(inputId="cppac_EN_q8", label="In the past 7 days, how tired were you in general during your activities\u003f",
                                                   choices=c(
                                                     "Not at all",
                                                     "A little bit",
                                                     "Moderately",
                                                     "Very",
                                                     "Extremely"
                                                     
                                                   )
                                ),
                              ),
                              
                       ),
                       
                     ),
                     fluidRow(
                       column(12,
                              wellPanel(
                                radioButtons(inputId="cppac_EN_q9", label="In the past 7 days, how often did you have to take breaks during your physical activities\u003f",
                                                   choices=c(
                                                     "Not at all",
                                                     "Rarely",
                                                     "Sometimes",
                                                     "Frequently",
                                                     "All the time"
                                                     
                                                   )
                                ),
                              ),
                              
                       ),
                       
                     ),
                     fluidRow(
                       column(12,
                              wellPanel(
                                radioButtons(inputId="cppac_EN_q10", label="In the past 7 days, how breathless were you when walking on level ground indoors and outdoors\u003f",
                                                   choices=c(
                                                     "Not at all",
                                                     "A little bit",
                                                     "Moderately",
                                                     "Very",
                                                     "Extremely"
                                                     
                                                   )
                                ),
                              ),
                              
                       ),
                       
                     ),
                     fluidRow(
                       column(12,
                              wellPanel(
                                radioButtons(inputId="cppac_EN_q11", label="In the past 7 days, how much time did you need to recover from your physical activities\u003f",
                                                   choices=c(
                                                     "Not at all",
                                                     "A little bit",
                                                     "Some",
                                                     "A lot",
                                                     "A great deal"
                                                     
                                                   )
                                ),
                              ),
                              
                       ),
                       
                     ),
                     fluidRow(
                       column(12,
                              wellPanel(
                                radioButtons(inputId="cppac_EN_q12", label="In the past 7 days, did you need to consider your lung problems when you planned your activities because of your lung problems\u003f Examples are a trip out, an appointment or expecting visitors.",
                                                   choices=c(
                                                     "No",
                                                     "A little bit",
                                                     "Sometimes",
                                                     "A lot",
                                                     "A great deal"
                                                     
                                                   )
                                ),
                              ),
                              
                       ),
                       
                     ),
                    fluidRow(
                      column(6,
                             wellPanel("Accelerometer steps score (based on the daily median of valid days)", class = "control-label",
                                       h4(""),
                             reactable::reactableOutput("table_cppac_en_steps_med")
                             )
                     ),
                     column(6,
                            wellPanel("Accelerometer steps score (Based on the daily mean of valid days)", class = "control-label",
                                      h4(""),
                              reactable::reactableOutput("table_cppac_en_steps_mean")
                            )
                     )
                    ),
                    fluidRow(
                      column(6,
                             wellPanel("Accelerometer VMU score (based on the daily median of valid days)", class = "control-label",
                                       h4(""),
                               reactable::reactableOutput("table_cppac_en_vmu_med")
                             )
                      ),
                      column(6,
                             wellPanel("Accelerometer VMU score (based on the daily mean of valid days)", class = "control-label",
                                       h4(""),
                               reactable::reactableOutput("table_cppac_en_vmu_mean")
                             )
                      )
                    ),
                    fluidRow(
                      column(12,
                             h2("Summary"),
                             h4("")
                      ),
                    ),
                    fluidRow(
                      column(12,
                             wellPanel(
                             radioButtons(inputId="cppac_EN_summary_metric", label="Do you want to use PROactive steps / VMU scores based on the MEDIANS or the MEANS of valid days\u003f",
                                          choices=c(
                                            "Scores based on MEDIANS",
                                            "Scores based on MEANS"
                                          )
                             )
                        ),
                      ),
                    ),
                    fluidRow(
                      column(1,
                             shiny::actionButton("get_cppac_summary_en", "Results / Update", class = "btn-validate"),
                             h3("")
                      ),
                    ),
                    
                    fluidRow(
                      column(6,
                             h4(""),
                             reactable::reactableOutput("PROactive_scores_cppac_summary_en"), 
                             ),
                      column(6,
                             h4(""),
                             shinydashboard::valueBoxOutput("infoBox_cppac_en_total_diff"),
                             shinydashboard::valueBoxOutput("infoBox_cppac_en_total_amount"),
                             shinydashboard::valueBoxOutput("infoBox_cppac_en_total_all"),
                             h4(""),
                             h4(""),
                             shinydashboard::valueBoxOutput("infoBox_cppac_en_total_diff_rasch"),
                             shinydashboard::valueBoxOutput("infoBox_cppac_en_total_amount_rasch"),
                             shinydashboard::valueBoxOutput("infoBox_cppac_en_total_all_rasch"),
                             ),
                      
                    ),
                    fluidRow(
                      column(12,
                             h4(""),
                             h4(""),
                             downloadButton("report_en_cppac_html", "Generate C-PPAC report (.html)", class = "btn-report"),
                             downloadButton("report_en_cppac_pdf", "Generate C-PPAC report (.pdf)", class = "btn-report")
                             )
                    )
                    
                   ), # End of tabPanel
                   
                   
                ###################
                # Panel D-PPAC (EN) ----
                ###################
                  
                tabPanel("D-PPAC (EN)",
                       wellPanel(
                         fluidRow(
                           column(12,
                                  "Reference: Gimeno-Santos et al. European Respiratory Journal 2015:46 988\u201310008 [reproduced according to the terms Creative Commons Attribution Non-Commercial Licence 4.0; https://creativecommons.org/licenses/by-nc/4.0]", style = "font-weight: bold;"
                           )
                         ),
                         fluidRow(
                           column(12,
                                  h4("")
                           )
                         ),
                         fluidRow(
                           column(12,
                               "INSTRUCTIONS TO PATIENTS DAY 1:
                                Patients with chronic lung disease like you often report that they have problems during physical activity. 
                                By physical activity, we mean all activities that require movement of your body. Examples are household activities, 
                                walking, going to work, or getting dressed. However, please consider all activities you do, and not only these examples. 
                                We would like to know how you experienced your physical activity since you woke up TODAY."
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h4("")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  "Please complete this questionnaire in the evening before going to bed. Please select the box next to the response that 
                                  best applies to you TODAY."
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h4("")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  "There are no wrong answers. We very much value your response."
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h4("")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  "INSTRUCTIONS FOR SUBSEQUENT DAYS: We would like to know how you experienced your physical activity since you woke up TODAY. 
                                  Please complete this questionnaire in the evening before going to bed. 
                                  Please select the box next to the response that best applies to you TODAY."
                           ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Day 1"),
                                h4()
                                ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d1_q1", label="Day 1. How much walking did you do outside today\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (up to 10 minutes in total)",
                                                 "Some (up to 30 minutes in total)",
                                                 "A lot (up to 1 hour in total)",
                                                 "A great deal (more than 1 hour in total)"
                                               )
                                  ),
                                ),
                                
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d1_q2", label="Day 1. How many chores did you do outside the house today\u003f 
                                               Some examples are gardening, taking the rubbish out, or doing small errands.",
                                               choices=c(
                                                 "None at all",
                                                 "A few",
                                                 "Some",
                                                 "A lot",
                                                 "A large amount"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d1_q3", label="Day 1. How much difficulty did you have getting dressed today\u003f",
                                               choices=c(
                                                 "None at all",
                                                 "A little bit",
                                                 "Some",
                                                 "A lot",
                                                 "A great deal"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d1_q4", label="Day 1. How often did you avoid doing activities because of your lung problems today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                         ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d1_q5", label="Day 1. How breathless were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ), 
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d1_q6", label="Day 1. How tired were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d1_q7", label="Day 1. How often did you have to take breaks during your physical activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(6,
                                wellPanel("Day 1. Accelerometer steps score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d1_steps")
                                )
                         ),
                         column(6,
                                wellPanel("Day 1. Accelerometer VMU score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d1_vmu")
                                )
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Day 2"),
                                h4()
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d2_q1", label="Day 2. How much walking did you do outside today\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (up to 10 minutes in total)",
                                                 "Some (up to 30 minutes in total)",
                                                 "A lot (up to 1 hour in total)",
                                                 "A great deal (more than 1 hour in total)"
                                               )
                                  ),
                                ),
                                
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d2_q2", label="Day 2. How many chores did you do outside the house today\u003f 
                                               Some examples are gardening, taking the rubbish out, or doing small errands.",
                                               choices=c(
                                                 "None at all",
                                                 "A few",
                                                 "Some",
                                                 "A lot",
                                                 "A large amount"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d2_q3", label="Day 2. How much difficulty did you have getting dressed today\u003f",
                                               choices=c(
                                                 "None at all",
                                                 "A little bit",
                                                 "Some",
                                                 "A lot",
                                                 "A great deal"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d2_q4", label="Day 2. How often did you avoid doing activities because of your lung problems today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                         ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d2_q5", label="Day 2. How breathless were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ), 
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d2_q6", label="Day 2. How tired were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d2_q7", label="Day 2. How often did you have to take breaks during your physical activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(6,
                                wellPanel("Day 2. Accelerometer steps score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d2_steps")
                                )
                         ),
                         column(6,
                                wellPanel("Day 2. Accelerometer VMU score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d2_vmu")
                                )
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Day 3"),
                                h4()
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d3_q1", label="Day 3. How much walking did you do outside today\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (up to 10 minutes in total)",
                                                 "Some (up to 30 minutes in total)",
                                                 "A lot (up to 1 hour in total)",
                                                 "A great deal (more than 1 hour in total)"
                                               )
                                  ),
                                ),
                                
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d3_q2", label="Day 3. How many chores did you do outside the house today\u003f 
                                               Some examples are gardening, taking the rubbish out, or doing small errands.",
                                               choices=c(
                                                 "None at all",
                                                 "A few",
                                                 "Some",
                                                 "A lot",
                                                 "A large amount"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d3_q3", label="Day 3. How much difficulty did you have getting dressed today\u003f",
                                               choices=c(
                                                 "None at all",
                                                 "A little bit",
                                                 "Some",
                                                 "A lot",
                                                 "A great deal"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d3_q4", label="Day 3. How often did you avoid doing activities because of your lung problems today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                         ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d3_q5", label="Day 3. How breathless were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ), 
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d3_q6", label="Day 3. How tired were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d3_q7", label="Day 3. How often did you have to take breaks during your physical activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(6,
                                wellPanel("Day 3. Accelerometer steps score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d3_steps")
                                )
                         ),
                         column(6,
                                wellPanel("Day 3. Accelerometer VMU score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d3_vmu")
                                )
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Day 4"),
                                h4()
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d4_q1", label="Day 4. How much walking did you do outside today\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (up to 10 minutes in total)",
                                                 "Some (up to 30 minutes in total)",
                                                 "A lot (up to 1 hour in total)",
                                                 "A great deal (more than 1 hour in total)"
                                               )
                                  ),
                                ),
                                
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d4_q2", label="Day 4. How many chores did you do outside the house today\u003f 
                                               Some examples are gardening, taking the rubbish out, or doing small errands.",
                                               choices=c(
                                                 "None at all",
                                                 "A few",
                                                 "Some",
                                                 "A lot",
                                                 "A large amount"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d4_q3", label="Day 4. How much difficulty did you have getting dressed today\u003f",
                                               choices=c(
                                                 "None at all",
                                                 "A little bit",
                                                 "Some",
                                                 "A lot",
                                                 "A great deal"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d4_q4", label="Day 4. How often did you avoid doing activities because of your lung problems today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                         ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d4_q5", label="Day 4. How breathless were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ), 
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d4_q6", label="Day 4. How tired were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d4_q7", label="Day 4. How often did you have to take breaks during your physical activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(6,
                                wellPanel("Day 4. Accelerometer steps score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d4_steps")
                                )
                         ),
                         column(6,
                                wellPanel("Day 4. Accelerometer VMU score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d4_vmu")
                                )
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Day 5"),
                                h4()
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d5_q1", label="Day 5. How much walking did you do outside today\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (up to 10 minutes in total)",
                                                 "Some (up to 30 minutes in total)",
                                                 "A lot (up to 1 hour in total)",
                                                 "A great deal (more than 1 hour in total)"
                                               )
                                  ),
                                ),
                                
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d5_q2", label="Day 5. How many chores did you do outside the house today\u003f 
                                               Some examples are gardening, taking the rubbish out, or doing small errands.",
                                               choices=c(
                                                 "None at all",
                                                 "A few",
                                                 "Some",
                                                 "A lot",
                                                 "A large amount"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d5_q3", label="Day 5. How much difficulty did you have getting dressed today\u003f",
                                               choices=c(
                                                 "None at all",
                                                 "A little bit",
                                                 "Some",
                                                 "A lot",
                                                 "A great deal"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d5_q4", label="Day 5. How often did you avoid doing activities because of your lung problems today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                         ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d5_q5", label="Day 5. How breathless were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ), 
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d5_q6", label="Day 5. How tired were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d5_q7", label="Day 5. How often did you have to take breaks during your physical activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(6,
                                wellPanel("Day 5. Accelerometer steps score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d5_steps")
                                )
                         ),
                         column(6,
                                wellPanel("Day 5. Accelerometer VMU score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d5_vmu")
                                )
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Day 6"),
                                h4()
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d6_q1", label="Day 6. How much walking did you do outside today\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (up to 10 minutes in total)",
                                                 "Some (up to 30 minutes in total)",
                                                 "A lot (up to 1 hour in total)",
                                                 "A great deal (more than 1 hour in total)"
                                               )
                                  ),
                                ),
                                
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d6_q2", label="Day 6. How many chores did you do outside the house today\u003f 
                                               Some examples are gardening, taking the rubbish out, or doing small errands.",
                                               choices=c(
                                                 "None at all",
                                                 "A few",
                                                 "Some",
                                                 "A lot",
                                                 "A large amount"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d6_q3", label="Day 6. How much difficulty did you have getting dressed today\u003f",
                                               choices=c(
                                                 "None at all",
                                                 "A little bit",
                                                 "Some",
                                                 "A lot",
                                                 "A great deal"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d6_q4", label="Day 6. How often did you avoid doing activities because of your lung problems today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                         ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d6_q5", label="Day 6. How breathless were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ), 
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d6_q6", label="Day 6. How tired were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d6_q7", label="Day 6. How often did you have to take breaks during your physical activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(6,
                                wellPanel("Day 6. Accelerometer steps score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d6_steps")
                                )
                         ),
                         column(6,
                                wellPanel("Day 6. Accelerometer VMU score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d6_vmu")
                                )
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Day 7"),
                                h4()
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d7_q1", label="Day 7. How much walking did you do outside today\u003f", 
                                               choices=c(
                                                 "None at all",
                                                 "A little bit (up to 10 minutes in total)",
                                                 "Some (up to 30 minutes in total)",
                                                 "A lot (up to 1 hour in total)",
                                                 "A great deal (more than 1 hour in total)"
                                               )
                                  ),
                                ),
                                
                         )
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d7_q2", label="Day 7. How many chores did you do outside the house today\u003f 
                                               Some examples are gardening, taking the rubbish out, or doing small errands.",
                                               choices=c(
                                                 "None at all",
                                                 "A few",
                                                 "Some",
                                                 "A lot",
                                                 "A large amount"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d7_q3", label="Day 7. How much difficulty did you have getting dressed today\u003f",
                                               choices=c(
                                                 "None at all",
                                                 "A little bit",
                                                 "Some",
                                                 "A lot",
                                                 "A great deal"
                                               )
                                  ),
                                ),
                         ),
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d7_q4", label="Day 7. How often did you avoid doing activities because of your lung problems today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                         ), 
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d7_q5", label="Day 7. How breathless were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ), 
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d7_q6", label="Day 7. How tired were you in general during your activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "A little bit",
                                                 "Moderately",
                                                 "Very",
                                                 "Extremely"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(12,
                                wellPanel(
                                  radioButtons(inputId="dppac_EN_d7_q7", label="Day 7. How often did you have to take breaks during your physical activities today\u003f",
                                               choices=c(
                                                 "Not at all",
                                                 "Rarely",
                                                 "Sometimes",
                                                 "Frequently",
                                                 "All the time"
                                                 
                                               )
                                  ),
                                ),
                                
                         ),
                         
                       ),
                       fluidRow(
                         column(6,
                                wellPanel("Day 7. Accelerometer steps score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d7_steps")
                                )
                         ),
                         column(6,
                                wellPanel("Day 7. Accelerometer VMU score", class = "control-label",
                                          h4(""),
                                          reactable::reactableOutput("table_dppac_en_d7_vmu")
                                )
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h2("Summary"),
                                h4("")
                         ),
                       ),
                       fluidRow(
                         column(12,
                                shiny::actionButton("get_dppac_summary_en", "Results / Update", class = "btn-validate"),
                         ),
                       ),
                       fluidRow(
                         column(6,
                                h4(""),
                                reactable::reactableOutput("PROactive_scores_dppac_summary_en"), 
                         ),
                         column(6,
                                h4(""),
                                shinydashboard::valueBoxOutput("infoBox_dppac_en_total_diff"),
                                shinydashboard::valueBoxOutput("infoBox_dppac_en_total_amount"),
                                shinydashboard::valueBoxOutput("infoBox_dppac_en_total_all"),
                                h4(""),
                                h4(""),
                                shinydashboard::valueBoxOutput("infoBox_dppac_en_total_diff_rasch"),
                                shinydashboard::valueBoxOutput("infoBox_dppac_en_total_amount_rasch"),
                                shinydashboard::valueBoxOutput("infoBox_dppac_en_total_all_rasch")
                         ),
                       ),
                       fluidRow(
                         column(12,
                                h4(""),
                                h4(""),
                                downloadButton("report_en_dppac_html", "Generate D-PPAC report (.html)", class = "btn-report"),
                                downloadButton("report_en_dppac_pdf", "Generate D-PPAC report (.pdf)", class = "btn-report")
                         )
                       )
                       
                         
                 ), # End of tabPanel
                 
                ###################
                # Panel C-PPAC (FR) ----
                ###################
                
                tabPanel("C-PPAC (FR)",

                        wellPanel(
                         fluidRow(
                           column(12,
                           "R\u00e9f\u00e9rence : Vaidya et al. International Journal of Chronic Obstructive Pulmonary Disease 2020:15 471\u2013478 [reproduced according to the terms Creative Commons Attribution Non-Commercial Licence 3.0; https://creativecommons.org/licenses/by-nc/3.0/]", style = "font-weight: bold;"
                           )
                         ),
                         fluidRow(
                           column(12,
                                  h4("")
                            )
                         ),
                         fluidRow(
                           column(12,

                                    "INSTRUCTIONS POUR LES PATIENTS : 
                                    Les patients souffrant d\u2019une maladie pulmonaire chronique comme vous signalent qu\u2019ils connaissent des probl\u00e8mes au cours de leurs activit\u00e9s physiques. 
                                    Par activit\u00e9s physiques, nous entendons toutes les activit\u00e9s n\u00e9cessitant un mouvement de votre corps. (Exemples : les activit\u00e9s domestiques, la marche, aller au travail ou s\u2019habiller). Veuillez prendre en compte toutes les activit\u00e9s que vous r\u00e9alisez, et pas seulement ces exemples. Nous aimerions savoir comment vous avez v\u00e9cu vos activit\u00e9s physiques AU COURS DES 7 DERNIERS JOURS."
                                  ),
                           ),
                         fluidRow(
                           column(12,
                                  h4("")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  "Veuillez cocher la case de la r\u00e9ponse qui vous correspond le mieux AU COURS DES 7 DERNIERS JOURS."
                                  )
                          ),
                         fluidRow(
                           column(12,
                                  h4("")
                           )
                         ),
                         fluidRow(
                           column(12,
                                  "Il n\u2019y a pas de mauvaise r\u00e9ponse. Nous vous remercions de votre participation."
                           )
                          ),
                         ),  
                         
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q1", label="Au cours des 7 derniers jours, avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (environ 10 minutes chaque jour)",
                                                   "Un peu (environ 30 minutes chaque jour)",
                                                   "Beaucoup (environ 1 heure chaque jour)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure chaque jour)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q2", label="Au cours des 7 derniers jours, avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou faire des petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q3", label="Au cours des 7 derniers jours, avez-vous eu des difficult\u00e9s pour vous habiller \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q4", label="Au cours des 7 derniers jours, avez-vous eu des difficult\u00e9s pour sortir de chez vous \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q5", label="Au cours des 7 derniers jours, avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q6", label="Au cours des 7 derniers jours, \u00e9tiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q7", label="Au cours des 7 derniers jours, avez-vous manqu\u00e9 de force pour effectuer des t\u00e2ches \u00e0 cause de vos probl\u00e8mes respiratoires \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q8", label="Au cours des 7 derniers jours, \u00e9tiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q9", label="Au cours des 7 derniers jours, avez-vous d\u00fb faire des pauses pendant vos activit\u00e9s physiques \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q10", label="Au cours des 7 derniers jours, \u00e9tiez-vous essouffl\u00e9(e) lors de la marche sur terrain plat, \u00e0 l\u2019int\u00e9rieur et \u00e0 l\u2019ext\u00e9rieur \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q11", label="Au cours des 7 derniers jours, combien de temps vous a-t-il fallu pour r\u00e9cup\u00e9rer de vos activit\u00e9s physiques \u003f",
                                                 choices=c(
                                                   "Aucun",
                                                   "Un petit peu",
                                                   "Un peu",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="cppac_FR_q12", label="Au cours des 7 derniers jours, avez-vous eu besoin de prendre en compte vos probl\u00e8mes respiratoires lorsque vous avez planifi\u00e9 vos activit\u00e9s \u003f 
                                                 (exemples : une sortie, un rendez-vous ou recevoir des invit\u00e9s)",
                                                 choices=c(
                                                   "Non",
                                                   "Un petit peu",
                                                   "Quelques fois",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Score acc\u00e9l\u00e9rom\u00e9trique pour les pas (\u00e0 partir de la m\u00e9diane journali\u00e8re des jours valides)", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_cppac_en_steps_med_fr")
                                  )
                           ),
                           column(6,
                                  wellPanel("Score acc\u00e9l\u00e9rom\u00e9trique pour les pas (\u00e0 partir de la moyenne journali\u00e8re des jours valides)", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_cppac_en_steps_mean_fr")
                                  )
                           )
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU (\u00e0 partir de la m\u00e9diane journali\u00e8re des jours valides)", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_cppac_en_vmu_med_fr")
                                  )
                           ),
                           column(6,
                                  wellPanel("Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU (\u00e0 partir de la moyenne journali\u00e8re des jours valides)", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_cppac_en_vmu_mean_fr")
                                  )
                           ),
                         ),
                        fluidRow(
                          column(12,
                                 h2("Bilan"),
                                 h4()
                          ),
                        ),
                        fluidRow(
                          column(12,
                                 wellPanel(
                                   radioButtons(inputId="cppac_FR_summary_metric", label="Voulez-vous utiliser les scores de pas / VMU PROactive calcul\u00e9s \u00e0 partir des M\u00c9DIANES ou des MOYENNES
                                   des jours valides\u003f",
                                                choices=c(
                                                  "Scores calcul\u00e9s \u00e0 partir des M\u00c9DIANES",
                                                  "Scores calcul\u00e9s \u00e0 partir des MOYENNES"
                                                )
                                   )
                                 ),
                          ),
                        ),
                        fluidRow(
                          column(1,
                                 shiny::actionButton("get_cppac_summary_fr", "R\u00e9sulats / Actualiser", class = "btn-validate"),
                                 h3("")
                          ),
                        ),
                        fluidRow(
                          column(6,
                                 h4(""),
                                 reactable::reactableOutput("PROactive_scores_cppac_summary_fr"), 
                          ),
                          column(6,
                                 h4(""),
                                 shinydashboard::valueBoxOutput("infoBox_cppac_fr_total_diff"),
                                 shinydashboard::valueBoxOutput("infoBox_cppac_fr_total_amount"),
                                 shinydashboard::valueBoxOutput("infoBox_cppac_fr_total_all"),
                                 h4(""),
                                 h4(""),
                                 shinydashboard::valueBoxOutput("infoBox_cppac_fr_total_diff_rasch"),
                                 shinydashboard::valueBoxOutput("infoBox_cppac_fr_total_amount_rasch"),
                                 shinydashboard::valueBoxOutput("infoBox_cppac_fr_total_all_rasch")
                          ),
                          ),
                        fluidRow(
                          column(12,
                                 h4(""),
                                 h4(""),
                                 downloadButton("report_fr_cppac_html", "G\u00e9n\u00e9rer le rapport du C-PPAC (.html)", class = "btn-report"),
                                 downloadButton("report_fr_cppac_pdf", "G\u00e9n\u00e9rer le rapport du C-PPAC (.pdf)", class = "btn-report")
                                 )
                        ),
                ), # End of tabPanel
                
                ###################
                # Panel D-PPAC (FR) ----
                ###################
                
                tabPanel("D-PPAC (FR)",
                         wellPanel(
                           fluidRow(
                             column(12,
                                    "R\u00e9f\u00e9rence : Vaidya et al. International Journal of Chronic Obstructive Pulmonary Disease 2020:15 471\u2013478 [reproduced according to the terms Creative Commons Attribution Non-Commercial Licence 3.0; https://creativecommons.org/licenses/by-nc/3.0/]", style = "font-weight: bold;"
                             )
                           ),
                           fluidRow(
                             column(12,
                                    h4("")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    "INSTRUCTIONS POUR LES PATIENTS :
                               Les patients souffrant d\u2019une maladie pulmonaire chronique comme vous signalent qu\u2019ils connaissent des probl\u00e8mes au 
                                    cours de leurs activit\u00e9s physiques. Par activit\u00e9s physiques, nous entendons toutes les activit\u00e9s n\u00e9cessitant un
                                    mouvement de votre corps. (Exemples : les activit\u00e9s domestiques, la marche, aller au travail ou s\u2019habiller). 
                                    Veuillez prendre en compte toutes les activit\u00e9s que vous r\u00e9alisez, et pas seulement ces exemples. Nous aimerions savoir 
                                    comment vous avez v\u00e9cu vos activit\u00e9s physiques depuis que vous vous \u00eates r\u00e9veill\u00e9(e) AUJOURD\u2019HUI."
                             ),
                           ),
                           fluidRow(
                             column(12,
                                    h4("")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    "Veuillez remplir ce questionnaire le soir avant de vous coucher. Veuillez cocher la case de la r\u00e9ponse qui vous correspond le mieux AUJOURD\u2019HUI."
                             ),
                           ),
                           fluidRow(
                             column(12,
                                    h4("")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    "Il n\u2019y a pas de mauvaise r\u00e9ponse. Nous vous remercions de votre participation."
                             ),
                           ),
                           fluidRow(
                             column(12,
                                    h4("")
                             )
                           ),
                           fluidRow(
                             column(12,
                                    "INSTRUCTIONS POUR LES JOURS SUIVANTS : Nous aimerions savoir comment vous avez v\u00e9cu vos 
                                    activit\u00e9s physiques depuis que vous vous \u00eates r\u00e9veill\u00e9(e) AUJOURD\u2019HUI. Veuillez remplir ce questionnaire le soir avant de vous coucher. 
                                    Veuillez cocher la case de la r\u00e9ponse qui vous correspond le mieux AUJOURD\u2019HUI."
                             ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h2("Jour 1"),
                                  h4()
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d1_q1", label="Jour 1. Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)",
                                                   "Un peu (jusqu\u2019\u00e0 30 minutes au total)",
                                                   "Beaucoup (jusqu\u2019\u00e0 1 heure au total)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure au total)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d1_q2", label="Jour 1. Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou effectuer de petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d1_q3", label="Jour 1. Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d1_q4", label="Jour 1. Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d1_q5", label="Jour 1. Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d1_q6", label="Jour 1. Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d1_q7", label="Jour 1. Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Jour 1. Score acc\u00e9l\u00e9rom\u00e9trique pour les pas", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d1_steps")
                                  )
                           ),
                           column(6,
                                  wellPanel("Jour 1. Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d1_vmu")
                                  )
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h2("Jour 2"),
                                  h4()
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d2_q1", label="Jour 2. Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)",
                                                   "Un peu (jusqu\u2019\u00e0 30 minutes au total)",
                                                   "Beaucoup (jusqu\u2019\u00e0 1 heure au total)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure au total)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d2_q2", label="Jour 2. Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou effectuer de petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d2_q3", label="Jour 2. Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d2_q4", label="Jour 2. Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d2_q5", label="Jour 2. Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d2_q6", label="Jour 2. Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d2_q7", label="Jour 2. Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Jour 2. Score acc\u00e9l\u00e9rom\u00e9trique pour les pas", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d2_steps")
                                  )
                           ),
                           column(6,
                                  wellPanel("Jour 2. Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d2_vmu")
                                  )
                           ),
                         ),
                         
                         fluidRow(
                           column(12,
                                  h2("Jour 3"),
                                  h4()
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d3_q1", label="Jour 3. Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)",
                                                   "Un peu (jusqu\u2019\u00e0 30 minutes au total)",
                                                   "Beaucoup (jusqu\u2019\u00e0 1 heure au total)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure au total)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d3_q2", label="Jour 3. Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou effectuer de petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d3_q3", label="Jour 3. Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d3_q4", label="Jour 3. Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d3_q5", label="Jour 3. Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d3_q6", label="Jour 3. Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d3_q7", label="Jour 3. Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Jour 3. Score acc\u00e9l\u00e9rom\u00e9trique pour les pas", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d3_steps")
                                  )
                           ),
                           column(6,
                                  wellPanel("Jour 3. Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d3_vmu")
                                  )
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h2("Jour 4"),
                                  h4()
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d4_q1", label="Jour 4. Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)",
                                                   "Un peu (jusqu\u2019\u00e0 30 minutes au total)",
                                                   "Beaucoup (jusqu\u2019\u00e0 1 heure au total)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure au total)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d4_q2", label="Jour 4. Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou effectuer de petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d4_q3", label="Jour 4. Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d4_q4", label="Jour 4. Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d4_q5", label="Jour 4. Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d4_q6", label="Jour 4. Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d4_q7", label="Jour 4. Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Jour 4. Score acc\u00e9l\u00e9rom\u00e9trique pour les pas", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d4_steps")
                                  )
                           ),
                           column(6,
                                  wellPanel("Jour 4. Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d4_vmu")
                                  )
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h2("Jour 5"),
                                  h4()
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d5_q1", label="Jour 5. Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)",
                                                   "Un peu (jusqu\u2019\u00e0 30 minutes au total)",
                                                   "Beaucoup (jusqu\u2019\u00e0 1 heure au total)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure au total)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d5_q2", label="Jour 5. Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou effectuer de petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d5_q3", label="Jour 5. Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d5_q4", label="Jour 5. Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d5_q5", label="Jour 5. Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d5_q6", label="Jour 5. Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d5_q7", label="Jour 5. Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Jour 5. Score acc\u00e9l\u00e9rom\u00e9trique pour les pas", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d5_steps")
                                  )
                           ),
                           column(6,
                                  wellPanel("Jour 5. Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d5_vmu")
                                  )
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h2("Jour 6"),
                                  h4()
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d6_q1", label="Jour 6. Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)",
                                                   "Un peu (jusqu\u2019\u00e0 30 minutes au total)",
                                                   "Beaucoup (jusqu\u2019\u00e0 1 heure au total)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure au total)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d6_q2", label="Jour 6. Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou effectuer de petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d6_q3", label="Jour 6. Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d6_q4", label="Jour 6. Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d6_q5", label="Jour 6. Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d6_q6", label="Jour 6. Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d6_q7", label="Jour 6. Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Jour 6. Score acc\u00e9l\u00e9rom\u00e9trique pour les pas", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d6_steps")
                                  )
                           ),
                           column(6,
                                  wellPanel("Jour 6. Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d6_vmu")
                                  )
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h2("Jour 7"),
                                  h4()
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d7_q1", label="Jour 7. Avez-vous march\u00e9 \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f", 
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)",
                                                   "Un peu (jusqu\u2019\u00e0 30 minutes au total)",
                                                   "Beaucoup (jusqu\u2019\u00e0 1 heure au total)",
                                                   "Enorm\u00e9ment (plus d\u20191 heure au total)"
                                                 )
                                    ),
                                  ),
                                  
                           )
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d7_q2", label="Jour 7. Avez-vous effectu\u00e9 des t\u00e2ches \u00e0 l\u2019ext\u00e9rieur aujourd\u2019hui \u003f 
                                                 (exemples : le jardinage, sortir les poubelles ou effectuer de petites courses)",
                                                 choices=c(
                                                   "Aucune",
                                                   "Tr\u00e8s peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d7_q3", label="Jour 7. Avez-vous eu des difficult\u00e9s pour vous habiller aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Quelques-unes",
                                                   "Beaucoup",
                                                   "Enorm\u00e9ment"
                                                 )
                                    ),
                                  ),
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d7_q4", label="Jour 7. Avez-vous \u00e9vit\u00e9 des activit\u00e9s \u00e0 cause de vos probl\u00e8mes respiratoires aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                           ), 
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d7_q5", label="Jour 7. Etiez-vous essouffl\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ), 
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d7_q6", label="Jour 7. Etiez-vous fatigu\u00e9(e) en g\u00e9n\u00e9ral durant vos activit\u00e9s aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Pas du tout",
                                                   "Un petit peu",
                                                   "Mod\u00e9r\u00e9ment",
                                                   "Tr\u00e8s",
                                                   "Extr\u00eamement"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(12,
                                  wellPanel(
                                    radioButtons(inputId="dppac_FR_d7_q7", label="Jour 7. Avez-vous du faire des pauses pendant vos activit\u00e9s physiques aujourd\u2019hui \u003f",
                                                 choices=c(
                                                   "Jamais",
                                                   "Rarement",
                                                   "Quelques fois",
                                                   "Fr\u00e9quemment",
                                                   "Tout le temps"
                                                   
                                                 )
                                    ),
                                  ),
                                  
                           ),
                           
                         ),
                         fluidRow(
                           column(6,
                                  wellPanel("Jour 7. Score acc\u00e9l\u00e9rom\u00e9trique pour les pas", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d7_steps")
                                  )
                           ),
                           column(6,
                                  wellPanel("Jour 7. Score acc\u00e9l\u00e9rom\u00e9trique pour le VMU", class = "control-label",
                                            h4(""),
                                            reactable::reactableOutput("table_dppac_fr_d7_vmu")
                                  )
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h2("Bilan"),
                                  h4("")
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  shiny::actionButton("get_dppac_summary_fr", "R\u00e9sultats / Actualiser", class = "btn-validate"),
                           ),
                         ),
                         fluidRow(
                           column(6,
                                  h4(""),
                                  reactable::reactableOutput("PROactive_scores_dppac_summary_fr"), 
                           ),
                           column(6,
                                  h4(""),
                                  shinydashboard::valueBoxOutput("infoBox_dppac_fr_total_diff"),
                                  shinydashboard::valueBoxOutput("infoBox_dppac_fr_total_amount"),
                                  shinydashboard::valueBoxOutput("infoBox_dppac_fr_total_all"),
                                  h4(""),
                                  h4(""),
                                  shinydashboard::valueBoxOutput("infoBox_dppac_fr_total_diff_rasch"),
                                  shinydashboard::valueBoxOutput("infoBox_dppac_fr_total_amount_rasch"),
                                  shinydashboard::valueBoxOutput("infoBox_dppac_fr_total_all_rasch")
                           ),
                         ),
                         fluidRow(
                           column(12,
                                  h4(""),
                                  h4(""),
                                  downloadButton("report_fr_dppac_html", "G\u00e9n\u00e9rer le rapport du D-PPAC (.html)", class = "btn-report"),
                                  downloadButton("report_fr_dppac_pdf", "G\u00e9n\u00e9rer le rapport du D-PPAC (.pdf)", class = "btn-report")
                           ),
                         ),
                ),
               ),
               ),
          
          # $$$$$$$$$$$$$$$$$$
          # Third tab content ----
          # $$$$$$$$$$$$$$$$$$
          
          shinydashboard::tabItem(tabName = "guide",
                  h2("User's guide"),
                  
                  ################
                  # Notes to users ----
                  ################
                  
                  fluidRow(
                    column(12, 
                        wellPanel(
                           h4("Welcome to the activAnalyzer app. activAnalyzer is a Shiny app that has been developed 
                              to analyze daily physical behavior data recorded at the hip in adults using an ActiGraph 
                              accelerometer (.agd files). Once analysis is completed, the app allows to export results 
                              (summarized by day with means or medians of the valid days) to .csv files and to generate 
                              a report of the measurement (.pdf file). All the configured inputs relevant for interpreting 
                              the results are recorded in the report. Be sure that the inputs that are configured when 
                              generating the report correspond to the analysis that was actually performed (in other words, 
                              avoid modifying the inputs after generating satisfactory results). In addition to a general 
                              analysis of physical behavior, the app also allows to implement the Daily\u002d and Clinical visit\u002dPROactive 
                              Physical Activity in COPD (D\u002dPPAC and C\u002dPPAC) instruments. Please read the user\u2019s guide for 
                              details about how the app works.", 
                              style = "line-height: 1.4em"),
                        ),
                           downloadButton("user_guide_en", "Download user's guide (EN) (.pdf)", class = "btn-guide"),
                           downloadButton("user_guide_fr", "Download user's guide (FR) (.pdf)", class = "btn-guide")
                    ),
                  ),

                  fluidRow(
                    column(12,
                           h2("Authors"),
                      wellPanel(
                           h4("Pierre-Yves de M\u00fcllenheim, PhD", style = "font-weight: bold; font-size: 20px"),
                           h4("Associate professor"),
                           h4("Institut de formation en \u00e9ducation physique et en sport d\u2019Angers \u0028IFEPSA\u0029, UCO, France"),
                           h4("Email: pydemull@uco.fr")
                        ),
                      wellPanel(
                           h4("Arnaud Chambellan, MD, PhD", style = "font-weight: bold; font-size: 20px"),
                           h4("Pulmonology Department, H\u00f4pital Saint Philibert, GHICL, France"),
                           h4("Professor of Physiology, Universit\u00e9 Catholique de Lille, France"),
                           h4("Email: chambellan.arnaud@ghicl.net")
                        )
                           
                    ),
                  ),
          ) # End second tab
          
        )
      ), # End dashboardBody 
      
      footer = shinydashboardPlus::dashboardFooter(
        left = "\u00a9 2021-2023 Conceived by Pierre-Yves de M\u00fcllenheim and Arnaud Chambellan. Developed by Pierre-Yves de M\u00fcllenheim - GNU General Public License Version 3.0",
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



