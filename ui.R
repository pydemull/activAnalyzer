
#######################################################################################################
# Packages, functions, and lists of inputs
#######################################################################################################

# Loading packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFeedback)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(PhysicalActivity)
library(RSQLite)
library(actigraph.sleepr)
library(assertthat)
library(lubridate)
library(hms)
library(reactable)
library(readr)
library(tidyr)

# Loading functions
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)

# Setting lists of inputs
assessor_title <- c("...", "Mr", "Ms", "Mrs", "Dr", "Pr")
patient_title <- c("...", "Mr", "Ms", "Mrs")
sex <- c("...", "male", "female", "undefined")
device <- c("...", "7164", "GT1M", "GT3X", "GT3X+", "wGT3X+", "wGT3X-BT", "GT9X")
position <- c("hip", "wrist", "thigh", "ankle")
side <- c("...", "right", "left")
filter <- c("...", "normal", "LFE")
axis_weartime <- c("vector magnitude", "vertical axis")
metrics <- c("axis1", "axis2", "axis3", "vm", "steps", "inclineStanding", "inclineSitting", "inclineLying")
equations <- c("...",
               "Sasaki et al. (2011) [Adults]",
               "Santos-Lozano et al. (2013) [Adults]",
               "Freedson et al. (1998) [Adults]",
               "Santos-Lozano et al. (2013) [Older adults]" 
               )
sed_cutpoints <- c("...", 
                   "Aguilar-Farias et al. (2014) [Older adults]", 
                   "Personalized...")
mvpa_cutpoints <- c("...", 
                    "Sasaki et al. (2011) [Adults]", 
                    "Santos-Lozano et al. (2013) [Adults]", 
                    "Santos-Lozano et al. (2013) [Older adults]", 
                    "Personalized...")
perso_sed_axis <- c("vector magnitude", "vertical axis")
perso_mvpa_axis <- c("vector magnitude", "vertical axis")


#######################################################################################################
# UI
#######################################################################################################

ui <- 
    dashboardPage(
    dashboardHeader(title = "activAnalyzer (bêta)"),
    dashboardSidebar(
      sidebarMenu(
      menuItem("App", tabName = "app", icon = icon("fas fa-tablet-alt")),
      menuItem("User's guide", tabName = "guide", icon = icon("far fa-file-alt"))
    )),
    dashboardBody(
      
      tabItems(
        
    # $$$$$$$$$$$$$$$$$
    # First tab content
    # $$$$$$$$$$$$$$$$$
      
        tabItem(tabName = "app",
        
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
        # Section 1. Information
        ########################
         
         fluidRow(
           column(12,
                  h3("Section 1. Information", style="font-weight: bold; font-size: 30px; color: #337ab7;"),
                  tags$hr(style="border-color: #337ab7;")
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
                   selectInput("assessor_title", "Title", assessor_title)
            ),
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
                   selectInput("patient_title", "Title", patient_title)
            ),
            column(2,
                   textInput("patient_name", "Name", placeholder = "")
            ),
            column(2,
                   textInput("patient_surname", "Surname", placeholder = "" )
            ),
        ), 
        fluidRow(
            column(2,
                   selectInput("sex", "Sex (REQUIRED)", sex)
            ),
            column(2,
                   numericInput("age", "Age (yr) (REQUIRED)", value = "", min = 0)
            ),
            column(2,
                   numericInput("weight", "Weight (kg) (REQUIRED)", value = "", min = 0)
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
        # Section 2. Data uploading, nonwear time detection, and data visualization
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
            column(2,
                   selectInput("axis_weartime", "Axis to be considered to detect nonwear time", axis_weartime)
            ),  
            column(3,
                    numericInput("frame_size", "Time interval to be considered to detect nonwear time (min)", value = 90, min = 0)
            ),
            column(4,
                   numericInput("allowanceFrame_size", "Time interval with nonzero counts allowed during a nonwear period (min)", value = 2, min = 0)
            ),
            column(3,
                   shiny::actionButton("reset_nonwear", "Return to default values", style = "border-color: #2e6da4")
            ),
        ),
        fluidRow(
            column(3,
                   shiny::actionButton("validate", "Validate configuration", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            ),
        ),

        #*************************
        # Box showing monitor data
        #*************************
        
        fluidRow(
            h3(""),
            box(withSpinner(plotOutput("graph")), width = 12)
        ),
        fluidRow(
            align = "center",
                   selectInput("Metric", "Metric to visualize", metrics),
        ),

        
        ###################################
        # Section 3. Computation of metrics
        ###################################
           
        fluidRow(
          column(12,
                 h3("Section 3. Computation of metrics", style="font-weight: bold; font-size: 30px; color: #337ab7;"),
                 tags$hr(style="border-color: #337ab7;")
          ),
        ),    
        
        #******************
        # Choosing Equation
        #******************

        fluidRow(
            column(10,
                   h3("Choose a MET equation"),
                   selectInput("equation_mets", "Equation", equations)
                   ),
        ),
        fluidRow(
          column(12,
            reactableOutput("table_equations")
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
                  selectInput("sed_cutpoints", "SED cut-point", choices = sed_cutpoints)
                        )
                ),
        
                fluidRow(
                  column(12,
                  tabsetPanel(
                    id = "switcher_sed",
                    type = "hidden",
                    tabPanelBody("...", ""),
                    tabPanelBody("Aguilar-Farias et al. (2014) [Older adults]", reactableOutput("table_sed_cutpoints")),
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
                         selectInput("mvpa_cutpoints", "MVPA cut-points", choices = mvpa_cutpoints)
                  )
                ),
                fluidRow(
                  column(12,
                         tabsetPanel(
                           id = "switcher_mvpa",
                           type = "hidden",
                           tabPanelBody("...", ""),
                           tabPanelBody("Sasaki et al. (2011) [Adults]", reactableOutput("table_mvpa_cutpoints_sasaki")),
                           tabPanelBody("Santos-Lozano et al. (2013) [Adults]", reactableOutput("table_mvpa_cutpoints_santos_adults")),
                           tabPanelBody("Santos-Lozano et al. (2013) [Older adults]", reactableOutput("table_mvpa_cutpoints_santos_older")),
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
                 h3("Choose a minimum value for wear time to consider a day as valid"),
          ), 
          column(2,
                 numericInput("minimum_wear_time_for_analysis", "Minimum wear time (hours)", value = 10)
          )
        ),
       
        #*****************
        # Running analysis
        #*****************
        
         fluidRow(
          column(3,
                 shiny::actionButton("Run", "Run Analysis", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
        ),

        #######################################
        # Section 4. Results, Export, and Reset
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
        
         fluidRow(
            column(12,
                   h2("Results"),
                   
                   #************************
                   # Table of results by day
                   #************************
                   
                   h3("Results by day"),
                   withSpinner(reactableOutput("results_by_day"))
                   ),
         ),

                   #******************************************
                   # Table of results averaged over valid days
                   #******************************************
        
        fluidRow(
             column(12,
                    h3("Results averaged over valid days"),
                    withSpinner(reactableOutput("results_summary"))
                   ),
        ),
        fluidRow(
         column(12,
                tags$hr(style="border-color: #337ab7;")
         ),
        ),
      
        #******************
        # Export
        #******************
        
        fluidRow(
          column(12,
                 h2("Export"),
                 downloadButton("ExpResultsByDays", "Export results by day (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 downloadButton("ExpDailySummary", "Export daily summary (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 downloadButton("report", "Generate report (.pdf)", style="border-color: #FF9900; color: black; background-color: #FFCC33")
               ),

        ),
        fluidRow(
          column(12,
                 tags$hr(style="border-color: #337ab7;")
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
     # Second tab content
     # $$$$$$$$$$$$$$$$$$
     
        tabItem(tabName = "guide",
              h2("User's guide"),
              
              ################
              # Notes to users
              ################
              
              fluidRow(
                column(12, 
                       h4("Welcome to the activAnalyzer app. This app was developped to analyse ActiGraph accelerometer data (.agd files) recorded at the hip in adults. Once analysis is completed,
                   the app allows to export results to .csv files and to generate a report of the measurement. All the configured inputs
                   relevant for interpreting the results are recorded into the report. Be sure that the inputs that are configured
                   when generating the report correspond to the analysis that was actually performed (in other words, avoid modifying the inputs
                   after generating satisfactory results). Please read the user's guide for details about how the app works."),
                ),
              ),
              fluidRow(
                column(12,
                       downloadButton("user_guide", "Download user's guide (.pdf)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
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
                       h2("Contact"),
                       h4("Pierre-Yves de Müllenheim", style = "font-weight: bold; font-size: 20px"),
                       h4("Associate professor"),
                       h4("Institute of Physical Education and Sport Sciences (IFEPSA), UCO, Les Ponts-de-Cé, France"),
                       h4("Email: pydemull@uco.fr")
                       
                ),
              ),
      ) # End second tab
      
              )
     ), # End dashboardBody 
    
    footer = dashboardFooter(
      left = "© 2021 Pierre-Yves de Müllenheim. All Rights Reserved.",
   )
  )