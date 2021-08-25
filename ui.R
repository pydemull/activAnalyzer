
#######################################################################################################
# Packages, functions, and lists of inputs
#######################################################################################################

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyFeedback)
library(ggplot2)
library(dplyr)
library(PhysicalActivity)
library(RSQLite)
library(hms)
library(reactable)
library(readr)
library(tidyr)
sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)

assessor_title <- c("...", "Mr", "Ms", "Mrs", "Dr", "Pr")
patient_title <- c("...", "Mr", "Ms", "Mrs")
sex <- c("...", "male", "female", "undefined")
device <- c("...", "7164", "GT1M", "GT3X", "GT3X+", "wGT3X+", "wGT3X-BT", "GT9X")
position <- c("hip", "wrist", "thigh", "ankle")
side <- c("...", "right", "left")
filter <- c("...", "normal", "LFE")
axis_weartime <- c("vector magnitude", "vertical axis")
metrics <- c("axis1", "axis2", "axis3", "vm", "steps", "inclineStanding", "inclineSitting", "inclineLying")
equations <- c("santos_lozano_2013_mixed_older_adults", "santos_lozano_2013_mixed_adults", "santos_lozano_2013_mixed_youth",
               "freedson_1998_walk_adults", "hendelman_2000_walk_adults", "hendelman_2000_walk_adl_adults", "nichols_2000_walk_adults", "sasaki_2011_walk_adults")
axis <- c("vector magnitude", "vertical axis")

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
        
          waiter::use_waitress(),
          shinyFeedback::useShinyFeedback(),
          tags$head(
               tags$style(HTML("
           .shiny-output-error-validation {
             color: #ff0000;
             font-weight: bold;
           }
         "))
             ),
          tags$head(tags$style('h2 {color:#337ab7;}')),
        
        ########################
        # Assessor's information
        ########################
        
        fluidRow(
            column(6,                   
                   h2("Assessor's information")
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
        
        #######################
        # Patient's information
        #######################
        
        fluidRow(
            column(6,                   
                   h2("Patient's information")
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
       
        ######################
        # Device's information
        ######################
        
        fluidRow(
            column(6,                   
                   h2("Device's information")
            ),
        ),
        fluidRow(
            column(2,
                   selectInput("device", "Device", device)
            ),
            column(2,
                   selectInput("position", "Position", position)
            ),
            column(2,
                   selectInput("side", "Side", side)
            ),
            column(2,
                   numericInput("sampling_rate", "Sampling_rate (Hz)", value = "", min = 0)
            ),
            column(2,
                   selectInput("filter", "Filter", filter)
            ),
        ),
        
        ################################################################
        # Data uploading, nonwear time detection, and data visualization
        ################################################################
        
        fluidRow(
            column(12,                   
                   h2("Data uploading, nonwear time detection, and data visualization")
            ),
        ),
        fluidRow(
            column(6,
                   fileInput("upload", NULL, placeholder = ".agd")
            ),
        ),
        fluidRow(
            column(2,
                   selectInput("axis_weartime", "Axis to be considered to detect nonwear time", axis_weartime)
            ),  
            column(3,
                    numericInput("frame_size", "Time interval to be considered for nonwear time detection (min)", value = 90, min = 0)
            ),
            column(4,
                   numericInput("allowanceFrame_size", "Time interval with nonzero counts allowed during a nonwear period (min)", value = 2, min = 0)
            ),
        ),
        fluidRow(
            column(3,
                   actionButton("validate", "Validate configuration", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
            ),
        ),

        ###########################################
        # Box for the plot showing the monitor data
        ###########################################
        
        fluidRow(
            box(plotOutput("graph"), width = 12)
        ),
        fluidRow(
            align = "center",
                   selectInput("Metric", "Metric to visualize", metrics),
        ),
        
        ################################################
        # Inputs for computing physical activity metrics
        ################################################
        
        fluidRow(
          column(12,                   
                 h2("Computation of metrics (Please choose appropriate values based on scientific literature)")
                 ),
        ),
        
        fluidRow(
            column(4,
                   selectInput("equation_mets", "Equation to compute METs", equations)
                   ),

        ),
        fluidRow(
            column(2,
                   selectInput("axis", "Axis for SED/PA categorization", axis)
            ),
            column(2,
                   numericInput("sed_cutpoint", "SED cut-point (<)", value = 200)
            ),
            column(2,
                   numericInput("mpa_cutpoint", "MPA cut-point (>=)", value = 2751)
            ),
            column(2,
                   numericInput("vpa_cutpoint", "VPA cut-point (>=)", value = 9359)
            ),
            column(2,
                   numericInput("minimum_wear_time_for_analysis", "Minimum wear time (hours)", value = 10)
            ), 
        ),
        fluidRow(
          column(3,
                 actionButton("Run", "Run Analysis", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
          ),
        ),
        
        #########
        # Results
        #########
        
        fluidRow(
            column(12,
                   h2("Results"),
                   
                   ###############################
                   # Table of results shown by day
                   ###############################
                   h3("Results by day"),
                   reactableOutput("results_by_day")
                   ),
        ),

                   ############################################
                   # Table of results averaged on a daily basis
                   ############################################
        
        fluidRow(
            column(12,
                   h3("Results averaged on a valid days"),
                   reactableOutput("results_summary")
                  ),
        ),
      
        ########
        # Export
        ########
        
        fluidRow(
          column(12,
                 h2("Export"),
                 downloadButton("ExpResultsByDays", "Export results by day (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 downloadButton("ExpDailySummary", "Export daily summary (.csv)", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 downloadButton("report", "Generate report (.pdf)")
               ),

        ),
        
        
        #######
        # Reset
        #######
        
        fluidRow(
          column(4,
                 h2("Reset app"),
                 actionButton('reset',"Reset App",  style="color: #fff; background-color: #F8766D; border-color: #FC717F")
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
                       "Author: Pierre-Yves de Müllenheim (pydemull@uco.fr)"
                ),
              ),
              fluidRow(
                column(12, 
                       "Note: Welcome to the ActivAnalyzer app. This app was developped to analyse ActiGraph accelerometer data (.agd files) recorded at the hip in adults. Once analysis is completed,
                   the app allows to export results to .csv files and to generate a report of the measurement. All the configured inputs
                   relevant for interpreting the results are recorded into the report. Please be sure that the inputs that are configured
                   when generating the report correspond to the analysis that was actually performed (in other words, avoid modifying the inputs
                   after generating satisfactory results)."
                ),
              ),
      ) # End second tab
      
              )
     ), # End dashboardBody 
    
    footer = dashboardFooter(
      left = "© 2021 Pierre-Yves de Müllenheim. All Rights Reserved.",
   )
  )