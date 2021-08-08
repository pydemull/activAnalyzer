
#######################################################################################################
# Packages, functions, and lists of inputs
#######################################################################################################

library(shiny)
library(shinydashboard)
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
position <- c("...", "wrist", "hip", "thigh", "ankle")
side <- c("...", "right", "left")
filter <- c("...", "normal", "LFE")
metrics <- c("...", "axis1", "axis2", "axis3", "vm", "steps", "inclineStanding", "inclineSitting", "inclineLying")
equations <- c("santos_lozano_2013_mixed_older_adults", "santos_lozano_2013_mixed_adults", "santos_lozano_2013_mixed_youth",
               "freedson_1998_walk_adults", "hendelman_2000_walk_adults", "hendelman_2000_walk_adl_adults", "nichols_2000_walk_adults", "sasaki_2011_walk_adults")
axis <- c("vector magnitude", "vertical axis")

#######################################################################################################
# UI
#######################################################################################################

ui <- 
    dashboardPage(
    dashboardHeader(title = "activAnalyzer (bêta)"),
    dashboardSidebar(),
    dashboardBody(

        ################
        # Notes to users
        ################
        fluidRow(
            column(6, 
            "Author: Pierre-Yves de Müllenheim (pydemull@uco.fr)"
                  ),
        ),
        fluidRow(
            column(6, 
                   "Note: This app was developped to analyse ActiGraph data recorded at the hip."
            ),
        ),
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
                   textInput("assessor_name", "Name", placeholder = "NAME")
            ),
            column(2,
                   textInput("assessor_surname", "Surname", placeholder = "Surname" )
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
                   textInput("patient_name", "Name", placeholder = "NAME")
            ),
            column(2,
                   textInput("patient_surname", "Surname", placeholder = "Surname" )
            ),
        ), 
        fluidRow(
            column(2,
                   selectInput("sex", "Sex", sex)
            ),
            column(2,
                   numericInput("age", "Age (yr)", value = "", min = 0)
            ),
            column(2,
                   numericInput("height", "Height (m)", value = "", min = 0)
            ),
            column(2,
                   numericInput("weight", "Weight (kg)", value = "", min = 0)
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
        
        ################
        # Data selection
        ################
        
        fluidRow(
            column(6,                   
                   h2("Data selection")
            ),
        ),
        fluidRow(
            column(6,
                   fileInput("upload", NULL, placeholder = "agd. file")
            ),
        ),
        
        ###################################
        # Input for the metric to visualize
        ###################################
        
        fluidRow(
          column(12,                   
                 h2("Data visualisation")
                 ),
        ),
        fluidRow(
            column(6,
                   selectInput("Metric", "Metric to visualise", metrics)
                   ),
        ),
        
        ###########################################
        # Box for the plot showing the monitor data
        ###########################################
        
        fluidRow(
            box(plotOutput("graph"), width = 12)
        ),
        
        ################################################
        # Inputs for computing physical activity metrics
        ################################################
        
        fluidRow(
          column(12,                   
                 h2("Data analysis")
                 ),
        ),
        
        fluidRow(
            column(2,
                   sliderInput(inputId = "period",
                        label = "Period to compute daily mean METs (hours)",
                        min = 0,
                        max = 24,
                        step = 1,
                        value = c(7, 18))
                   ),
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

        ####################################
        # Table of results summarised by day
        ####################################
        
        fluidRow(
            column(12,
                   h2("Results by day"),
                   reactableOutput("results_by_day")
                   ),
        ),
        
        ############################################
        # Table of results averaged on a daily basis
        ############################################
        
        fluidRow(
            column(12,
                   h2("Results averaged on a daily basis"),
                   reactableOutput("results_summary")
                  ),
        ),
        fluidRow(
          column(4,
                 downloadButton("ExpResultsByDays", "Export results by day", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                 downloadButton("ExpDailySummary", "Export daily summary", style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
               ),
        ),
    
        #################
        # Generate report
        #################
        
        fluidRow(
            column(4,
                   downloadButton("report", "Generate report")
                   ),
        )
    )
)