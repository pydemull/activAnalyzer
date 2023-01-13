#' Create a formatted table of results
#' 
#' The function generates a formatted table with both means and medians of the metrics obtained following the physical behavior measurement.
#'
#' @param results_summary_means A dataframe with mean results obtained using the \code{\link{prepare_dataset}},  \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, \code{\link{recap_by_day}}, and then the \code{\link{average_results}} functions.
#' @param results_summary_medians A dataframe with median results obtained using the \code{\link{prepare_dataset}},  \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, \code{\link{recap_by_day}}, and then the \code{\link{average_results}} functions.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#' @param metrics A character value for setting the metrics to be shown in the figure. "volume" refers to "activity volume" metrics, step_acc" refers 
#'     to "step accumlulation" metrics, and "int_distri" refers to intensity distribution metrics. By default, the function provides all computed metrics.
#' @param epoch_label A character value to be pasted into the names of the variables to build the figure
#'
#' @return A flextable object
#' @export
#'
#' @examples
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file)
#' mydata_with_wear_marks <- mark_wear_time(
#'     dataset = mydata, 
#'     TS = "TimeStamp", 
#'     cts  = "vm", 
#'     frame = 90, 
#'     allowanceFrame = 2, 
#'     streamFrame = 30
#'     )
#' mydata_with_intensity_marks <- mark_intensity(
#'     data = mydata_with_wear_marks, 
#'     col_axis = "vm", 
#'     equation = "Sasaki et al. (2011) [Adults]",
#'     sed_cutpoint = 200, 
#'     mpa_cutpoint = 2690, 
#'     vpa_cutpoint = 6167, 
#'     age = 32,
#'     weight = 67,
#'     sex = "male",
#'     )
#' summary_by_day <- recap_by_day(
#'     data = mydata_with_intensity_marks, 
#'     age = 32, 
#'     weight = 67, 
#'     sex = "male",
#'     valid_wear_time_start = "07:00:00",
#'     valid_wear_time_end = "22:00:00"
#'     )$df_all_metrics
#' results_summary_means <- average_results(
#'     data = summary_by_day, 
#'     minimum_wear_time = 10, 
#'     fun = "mean"
#'     )
#' results_summary_medians <- average_results(
#'     data = summary_by_day, 
#'     minimum_wear_time = 10, 
#'     fun = "median"
#'     )
#' create_flextable_summary(
#'     results_summary_means,
#'     results_summary_medians, 
#'     language = "en"
#'     )
#' 
create_flextable_summary <- function(
    results_summary_means, 
    results_summary_medians, 
    language = c("en", "fr"),
    metrics = c("all", "volume", "step_acc", "int_distri"),
    epoch_label = "60s"
    ) {

# Get arguments
metrics <- match.arg(metrics)
language <- match.arg(language)
epoch_label = as.name(epoch_label)

flextable::set_flextable_defaults(fonts_ignore = TRUE)

# Create table for english language ===============================================================================
  if (language == "en") {
    
    
# Set list of metrics to be used for filerting table of results
    if (metrics == "all") {
      selected_metrics <- c(
        "Wear time (min)",
        "Axis 1 total counts",
        "VM total counts",
        "Axis 1 mean (counts/min)",
        "VM mean (counts/min)",
        "SED time (min)", 
        "LPA time (min)", 
        "MPA time (min)",
        "VPA time (min)",
        "MVPA time (min)",
        "SED wear time proportion (%)",
        "LPA wear time proportion (%)",
        "MPA wear time proportion (%)",
        "VPA wear time proportion (%)",
        "MVPA wear time proportion (%)",
        "Ratio MVPA / SED",
        "Total MVPA MET-hr", 
        "Total kcal",
        "PAL",
        "Total steps",
        "Max step acc. 60 min (steps/min)",
        "Max step acc. 30 min (steps/min)",
        "Max step acc. 20 min (steps/min)",
        "Max step acc. 5 min (steps/min)",
        "Max step acc. 1 min (steps/min)",
        "Peak step acc. 60 min (steps/min)",
        "Peak step acc. 30 min (steps/min)",
        "Peak step acc. 20 min (steps/min)",
        "Peak step acc. 5 min (steps/min)",
        "Peak step acc. 1 min (steps/min)",
        "Intensity gradient",
        paste0("M1/3", " (counts/", epoch_label, ")"),
        paste0("M120", " (counts/", epoch_label, ")"),
        paste0("M60", " (counts/", epoch_label, ")"),
        paste0("M30", " (counts/", epoch_label, ")"),
        paste0("M15", " (counts/", epoch_label, ")"),
        paste0("M5", " (counts/", epoch_label, ")")
      )
      
    } 
    
    if (metrics == "volume") {
      selected_metrics <- c(
        "Wear time (min)",
        "Axis 1 total counts",
        "VM total counts",
        "Axis 1 mean (counts/min)",
        "VM mean (counts/min)",
        "SED time (min)", 
        "LPA time (min)", 
        "MPA time (min)",
        "VPA time (min)",
        "MVPA time (min)",
        "SED wear time proportion (%)",
        "LPA wear time proportion (%)",
        "MPA wear time proportion (%)",
        "VPA wear time proportion (%)",
        "MVPA wear time proportion (%)",
        "Ratio MVPA / SED",
        "Total MVPA MET-hr", 
        "Total kcal",
        "PAL",
        "Total steps"
      )
      
    } 
    
    if (metrics == "step_acc") {
      selected_metrics <- c(
        "Max step acc. 60 min (steps/min)",
        "Max step acc. 30 min (steps/min)",
        "Max step acc. 20 min (steps/min)",
        "Max step acc. 5 min (steps/min)",
        "Max step acc. 1 min (steps/min)",
        "Peak step acc. 60 min (steps/min)",
        "Peak step acc. 30 min (steps/min)",
        "Peak step acc. 20 min (steps/min)",
        "Peak step acc. 5 min (steps/min)",
        "Peak step acc. 1 min (steps/min)"
      )
      
    } 
    
    if (metrics == "int_distri") {
      selected_metrics <- c(
        "Intensity gradient",
        paste0("M1/3", " (counts/", epoch_label, ")"),
        paste0("M120", " (counts/", epoch_label, ")"),
        paste0("M60", " (counts/", epoch_label, ")"),
        paste0("M30", " (counts/", epoch_label, ")"),
        paste0("M15", " (counts/", epoch_label, ")"),
        paste0("M5", " (counts/", epoch_label, ")")
      )
      
    } 
    
    flextable_summary <- 
      flextable::flextable(
        tibble::tibble(
          Metric = c("Number of valid days",
                     "Wear time (min)",              
                     "Axis 1 total counts",          
                     "VM total counts",              
                     "Axis 1 mean (counts/min)",     
                     "VM mean (counts/min)",         
                     "SED time (min)",               
                     "LPA time (min)",               
                     "MPA time (min)",               
                     "VPA time (min)",               
                     "MVPA time (min)",              
                     "SED wear time proportion (%)", 
                     "LPA wear time proportion (%)", 
                     "MPA wear time proportion (%)", 
                     "VPA wear time proportion (%)", 
                     "MVPA wear time proportion (%)",
                     "Ratio MVPA / SED",             
                     "Total MVPA MET-hr",            
                     "Total kcal",                   
                     "PAL",                          
                     "Total steps",
                     "Max step acc. 60 min (steps/min)",
                     "Max step acc. 30 min (steps/min)",
                     "Max step acc. 20 min (steps/min)",
                     "Max step acc. 5 min (steps/min)",
                     "Max step acc. 1 min (steps/min)",
                     "Peak step acc. 60 min (steps/min)",
                     "Peak step acc. 30 min (steps/min)",
                     "Peak step acc. 20 min (steps/min)",
                     "Peak step acc. 5 min (steps/min)",
                     "Peak step acc. 1 min (steps/min)",
                     "Intensity gradient",
                     paste0("M1/3", " (counts/", epoch_label, ")"),
                     paste0("M120", " (counts/", epoch_label, ")"),
                     paste0("M60", " (counts/", epoch_label, ")"),
                     paste0("M30", " (counts/", epoch_label, ")"),
                     paste0("M15", " (counts/", epoch_label, ")"),
                     paste0("M5", " (counts/", epoch_label, ")")
                     
          ),
          
          "Daily mean | median" = c(
            
            # Number of valid days
            paste0(results_summary_means[["valid_days"]]),
            
            # Wear time
            paste0(format(round(results_summary_means[["wear_time"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["wear_time"]]), ") | ",
                   format(round(results_summary_medians[["wear_time"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["wear_time"]]), ")"),  
            
            # Total counts Axis 1
            paste0(format(round(results_summary_means[["total_counts_axis1"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["total_counts_axis1"]], 1), nsmall = 1)), 
            
            # Total counts VM
            paste0(format(round(results_summary_means[["total_counts_vm"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["total_counts_vm"]], 1), nsmall = 1)), 
            
            # Axis 1 per min
            paste0(format(round(results_summary_means[["axis1_per_min"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["axis1_per_min"]], 1), nsmall = 1)), 
            
            # VM per min
            paste0(format(round(results_summary_means[["vm_per_min"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["vm_per_min"]], 1), nsmall = 1)), 
            
            # SED time
            paste0(format(round(results_summary_means[["minutes_SED"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_SED"]]), ") | ",  
                   format(round(results_summary_medians[["minutes_SED"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_SED"]]), ")"),  
            
            # LPA time 
            paste0(format(round(results_summary_means[["minutes_LPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_LPA"]]), ") | ",  
                   format(round(results_summary_medians[["minutes_LPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_LPA"]]), ")"), 
            
            # MPA time 
            paste0(format(round(results_summary_means[["minutes_MPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_MPA"]]), ") | ",  
                   format(round(results_summary_medians[["minutes_MPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_MPA"]]), ")"), 
            
            # VPA time 
            paste0(format(round(results_summary_means[["minutes_VPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_VPA"]]), ") | ",  
                   format(round(results_summary_medians[["minutes_VPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_VPA"]]), ")"), 
            
            # MVPA time
            paste0(format(round(results_summary_means[["minutes_MVPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_MVPA"]]), ") | ",
                   format(round(results_summary_medians[["minutes_MVPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_MVPA"]]), ")"),  
            
            # Percent time SED 
            paste0(format(round(results_summary_means[["percent_SED"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_SED"]], 1), nsmall = 1)),
            
            # Percent time LPA 
            paste0(format(round(results_summary_means[["percent_LPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_LPA"]], 1), nsmall = 1)),
            
            # Percent time MPA 
            paste0(format(round(results_summary_means[["percent_MPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_MPA"]], 1), nsmall = 1)),      
            
            # Percent time VPA 
            paste0(format(round(results_summary_means[["percent_VPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_VPA"]], 1), nsmall = 1)),  
            
            # Percent time MVPA  
            paste0(format(round(results_summary_means[["percent_MVPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_MVPA"]], 1), nsmall = 1)),                 
            
            # Ratio MVPA/SED
            paste0(format(round(results_summary_means[["ratio_mvpa_sed"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["ratio_mvpa_sed"]], 2), nsmall = 2)),
            
            # MET-hr MVPA
            paste0(format(round(results_summary_means[["mets_hours_mvpa"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["mets_hours_mvpa"]], 2), nsmall = 2)),         
            # Total kilocalories
            paste0(format(round(results_summary_means[["total_kcal"]], 2), nsmall = 2),   " | ", format(round(results_summary_medians[["total_kcal"]], 2), nsmall = 2)),
            
            # Physical activity level (PAL) 
            paste0(format(round(results_summary_means[["pal"]], 2), nsmall = 2),  " | ", format(round(results_summary_medians[["pal"]], 2), nsmall = 2)),  
            
            # Total number of steps
            paste0(round(results_summary_means[["total_steps"]], 0), " | ", round(results_summary_medians[["total_steps"]], 0)),
            
            # Max step accum 60min
            paste0(format(round(results_summary_means[["max_steps_60min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_60min"]], 2), nsmall = 2)),
            
            # Max step accum 30min
            paste0(format(round(results_summary_means[["max_steps_30min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_30min"]], 2), nsmall = 2)),
            
            # Max step accum 20min
            paste0(format(round(results_summary_means[["max_steps_20min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_20min"]], 2), nsmall = 2)),
            
            # Max step accum 5min
            paste0(format(round(results_summary_means[["max_steps_5min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_5min"]], 2), nsmall = 2)),
            
            # Max step accum 1min
            paste0(format(round(results_summary_means[["max_steps_1min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_1min"]], 2), nsmall = 2)),
            
            # Peak step accum 60min
            paste0(format(round(results_summary_means[["peak_steps_60min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_60min"]], 2), nsmall = 2)),
            
            # Peak step accum 30min
            paste0(format(round(results_summary_means[["peak_steps_30min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_30min"]], 2), nsmall = 2)),
            
            # Peak step accum 20min
            paste0(format(round(results_summary_means[["peak_steps_20min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_20min"]], 2), nsmall = 2)),
            
            # Peak step accum 5min
            paste0(format(round(results_summary_means[["peak_steps_5min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_5min"]], 2), nsmall = 2)),
            
            # Peak step accum 1min
            paste0(format(round(results_summary_means[["peak_steps_1min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_1min"]], 2), nsmall = 2)),
            
            # Intensity gradient
            paste0(format(round(results_summary_means[["ig"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["ig"]], 2), nsmall = 2)),
            
            # M1/3
            paste0(format(round(results_summary_means[["M1/3"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["M1/3"]], 1), nsmall = 1)), 
            
            # M120
            paste0(format(round(results_summary_means[["M120"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["M120"]], 1), nsmall = 1)), 
            
            # M60
            paste0(format(round(results_summary_means[["M60"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["M60"]], 1), nsmall = 1)), 
            
            # M30
            paste0(format(round(results_summary_means[["M30"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["M30"]], 1), nsmall = 1)),
            
            # M15
            paste0(format(round(results_summary_means[["M15"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["M15"]], 1), nsmall = 1)), 
            
            # M5
            paste0(format(round(results_summary_means[["M5"]], 1), nsmall = 1), " | ",
                   format(round(results_summary_medians[["M5"]], 1), nsmall = 1))
            
          )
        ) %>% dplyr::filter(Metric %in% c("Number of valid days", selected_metrics))
      ) %>%
      flextable::theme_zebra() %>%
      flextable::align(align = "left", part = "all" ) %>%
      flextable::width(width = 3.25)
    
    return(flextable_summary)
    
  }

# Create table for french language ===============================================================================
if (language == "fr") {
  
  # Set list of metrics to be used for filerting table of results
  if (metrics == "all") {
    selected_metrics <- c(
      "Temps de port (min)",
      "Total counts Axe 1 ",
      "Total counts VM",
      "Moyenne Axe 1 (counts/min)",
      "Moyenne VM (counts/min)",
      "Temps SED (min)", 
      "Temps LPA (min)", 
      "Temps MPA (min)",
      "Temps VPA (min)",
      "Temps MVPA (min)",
      "Proportion du temps de port SED (%)",
      "Proportion du temps de port LPA (%)",
      "Proportion du temps de port MPA (%)",
      "Proportion du temps de port VPA (%)",
      "Proportion du temps de port MVPA (%)",
      "Ratio MVPA / SED",
      "Total MET-hr", 
      "Total kcal",
      "NAP",
      "Total pas",
      "Acc. pas max. 60 min (pas/min)",
      "Acc. pas max. 30 min (pas/min)",
      "Acc. pas max. 20 min (pas/min)",
      "Acc. pas max. 5 min (pas/min)",
      "Acc. pas max. 1 min (pas/min)",
      "Acc. pas pic 60 min (pas/min)",
      "Acc. pas pic 30 min (pas/min)",
      "Acc. pas pic 20 min (pas/min)",
      "Acc. pas pic 5 min (pas/min)",
      "Acc. pas pic 1 min (pas/min)",
      "Gradient d'intensité",
      paste0("M1/3", " (counts/", epoch_label, ")"),
      paste0("M120", " (counts/", epoch_label, ")"),
      paste0("M60", " (counts/", epoch_label, ")"),
      paste0("M30", " (counts/", epoch_label, ")"),
      paste0("M15", " (counts/", epoch_label, ")"),
      paste0("M5", " (counts/", epoch_label, ")")
    )

    
  } 
  
  if (metrics == "volume") {
    selected_metrics <- c(
      "Temps de port (min)",
      "Total counts Axe 1 ",
      "Total counts VM",
      "Moyenne Axe 1 (counts/min)",
      "Moyenne VM (counts/min)",
      "Temps SED (min)", 
      "Temps LPA (min)", 
      "Temps MPA (min)",
      "Temps VPA (min)",
      "Temps MVPA (min)",
      "Proportion du temps de port SED (%)",
      "Proportion du temps de port LPA (%)",
      "Proportion du temps de port MPA (%)",
      "Proportion du temps de port VPA (%)",
      "Proportion du temps de port MVPA (%)",
      "Ratio MVPA / SED",
      "Total MET-hr", 
      "Total kcal",
      "NAP",
      "Total pas"
    )

    
  } 
  
  if (metrics == "step_acc") {
    selected_metrics <- c(
      "Acc. pas max. 60 min (pas/min)",
      "Acc. pas max. 30 min (pas/min)",
      "Acc. pas max. 20 min (pas/min)",
      "Acc. pas max. 5 min (pas/min)",
      "Acc. pas max. 1 min (pas/min)",
      "Acc. pas pic 60 min (pas/min)",
      "Acc. pas pic 30 min (pas/min)",
      "Acc. pas pic 20 min (pas/min)",
      "Acc. pas pic 5 min (pas/min)",
      "Acc. pas pic 1 min (pas/min)"
    )

    
  } 
  
  if (metrics == "int_distri") {
    selected_metrics <- c(
      "Gradient d'intensité",
      paste0("M1/3", " (counts/", epoch_label, ")"),
      paste0("M120", " (counts/", epoch_label, ")"),
      paste0("M60", " (counts/", epoch_label, ")"),
      paste0("M30", " (counts/", epoch_label, ")"),
      paste0("M15", " (counts/", epoch_label, ")"),
      paste0("M5", " (counts/", epoch_label, ")")
    )
    
  } 
  
  
  flextable_summary <- 
    flextable::flextable(
      tibble::tibble(
        Indicateur = c(
          "Nombre de jours valides",  
          "Temps de port (min)",                 
          "Total counts Axe 1 ",                 
          "Total counts VM",                     
          "Moyenne Axe 1 (counts/min)",          
          "Moyenne VM (counts/min)",             
          "Temps SED (min)",                     
          "Temps LPA (min)",                     
          "Temps MPA (min)",                     
          "Temps VPA (min)",                     
          "Temps MVPA (min)",                    
          "Proportion du temps de port SED (%)",
          "Proportion du temps de port LPA (%)",
          "Proportion du temps de port MPA (%)",
          "Proportion du temps de port VPA (%)",
          "Proportion du temps de port MVPA (%)",
          "Ratio MVPA / SED",                    
          "Total MET-hr",                        
          "Total kcal",                          
          "NAP",                                
          "Total pas",
          "Acc. pas max. 60 min (pas/min)",
          "Acc. pas max. 30 min (pas/min)",
          "Acc. pas max. 20 min (pas/min)",
          "Acc. pas max. 5 min (pas/min)", 
          "Acc. pas max. 1 min (pas/min)", 
          "Acc. pas pic 60 min (pas/min)", 
          "Acc. pas pic 30 min (pas/min)", 
          "Acc. pas pic 20 min (pas/min)", 
          "Acc. pas pic 5 min (pas/min)",  
          "Acc. pas pic 1 min (pas/min)",
          "Gradient d'intensité",
          paste0("M1/3", " (counts/", epoch_label, ")"),
          paste0("M120", " (counts/", epoch_label, ")"),
          paste0("M60", " (counts/", epoch_label, ")"),
          paste0("M30", " (counts/", epoch_label, ")"),
          paste0("M15", " (counts/", epoch_label, ")"),
          paste0("M5", " (counts/", epoch_label, ")")
          
        ),
        
        "Moyenne | m\xc3\xa9diane journali\xc3\xa8re obtenue \xc3\xa0 partir des jours valides" = c(
          
          # Nombre de jours valides
          paste0(results_summary_means[["valid_days"]]),
          
          # Temps de port
          paste0(format(round(results_summary_means[["wear_time"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["wear_time"]]), ") | ",  
                 format(round(results_summary_medians[["wear_time"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["wear_time"]]), ")"),  
          
          # Total counts Axis 1
          paste0(format(round(results_summary_means[["total_counts_axis1"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["total_counts_axis1"]], 1), nsmall = 1)), 
          
          # Total counts VM
          paste0(format(round(results_summary_means[["total_counts_vm"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["total_counts_vm"]], 1), nsmall = 1)), 
          
          # Axis 1 par min
          paste0(format(round(results_summary_means[["axis1_per_min"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["axis1_per_min"]], 1), nsmall = 1)), 
          
          # VM par min
          paste0(format(round(results_summary_means[["vm_per_min"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["vm_per_min"]], 1), nsmall = 1)), 
          
          # Minutes SED
          paste0(format(round(results_summary_means[["minutes_SED"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_SED"]]), ") | ",  
                 format(round(results_summary_medians[["minutes_SED"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_SED"]]), ")"), 
          
          # Minutes LPA
          paste0(format(round(results_summary_means[["minutes_LPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_LPA"]]), ") | ",  
                 format(round(results_summary_medians[["minutes_LPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_LPA"]]), ")"),
          
          # Minutes MPA
          paste0(format(round(results_summary_means[["minutes_MPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_MPA"]]), ") | ",  
                 format(round(results_summary_medians[["minutes_MPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_MPA"]]), ")"),
          
          # Minutes VPA
          paste0(format(round(results_summary_means[["minutes_VPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_VPA"]]), ") | ",  
                 format(round(results_summary_medians[["minutes_VPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_VPA"]]), ")"),
          
          # Minutes MVPA
          paste0(format(round(results_summary_means[["minutes_MVPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_means[["minutes_MVPA"]]), ") | ",
                 format(round(results_summary_medians[["minutes_MVPA"]], 1), nsmall = 1), " (", hms::hms(minutes = results_summary_medians[["minutes_MVPA"]]), ")"), 
          
          # % SED
          paste0(format(round(results_summary_means[["percent_SED"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_SED"]], 1), nsmall = 1)),
          
          # % LPA
          paste0(format(round(results_summary_means[["percent_LPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_LPA"]], 1), nsmall = 1)),  
          
          # % MPA
          paste0(format(round(results_summary_means[["percent_MPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_MPA"]], 1), nsmall = 1)), 
          
          # % VPA
          paste0(format(round(results_summary_means[["percent_VPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_VPA"]], 1), nsmall = 1)), 
          
          
          # % MVPA
          paste0(format(round(results_summary_means[["percent_MVPA"]], 1), nsmall = 1), " | ", format(round(results_summary_medians[["percent_MVPA"]], 1), nsmall = 1)),  
          
          # Ratio MVPA/SED
          paste0(format(round(results_summary_means[["ratio_mvpa_sed"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["ratio_mvpa_sed"]], 2), nsmall = 2)),
          
          # Total METs-hr
          paste0(format(round(results_summary_means[["mets_hours_mvpa"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["mets_hours_mvpa"]], 2), nsmall = 2)),
          
          # Total kcal
          paste0(format(round(results_summary_means[["total_kcal"]], 2), nsmall = 2),   " | ", format(round(results_summary_medians[["total_kcal"]], 2), nsmall = 2)),
          
          # NAP
          paste0(format(round(results_summary_means[["pal"]], 2), nsmall = 2),  " | ", format(round(results_summary_medians[["pal"]], 2), nsmall = 2)),  
          
          # Total pas
          paste0(round(results_summary_means[["total_steps"]], 0), " | ", round(results_summary_medians[["total_steps"]], 0)),
          
          # Max step accum 60min
          paste0(format(round(results_summary_means[["max_steps_60min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_60min"]], 2), nsmall = 2)),
          
          # Max step accum 30min
          paste0(format(round(results_summary_means[["max_steps_30min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_30min"]], 2), nsmall = 2)),
          
          # Max step accum 20min
          paste0(format(round(results_summary_means[["max_steps_20min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_20min"]], 2), nsmall = 2)),
          
          # Max step accum 5min
          paste0(format(round(results_summary_means[["max_steps_5min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_5min"]], 2), nsmall = 2)),
          
          # Max step accum 1min
          paste0(format(round(results_summary_means[["max_steps_1min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["max_steps_1min"]], 2), nsmall = 2)),
          
          # Peak step accum 60min
          paste0(format(round(results_summary_means[["peak_steps_60min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_60min"]], 2), nsmall = 2)),
          
          # Peak step accum 30min
          paste0(format(round(results_summary_means[["peak_steps_30min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_30min"]], 2), nsmall = 2)),
          
          # Peak step accum 20min
          paste0(format(round(results_summary_means[["peak_steps_20min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_20min"]], 2), nsmall = 2)),
          
          # Peak step accum 5min
          paste0(format(round(results_summary_means[["peak_steps_5min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_5min"]], 2), nsmall = 2)),
          
          # Peak step accum 1min
          paste0(format(round(results_summary_means[["peak_steps_1min"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["peak_steps_1min"]], 2), nsmall = 2)),
          
          # Gradient d'intensité
          paste0(format(round(results_summary_means[["ig"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["ig"]], 2), nsmall = 2)),
          
          # M1/3
          paste0(format(round(results_summary_means[["M1/3"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["M1/3"]], 1), nsmall = 1)), 
          
          # M120
          paste0(format(round(results_summary_means[["M120"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["M120"]], 1), nsmall = 1)), 
          
          # M60
          paste0(format(round(results_summary_means[["M60"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["M60"]], 1), nsmall = 1)), 
          
          # M30
          paste0(format(round(results_summary_means[["M30"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["M30"]], 1), nsmall = 1)),
          
          # M15
          paste0(format(round(results_summary_means[["M15"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["M15"]], 1), nsmall = 1)), 
          
          # M5
          paste0(format(round(results_summary_means[["M5"]], 1), nsmall = 1), " | ",
                 format(round(results_summary_medians[["M5"]], 1), nsmall = 1))
          
        )
      ) %>% dplyr::filter(Indicateur %in% c("Nombre de jours valides", selected_metrics))
    ) %>%
    flextable::theme_zebra() %>%
    flextable::align(align = "left", part = "all" ) %>%
    flextable::width(width = 3.25)
  
  return(flextable_summary)
  
 }
  

}

