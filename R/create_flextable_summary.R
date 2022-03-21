#' Create a formatted table of results
#' 
#' The function generates a formatted table with both means and medians of the metrics obtained following the physical behavior measurement.
#'
#' @param results_summary_means A dataframe with mean results obtained using the \code{\link{prepare_dataset}},  \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, \code{\link{recap_by_day}}, and then the \code{\link{average_results}} functions.
#' @param results_summary_medians A dataframe with meadian results obtained using the \code{\link{prepare_dataset}},  \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, \code{\link{recap_by_day}}, and then the \code{\link{average_results}} functions.
#' @param language A character value for setting the language with which the table should be created: `en` for english; `fr` for french.
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
#'     )
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
create_flextable_summary <- function(results_summary_means, results_summary_medians, language = c("en", "fr")) {
  
language <- match.arg(language)
  
flextable::set_flextable_defaults(fonts_ignore = TRUE)
  
  if (language == "en") {
    
    flextable_summary <- 
      flextable::flextable(
        tibble::tibble(
          Metric = c("Number of valid days",
                     "Wear time",
                     "Total kilocalories",
                     "Physical activity level (PAL)",
                     "Total number of steps",
                     "SED time",
                     "LPA time",
                     "MVPA time",
                     "SED wear time proportion",     
                     "LPA wear time proportion",     
                     "MVPA wear time proportion",    
                     "MVPA dose",                        
                     "MVPA/SED ratio"
          ),
          
          "Daily mean | median over valid days" = c(
            paste0(results_summary_means[["valid_days"]], " day(s)"),
            paste0(format(round(results_summary_means[["wear_time"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["wear_time"]]), ") | ",  
                   format(round(results_summary_medians[["wear_time"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["wear_time"]]), ")"),  
            paste0(format(round(results_summary_means[["total_kcal"]], 2), nsmall = 2),   " kcal | ", format(round(results_summary_medians[["total_kcal"]], 2), nsmall = 2), " kcal"),
            paste0(format(round(results_summary_means[["pal"]], 2), nsmall = 2),  " | ", format(round(results_summary_medians[["pal"]], 2), nsmall = 2)),  
            paste0(round(results_summary_means[["total_steps"]], 0), " steps | ", round(results_summary_medians[["total_steps"]], 0), " steps"), 
            paste0(format(round(results_summary_means[["minutes_SED"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["minutes_SED"]]), ") | ",  
                   format(round(results_summary_medians[["minutes_SED"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["minutes_SED"]]), ")"),  
            paste0(format(round(results_summary_means[["minutes_LPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["minutes_LPA"]]), ") | ",  
                   format(round(results_summary_medians[["minutes_LPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["minutes_LPA"]]), ")"), 
            paste0(format(round(results_summary_means[["minutes_MVPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["minutes_MVPA"]]), ") | ",
                   format(round(results_summary_medians[["minutes_MVPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["minutes_MVPA"]]), ")"),  
            paste0(format(round(results_summary_means[["percent_SED"]], 1), nsmall = 1), " % | ", format(round(results_summary_medians[["percent_SED"]], 1), nsmall = 1), " %"),
            paste0(format(round(results_summary_means[["percent_LPA"]], 1), nsmall = 1), " % | ", format(round(results_summary_medians[["percent_LPA"]], 1), nsmall = 1), " %"),        
            paste0(format(round(results_summary_means[["percent_MVPA"]], 1), nsmall = 1), " % | ", format(round(results_summary_medians[["percent_MVPA"]], 1), nsmall = 1), " %"),                 
            paste0(format(round(results_summary_means[["mets_hours_mvpa"]], 2), nsmall = 2), " MET-hr | ", format(round(results_summary_medians[["mets_hours_mvpa"]], 2), nsmall = 2), " MET-hr"),         
            paste0(format(round(results_summary_means[["ratio_mvpa_sed"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["ratio_mvpa_sed"]], 2), nsmall = 2)))
        )
      ) %>%
      flextable::theme_zebra() %>%
      flextable::align(align = "left", part = "all" ) %>%
      flextable::width(width = 3.25)
    
    return(flextable_summary)
    
  }

if (language == "fr") {
  
  flextable_summary <- 
    flextable::flextable(
      tibble::tibble(
        Indicateur = c(
          "Nombre de jours valides",                
          "Temps de port", 
          "Kilocalories",                           
          "NAP",       
          "Nombre total de pas", 
          "Temps SED",                              
          "Temps LPA",                              
          "Temps MVPA",                             
          "Proportion de temps de port SED",        
          "Proportion de temps de port LPA",        
          "Proportion de temps de port MVPA",       
          "Dose MVPA",                              
          "Ratio MVPA/SED"                       
        ),
        
        "Moyenne | m\u00e9diane journali\u00e8re obtenue \u00e0 partir des jours valides" = c(
          paste0(results_summary_means[["valid_days"]], " jour(s)"),
          paste0(format(round(results_summary_means[["wear_time"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["wear_time"]]), ") | ",  
                 format(round(results_summary_medians[["wear_time"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["wear_time"]]), ")"),  
          paste0(format(round(results_summary_means[["total_kcal"]], 2), nsmall = 2),   " kcal | ", format(round(results_summary_medians[["total_kcal"]], 2), nsmall = 2), " kcal"),
          paste0(format(round(results_summary_means[["pal"]], 2), nsmall = 2),  " | ", format(round(results_summary_medians[["pal"]], 2), nsmall = 2)),  
          paste0(round(results_summary_means[["total_steps"]], 0), " pas | ", round(results_summary_medians[["total_steps"]], 0), " pas"), 
          paste0(format(round(results_summary_means[["minutes_SED"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["minutes_SED"]]), ") | ",  
                 format(round(results_summary_medians[["minutes_SED"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["minutes_SED"]]), ")"),  
          paste0(format(round(results_summary_means[["minutes_LPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["minutes_LPA"]]), ") | ",  
                 format(round(results_summary_medians[["minutes_LPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["minutes_LPA"]]), ")"), 
          paste0(format(round(results_summary_means[["minutes_MVPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_means[["minutes_MVPA"]]), ") | ",
                 format(round(results_summary_medians[["minutes_MVPA"]], 1), nsmall = 1), " min (", hms::hms(minutes = results_summary_medians[["minutes_MVPA"]]), ")"),  
          paste0(format(round(results_summary_means[["percent_SED"]], 1), nsmall = 1), " % | ", format(round(results_summary_medians[["percent_SED"]], 1), nsmall = 1), " %"),
          paste0(format(round(results_summary_means[["percent_LPA"]], 1), nsmall = 1), " % | ", format(round(results_summary_medians[["percent_LPA"]], 1), nsmall = 1), " %"),        
          paste0(format(round(results_summary_means[["percent_MVPA"]], 1), nsmall = 1), " % | ", format(round(results_summary_medians[["percent_MVPA"]], 1), nsmall = 1), " %"),                 
          paste0(format(round(results_summary_means[["mets_hours_mvpa"]], 2), nsmall = 2), " MET-hr | ", format(round(results_summary_medians[["mets_hours_mvpa"]], 2), nsmall = 2), " MET-hr"),         
          paste0(format(round(results_summary_means[["ratio_mvpa_sed"]], 2), nsmall = 2), " | ", format(round(results_summary_medians[["ratio_mvpa_sed"]], 2), nsmall = 2)))
      )
    ) %>%
    flextable::theme_zebra() %>%
    flextable::align(align = "left", part = "all" ) %>%
    flextable::width(width = 3.25)
  
  return(flextable_summary)
  
 }
  

}

