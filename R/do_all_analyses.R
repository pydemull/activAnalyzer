
#' Do all analyses at once
#' 
#' This function performs all analyses successively using the default data file associated to
#'     the package. It is an internal function allowing the computation of the speed of the whole
#'     analysis process, from the data importation to the final line of the results.
#'
#' @return
#' A dataset (1 row) with all computed metrics.

do_all_analyses <- function(to_epoch = 60){
  
  # Load file
  file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
  
  # Prepare data
  mydata <- prepare_dataset(data = file)
  
  # Detect nonwear time
  mydata_with_wear_marks <- mydata %>% mark_wear_time(to_epoch = to_epoch)
  
  # Add intensity marks
  mydata_with_intensity_marks <- mark_intensity(data = mydata_with_wear_marks)
  
  # Get results by day
  results_by_day <- mydata_with_intensity_marks %>% recap_by_day()
  
  # Average results
  mean_results <- results_by_day$df_all_metrics  %>% average_results()
    
  # Compute accumulation metrics related to sedentary behaviour if the measurement is valid (with wear time >= 600 min)
    valid_dates <- results_by_day$df_all_metrics %>% dplyr::filter(wear_time >= 600) %>% dplyr::pull(date)
    
    if(length(valid_dates) > 0) {
      accum_metrics_sed <- 
        compute_accumulation_metrics(
          data = mydata_with_intensity_marks, 
          behaviour = "sed",
          dates = valid_dates
        )$metrics
    }
    
    if(length(valid_dates) == 0) {
      accum_metrics_sed <- 
        data.frame(
          mean_breaks = NA,
          alpha = NA,
          MBD = NA,
          UBD = NA,
          gini = NA
        )
      
    }
    
    all_results <- cbind(mean_results, accum_metrics_sed)
    
  # Return results
  return(all_results)
}
