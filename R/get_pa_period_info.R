#' Get relevant missing physical activity information indicated by the user of the app
#'
#' @param period A character string pointing to the module id from which information
#'     have to be catched
#'
#' @return
#'
get_pa_period_info <- function(period) {
  
  # Getting inputs from module
  date      <- period$corr_date()
  start_hh  <- period$corr_start_time_hh()
  start_mm  <- period$corr_start_time_mm()
  end_hh    <- period$corr_end_time_hh()
  end_mm    <- period$corr_end_time_mm()
  METS      <- period$corr_mets() 
  
  # Building dataframe from module inputs
  df <- 
    data.frame(
      date     = date,
      start_hh = start_hh,
      start_mm = start_mm,
      end_hh   = end_hh,
      end_mm   = end_mm,
      METS     = METS
    ) %>%
    dplyr::mutate(
      start = hms::as_hms(start_hh*3600 + start_mm*60),
      end = hms::as_hms(end_hh*3600 + end_mm*60),
    ) %>%
    dplyr::select(date, start, end, METS)
  
  # Return dataframe
  return(df)
  
}
