#' Prepare accelerometer data
#' 
#' This function performs the following actions: it reads the .agd file, then it collapses 
#'     accelerometer data based on the configured epoch length (for now, only 60 s can be used), 
#'     then it creates the vector magnitude variable, creates `date` and `time` columns, and finally organizes columns 
#'     in a predefined order.
#'     
#' The .agd file must contain at least the following columns:
#' \itemize{
#'   \item \strong{axis1}
#'   \item \strong{axis2}
#'   \item \strong{axis3}
#'   \item \strong{steps}
#'   \item \strong{inclinestanding}
#'   \item \strong{inclinesitting}
#'   \item \strong{inclinelying}

#'}
#'
#' @param data Path to an .agd file that was exported from Actilife software.
#' @param epoch_len_out A numeric value (in seconds) setting the length of the desired epoch for accumulating accelerometer data. For now, only 60 can be used.
#' @param col_time_stamp A character value that corresponds to the name of the variable that contains date and time information.
#'
#' @return A dataframe.
#' 
#' @export
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp")
#' head(mydata)
#' 
prepare_dataset <- function(data, epoch_len_out = 60, col_time_stamp = "timestamp") {
  
  df <-
    data %>%
    actigraph.sleepr::read_agd() %>%
    actigraph.sleepr::collapse_epochs(epoch_len_out = epoch_len_out) %>%
    dplyr::mutate(
      vm = round(sqrt(axis1^2 + axis2^2 + axis3^2), 2),
      col_time_stamp = as.character(.data[[col_time_stamp]]),
      timeStamp2 = col_time_stamp
      ) %>%
    tidyr::separate("timeStamp2", c("date", "time"), sep = " ") %>%
    dplyr::mutate(date = as.Date(date), time = hms::as_hms(time)) %>%
    dplyr::rename(
      inclineStanding = inclinestanding, 
      inclineSitting = inclinesitting,
      inclineLying = inclinelying
      ) %>%
    dplyr::select(
      col_time_stamp, 
      date, 
      time, 
      everything()
      ) %>%
    as.data.frame()
  
  return(df)
  
}
