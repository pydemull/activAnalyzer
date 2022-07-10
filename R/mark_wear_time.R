#' Mark dataset for nonwear/wear time
#' 
#' This function wraps the \code{\link[PhysicalActivity]{dataCollapser}} and the 
#'     \code{\link[PhysicalActivity]{wearingMarking}} functions from the `PhysicalActivity` package.
#'     After collapsing data, the function adds `time` and
#'     `date` columns. Then, the function analyzes the dataset for nonwear time detection. Finally, the function
#'     adds two variables to the dataset: the variable `non_wearing_count` 
#'     that contains the number 1 when the device was *not* worn (otherwise, 0 is used), 
#'     and the variable `wearing_count` that contains the number 1 when the device 
#'     was worn (otherwise, 0 is used).
#'
#' @param dataset A dataframe obtained using the \code{\link{prepare_dataset}} function.
#' @param TS A character value indicating the name of the variable where date and time information are provided.
#' @param to_epoch A numeric value indicating the length of the epoch to use (in seconds) for accumulating data. The value must be superior or equal to the recording epoch that was used for the measurement.
#' @param cts A character value indicating the name of the variable used by the nonwear/wear detection algorithm.
#' @param frame A numeric value for the length of the time window (in minutes) used to detect nonwear/wear time.
#' @param allowanceFrame A numeric value for the length of the time window (in minutes) with nonzero counts allowed 
#'     within the detected nonwear period.
#' @param streamFrame A numeric value for the length of the time window required around the detected activity 
#'     to validate nonwear time.
#'
#' @return A dataframe.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file)
#' mydata_with_wear_marks <- mark_wear_time(
#'     dataset = mydata, 
#'     TS = "TimeStamp", 
#'     to_epoch = 60,
#'     cts  = "vm",
#'     frame = 90, 
#'     allowanceFrame = 2, 
#'     streamFrame = 30
#'     )
#' head(mydata_with_wear_marks)
#' 
mark_wear_time <- function(
  dataset, 
  TS = "TimeStamp", 
  to_epoch = 60, 
  cts  = "vm",  
  frame = 90, 
  allowanceFrame = 2, 
  streamFrame = 30
  ) {
  
  # Collapsing data and creating date and time columns
  
  if (to_epoch == as.numeric(difftime(dataset[[TS]][2], dataset[[TS]][1], units = "secs")))  { 
    
    df <- 
      dataset %>%
      dplyr::mutate(
            timestamp = as.character(.data[[TS]]),
            timeStamp2 = timestamp
          ) %>%
      tidyr::separate("timeStamp2", c("date", "time"), sep = " ") %>%
      dplyr::mutate(date = as.Date(date), time = hms::as_hms(time)) %>%
      dplyr::select(
        timestamp, 
        date, 
        time, 
        everything(),
        -TimeStamp
          )
    
  } else {
      
    
   df <- 
     PhysicalActivity::dataCollapser(
       dataset = dataset, 
       TS = TS, 
       by = to_epoch
       ) %>%
     dplyr::mutate(
       timestamp = as.character(.data[[TS]]),
       timeStamp2 = timestamp
     ) %>%
   tidyr::separate("timeStamp2", c("date", "time"), sep = " ") %>%
   dplyr::mutate(date = as.Date(date), time = hms::as_hms(time)) %>%
   dplyr::select(
     timestamp, 
     date, 
     time, 
     everything(),
     -TimeStamp
   )
  
  }
  
  # Getting time factor for using the wearingMarking() function
  perMinuteCts <-  60 / (as.numeric(df$time[2] - df$time[1]))

  # Marking dataset for wear time
  df2 <-
    PhysicalActivity::wearingMarking(
      dataset = df, 
      TS = "timestamp", 
      cts = cts,
      perMinuteCts = perMinuteCts,
      frame = frame, 
      allowanceFrame = allowanceFrame,
      streamFrame = streamFrame
      ) %>%
    dplyr::mutate(
      non_wearing_count = ifelse(wearing == "nw", 1, 0),
      wearing_count = ifelse(wearing == "w", 1, 0)
      )
  
  return(df2)
  
}
