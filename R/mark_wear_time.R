#' Mark dataset for nonwear/wear time
#' 
#' This function wraps the \code{\link[PhysicalActivity]{wearingMarking}} function
#'     from the `PhysicalActivity` package, and adds two variables to the dataset: the variable `non_wearing_count` 
#'     that contains the number 1 when the device was *not* worn (otherwise, 0 is used), 
#'     and the variable `wearing_count` that contains the number 1 when the device 
#'     was worn (otherwise, 0 is used).
#'
#' @param dataset A dataframe obtained using the \code{\link{prepare_dataset}} function.
#' @param TS A character value indicating the name of the variable where date and time information are provided.
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
#' mydata <- prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp")
#' mark_wear_time(
#'     dataset = mydata, 
#'     TS = "timestamp", 
#'     cts  = "vm", 
#'     frame = 90, 
#'     allowanceFrame = 2, 
#'     streamFrame = 30
#'     )
#' 
mark_wear_time <- function(dataset, TS = "timestamp", cts  = "vm", frame = 90, allowanceFrame = 2, streamFrame = 30) {
  
  df <-
    PhysicalActivity::wearingMarking(
                 dataset = dataset, 
                 TS = TS, 
                 cts = cts, 
                 frame = frame, 
                 allowanceFrame = allowanceFrame,
                 streamFrame = streamFrame) %>%
    dplyr::mutate(non_wearing_count = ifelse(wearing == "nw", 1, 0),
                  wearing_count = ifelse(wearing == "w", 1, 0))
  
  return(df)
  
}
