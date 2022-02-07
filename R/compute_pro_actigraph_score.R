#' Compute PROactive monitor-based physical activity score
#' 
#' This function computes the PROactive activity score based on the daily median of 
#' step count or vector magnitude unit (in counts/min) obtained using an ActiGraph accelerometer.
#'
#' @param x A numeric value that should be the daily median of step count or vector magnitude unit following a measurement of physical activity (see Gimeno-Santos et al., 2015; doi: 10.1183/09031936.00183014).
#' @param metric A character value to indicate the metric for which the PROactive score should be obtained.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' compute_pro_actigraph_score(x= 3500, metric = "steps")
#' 
#' compute_pro_actigraph_score(x= 340, metric = "vmu")
#' 
compute_pro_actigraph_score <- function(x, metric = c("steps", "vmu")) {
  
metric = match.arg(metric)
  
if (metric == "steps") {
  
    score <- dplyr::case_when(
      x >= 6000 ~ 4,
      x >= 4000 ~ 3,
      x >= 2000 ~ 2,
      x >= 1000 ~ 1,
      x < 1000 ~ 0
    )
    
    return(score)
    
   }

if (metric == "vmu") {
    
  score <- dplyr::case_when(
      x >= 500 ~ 4,
      x >= 300 ~ 3,
      x >= 200 ~ 2,
      x >= 100 ~ 1,
      x < 100 ~ 0 
    )
  
  return(score)
  
   }

}
