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
      x >  5700 ~ 4,
      x >= 4001 ~ 3,
      x >= 2201 ~ 2,
      x >= 1301 ~ 1,
      x <  1301 ~ 0
    )
    
    return(score)
    
   }

if (metric == "vmu") {
    
  score <- dplyr::case_when(
      x >  490 ~ 4,
      x >= 351 ~ 3,
      x >= 261 ~ 2,
      x >= 181 ~ 1,
      x <  181 ~ 0 
    )
  
  return(score)
  
   }

}
