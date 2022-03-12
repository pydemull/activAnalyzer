#' Compute PROactive monitor-based physical activity score for C-PPAC tool
#' 
#' This function computes the PROactive activity score based on the daily median or mean of 
#' step count or vector magnitude unit (in counts/min) obtained using an ActiGraph accelerometer.
#'
#' @param x A numeric value that should be the daily median or the daily mean of step count or vector magnitude unit following a measurement of physical activity; 
#' see Gimeno-Santos et al. (2015, online supplement, p.71, doi: 10.1183/09031936.00183014) and Garcia-Aymerich et al. (2021, supplemental material, p.17; doi: 10.1136/thoraxjnl-2020-214554).
#' @param metric A character value to indicate the metric for which the PROactive score should be obtained.
#' @param fun A character value to indicate if the metric used in the function is the median or the mean of the results obtained each day of the measurement.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' compute_pro_actigraph_score(x = 3500, metric = "steps")
#' 
#' compute_pro_actigraph_score(x = 340, metric = "vmu")
#' 
compute_pro_actigraph_score <- function(x, metric = c("steps", "vmu"), fun = "median") {
  
metric = match.arg(metric)

# Algorithms based on Gimeno-Santos et al., 2015, online supplement, p.71; doi: 10.1183/09031936.00183014).

if (metric == "steps" && fun == "median") {
  
  score <- dplyr::case_when(
    x >  6000 ~ 4,
    x >= 4001 ~ 3,
    x >= 2001 ~ 2,
    x >= 1001 ~ 1,
    x <  1001 ~ 0
  )
  
  return(score)
  
}

if (metric == "vmu" && fun == "median") {
  
  score <- dplyr::case_when(
    x >  500 ~ 4,
    x >= 301 ~ 3,
    x >= 201 ~ 2,
    x >= 101 ~ 1,
    x <  101 ~ 0 
  )
  
  return(score)
  
}

# Algorithms based on Garcia-Aymerich et al., 2021, supplemental material, p.17; doi: 10.1136/thoraxjnl-2020-214554)
  
if (metric == "steps" && fun == "mean") {
  
    score <- dplyr::case_when(
      x >  5700 ~ 4,
      x >= 4001 ~ 3,
      x >= 2201 ~ 2,
      x >= 1301 ~ 1,
      x <  1301 ~ 0
    )
    
    return(score)
    
   }

if (metric == "vmu" && fun == "mean") {
    
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
