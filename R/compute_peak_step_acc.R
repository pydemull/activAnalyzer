#' Compute mean step accumulation (per min) from a given number of the best continous 
#'     or discontinuous minutes
#'
#' @param x A vector of steps data, each value corresponding to a total of steps 
#'     in a minute.
#' @param n An integer value setting the number of minutes to be used to compute
#'     the metric.
#'
#' @return A numeric value.
#'
compute_peak_step_acc <- function(x, n) {
  
  vec_length <- length(head(sort(x, decreasing = TRUE), n = n))
  
  if (vec_length < n) {NA} 
    else {
      round(mean(head(sort(x, decreasing = TRUE), n = n), na.rm = TRUE), 2)
    }
}
