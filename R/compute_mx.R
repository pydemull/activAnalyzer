#' Compute MX metric
#' 
#' An MX metric is the count/epoch value at and above which a given amount of the
#'     most active minutes is spent (continuously or discontinuously)
#'
#' @param x A vector of counts data.
#' @param n A integer setting the number of rows in correspondance with the 
#'     targetted amount of time.
#'
#' @return A numeric value.
#'
compute_mx <- function(x, n) {
  
  vec_length <- length(head(sort(x, decreasing = TRUE), n = n))
  
  if (vec_length < n) {NA} 
  else {
    min(head(sort(x, decreasing = TRUE), n = n), na.rm = TRUE) %>% round(., 2)
  }
}