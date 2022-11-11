#' Prepare accelerometer data
#' 
#' This function reads an .agd file and then creates the vector magnitude variable as follows: 
#'     \eqn{vm = \sqrt{axis1^{2} + axis2^{2} + axis3^{2}}}. 
#'     The .agd file must contain at least the following columns:
#' \itemize{
#'   \item \strong{axis1}
#'   \item \strong{axis2}
#'   \item \strong{axis3}
#'   \item \strong{steps}


#'}
#'
#' @param data Path to an .agd file that was exported from ActiLife software.
#'
#' @return A dataframe.
#' 
#' @export
#' 
#' @importFrom magrittr %>%
#'
#' @examples
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file)
#' head(mydata)
#' 
prepare_dataset <- function(data) {
  
  df <-
    read_agd(data) %>%
    as.data.frame() %>%
    dplyr::rename(TimeStamp = timestamp) %>%
    dplyr::mutate(
      vm = round(sqrt(axis1^2 + axis2^2 + axis3^2), 2)
    )
  
  return(df)
  
}
