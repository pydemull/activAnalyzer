#' Compute metabolic equivalent of task (MET) values
#' 
#' This function computes metabolic equivalent of task (METs) from weight, sex, accelerometer counts, and a published 
#'      equation from one of the following scientific articles: Sasaki et al. (2011; doi:10.1016/j.jsams.2011.04.003); Santos-Lozano et al. 
#'     (2013; 10.1055/s-0033-1337945); Freedson et al. (1998; doi: 10.1097/00005768-199805000-00021). This function is wrapped 
#'     within the \code{\link{mark_intensity}} function.
#'
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}} function.
#' @param equation A character string to indicate the equation to be used for estimating METs.
#' @param weight A numeric value in kg.
#' @param sex A character value.
#'
#' @return A numeric vector.
#' @export
#'
#' @examples
#' library(magrittr)
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file)
#' mydata_with_wear_marks <- mydata %>% mark_wear_time() %>% 
#' dplyr::filter(days == 2 & time >= hms::as_hms("14:00:00") & time <= hms::as_hms("15:00:00")) 
#' mets <- compute_mets(
#'     data = mydata_with_wear_marks,
#'     equation = "Sasaki et al. (2011) [Adults]", 
#'     weight = 67, 
#'     sex = "male"
#'     )
#' mets
#' 
compute_mets <- function(data, 
                         equation = c("Sasaki et al. (2011) [Adults]",
                                      "Santos-Lozano et al. (2013) [Adults]",
                                      "Freedson et al. (1998) [Adults]",
                                      "Santos-Lozano et al. (2013) [Older adults]"),
                         weight = 70, 
                         sex = c("male", "female", "intersex", "undefined", "prefer not to say")) {
  
  equation <- match.arg(equation)
  sex <- match.arg(sex)
  
  BM <- weight

  
  METS <- dplyr::case_when(
     equation == "Santos-Lozano et al. (2013) [Older adults]" & sex == "male"                                                    ~ 2.5878 + 0.00047 * data$vm - 0.6453 * 2,
     equation == "Santos-Lozano et al. (2013) [Older adults]" & sex %in% c("female", "undefined", "prefer not to say")           ~ 2.5878 + 0.00047 * data$vm - 0.6453 * 1,
     equation == "Santos-Lozano et al. (2013) [Older adults]" & sex == "intersex"                                                ~ ((2.5878 + 0.00047 * data$vm - 0.6453 * 2) + (2.5878 + 0.00047 * data$vm - 0.6453 * 1)) / 2,
     
     equation == "Santos-Lozano et al. (2013) [Adults]" & sex == "male"                                                          ~ 2.8323 + 0.00054 * data$vm - 0.05912 * BM + 1.4410 * 2,
     equation == "Santos-Lozano et al. (2013) [Adults]" & sex %in% c("female", "undefined", "prefer not to say")                 ~ 2.8323 + 0.00054 * data$vm - 0.05912 * BM + 1.4410 * 1,
     equation == "Santos-Lozano et al. (2013) [Adults]" & sex == "intersex"                                                      ~ ((2.8323 + 0.00054 * data$vm - 0.05912 * BM + 1.4410 * 2) + (2.8323 + 0.00054 * data$vm - 0.05912 * BM + 1.4410 * 1)) / 2, 
                               
     equation == "Sasaki et al. (2011) [Adults]"                                                                                 ~ 0.668876 + 0.000863 * data$vm,
     equation == "Freedson et al. (1998) [Adults]"                                                                               ~ 1.439008 + 0.000795 * data$axis1
  )
  
  
  message(paste0("You have computed METs using the following inputs: 
    equation = " , equation, "
    weight = ", weight, "
    sex = ", sex))
  
  return(METS)
  

}