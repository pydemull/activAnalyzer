#' Get comment about the MPVA/SED ratio
#'
#' @param value A numeric value to indicate the daily mean of MVPA/SED ratio.
#' @param language A character value for setting the language with which the table should be created: "en" for english; "fr" for french.
#'
#' @return A character string.
#' @export
#'
#' @examples get_ratio_mvpa_sed_comment(value = 0.03)
get_ratio_mvpa_sed_comment <- function(value, language = c("en", "fr")) {
  
language <- match.arg(language)
  
if (language == "en") {
  comment_ratio <- 
    ifelse(value >= 0.04, "the patient likely already has most of the health benefits from their physical behavior",
           "the patient could get further health benefits by replacing more sedentary time by physical activity time")
  return(comment_ratio)
 }

  
  if (language == "fr") {
    comment_ratio <- 
    ifelse(value >= 0.04, "le patient obtient déjà probablement la plupart des bénéfices de santé liés à son comportement physique",
           "le patient pourrait probablement obtenir davantage de bénéfices de santé en replaçant du temps sédentaire par du temps d'activité physique")
    return(comment_ratio)
  }

}