#' Get comment about the MPVA/SED ratio
#'
#' @param value A numeric value to indicate the daily mean of MVPA/SED ratio.
#' @param language A character value for setting the language with which the table should be created: `en` for english; `fr` for french.
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
    ifelse(value >= 0.04, "le patient obtient d\u00e9j\u00e0 probablement la plupart des b\u00e9n\u00e9fices de sant\u00e9 li\u00e9s \u00e0 son comportement physique",
           "le patient pourrait probablement obtenir davantage de b\u00e9n\u00e9fices de sant\u00e9 en rempla\u00e7ant du temps s\u00e9dentaire par du temps d'activit\u00e9 physique")
    return(comment_ratio)
  }

}