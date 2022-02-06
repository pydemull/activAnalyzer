#' Get WHO physical activity guidelines status
#'
#' @param value A numeric value to indicate the daily mean of MET-hours spent at moderate-to-vigorous physical activity intensity.
#' @param language A character value for setting the language with which the table should be created: "en" for english; "fr" for french.
#'
#' @return A character string.
#' @export
#'
#' @examples get_guidelines_status(value = 5)
get_guidelines_status <- function(value, language = c("en", "fr")) {
  
language <- match.arg(language)

if (language == "en") {
  
  guidelines_status <- 
    ifelse(value < 150/7/60*3, "below the 2020 WHO physical activity guidelines",
           ifelse(value >  150/7/60*3*2, "above the 2020 WHO physical activity guidelines", "within the 2020 WHO physical activity guidelines"))
  return(guidelines_status)
}


if (language == "fr") { 
  
  guidelines_status <- 
    ifelse(value < 150/7/60*3, "en-dessous des recommandations d'AP 2020 de l'OMS",
           ifelse(value >  150/7/60*3*2, "au-dessus des recommandations d'AP 2020 de l'OMS", "within the 2020 WHO physical activity guidelines"))
  
  return(guidelines_status)
  
  }
  
}