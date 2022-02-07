#' Get FAO physical activity level (PAL) status (http://www.fao.org/3/y5686e/y5686e07.htm#bm07.3)
#'
#' @param value A numeric value to indicate the daily mean of PAL.
#' @param language A character value for setting the language with which the table should be created: `en` for english; `fr` for french.
#'
#' @return A character string.
#' @export
#'
#' @examples get_pal_status(value = 1.8)
get_pal_status <- function(value, language = c("en", "fr")) {
  
language <- match.arg(language)

if (language == "en") {
  
pal_status <- ifelse(value <1.40, "below the 'Sedentary or light activity lifestyle' category",
                     ifelse(value <= 1.69, "'Sedentary or light activity lifestyle'",
                            ifelse(value <= 1.99, "'Active or moderately active lifestyle'",
                                   ifelse(value <= 2.40, "'Vigorous or vigorously active lifestyle'", "'Likely not sustainable'"))))

return(pal_status)

    }

if (language == "fr") {
  
  pal_status <- ifelse(value <1.40, "en-dessous de la cat\u00e9gorie 'Style de vie s\u00e9dentaire ou l\u00e9g\u00e8rement actif'",
                       ifelse(value <= 1.69, "'Style de vie s\u00e9dentaire ou l\u00e9g\u00e8rement actif'",
                              ifelse(value <= 1.99, "'Style de vie actif ou mod\u00e9r\u00e9ment actif'",
                                     ifelse(value <= 2.40, "'Style de vie vigoureusement actif'", "'Susceptible de ne pas pouvoir \u00eAtre maintenu'"))))
  
  return(pal_status)

    }

}
