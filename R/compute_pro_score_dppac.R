#' Provide score for each question of the D-PPAC
#' 
#' This function provides a score (from 0 to 4) in relation to the response to a given question from the D-PPAC questionnaire.
#'
#' @param x A character string that is the exact response to the considered question from the D-PPAC questionnaire.
#' @param question A character value to inditify the question to be considered when providing the score.
#' @param language  A character value for setting the language of the considered questionnaire.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' compute_pro_score_dppac(
#'     x = "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)", 
#'     question = "q1",
#'     language = "fr"
#'     )
#'     
compute_pro_score_dppac <- function(x, question = c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10", "q11", "q12"), language = c("en", "fr")){
  
question <- match.arg(question)  
language <- match.arg(language)
  
  # English version ----------------------------------------------
  
  if (question == "q1" && language == "en") {
  score <- dplyr::case_when(
      x == "None at all"                               ~ 0,
      x == "A little bit (up to 10 minutes in total)"  ~ 1,
      x == "Some (up to 30 minutes in total)"          ~ 2,
      x == "A lot (up to 1 hour in total)"             ~ 3,
      x == "A great deal (more than 1 hour in total)"  ~ 4
  )
  }
  
  if (question == "q2" && language == "en") {
    score <- dplyr::case_when(
      x ==  "None at all"      ~ 0,
      x ==  "A few"            ~ 1,
      x ==  "Some"             ~ 2,
      x ==  "A lot"            ~ 3,
      x ==  "A large amount"   ~ 4
    )
  }
  
  if (question == "q3" && language == "en") {
    score <- dplyr::case_when(
      x ==  "None at all"      ~ 4,
      x ==  "A little bit"     ~ 3,
      x ==  "Some"             ~ 2,
      x ==  "A lot"            ~ 1,
      x ==  "A great deal"     ~ 0
    )
  }
  
  if (question == "q4" && language == "en") {
    score <- dplyr::case_when(
      x ==  "Not at all"     ~ 4,
      x ==  "Rarely"         ~ 3,
      x ==  "Sometimes"      ~ 2,
      x ==  "Frequently"     ~ 1,
      x ==  "All the time"   ~ 0
    )
  }
  
  if (question == "q5" && language == "en") {
    score <- dplyr::case_when(
      x ==  "Not at all"      ~ 4,
      x ==  "A little bit"    ~ 3,
      x ==  "Moderately"      ~ 2,
      x ==  "Very"            ~ 1,
      x ==  "Extremely"       ~ 0
    )
  }
  
  if (question == "q6" && language == "en") {
    score <- dplyr::case_when(
      x ==  "Not at all"    ~ 4,
      x ==  "A little bit"  ~ 3,
      x ==  "Moderately"    ~ 2,
      x ==  "Very"          ~ 1,
      x ==  "Extremely"     ~ 0
    )
  }
  
  if (question == "q7" && language == "en") {
    score <- dplyr::case_when(
      x ==   "Not at all"     ~ 4,
      x ==   "Rarely"         ~ 3,
      x ==   "Sometimes"      ~ 2,
      x ==   "Frequently"     ~ 1,
      x ==   "All the time"   ~ 0
    )
  }
  
  
  # French version ----------------------------------------------
  
  if (question == "q1" && language == "fr") {
    score <- dplyr::case_when(
      x ==  "Pas du tout"                                                 ~ 0,
      x ==  "Un petit peu (jusqu\u2019\u00e0 10 minutes au total)"        ~ 1,
      x ==  "Un peu (jusqu\u2019\u00e0 30 minutes au total)"              ~ 2,
      x ==  "Beaucoup (jusqu\u2019\u00e0 1 heure au total)"               ~ 3,
      x ==  "Enorm\u00e9ment (plus d\u20191 heure au total)"              ~ 4
    )
  }
  
  if (question == "q2" && language == "fr") {
    score <- dplyr::case_when(
      x == "Aucune"          ~ 0,
      x == "Tr\u00e8s peu"   ~ 1,
      x == "Quelques-unes"   ~ 2,
      x == "Beaucoup"        ~ 3,
      x == "Enorm\u00e9ment" ~ 4
    )
  }
  
  if (question == "q3" && language == "fr") {
    score <- dplyr::case_when(
      x ==   "Pas du tout"      ~ 4,
      x ==   "Un petit peu"     ~ 3,
      x ==   "Quelques-unes"    ~ 2,
      x ==   "Beaucoup"         ~ 1,
      x ==   "Enorm\u00e9ment"  ~ 0
    )
  }
  
  if (question == "q4" && language == "fr") {
    score <- dplyr::case_when(
      x ==   "Jamais"            ~ 4,
      x ==   "Rarement"          ~ 3,
      x ==   "Quelques fois"     ~ 2,
      x ==   "Fr\u00e9quemment"  ~ 1,
      x ==   "Tout le temps"     ~ 0
    )
  }
  
  if (question == "q5" && language == "fr") {
    score <- dplyr::case_when(
      x ==   "Pas du tout"             ~ 4,
      x ==   "Un petit peu"            ~ 3,
      x ==   "Mod\u00e9r\u00e9ment"    ~ 2,
      x ==   "Tr\u00e8s"               ~ 1,
      x ==   "Extr\u00eamement"        ~ 0
    )
  }
  
  if (question == "q6" && language == "fr") {
    score <- dplyr::case_when(
      x ==   "Pas du tout"            ~ 4,
      x ==   "Un petit peu"           ~ 3,
      x ==   "Mod\u00e9r\u00e9ment"   ~ 2,
      x ==   "Tr\u00e8s"              ~ 1,
      x ==   "Extr\u00eamement"       ~ 0
    )
  }
  
  if (question == "q7" && language == "fr") {
    score <- dplyr::case_when(
      x ==  "Jamais"           ~ 4,
      x ==  "Rarement"         ~ 3,
      x ==  "Quelques fois"    ~ 2,
      x ==  "Fr\u00e9quemment" ~ 1,
      x ==  "Tout le temps"    ~ 0
    )
  }

  return(score)

}


