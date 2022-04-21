#' Compute Rasch transformation for PROactive scores
#' 
#' This function provides the 0-100 Rasch scaled score of a given C-PPAC or D-PPAC raw score (based on: Garcia-Aymerich J, et al. Thorax 2021;0:1–11. doi: 10.1136/thoraxjnl-2020-214554).
#'
#' @param x A numeric value that is the difficulty score (between 0 and 40 for C-PPAC or 0 and 20 for D-PPAC) or the quantity score (between 0 and 15 for C-PPAC or 0 and 17 for D-PPAC) obtained using a PROactive questionnaire.
#' @param score A character value.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' rasch_transform(33, quest = "C-PPAC", score = "difficulty")
#' 
rasch_transform <- function(x, quest = c("C-PPAC", "D-PPAC"), score = c("difficulty", "quantity")) {

quest <- match.arg(quest)
score <- match.arg(score)

if (quest == "C-PPAC" && score == "difficulty") {
  
rasch_score <- dplyr::case_when(
      x == 0 ~ 0,
      x == 1 ~ 8,
      x == 2 ~ 15,
      x == 3 ~ 20,
      x == 4 ~ 24,
      x == 5 ~ 28,
      x == 6 ~ 31,
      x == 7 ~ 34,
      x == 8 ~ 36,
      x == 9 ~ 38,
      x == 10 ~ 40,
      x == 11 ~ 42,
      x == 12 ~ 44,
      x == 13 ~ 46,
      x == 14 ~ 48,
      x == 15 ~ 50,
      x == 16 ~ 51,
      x == 17 ~ 53,
      x == 18 ~ 55,
      x == 19 ~ 56,
      x == 20 ~ 58,
      x == 21 ~ 60,
      x == 22 ~ 61,
      x == 23 ~ 63,
      x == 24 ~ 65,
      x == 25 ~ 66,
      x == 26 ~ 68,
      x == 27 ~ 70,
      x == 28 ~ 72,
      x == 29 ~ 73,
      x == 30 ~ 75,
      x == 31 ~ 77,
      x == 32 ~ 79,
      x == 33 ~ 81,
      x == 34 ~ 83,
      x == 35 ~ 86,
      x == 36 ~ 89,
      x == 37 ~ 92,
      x == 38 ~ 94,
      x == 39 ~ 97,
      x == 40 ~ 100
)

}

if (quest == "C-PPAC" && score == "quantity") {
  
  rasch_score <- dplyr::case_when(
    
      x == 0 ~ 0,
      x == 1 ~ 13,
      x == 2 ~ 25,
      x == 3 ~ 33,
      x == 4 ~ 39,
      x == 5 ~ 45,
      x == 6 ~ 50,
      x == 7 ~ 54,
      x == 8 ~ 59,
      x == 9 ~ 63,
      x == 10 ~ 67,
      x == 11 ~ 72, 
      x == 12 ~ 77,
      x == 13 ~ 83,
      x == 14 ~ 91,
      x == 15 ~ 100
    
  )
  
}


if (quest == "D-PPAC" && score == "difficulty") {
  
  rasch_score <- dplyr::case_when(
       x == 0 ~ 0,
       x == 1 ~ 10,
       x == 2 ~ 20,
       x == 3 ~ 26,
       x == 4 ~ 32.,
       x == 5 ~ 36,
       x == 6 ~ 40,
       x == 7 ~ 43,
       x == 8 ~ 46,
       x == 9 ~ 49,
       x == 10 ~ 52,
       x == 11 ~ 56,
       x == 12 ~ 59,
       x == 13 ~ 62,
       x == 14 ~ 65,
       x == 15 ~ 68,
       x == 16 ~ 72,
       x == 17 ~ 77,
       x == 18 ~ 84,
       x == 19 ~ 92,
       x == 20 ~ 100
  )
  
}
  

if (quest == "D-PPAC" && score == "quantity") {
  
  rasch_score <- dplyr::case_when(
    
       x == 0 ~ 0,
       x == 1 ~ 10,
       x == 2 ~ 19,
       x == 3 ~ 25,
       x == 4 ~ 31,
       x == 5 ~ 35,
       x == 6 ~ 39,
       x == 7 ~ 43,
       x == 8 ~ 47,
       x == 9 ~ 50,
       x == 10 ~ 54,
       x == 11 ~ 57, 
       x == 12 ~ 61,
       x == 13 ~ 65,
       x == 14 ~ 71,
       x == 15 ~ 80,
       x == 16 ~ 90,
       x == 17 ~ 100
    
  )
  
}

return(rasch_score)


}