#' Compute Basal Metabolic Rate (BMR)
#' 
#' This function computes Basal Metabolic Rate using an Henry et al. 
#'     (2005; doi: 10.1079/PHN2005801) equation. This function is wrapped within 
#'     the \code{\link{mark_intensity}} and \code{\link{recap_by_day}} function.
#'
#' @param age A numeric value in yr.
#' @param weight A numeric value in kg.
#' @param sex A character value.
#'
#' @return A numeric value.
#' @export
#'
#' @examples
#' compute_bmr(age = 32, sex = "male", weight = 67)
#' 
compute_bmr <- function(age = 40, sex = c("male", "female", "undefined"), weight = 70) {
  
sex <- match.arg(sex)
  

# BMR using sex and weight only (Henry, 2005; doi: 10.1079/PHN2005801)
bmr <-
  ifelse(age <3 & sex == "male", 61.0 * weight - 33.7,
         ifelse(age >=3 & age <10 & sex == "male", 23.3 * weight + 514,
                ifelse(age >=10 & age <18 & sex == "male", 18.4 * weight + 581,
                       ifelse(age >=18 & age <30 & sex == "male", 16.0 * weight + 545,
                              ifelse(age >=30 & age <60 & sex == "male", 14.2  * weight + 593,
                                     ifelse(age >=60 & age <70 & sex == "male", 13.0 * weight + 567,
                                            ifelse(age >=70 & sex == "male", 13.7 * weight + 481,
                                                   
                                                   ifelse(age <3 & sex == "female", 58.9 * weight - 23.1,
                                                          ifelse(age >=3 & age <10 & (sex == "female" | sex == "undefined"), 20.1 * weight + 507,
                                                                 ifelse(age >=10 & age <18 & (sex == "female" | sex == "undefined"), 11.1 * weight + 761,
                                                                        ifelse(age >=18 & age <30 & (sex == "female" | sex == "undefined"), 13.1 * weight + 558,
                                                                               ifelse(age >=30 & age <60 & (sex == "female" | sex == "undefined"), 9.74 * weight + 694,
                                                                                      ifelse(age >=60 & age <70 & (sex == "female" | sex == "undefined"), 10.2 * weight +  572,
                                                                                             ifelse(age >=70 & (sex == "female" | sex == "undefined"), 10.0 * weight + 577))))))))))))))

# BMR using sex, weight and height (Henry, 2005; doi: 10.1079/PHN2005801)

#bmr_bis <-
#  ifelse(age <3 & sex == "male", 28.2 * weight + 859 * height - 371,
#         ifelse(age >=3 & age <10 & sex == "male", 15.1 * weight + 74.2 * height + 306,
#                ifelse(age >=10 & age <18 & sex == "male", 15.6 * weight + 266 * height + 299,
#                       ifelse(age >=18 & age <30 & sex == "male", 14.4 * weight + 313 * height + 113,
#                              ifelse(age >=30 & age <60 & sex == "male", 11.4 * weight + 541 * height - 137,
#                                     ifelse(age >=60 & sex == "male", 11.4 * weight + 541 * height - 256,
#                                            
#                                            ifelse(age <3 & sex == "female", 30.4 * weight + 703 * height - 287,
#                                                   ifelse(age >=3 & age <10 & (sex == "female" | sex == "undefined"), 15.9 * weight + 210 * height + 349,
#                                                          ifelse(age >=10 & age <18 & (sex == "female" | sex == "undefined"), 9.40 * weight + 249 * height + 462,
#                                                                 ifelse(age >=18 & age <30 & (sex == "female" | sex == "undefined"), 10.4 * weight + 615 * height - 282,
#                                                                        ifelse(age >=30 & age <60 & (sex == "female" | sex == "undefined"), 8.18 * weight + 502 * height - 11.6,
#                                                                               ifelse(age >=60 & (sex == "female" | sex == "undefined"), 8.52 * weight + 421 * height + 10.7))))))))))))

message(paste0("You have computed BMR using the following inputs: 
    age = " , age, "
    weight = ", weight, "
    sex = ", sex))

return(bmr)

}

