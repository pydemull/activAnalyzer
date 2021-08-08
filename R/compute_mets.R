
compute_mets <- function(data = NULL, equation = NULL, weight = NULL, gender = NULL) {
  
  BM <- weight
  
  if (gender == "male") {G <- 2
  } else { G <- 1}
  

  
  if (equation == "freedson_1998_walk_adults") { METS <- 1.439008 + 0.000795 * data$axis1 }
  if (equation == "hendelman_2000_walk_adults") { METS <- 1.602 + 0.000638 * data$axis1 }
  if (equation == "hendelman_2000_walk_adl_adults") { METS <- 2.922 + 0.000409 * data$axis1 }
  if (equation == "nichols_2000_walk_adults") { METS <- (6.057359 + 0.002545 * data$axis1) / 3.5 }
  if (equation == "sasaki_2011_walk_adults") { METS <- 0.668876 + 0.000863 * data$vm }
  if (equation == "santos_lozano_2013_mixed_youth") { METS <- 1.546618 + 0.000658 * data$vm }
  if (equation == "santos_lozano_2013_mixed_adults") { METS <- 2.8323 + 0.00054 * data$vm - 0.05912 * BM + 1.4410 * G }
  if (equation == "santos_lozano_2013_mixed_older_adults") { METS <- 2.5878 + 0.00047 * data$vm - 0.6453 * G }

  return(METS)
}