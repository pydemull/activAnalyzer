
compute_mets <- function(data, equation, weight, sex) {
  
  BM <- weight
  
  if (sex == "male") {G <- 2
  } else { G <- 1}
  

  if (equation == "Santos-Lozano et al. (2013) [Older adults]") { METS <- 2.5878 + 0.00047 * data$vm - 0.6453 * G }
  if (equation == "Santos-Lozano et al. (2013) [Adults]") { METS <- 2.8323 + 0.00054 * data$vm - 0.05912 * BM + 1.4410 * G }
  if (equation == "Sasaki et al. (2011) [Adults]") { METS <- 0.668876 + 0.000863 * data$vm }
  if (equation == "Freedson et al. (1998) [Adults]") { METS <- 1.439008 + 0.000795 * data$axis1 }
  

  return(METS)
}