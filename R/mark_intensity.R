
mark_intensity <- function(data, axis = vm, sed_cutpoint = 200, mpa_cutpoint = 2690, vpa_cutpoint = 6167, 
                           equation = "Sasaki et al. (2011) [Adults]",
                           age = 40, weight = 70, sex = "male") {
  
  # Computing basal metabolic rate
    bmr_kcal_min <- compute_bmr(age = age, sex = sex, weight = weight) / (24*60)
  
  # Adding metrics
    df <-
      data %>%
      mutate(
      SED = ifelse({{ axis }} < sed_cutpoint, 1, 0),
      LPA = ifelse({{ axis }} >= sed_cutpoint & {{ axis }} < mpa_cutpoint, 1, 0),
      MPA = ifelse({{ axis }} >= mpa_cutpoint & {{ axis }} < vpa_cutpoint, 1, 0), 
      VPA = ifelse({{ axis }} >= vpa_cutpoint, 1, 0),
      METS = compute_mets(data = .data, equation = equation, weight = weight, sex = sex),
      kcal = ifelse(SED == 0, METS * bmr_kcal_min, bmr_kcal_min),
      
  # Computing MET-hr corresponding to MVPA only, for each epoch
    mets_hours_mvpa = ifelse(METS >=3, 1/60 * METS, 0))
    
    return(df)
    
}
