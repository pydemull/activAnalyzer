recap_by_day <- function(data, col_date = date, age = 40, weight = 70, sex = "male") {
  
  # Computing basal metabolic rate
    bmr_kcal_min <- compute_bmr(age = age, sex = sex, weight = weight) / (24*60)
  
  # getting results by day
  df <-
    data %>%
    group_by({{ col_date }}, .drop = FALSE) %>%
    filter(wearing == "w") %>%
    summarise(
      wear_time = sum(wearing_count),
      total_counts_axis1 = sum(axis1),
      total_counts_vm = sum(vm),
      total_steps = sum(steps),
      total_kcal_wear_time = round(sum(kcal), 2),
      minutes_SED = sum(SED),
      minutes_LPA = sum(LPA),
      minutes_MPA = sum(MPA),
      minutes_VPA = sum(VPA),
      minutes_MVPA = sum(MPA) + sum(VPA),
      percent_SED = round(minutes_SED / wear_time * 100, 2),
      percent_LPA = round(minutes_LPA / wear_time * 100, 2),
      percent_MPA = round(minutes_MPA / wear_time * 100, 2),
      percent_VPA = round(minutes_VPA / wear_time * 100, 2), 
      percent_MVPA = round(minutes_MVPA / wear_time * 100, 2),
      mets_hours_mvpa = round(sum(mets_hours_mvpa), 2),
      ratio_mvpa_sed = round(minutes_MVPA / minutes_SED, 2),
      
      # Computing physical activity level (PAL), that is, total EE / BMR. BMR is assigned to nonwear time; 
      # the term 10/9 is used to take into account the thermic effect of food
      pal = round((total_kcal_wear_time + bmr_kcal_min * (24*60 - wear_time)) * 10/9 / (bmr_kcal_min * (24*60)), 2)) %>%
    ungroup()
  
  return(df)
}

