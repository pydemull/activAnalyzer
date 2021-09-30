
average_results <- function(data, minimum_wear_time = 10) {
  
  data %>%
  mutate(validity = ifelse(wear_time >= minimum_wear_time * 60, "valid", "invalid")) %>%
  filter(validity == "valid") %>%
  summarise(valid_days = n(),
              wear_time = mean(wear_time),
              total_counts_axis1 = round(mean(total_counts_axis1), 2),
              total_counts_vm = round(mean(total_counts_vm), 2),
              total_steps = round(mean(total_steps), 2),
              total_kcal_wear_time = round(mean(total_kcal_wear_time), 2),
              minutes_SED = round(mean(minutes_SED), 2),
              minutes_LPA = round(mean(minutes_LPA), 2),
              minutes_MPA = round(mean(minutes_MPA), 2),
              minutes_VPA = round(mean(minutes_VPA), 2),
              minutes_MVPA = round(mean(minutes_MVPA), 2),
              percent_SED = round(mean(percent_SED), 2),
              percent_LPA = round(mean(percent_LPA), 2),
              percent_MPA = round(mean(percent_MPA), 2),
              percent_VPA = round(mean(percent_VPA), 2),
              percent_MVPA = round(mean(percent_MVPA), 2),
              mets_hours_mvpa = round(mean(mets_hours_mvpa), 1),
              ratio_mvpa_sed = round(mean(ratio_mvpa_sed), 1),
              pal = round(mean(pal), 2))
  
}
