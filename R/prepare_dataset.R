prepare_dataset <- function(data) {
  
  df <-
    data %>%
    collapse_epochs_updated(60) %>%
    mutate(vm = round(sqrt(axis1^2 + axis2^2 + axis3^2), 2),
           timestamp = as.character(timestamp),
           TimeStamp2 = timestamp) %>%
    tidyr::separate("TimeStamp2", c("date", "time"), sep = " ") %>%
    mutate(time = hms::as_hms(time)) %>%
           rename(inclineStanding = inclinestanding, 
                  inclineSitting = inclinesitting,
                  inclineLying = inclinelying) %>%
    dplyr::select(timestamp, date, time, everything()) %>%
    as.data.frame
  
  return(df)
  
}
