prepare_dataset <- function(data, epoch_len_out = 60, time_stamp = timestamp) {
  
  df <-
    data %>%
    read_agd() %>%
    collapse_epochs_updated(epoch_len_out) %>%
    mutate(vm = round(sqrt(axis1^2 + axis2^2 + axis3^2), 2),
           timestamp = as.character({{ time_stamp }}),
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
