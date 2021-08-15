prepare_dataset <- function(data) {
  
  df <-
    readActigraph(data) %>%
    dataCollapser(TS = "TimeStamp", by = 60) %>%
    mutate(TimeStamp = as.character(TimeStamp),
           TimeStamp2 = TimeStamp) %>%
    tidyr::separate("TimeStamp2", c("date", "time"), sep = " ") %>%
    mutate(time = hms::as_hms(time)) %>%
    dplyr::select(TimeStamp, date, time, everything())
  
  return(df)
  
}