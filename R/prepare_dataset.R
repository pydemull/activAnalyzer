prepare_dataset <- function(data, frame_size, allowanceFrame_size) {
  
  # Formatting dataset
  df <-
    readActigraph(data) %>%
    dataCollapser(TS = "TimeStamp", by = 60) %>%
    mutate(TimeStamp = as.character(TimeStamp),
           TimeStamp2 = TimeStamp) %>%
    tidyr::separate("TimeStamp2", c("date", "time"), sep = " ") %>%
    mutate(time = hms::as_hms(time)) %>%
    select(TimeStamp, date, time, everything())
  
  # Adding wearing marks
  df <- wearingMarking(df, TS = "TimeStamp", cts = "vm", frame = frame_size, allowanceFrame = allowanceFrame_size) %>%
    mutate(non_wearing_count = ifelse(wearing == "nw", 1, 0),
           wearing_count = ifelse(wearing == "w", 1, 0))
  
  return(df)
  
}