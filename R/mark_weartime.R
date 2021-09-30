
mark_weartime <- function(dataset, TS = "timestamp", cts  = "vm", frame = 90, allowanceFrame = 2) {
  
  df <-
    wearingMarking(dataset = dataset, 
                 TS = TS, 
                 cts = cts, 
                 frame = frame, 
                 allowanceFrame = allowanceFrame) %>%
    mutate(non_wearing_count = ifelse(wearing == "nw", 1, 0),
           wearing_count = ifelse(wearing == "w", 1, 0))
  
  return(df)
  
}
