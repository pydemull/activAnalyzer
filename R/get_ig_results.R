
#' Get intensity gradient values and graphics
#' 
#' The values and graphics are respectively computed and created from the daily periods set for analysis and are based on detected wear time.
#'
#' @param data  A dataframe obtained using the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, 
#'     and then the \code{\link{mark_intensity}} functions. Data should be grouped by day and then nested.
#' @param col_axis A character value to indicate the name of the variable to be used to compute total time per bin of intensity.
#' @param col_time A character value to indicate the name of the variable to be used to determine the epoch length of the dataset.
#' @param valid_wear_time_start A character value with the HH:MM:SS format to set the start of the daily period that will be considered for computing metrics.
#' @param valid_wear_time_end A character value with the HH:MM:SS format to set the end of the daily period that will be considered for computing metrics.
#' @param start_first_bin A numeric value to set the lower bound of the first bin of the intensity band (in counts/epoch duration).
#' @param start_last_bin A numeric value to set the lower bound of the last bin of the intensity band (in counts/epoch duration).
#' @param bin_width A numeric value to set the width of the bins of the intensity band (in counts/epoch duration).
#' @param cor_factor A numeric value resulting from the ratio between 60s and the epoch length of the analysed dataset. This is used to convert 
#'     the number of rows into minutes when getting the results.

#' @return A list of objects.
#'
get_ig_results <- function(
    data, 
    col_axis = "vm",
    col_time = "time",
    valid_wear_time_start = "00:00:00",
    valid_wear_time_end = "23:59:59",
    start_first_bin = 0,
    start_last_bin = 10000,
    bin_width = 500,
    cor_factor = 1
    ){
  
  
  #=================================================================
  # Setting the table of bins that will be used for summing time by 
  # intensity bins
  #=================================================================
  
  # Initializing table of bins
  df_bins <-
    data.frame(bin_start = seq(start_first_bin, start_last_bin, bin_width)) %>%
    dplyr::mutate(
      bin_end = bin_start + bin_width,
      bin_start = bin_start + 1,
      bin_num = seq_along(bin_start)
    )
  
  # Correcting the lower bound of the first bin
  df_bins[1, "bin_start"] <- 0
  
  # Getting middles of the bins
  df_bins$bin_mid <- (df_bins$bin_start +  df_bins$bin_end) / 2
  
  # Getting labels
  df_bins$bin_label <- paste0(round(df_bins$bin_start, 0),"-", round(df_bins$bin_end, 0))
  
  # Correcting the value of the upper bound of the last bin (the value has been arbitrarily set so that it is very high)
  df_bins[nrow(df_bins), "bin_end"] <- 50000
  
  # Correcting the label of the last bin
  df_bins[nrow(df_bins), "bin_label"] <- paste0(">", round(df_bins[nrow(df_bins), "bin_start"]-1, 0))
  
  #=============================
  # Getting results and graphics
  #=============================
  
  # Getting dates
  date <- levels(as.factor(data$date))
  
  # Filtering dataset based on selected time periods and wear time
  data <- 
    data %>% 
    dplyr::filter(.data[[col_time]] >= hms::as_hms(valid_wear_time_start) &
                    .data[[col_time]] <= hms::as_hms(valid_wear_time_end) &
                    wearing == "w"
    )
  
  # Initializing vectors for labelling the dataset
  data$bin_num <- vector("double", nrow(data))
  data$bin_mid <- vector("double", nrow(data))
  data$bin_label <- vector("character", nrow(data))
  
  # Marking the dataset with intensity bins
  for (i in 1:nrow(df_bins)) {
    
    data$bin_num <- ifelse(
      data[[col_axis]] >= df_bins[i, "bin_start"],
      df_bins[i, "bin_num"],
      data$bin_num 
    )
    
    data$bin_mid <- ifelse(
      data[[col_axis]] >= df_bins[i, "bin_start"],
      df_bins[i, "bin_mid"],
      data$bin_mid 
    )
    
    data$bin_label <- ifelse(
      data[[col_axis]] >= df_bins[i, "bin_start"],
      df_bins[i, "bin_label"],
      data$bin_label 
    )
    
  }
  
  # Getting total time spent in each of the intensity bins
  recap_bins_int <-
    data %>%
    dplyr::group_by(bin_mid, bin_label) %>%
    dplyr::summarise(duration = dplyr::n() / cor_factor)
  
  
  # Building intensity gradient model
  model_log <- if(nrow(data)>=1){
    summary(lm(log(duration) ~ log(bin_mid), data = recap_bins_int))
  } else {
    NA
  }
  
  
  # Getting plot for accumulated minutes vs Intensity band
  blank_plot <- 
    ggplot() +
    theme_bw() +
    geom_text(aes(0,0,label='N/A')) +
    labs(title = date, x = "", y = "") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
  
  p1 <- if (nrow(data)>=1) {
    ggplot(data = recap_bins_int, aes(x = forcats::fct_reorder(bin_label, bin_mid), y = duration)) +
      geom_bar(stat = "identity") +
      labs(title = date, x = paste0("Intensity band (counts/", 60/cor_factor, "s)"), y = "Accumulated minutes" ) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    blank_plot
  }
  
  
  # Getting Log-Log plot
      
     # Getting equation label
       label_eq <- ifelse(nrow(data)>=1,  paste0("y = ", round(model_log$coefficient[2, 1], 2), "x + ", round(model_log$coefficient[1, 1], 2)), "")
  
     # Getting plot
       p2 <- if(nrow(data) >=1){
         ggplot(data = recap_bins_int, aes(x = log(bin_mid), y = log(duration))) +
           geom_point(size = 4, alpha = 0.5) +
           geom_smooth(method = "lm") +
           theme_bw() +
           annotate(
             x = min(log(recap_bins_int$bin_mid)), 
             y = min(log(recap_bins_int$duration)), "text", 
             label = label_eq,
             hjust = 0, size = 5
           ) +
           labs(title = date, x = "Log(Middle of intensity bin)", y = "Log(Duration)")
       } else {
         blank_plot
       }
  
  
  # Getting intensity gradient
  ig <- ifelse(nrow(data)>=1, round(model_log$coefficient[2, 1], 2), NA)
  
  
  # Making and returning list
  objects <- list(
    date = date,
    ig = ig, 
    p_band = p1,
    p_log = p2
  )
  
  return(objects)
  
  
}