#' Compute intensity distribution metrics
#'
#' This function computes metrics that describe the distribution of intensity for each day of a dataset. Computations are performed 
#' based on the daily periods set for analysis and on the detected wear time.
#' 
#' @param data  A dataframe obtained using the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, and then the \code{\link{mark_intensity}} functions.
#' @param col_axis A character value to indicate the name of the variable to be used to compute total time per bin of intensity.
#' @param col_time A character value to indicate the name of the variable to be used to determine the epoch length of the dataset.
#' @param valid_wear_time_start A character value with the HH:MM:SS format to set the start of the daily period that will be considered for computing metrics.
#' @param valid_wear_time_end A character value with the HH:MM:SS format to set the end of the daily period that will be considered for computing metrics.
#' @param start_first_bin A numeric value to set the lower bound of the first bin of the intensity band (in counts/epoch duration).
#' @param start_last_bin A numeric value to set the lower bound of the last bin of the intensity band (in counts/epoch duration).
#' @param bin_width A numeric value to set the width of the bins of the intensity band (in counts/epoch duration).
#'
#' @return A list of objects: `metrics`, `p_band`, and `p_log`. `metrics` is a dataframe containing 
#'     the intensity gradients and the MX metrics (in counts/epoch duration used) as described in Rowlands et al. (2018; doi:10.1249/MSS.0000000000001561).
#'     The graphic `p_band` shows the distribution of time spent in the configured bins of intensity for each day of the dataset. 
#'     The graphic `p_log` shows, for each day, the relationship between the natural log of time spent in each bin with the natural 
#'     log of the middle values of the intensity bins. 
#'
#' @examples
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file)
#' mydata_with_wear_marks <- mark_wear_time(
#'     dataset = mydata, 
#'     TS = "TimeStamp", 
#'     to_epoch = 60,
#'     cts  = "vm",
#'     frame = 90, 
#'     allowanceFrame = 2, 
#'     streamFrame = 30
#'     )
#' mydata_with_intensity_marks <- mark_intensity(
#'     data = mydata_with_wear_marks, 
#'     col_axis = "vm", 
#'     equation = "Sasaki et al. (2011) [Adults]",
#'     sed_cutpoint = 200, 
#'     mpa_cutpoint = 2690, 
#'     vpa_cutpoint = 6167, 
#'     age = 32,
#'     weight = 67,
#'     sex = "male"
#'     )
#' compute_intensity_distri_metrics(
#'    data = mydata_with_intensity_marks,
#'    col_axis = "vm",
#'    col_time = "time",
#'    valid_wear_time_start = "00:00:00",
#'    valid_wear_time_end = "23:59:59",
#'    start_first_bin = 0,
#'    start_last_bin = 10000,
#'    bin_width = 500
#'     )
#'
compute_intensity_distri_metrics <- function(
    data,
    col_axis = "vm",
    col_time = "time",
    valid_wear_time_start = "00:00:00",
    valid_wear_time_end = "23:59:59",
    start_first_bin = 0,
    start_last_bin = 10000,
    bin_width = 500
){
  
  
#=================================================================================
# Setting correction factor to compute time spent in the intensity bins in minutes
#=================================================================================
cor_factor = 60 / (as.numeric(data[[col_time]][2] - data[[col_time]][1]))

#==============================
# Getting all dates of interest
#==============================

all_dates <- levels(as.factor(data$date))

#======================
# Nesting dataset
#======================

data <- 
  data %>% 
  dplyr::filter(date %in% as.Date(all_dates)) %>%
  dplyr::group_by(days) %>%
  tidyr::nest()

#=========================================================================
# Getting the list of intensity gradients for each day and related figures
#=========================================================================

# Running `get_ig_results()` function for each day of the dataset and storing results
res <- lapply(
  data$data, 
  get_ig_results, # `get_ig_results` is an internal function   
  col_axis = col_axis,
  col_time = col_time,
  valid_wear_time_start = valid_wear_time_start,
  valid_wear_time_end = valid_wear_time_end,
  start_first_bin = start_first_bin,
  start_last_bin = start_last_bin,
  bin_width = bin_width,
  cor_factor = cor_factor
  ) 

# Getting dataframe with all ig values
df_ig <- data.frame(
  date = all_dates,
  ig = rep(NA, nlevels(as.factor(all_dates)))
  )

for (i in 1:length(res)){
  df_ig$ig <- ifelse(res[[i]]$date == df_ig$date, res[[i]]$ig, df_ig$ig)
  }

# Getting figure with all `p_band` plots
plot_list_p_band <- vector("list", length = nlevels(as.factor(all_dates)))

for (i in 1:length(res)){
  plot_list_p_band[[i]] <- res[[i]]$p_band
}

p_band <- patchwork::wrap_plots(plot_list_p_band)

# Getting figure with all `p_log` plots
plot_list_p_log <- vector("list", length = nlevels(as.factor(all_dates)))

for (i in 1:length(res)){
  plot_list_p_log[[i]] <- res[[i]]$p_log
}

p_log <- patchwork::wrap_plots(plot_list_p_log)


#===================================
# Getting MX metrics
#===================================

# Getting MX metrics by day
df_mx <-
  data %>%
  tidyr::unnest(data) %>%
  dplyr::group_by(date, .drop = FALSE) %>%
  dplyr::filter(.data[[col_time]] >= hms::as_hms(valid_wear_time_start) &
                  .data[[col_time]] <= hms::as_hms(valid_wear_time_end) &
                  wearing == "w"
  ) %>%
  dplyr::summarise(
    `M1/3` = mean(head(sort(.data[[col_axis]], decreasing = TRUE), n = 8 * 60 * cor_factor), na.rm = TRUE) %>% round(., 2),
    M120 = mean(head(sort(.data[[col_axis]], decreasing = TRUE), n = 120 * cor_factor), na.rm = TRUE) %>% round(., 2),
    M60 = mean(head(sort(.data[[col_axis]], decreasing = TRUE), n = 60 * cor_factor), na.rm = TRUE) %>% round(., 2),
    M30 = mean(head(sort(.data[[col_axis]], decreasing = TRUE), n = 30 * cor_factor), na.rm = TRUE) %>% round(., 2),
    M15 = mean(head(sort(.data[[col_axis]], decreasing = TRUE), n = 15 * cor_factor), na.rm = TRUE) %>% round(., 2),
    M5 = mean(head(sort(.data[[col_axis]], decreasing = TRUE), n = 5 * cor_factor), na.rm = TRUE) %>% round(., 2)
  ) 
  

#==========================
# Returning list of results
#==========================
results <- 
  list(
    metrics = dplyr::left_join(df_ig %>% dplyr::mutate(date = as.Date(date)), df_mx, key = "date"), 
    p_band = p_band, 
    p_log = p_log
    )

return(results)

}
