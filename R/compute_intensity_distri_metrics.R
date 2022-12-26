#' Compute intensity distribution metrics
#'
#' This function computes metrics that describe the distribution of intensity.
#' 
#' @param data  A dataframe obtained using the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, and then the \code{\link{mark_intensity}} functions.
#' @param col_time A character value to indicate the name of the variable to be used to determine the epoch duration of the dataset.
#' @param col_axis A character value to indicate the name of the variable to be used to compute total time per bin of intensity.
#' @param dates A character vector containing the dates to be retained for analysis. The dates must be with the "YYYY-MM-DD" format.
#' @param valid_wear_time_start A character value with the HH:MM:SS format to set the start of the daily period that will be considered for computing metrics.
#' @param valid_wear_time_end A character value with the HH:MM:SS format to set the end of the daily period that will be considered for computing metrics.
#' @param start_first_bin A numeric value to set the lower bound of the first bin of the intensity band (in counts/epoch duration).
#' @param start_last_bin A numeric value to set the lower bound of the last bin of the intensity band (in counts/epoch duration).
#' @param bin_width A numeric value to set the width of the bins of the intensity band (in counts/epoch duration).
#'
#' @return A list of objects: `p_band`,`p_log`, and `model_log`. The graphic `band` shows the distribution of time spent in the configured bins of intensity.
#'     The graphic `p_log` shows the relationship between the natural log of time spent in each bin with the natural log of the middle values of the bins.
#'     `model_log` is the summary of a linear model that has been built with the natural log of time spent in each bin as the dependent variable and the 
#'     natural of the middle values of the bins as the independant variable.
#' @export
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
#'     sex = "male",
#'     )
#' compute_intensity_distri_metrics(
#'    data = mydata_with_intensity_marks,
#'    dates = c("2021-04-07", "2021-04-08", "2021-04-09", "2021-04-10", "2021-04-11"),
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
    dates = NULL,
    valid_wear_time_start = "00:00:00",
    valid_wear_time_end = "23:59:59",
    zoom_from = "00:00:00",
    zoom_to = "23:59:59",
    start_first_bin = 0,
    start_last_bin = 10000,
    bin_width = 500
){
  
  
#=================================================================
# Setting the table of bins that will be used for summing time by 
# intensity bins
#=================================================================

# Setting correction factor
cor_factor = 60 / (as.numeric(data[[col_time]][2] - data[[col_time]][1]))

# Filtering data based on selected dates and time periods
if (is.null(dates)) {
  selected_dates <- attributes(as.factor(data$date))$levels
} else {
  selected_dates <- attributes(as.factor(dates))$levels
}
data <- 
  data %>% 
  dplyr::filter(date %in% as.Date(selected_dates) &
                  .data[[col_time]] >= hms::as_hms(valid_wear_time_start) &
                  .data[[col_time]] <= hms::as_hms(valid_wear_time_end)
  )

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
df_bins$bin_label <- paste0(df_bins$bin_start,"-", df_bins$bin_end)

# Correcting the value of the upper bound of the last bin
df_bins[nrow(df_bins), "bin_end"] <- 50000

# Correcting the label of the last bin
df_bins[nrow(df_bins), "bin_label"] <- paste0(">",df_bins[nrow(df_bins), "bin_start"]-1)


#========================================
# Marking the dataset with intensity bins
#========================================

# Initializing vectors for labelling the dataset
data$bin_num <- vector("double", nrow(data))
data$bin_mid <- vector("double", nrow(data))
data$bin_label <- vector("character", nrow(data))

# Marking the dataset
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

#=======================================
# Getting intensity distribution results
#=======================================

# Getting summarised data
recap_bins_int <-
  data %>%
  dplyr::group_by(bin_mid, bin_label) %>%
  dplyr::filter(wearing == "w") %>%
  dplyr::summarise(duration = dplyr::n() / cor_factor)


# Building log mofel
model_log <- summary(lm(log(duration) ~ log(bin_mid), data = recap_bins_int))

# Plotting accumulated minutes vs Intensity band
p1 <- ggplot(data = recap_bins_int, aes(x = forcats::fct_reorder(bin_label, bin_mid), y = duration)) +
  geom_bar(stat = "identity") +
  labs(x = "Intensity band (counts/min)", y = "Accumulated minutes" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Getting Log-Log plot
p2 <- ggplot(data = recap_bins_int, aes(x = log(bin_mid), y = log(duration))) +
  geom_point(size = 4, alpha = 0.5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  annotate(
    x = min(log(recap_bins_int$bin_mid)), 
    y = min(log(recap_bins_int$duration)), "text", 
    label = paste0("y = ", round(model_log$coefficient[2, 1], 2), "x + ", round(model_log$coefficient[1, 1], 2)),
    hjust = 0, size = 10
    )

# Making list of graphics
results <- list(p_band = p1, p_log = p2, model_log = model_log)
return(results)
}
