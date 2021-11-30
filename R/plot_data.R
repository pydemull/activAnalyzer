#' Plot accelerometer data for each day
#' 
#' This function plots accelerometer data against time for each day of measurement, 
#'     with the possibility to specify the metric to visualize.
#'
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}} and \code{\link{mark_wear_time}} functions.
#' @param metric A character value to indicate the name of the variable to be plotted against time.
#' @param col_time A character value to indicate the name of the variable to plot time data.
#' @param col_nonwear A character value to indicate the name of the variable used to count nonwear time.
#' @param col_date A character value to indicate the name of the variable to plot date data.
#'
#' @return
#' A `ggplot` object.
#' @export
#' @import ggplot2
#'
#' @examples
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp")
#' mydata_with_wear_marks <- mark_wear_time(dataset = mydata, TS = "timestamp", 
#'     cts  = "vm", frame = 90, allowanceFrame = 2)
#' plot_data(data = mydata_with_wear_marks, metric = "steps", col_time = "time", 
#'     col_nonwear = "non_wearing_count", col_date = "date")
#' 
plot_data <- function(data, metric = "axis1", col_time = "time", col_nonwear = "non_wearing_count", col_date = "date"){
  
  # Setting metric to visualize  
  max_metric <- max(data[[metric]], na.rm = TRUE)
  
  # Plotting data
    format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)
    data$date  <- format(lubridate::as_date(data$date) , "%d-%m-%y")
   
    ggplot() +
    geom_ribbon(data = data,
                aes(x = .data[[col_time]],
                    ymin = 0, ymax = .data[[col_nonwear]] / .data[[col_nonwear]] * max_metric, 
                    fill = "Nonwear time"), alpha = 0.5) +
    geom_line(data = data,
              aes(x = .data[[col_time]],
                  y = .data[[metric]])) +
    scale_y_continuous(position = "right", expand = c(0, 0)) +
    scale_x_time(breaks = hms::hms(seq(3600, 23*3600, 2*3600)), expand = c(0, 0), labels = format_hm) +
    scale_fill_manual(values = "red") +
    labs(x = "Time (hh:mm:ss)", y = metric, fill = "") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
    facet_grid(data[[col_date]] ~ ., switch = "y")
  
}
  
