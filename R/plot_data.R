#' Plot accelerometer data for each day
#' 
#' This function plots accelerometer data against time for each day of measurement, 
#'     with the possibility to specify the metric to visualize.
#'
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}} and \code{\link{mark_wear_time}} functions.
#' @param metric A character value to indicate the name of the variable to be plotted against time.
#' @param col_time A character value to indicate the name of the variable to plot time data.
#' @param col_nonwear A character value to indicate the name of the variable used to count nonwear time.
#' @param col_wear A character value to indicate the name of the variable used to count wear time.
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
plot_data <- function(data, 
                      metric = "axis1", 
                      col_time = "time", 
                      col_nonwear = "non_wearing_count",
                      col_wear = "wearing_count",
                      col_date = "date"){

  # Setting the format of the time variable
    format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)
    data[[col_date]] <- format(lubridate::as_date(data$date) , "%d-%m-%y")

  # Creating the plot
    ggplot(data = data) +
    geom_ribbon(
      aes(
      x = .data[[col_time]],
      ymin = 0, 
      ymax = ifelse(.data[[col_nonwear]] == 1, Inf, 0),
      fill = "Nonwear time"
      ), 
      alpha = 0.5
      ) +
    geom_ribbon(
      aes(
      x = .data[[col_time]],
      ymin = 0, 
      ymax = ifelse(.data[[col_wear]] == 1, Inf, 0),
      fill = "Wear time"
      ),
      alpha = 0.5) +
    geom_line(
      aes(
      x = .data[[col_time]],
      y = .data[[metric]])
      ) +
    scale_x_time(breaks = hms::hms(seq(3600, 23*3600, 2*3600)), 
                expand = c(0, 0), 
                labels = format_hm
                 ) +
    scale_y_continuous(position = "right", expand = c(0, 0)) +
    scale_fill_manual(values = c("red", "lemonchiffon1")) +
    labs(x = "Time (hh:mm)", y = metric, fill = "") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.key = element_rect(color = "grey"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.3))) +
    facet_grid(data[[col_date]] ~ ., switch = "y", scales = "free_x") +
    geom_vline(aes(xintercept = 3600*1),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*2),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*3),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*4),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*5),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*6),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*7),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*8),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*9),    linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*10),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*11),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*12),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*13),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*14),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*15),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*16),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*17),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*18),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*19),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*20),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*21),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*22),   linetype = "dotted", color = "grey50") +
    geom_vline(aes(xintercept = 3600*23),   linetype = "dotted", color = "grey50")
    

}
  
