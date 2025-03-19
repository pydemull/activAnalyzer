#' Plot accelerometer data for each day
#' 
#' This function plots accelerometer data against time for each day of measurement, 
#'     with the possibility to specify the metric to visualize.
#'
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}} and then the \code{\link{mark_wear_time}} functions.
#' @param metric A character value to indicate the name of the variable to be plotted against time.
#' @param col_time A character value to indicate the name of the variable to plot time data.
#' @param col_nonwear A character value to indicate the name of the variable used to count nonwear time.
#' @param col_wear A character value to indicate the name of the variable used to count wear time.
#' @param ehcv A numeric value to set the threshold above which vertical axis data should be considered as extremely high (abnormal).
#'     The value should be in counts/min.
#' @param zoom_from A character value with the HH:MM:SS format to set the start of the daily period to visualize.
#' @param zoom_to A character value with the HH:MM:SS format to set the end of the daily period to visualize.
#'
#' @return
#' A `ggplot` object.
#' @export
#' @import ggplot2
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
#' plot_data(
#'     data = mydata_with_wear_marks, 
#'     metric = "vm", 
#'     col_time = "time", 
#'     col_nonwear = "non_wearing_count", 
#'     col_wear = "wearing_count",
#'     ehcv = 15000,
#'     zoom_from = "02:00:00",
#'     zoom_to = "23:58:00"
#'     )
#' 
plot_data <- function(
    data, 
    metric = "axis1", 
    col_time = "time", 
    col_nonwear = "non_wearing_count",
    ehcv = 15000,
    col_wear = "wearing_count",
    zoom_from = "00:00:00",
    zoom_to = "23:59:59"
    ){

  
  # Setting parameters for computation of axis breaks labels
  if (as.numeric(hms::as_hms(zoom_to)) - as.numeric(hms::as_hms(zoom_from)) < 2*3600) {
    breaks_control_1 = hms::hms(0)
    breaks_control_2 = hms::hms(0)
    breaks_control_3 = 3600/2
  } else {
    breaks_control_1 = hms::hms(3600)
    breaks_control_2 = hms::hms(3599)
    breaks_control_3 = 2*3600
  }
  
  
  # Setting the format of the time variable
    format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)
    date_labs <- format(data$date, "%d-%m-%y")
    names(date_labs) <- data$days
    
  # Getting epoch length
    epoch <- as.numeric(data[[col_time]][2] - data[[col_time]][1])
  
  # Getting correction factor for epoch duration different from 60s
    cor_factor = 60 / epoch
    
  # Adjusting ehcv for the considered epoch
    ehcv <- ehcv / cor_factor

  # Creating the plot
    p <-
    ggplot(data = data %>% dplyr::filter(.data[[col_time]] >= hms::as_hms(zoom_from) & .data[[col_time]] <= hms::as_hms(zoom_to))) +
    geom_rect(aes(
        xmin = .data[[col_time]], 
        xmax =  .data[[col_time]] + hms::as_hms(epoch), 
        ymin = -Inf, 
        ymax = Inf, 
        color = as.factor(.data[[col_nonwear]]),
        fill = as.factor(.data[[col_nonwear]])
       )
    ) +
    geom_line(
        aes(
          x = .data[[col_time]],
          y = .data[[metric]]
          )
      ) +
    scale_x_time(
      limits = c(hms::as_hms(zoom_from), hms::as_hms(zoom_to)),
      breaks = hms::hms(seq(as.numeric(hms::as_hms(zoom_from) + breaks_control_1), as.numeric(hms::as_hms(zoom_to) - breaks_control_2), breaks_control_3)), 
      expand = c(0, 0),
      labels = format_hm
      ) +
    scale_y_continuous(position = "right", expand = c(0, 0)) +
    scale_color_manual(breaks = c("1", "0"), values = c("lemonchiffon3", "lemonchiffon1"), labels = c("Nonwear", "Wear")) +
    scale_fill_manual(breaks = c("1", "0"), values = c("lemonchiffon3", "lemonchiffon1"), labels = c("Nonwear", "Wear")) +
    labs(x = "Time (hh:mm)", y = paste0(metric, "/", epoch, "s"), fill = "", color = "") +
    guides(color = NULL) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.key = element_rect(color = "grey"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) +
    facet_grid(days ~ ., switch = "y", labeller = labeller(days = date_labs)) +
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
    
    
    # Add line for highlighting abnormal values if any
    max_axis1 <- max(data$axis1)
    
    if (metric == "axis1" && ehcv <= max_axis1) { 
      p <- p + geom_hline(yintercept = ehcv, color = "red", linewidth = 0.8)
      }
    
    suppressWarnings(print(p))
}
  
