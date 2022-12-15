#' Plot accelerometer data for each day with both nonwear time and physical activity intensity categories
#' 
#' This function plots accelerometer data with intensity categories against time for each day of measurement, 
#'     with the possibility to specify the metric to visualize.
#'
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, and then the \code{\link{mark_intensity}} functions.
#' @param metric A character value to indicate the name of the variable to be plotted against time.
#' @param col_time A character value to indicate the name of the variable to plot time data.
#' @param col_nonwear A character value to indicate the name of the variable used to count nonwear time.
#' @param col_wear A character value to indicate the name of the variable used to count wear time.
#' @param valid_wear_time_start A character value with the HH:MM:SS format to set the start of the daily period that will be considered for computing valid wear time.
#' @param valid_wear_time_end A character value with the HH:MM:SS format to set the end of the daily period that will be considered for computing valid wear time.
#' @param zoom_from A character value with the HH:MM:SS format to set the start of the daily period to visualize.
#' @param zoom_to A character value with the HH:MM:SS format to set the end of the daily period to visualize.

#'
#' @return
#' A `ggplot` object.
#' @export
#' @import ggplot2
#'
#' @examples
#' \donttest{
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
#' plot_data_with_intensity(
#'     data = mydata_with_intensity_marks,
#'     metric = "vm",
#'     valid_wear_time_start = "00:00:00",
#'     valid_wear_time_end = "23:59:59",
#'     zoom_from = "02:00:00",
#'     zoom_to = "23:58:00"
#'     )
#' }
#' 
plot_data_with_intensity <- function(
    data, 
    metric = "axis1", 
    col_time = "time", 
    col_nonwear = "non_wearing_count",
    col_wear = "wearing_count",
    valid_wear_time_start = "00:00:00",
    valid_wear_time_end = "23:59:59",
    zoom_from = "00:00:00",
    zoom_to = "23:59:59"
    ){

  # Setting the format of the time variable
    format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)
    date_labs <- format(data$date, "%d-%m-%y")
    names(date_labs) <- data$days
    
  # Creating the plot
    p <-
    ggplot(data = data %>% dplyr::filter(.data[[col_time]] >= hms::as_hms(zoom_from) & .data[[col_time]] <= hms::as_hms(zoom_to))) +
    geom_rect(aes(
      xmin = .data[[col_time]], 
      xmax =  .data[[col_time]] + hms::as_hms(60), 
      ymin = -Inf, 
      ymax = Inf, 
      color = intensity_category,
      fill = intensity_category)
      ) +
    geom_line(
      aes(
      x = .data[[col_time]],
      y = .data[[metric]])
      ) +
    geom_rect(aes(
        xmin = hms::as_hms(0), 
        xmax =  hms::as_hms(valid_wear_time_start), 
        ymin = -Inf, 
        ymax = Inf), 
        color = "grey",
        fill = "grey"
      ) +
    geom_rect(aes(
        xmin = hms::as_hms(valid_wear_time_end), 
        xmax =  hms::as_hms("23:59:59"),
        ymin = -Inf, 
        ymax = Inf), 
        color = "grey",
        fill = "grey"
      ) +
   scale_x_time(
     limits = c(hms::as_hms(zoom_from), hms::as_hms(zoom_to)),
     breaks = hms::hms(seq(as.numeric(hms::as_hms(zoom_from)), as.numeric(hms::as_hms(zoom_to)), 2*3600)), 
     expand = c(0, 0), 
     labels = format_hm
     ) +
   scale_y_continuous(position = "right", expand = c(0, 0)) +
   scale_fill_manual(breaks = c("Nonwear", "SED", "LPA", "MVPA"), values = c("lemonchiffon3", "#3F51B5", "#FFFF33", "#FF0066")) +
   scale_color_manual(breaks = c("Nonwear", "SED", "LPA", "MVPA"), values = c("lemonchiffon3", "#3F51B5", "#FFFF33", "#FF0066")) +
   labs(x = "Time (hh:mm)", y = metric, fill = "", color = "") +
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
   
    suppressWarnings(print(p))

}
  
