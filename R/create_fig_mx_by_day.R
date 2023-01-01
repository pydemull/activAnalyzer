#' Create a radar plot for MX metrics relating to each day of the measurement of physical behavior
#'
#' @param data A dataframe with physical behavior metrics summarised for each day of the measurement. It should have been obtained using 
#'     the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, and then 
#'     the \code{\link{recap_by_day}} functions.
#' @param labels A vector of numeric values setting the breaks of the Y axis of the radar plot. Default is a vector of 6 values with
#'     a start at 0 and an end at the maximum of all the computed MX metrics.
#'
#' @return A ggplot object
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
#'     sex = "male"
#'     )
#' summary_by_day <- recap_by_day(
#'     data = mydata_with_intensity_marks, 
#'     col_axis = "vm",
#'     age = 32, 
#'     weight = 67, 
#'     sex = "male",
#'     valid_wear_time_start = "07:00:00",
#'     valid_wear_time_end = "22:00:00",
#'     start_first_bin = 0,
#'     start_last_bin = 10000,
#'     bin_width = 500
#'     )$df_all_metrics
#' create_fig_mx_by_day(
#'     data = summary_by_day,
#'     labels = seq(0, 10000, 2500)
#' )

create_fig_mx_by_day <- function(
    data,
    labels = NULL
){
  
# Tidy dataframe
data2 <-
  data %>%
  tidyr::pivot_longer(cols = -date, names_to = "metric", values_to = "val") %>%
  dplyr::filter(metric %in% c("M1/3", "M120", "M60", "M30", "M15", "M5")) %>%
  dplyr::mutate(metric = forcats::fct_relevel(metric, "M1/3", "M120", "M60", "M30", "M15", "M5"))


# Setting labels
if (is.null(labels)) {
  
  max <- max(data2$val, na.rm = TRUE)
  min <- 0
  selected_labels = round(seq(0, max, (max-min)/5))
} else {
  selected_labels <- labels
}

# Creating a dataframe for labels
df_labels <-
  data.frame(
    date = as.Date(rep(levels(as.factor(data2$date)), each = length(selected_labels))),
    x_lab = rep(pi/2.1, nlevels(as.factor(data2$date)) * length(selected_labels)),
    y_lab = rep(selected_labels, nlevels(as.factor(data2$date)))
  )

# Making plot
p <-
  ggplot(data = data2, aes(x = metric, y = val)) +
  geom_polygon(aes(group = date,  color = as.factor(date)), alpha = 0) +
  geom_text(
    data = df_labels,
    aes(x = x_lab, y = y_lab, label = y_lab)
    ) +
  coord_radar(start = -pi/6) +
  scale_y_continuous(breaks = selected_labels, labels = selected_labels) +
  labs(x = NULL, y = NULL, color = "Date") +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  ) +
  facet_wrap(. ~date, ncol = 4)

# Returning plot
return(p)

}
