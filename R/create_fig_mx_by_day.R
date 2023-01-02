#' Create a radar plot for MX metrics relating to each day of the measurement of physical behavior
#'
#' @param data A dataframe with physical behavior metrics summarised for each day of the measurement. It should have been obtained using 
#'     the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, and then 
#'     the \code{\link{recap_by_day}} functions.
#' @param labels A vector of numeric values setting the breaks of the Y axis of the radar plot. Default is a vector of 6 values with
#'     a start at 0 and an end at the maximum of all the computed MX metrics.
#' @param mpa_cutpoint A numeric value at and above which time is considered as spent in moderate-to-vigorous physical activity (in counts/epoch length used to compute MX metrics). 
#'    Defaut value is from Sasaki et al. (2011; doi:10.1016/j.jsams.2011.04.003) relating to vector magnitude in counts/min.
#' @param vpa_cutpoint A numeric value at and above which time is considered as spent in vigorous physical activity (in counts/epoch length used to compute MX metrics). 
#'    Defaut value is from Sasaki et al. (2011; doi:10.1016/j.jsams.2011.04.003) relating to vector magnitude in counts/min.
#' 
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
#'     labels = seq(2500, 12500, 2500)
#' )

create_fig_mx_by_day <- function(
    data,
    labels = NULL,
    mpa_cutpoint = 2690, 
    vpa_cutpoint = 6167
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
    x_lab = rep(30, nlevels(as.factor(data2$date)) * length(selected_labels)),
    y_lab = rep(selected_labels, nlevels(as.factor(data2$date)))
  )

# Creating a dataframe for circles (thresholds)
df_circles <-
  data.frame(x = seq(1, 360, 1)) %>%
  dplyr::mutate(
    y_mpa = mpa_cutpoint,
    y_vpa = vpa_cutpoint
  ) %>%
  tidyr::pivot_longer(cols = -x, names_to = "cutpoint", values_to = "val")

# Making plot
p <-
ggplot(data = data2, aes(x = rep(seq(0, 300, 60),  nlevels(as.factor(date))), y = val)) +
  geom_polygon(aes(group = date,  color = as.factor(date), fill =  as.factor(date)), alpha = 0.1) +
  geom_path(data = df_circles, aes(x = x, y = val, linetype = cutpoint), color = "red") +
  geom_text(
    data = df_labels,
    aes(x = x_lab, y = y_lab, label = y_lab),
    size = 3
  ) +
  coord_radar(start = 0) +
  scale_x_continuous(breaks = seq(0, 300, 60), labels = c("M1/3", "M120", "M60", "M30", "M15", "M5")) +
  scale_y_continuous(breaks = selected_labels, minor_breaks = mpa_cutpoint, labels = selected_labels) +
  scale_linetype_manual(values = c("dashed", "dotdash"), labels = c("MVPA cutpoint", "VPA cutpoint")) + 
  labs(x = NULL, y = NULL, linetype = "") +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom"
  ) +
  facet_wrap(. ~date, ncol = 4) +
  guides(color = "none", fill = "none")
  

# Returning plot
return(p)

}
