#' Create a figure with metrics shown for each day
#' 
#' The function generates a figure with several common metrics shown for each day of the physical behavior measurement.
#'
#' @param data A dataframe with results obtained using the \code{\link{prepare_dataset}},  \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, and then the \code{\link{recap_by_day}} functions.
#' @param minimum_wear_time_for_analysis A numeric value to indicate the minimum number of hours of wear time that was considered to valid a day.
#' @param start_day_analysis A character value to indicate the start of the period that was considered to valid a day based on wear time.
#' @param end_day_analysis A character value to indicate the end of the period that was considered to valid a day based on wear time.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' file <- system.file("extdata", "acc.agd", package = "activAnalyzer")
#' mydata <- prepare_dataset(data = file, epoch_len_out = 60, col_time_stamp = "timestamp")
#' mydata_with_wear_marks <- mark_wear_time(
#'     dataset = mydata, 
#'     TS = "timestamp", 
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
#'     col_steps = "steps"
#'     )
#' summary_by_day <- recap_by_day(
#'     data = mydata_with_intensity_marks, 
#'     age = 32, 
#'     weight = 67, 
#'     sex = "male",
#'     valid_wear_time_start = "07:00:00",
#'     valid_wear_time_end = "22:00:00"
#'     )
#' create_fig_res_by_day(summary_by_day, 
#'     minimum_wear_time_for_analysis = 10, 
#'     start_day_analysis = "00:00:00", 
#'     end_day_analysis = "23:59:00", 
#'     language = "en")
#' 
create_fig_res_by_day <- function(data, 
                                  minimum_wear_time_for_analysis, 
                                  start_day_analysis, 
                                  end_day_analysis, 
                                  language = c("en", "fr")) {
  
language <- match.arg(language)
  
# Set text sizes
title_axis_size = 10
label_text_size = 5

# Creating figure
if (language == "en") {
  
all_days <-
  data %>%
  dplyr::mutate(validity = ifelse(wear_time >= minimum_wear_time_for_analysis * 60, "Valid day", "Non valid day"),
                validity =  forcats::fct_relevel(validity, "Valid day", "Non valid day")) %>%
  tidyr::pivot_longer(cols = c(wear_time, 
                               total_steps, 
                               pal, 
                               minutes_SED, 
                               minutes_LPA, 
                               minutes_MVPA, 
                               percent_SED, 
                               percent_LPA, 
                               percent_MVPA),
                      names_to = "variable", values_to = "value") %>%
  dplyr::mutate(variable =  forcats::fct_relevel(
    variable, 
    "wear_time", 
    "total_steps", 
    "pal", 
    "minutes_MVPA", 
    "minutes_LPA",
    "minutes_SED", 
    "percent_MVPA", 
    "percent_LPA", 
    "percent_SED"
  ),
  variable = forcats::fct_recode(
    variable, 
    "Wear time (min)" = "wear_time", 
    "Total steps" = "total_steps", "PAL" = "pal",
    "MVPA time (min)" = "minutes_MVPA",
    "LPA time (min)" = "minutes_LPA", 
    "SED time (min)" = "minutes_SED", 
    "MVPA wear time proportion (%)" = "percent_MVPA",
    "LPA wear time proportion (%)" = "percent_LPA",
    "SED wear time proportion (%)" = "percent_SED"
  )
  ) %>%
  ggplot(aes(x = date, y = value, fill = validity)) +
  ggtitle("Results by day") +
  geom_bar(stat = "identity") +
  geom_point(size = 2, color = "red") +
  geom_line(aes(group = 1), size = 0.7, color = "red") +
  geom_text(aes(label = round(value, 1)), size = label_text_size, vjust = -0.8) +
  labs(x = "Date", y = "", fill = "") +
  scale_y_continuous(expand = expansion(mult = c(.05, .2))) +
  scale_fill_manual(labels = c("Valid day", 
                               paste0("Non-valid day (<", 
                                      minimum_wear_time_for_analysis, 
                                      " hours between ", start_day_analysis, 
                                      " and ", end_day_analysis, ")")),
                    values = c("#00BE6C", "#FC717F")) +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.text = element_text(size = 15),
        legend.background = element_rect(fill = "beige", colour = "beige"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = "beige", color = "beige"),
        plot.margin = margin(1, 1, 0.5, 1, "cm"),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
  facet_wrap(. ~ variable, scales = "free_y")

return(all_days)
}

if (language == "fr") {
  
  
  # Set text sizes
  title_axis_size = 10
  label_text_size = 5
  
  # Generate figure
  all_days <-
    data %>%
    dplyr::mutate(validity = ifelse(wear_time >= minimum_wear_time_for_analysis * 60, "Valid day", "Non valid day"),
                  validity = forcats::fct_relevel(validity, "Valid day", "Non valid day")) %>%
    tidyr::pivot_longer(cols = c(wear_time, 
                                 total_steps, 
                                 pal, 
                                 minutes_SED, 
                                 minutes_LPA, 
                                 minutes_MVPA, 
                                 percent_SED, 
                                 percent_LPA, 
                                 percent_MVPA),
                        names_to = "variable", values_to = "value") %>%
    dplyr::mutate(variable = forcats::fct_relevel(
      variable, 
      "wear_time", 
      "total_steps", 
      "pal",
      "minutes_MVPA", 
      "minutes_LPA",
      "minutes_SED", 
      "percent_MVPA", 
      "percent_LPA", 
      "percent_SED"),
      variable = forcats::fct_recode(
        variable, 
        "Temps de port (min)" = "wear_time", 
        "Nombre de pas" = "total_steps",
        "NAP" = "pal",
        "Temps MVPA (min)" = "minutes_MVPA",
        "Temps LPA (min)" = "minutes_LPA", 
        "Temps SED (min)" = "minutes_SED", 
        "Proportion de temps MVPA (%)" = "percent_MVPA",
        "Proportion de temps LPA (%)" = "percent_LPA",
        "Proportion de temps SED (%)" = "percent_SED"
      )
    ) %>%
    ggplot(aes(x = date, y = value, fill = validity)) +
    ggtitle("R\u00e9sultats par jour") +
    geom_bar(stat = "identity") +
    geom_point(size = 2, color = "red") +
    geom_line(aes(group = 1), size = 0.7, color = "red") +
    geom_text(aes(label = round(value, 1)), size = label_text_size, vjust = -0.8) +
    labs(x = "Date", y = "", fill = "") +
    scale_y_continuous(expand = expansion(mult = c(.05, .2))) +
    scale_fill_manual(labels = c("Jour valide", 
                                 paste0("Jour non valide (<", 
                                        minimum_wear_time_for_analysis, 
                                        " heures entre ", start_day_analysis, 
                                        " et ", end_day_analysis, ")")),
                      values = c("#00BE6C", "#FC717F")) +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.text = element_text(size = 15),
          legend.background = element_rect(fill = "beige", colour = "beige"),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0.5, 1, "cm"),
          strip.text.x = element_text(size = 15),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    facet_wrap(. ~ variable, scales = "free_y")
  
  return(all_days)
  
  }
}