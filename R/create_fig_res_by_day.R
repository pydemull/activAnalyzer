#' Create a figure with metrics shown for each day
#' 
#' The function generates a figure with several common metrics shown for each day of the physical behavior measurement.
#'
#' @param data A dataframe with results obtained using the \code{\link{prepare_dataset}},  \code{\link{mark_wear_time}}, \code{\link{mark_intensity}}, and then the \code{\link{recap_by_day}} functions.
#' @param minimum_wear_time_for_analysis A numeric value to indicate the minimum number of hours of wear time that was considered to validate a day.
#' @param start_day_analysis A character value to indicate the start of the period that was considered to validate a day based on wear time.
#' @param end_day_analysis A character value to indicate the end of the period that was considered to validate a day based on wear time.
#' @param language A character value for setting the language with which the figure should be created: `en` for english; `fr` for french.
#' @param metrics A character value for setting the metrics to be shown in the figure. "volume" refers to "activity volume" metrics, step_acc" refers 
#'     to "step accumlulation" metrics, and "int_distri" refers to intensity distribution metrics. By default, the function provides all computed metrics.
#' @param epoch_label A character value to be pasted into the names of the variables to build the figure
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
#' summary_by_day <- recap_by_day(
#'     data = mydata_with_intensity_marks, 
#'     age = 32, 
#'     weight = 67, 
#'     sex = "male",
#'     valid_wear_time_start = "07:00:00",
#'     valid_wear_time_end = "22:00:00"
#'     )$df_all_metrics
#' create_fig_res_by_day(summary_by_day, 
#'     minimum_wear_time_for_analysis = 10, 
#'     start_day_analysis = "00:00:00", 
#'     end_day_analysis = "23:59:00", 
#'     language = "en")
#' 
create_fig_res_by_day <- function(
    data, 
    minimum_wear_time_for_analysis = 10, 
    start_day_analysis = "00:00:00", 
    end_day_analysis = "23:59:00", 
    language = c("en", "fr"),
    metrics = c("all", "volume", "step_acc", "int_distri"),
    epoch_label = "60s"
    ){

# Get arguments
metrics <- match.arg(metrics)
language <- match.arg(language)
epoch_label = as.name(epoch_label)

# Set text sizes
strip_text = 10
title_text_size = 10
label_text_size = 3

# Create figure for english language ========================================================================
if (language == "en") {
  
  
  # Set list of metrics and parameters to be used for filerting table of results and creating the figure
  if (metrics == "all") {
    selected_metrics <- c(
      "Wear time (min)",
      "Axis 1 total counts",
      "VM total counts",
      "Axis 1 mean (counts/min)",
      "VM mean (counts/min)",
      "SED time (min)", 
      "LPA time (min)", 
      "MPA time (min)",
      "VPA time (min)",
      "MVPA time (min)",
      "SED wear time proportion (%)",
      "LPA wear time proportion (%)",
      "MPA wear time proportion (%)",
      "VPA wear time proportion (%)",
      "MVPA wear time proportion (%)",
      "Ratio MVPA / SED",
      "Total MVPA MET-hr", 
      "Total kcal",
      "PAL",
      "Total steps",
      "Max step acc. 60 min (steps/min)",
      "Max step acc. 30 min (steps/min)",
      "Max step acc. 20 min (steps/min)",
      "Max step acc. 5 min (steps/min)",
      "Max step acc. 1 min (steps/min)",
      "Peak step acc. 60 min (steps/min)",
      "Peak step acc. 30 min (steps/min)",
      "Peak step acc. 20 min (steps/min)",
      "Peak step acc. 5 min (steps/min)",
      "Peak step acc. 1 min (steps/min)",
      "Intensity gradient",
      paste0("M1/3 (counts/", epoch_label, ")"),
      paste0("M120 (counts/", epoch_label, ")"),
      paste0("M60 (counts/", epoch_label, ")"), 
      paste0("M30 (counts/", epoch_label, ")"),
      paste0("M15 (counts/", epoch_label, ")"),
      paste0("M5 (counts/", epoch_label, ")")
    )
    
    fig_title <- "All metrics"
    n_col = 5
    
  } 
  
  if (metrics == "volume") {
    selected_metrics <- c(
      "Wear time (min)",
      "Axis 1 total counts",
      "VM total counts",
      "Axis 1 mean (counts/min)",
      "VM mean (counts/min)",
      "SED time (min)", 
      "LPA time (min)", 
      "MPA time (min)",
      "VPA time (min)",
      "MVPA time (min)",
      "SED wear time proportion (%)",
      "LPA wear time proportion (%)",
      "MPA wear time proportion (%)",
      "VPA wear time proportion (%)",
      "MVPA wear time proportion (%)",
      "Ratio MVPA / SED",
      "Total MVPA MET-hr", 
      "Total kcal",
      "PAL",
      "Total steps"
    )
    
    fig_title <- "Activity volume metrics"
    n_col = 5
    
  } 
  
  if (metrics == "step_acc") {
    selected_metrics <- c(
      "Max step acc. 60 min (steps/min)",
      "Max step acc. 30 min (steps/min)",
      "Max step acc. 20 min (steps/min)",
      "Max step acc. 5 min (steps/min)",
      "Max step acc. 1 min (steps/min)",
      "Peak step acc. 60 min (steps/min)",
      "Peak step acc. 30 min (steps/min)",
      "Peak step acc. 20 min (steps/min)",
      "Peak step acc. 5 min (steps/min)",
      "Peak step acc. 1 min (steps/min)"
    )
    
    fig_title <- "Step-accumulation metrics"
    n_col = 5
    
  } 
  
  if (metrics == "int_distri") {
    selected_metrics <- c(
      "Intensity gradient",
      paste0("M1/3 (counts/", epoch_label, ")"),
      paste0("M120 (counts/", epoch_label, ")"),
      paste0("M60 (counts/", epoch_label, ")"), 
      paste0("M30 (counts/", epoch_label, ")"),
      paste0("M15 (counts/", epoch_label, ")"),
      paste0("M5 (counts/", epoch_label, ")")
    )
    
    fig_title <- "Intensity distribution metrics"
    n_col = 3
    
  } 
  
  # Make figure
  plot <-
    data %>%
    dplyr::mutate(date = as.factor(date),
                  validity = ifelse(wear_time >= minimum_wear_time_for_analysis * 60, "Valid day", "Non valid day"),
                  validity =  forcats::fct_relevel(validity, "Valid day", "Non valid day")) %>%
    tidyr::pivot_longer(cols = c(
      wear_time,
      total_counts_axis1,
      total_counts_vm,
      axis1_per_min,
      vm_per_min,
      minutes_SED,
      minutes_LPA,
      minutes_MPA,
      minutes_VPA,
      minutes_MVPA,
      percent_SED,
      percent_LPA,
      percent_MPA,
      percent_VPA,
      percent_MVPA,
      ratio_mvpa_sed,
      mets_hours_mvpa, 
      total_kcal,
      pal,
      total_steps,
      max_steps_60min,
      max_steps_30min,
      max_steps_20min,
      max_steps_5min, 
      max_steps_1min, 
      peak_steps_60min, 
      peak_steps_30min, 
      peak_steps_20min, 
      peak_steps_5min,
      peak_steps_1min,
      ig,
      "M1/3",
      M120,
      M60, 
      M30,
      M15,
      M5
    ),
          names_to = "variable", values_to = "value") %>%
    dplyr::mutate(variable =  forcats::fct_relevel(
      variable, 
      "wear_time",
      "total_counts_axis1",
      "total_counts_vm",
      "axis1_per_min",
      "vm_per_min",
      "minutes_SED",
      "minutes_LPA",
      "minutes_MPA",
      "minutes_VPA",
      "minutes_MVPA",
      "percent_SED",
      "percent_LPA",
      "percent_MPA",
      "percent_VPA",
      "percent_MVPA",
      "ratio_mvpa_sed",
      "mets_hours_mvpa", 
      "total_kcal",
      "pal",
      "total_steps",
      "max_steps_60min",
      "max_steps_30min",
      "max_steps_20min",
      "max_steps_5min", 
      "max_steps_1min", 
      "peak_steps_60min", 
      "peak_steps_30min", 
      "peak_steps_20min", 
      "peak_steps_5min",
      "peak_steps_1min",
      "ig",
      "M1/3",
      "M120",
      "M60", 
      "M30",
      "M15",
      "M5"
    ),
    variable = forcats::fct_recode(
      variable, 
      "Wear time (min)" = "wear_time",
      "Axis 1 total counts"= "total_counts_axis1",
      "VM total counts" = "total_counts_vm",
      "Axis 1 mean (counts/min)" = "axis1_per_min",
      "VM mean (counts/min)" = "vm_per_min",
      "SED time (min)" = "minutes_SED", 
      "LPA time (min)" = "minutes_LPA", 
      "MPA time (min)" = "minutes_MPA",
      "VPA time (min)" = "minutes_VPA",
      "MVPA time (min)" = "minutes_MVPA",
      "SED wear time proportion (%)" = "percent_SED",
      "LPA wear time proportion (%)" = "percent_LPA",
      "MPA wear time proportion (%)" = "percent_MPA",
      "VPA wear time proportion (%)" = "percent_VPA",
      "MVPA wear time proportion (%)" = "percent_MVPA",
      "Ratio MVPA / SED" = "ratio_mvpa_sed",
      "Total MVPA MET-hr" = "mets_hours_mvpa", 
      "Total kcal" = "total_kcal",
      "PAL" = "pal",
      "Total steps" = "total_steps",
      "Max step acc. 60 min (steps/min)" = "max_steps_60min",
      "Max step acc. 30 min (steps/min)" ="max_steps_30min",
      "Max step acc. 20 min (steps/min)" ="max_steps_20min",
      "Max step acc. 5 min (steps/min)" ="max_steps_5min", 
      "Max step acc. 1 min (steps/min)" ="max_steps_1min", 
      "Peak step acc. 60 min (steps/min)" ="peak_steps_60min", 
      "Peak step acc. 30 min (steps/min)" ="peak_steps_30min", 
      "Peak step acc. 20 min (steps/min)" ="peak_steps_20min", 
      "Peak step acc. 5 min (steps/min)" ="peak_steps_5min",
      "Peak step acc. 1 min (steps/min)" ="peak_steps_1min",
      "Intensity gradient" = "ig",
      "M1/3 (counts/{{epoch_label}})"  := "M1/3",
      "M120 (counts/{{epoch_label}})"  := "M120",
      "M60 (counts/{{epoch_label}})"   := "M60", 
      "M30 (counts/{{epoch_label}})"   := "M30",
      "M15 (counts/{{epoch_label}})"   := "M15",
      "M5 (counts/{{epoch_label}})"    := "M5"
    )
    ) %>%
    dplyr::filter(variable %in% selected_metrics) %>%
    dplyr::mutate(value_maj = ifelse(is.na(value), 0, value)) %>%
    ggplot(aes(x = date, y = value_maj, fill = validity, color = validity)) +
    ggtitle(fig_title) +
    geom_bar(stat = "identity") +
    geom_point(size = 2, color = "red") +
    geom_line(aes(group = 1), linewidth = 0.7, color = "red") +
    geom_text(aes(label = ifelse(is.na(value), "NA", round(value, 1))), size = label_text_size, vjust = -0.6, color = "black") +
    labs(x = "Date", y = "", fill = "") +
    scale_y_continuous(expand = expansion(mult = c(.05, .2))) +
    scale_fill_manual(labels = c("Valid day", 
                                 paste0("Non-valid day (<", 
                                        minimum_wear_time_for_analysis, 
                                        " hours between ", start_day_analysis, 
                                        " and ", end_day_analysis, ")")),
                      values = c("#00BE6C", "#FC717F")) +
    scale_color_manual(labels = c("Valid day", 
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
          axis.text.x = element_text(size = title_text_size, angle = 45, hjust = 1, vjust = 1.1),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "beige", color = "beige"),
          plot.margin = margin(1, 1, 0.5, 1, "cm"),
          strip.text.x = element_text(size = strip_text),
          plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
    facet_wrap(. ~ variable, scales = "free_y", ncol = n_col) +
    guides(color = "none")
  
# Return plot
return(plot)
  
}

# Create figure for french language ===============================================================================
if (language == "fr") {
  
# Generate figure
if (metrics == "all") {
 selected_metrics <- c(
   "Temps de port (min)",
   "Total counts Axe 1 ",
   "Total counts VM",
   "Moyenne Axe 1 (counts/min)",
   "Moyenne VM (counts/min)",
   "Temps SED (min)", 
   "Temps LPA (min)", 
   "Temps MPA (min)",
   "Temps VPA (min)",
   "Temps MVPA (min)",
   "Proportion du temps de port SED (%)",
   "Proportion du temps de port LPA (%)",
   "Proportion du temps de port MPA (%)",
   "Proportion du temps de port VPA (%)",
   "Proportion du temps de port MVPA (%)",
   "Ratio MVPA / SED",
   "Total MET-hr", 
   "Total kcal",
   "NAP",
   "Total pas",
   "Acc. pas max. 60 min (pas/min)",
   "Acc. pas max. 30 min (pas/min)",
   "Acc. pas max. 20 min (pas/min)",
   "Acc. pas max. 5 min (pas/min)",
   "Acc. pas max. 1 min (pas/min)",
   "Acc. pas pic 60 min (pas/min)",
   "Acc. pas pic 30 min (pas/min)",
   "Acc. pas pic 20 min (pas/min)",
   "Acc. pas pic 5 min (pas/min)",
   "Acc. pas pic 1 min (pas/min)",
   "Gradient d'intensité",
   paste0("M1/3 (counts/", epoch_label, ")"),
   paste0("M120 (counts/", epoch_label, ")"),
   paste0("M60 (counts/", epoch_label, ")"), 
   paste0("M30 (counts/", epoch_label, ")"),
   paste0("M15 (counts/", epoch_label, ")"),
   paste0("M5 (counts/", epoch_label, ")")
 )
 
 fig_title <- "Tous les indicateurs"
 n_col = 5
 
} 

if (metrics == "volume") {
 selected_metrics <- c(
   "Temps de port (min)",
   "Total counts Axe 1 ",
   "Total counts VM",
   "Moyenne Axe 1 (counts/min)",
   "Moyenne VM (counts/min)",
   "Temps SED (min)", 
   "Temps LPA (min)", 
   "Temps MPA (min)",
   "Temps VPA (min)",
   "Temps MVPA (min)",
   "Proportion du temps de port SED (%)",
   "Proportion du temps de port LPA (%)",
   "Proportion du temps de port MPA (%)",
   "Proportion du temps de port VPA (%)",
   "Proportion du temps de port MVPA (%)",
   "Ratio MVPA / SED",
   "Total MET-hr", 
   "Total kcal",
   "NAP",
   "Total pas"
 )
 
 fig_title <- "Indicteurs de volume d'activité"
 n_col = 5
 
} 

if (metrics == "step_acc") {
 selected_metrics <- c(
   "Acc. pas max. 60 min (pas/min)",
   "Acc. pas max. 30 min (pas/min)",
   "Acc. pas max. 20 min (pas/min)",
   "Acc. pas max. 5 min (pas/min)",
   "Acc. pas max. 1 min (pas/min)",
   "Acc. pas pic 60 min (pas/min)",
   "Acc. pas pic 30 min (pas/min)",
   "Acc. pas pic 20 min (pas/min)",
   "Acc. pas pic 5 min (pas/min)",
   "Acc. pas pic 1 min (pas/min)"
 )
 
 fig_title <- "Indicateurs d'accumulation de pas"
 n_col = 5
 
} 

if (metrics == "int_distri") {
 selected_metrics <- c(
   "Gradient d'intensité",
   paste0("M1/3 (counts/", epoch_label, ")"),
   paste0("M120 (counts/", epoch_label, ")"),
   paste0("M60 (counts/", epoch_label, ")"), 
   paste0("M30 (counts/", epoch_label, ")"),
   paste0("M15 (counts/", epoch_label, ")"),
   paste0("M5 (counts/", epoch_label, ")")
 )
 
 fig_title <- "Indicateurs de distribution de l'intensité"
 n_col = 3
 
} 

# Make figure
plot <-
 data %>%
 dplyr::mutate(date = as.factor(date),
               validity = ifelse(wear_time >= minimum_wear_time_for_analysis * 60, "Valid day", "Non valid day"),
               validity =  forcats::fct_relevel(validity, "Valid day", "Non valid day")) %>%
 tidyr::pivot_longer(cols = c(
   wear_time,
   total_counts_axis1,
   total_counts_vm,
   axis1_per_min,
   vm_per_min,
   minutes_SED,
   minutes_LPA,
   minutes_MPA,
   minutes_VPA,
   minutes_MVPA,
   percent_SED,
   percent_LPA,
   percent_MPA,
   percent_VPA,
   percent_MVPA,
   ratio_mvpa_sed,
   mets_hours_mvpa, 
   total_kcal,
   pal,
   total_steps,
   max_steps_60min,
   max_steps_30min,
   max_steps_20min,
   max_steps_5min, 
   max_steps_1min, 
   peak_steps_60min, 
   peak_steps_30min, 
   peak_steps_20min, 
   peak_steps_5min,
   peak_steps_1min,
   ig,
   "M1/3",
   M120,
   M60, 
   M30,
   M15,
   M5
 ),
 names_to = "variable", values_to = "value") %>%
 dplyr::mutate(variable =  forcats::fct_relevel(
   variable, 
   "wear_time",
   "total_counts_axis1",
   "total_counts_vm",
   "axis1_per_min",
   "vm_per_min",
   "minutes_SED",
   "minutes_LPA",
   "minutes_MPA",
   "minutes_VPA",
   "minutes_MVPA",
   "percent_SED",
   "percent_LPA",
   "percent_MPA",
   "percent_VPA",
   "percent_MVPA",
   "ratio_mvpa_sed",
   "mets_hours_mvpa", 
   "total_kcal",
   "pal",
   "total_steps",
   "max_steps_60min",
   "max_steps_30min",
   "max_steps_20min",
   "max_steps_5min", 
   "max_steps_1min", 
   "peak_steps_60min", 
   "peak_steps_30min", 
   "peak_steps_20min", 
   "peak_steps_5min",
   "peak_steps_1min",
   "ig",
   "M1/3",
   "M120",
   "M60", 
   "M30",
   "M15",
   "M5"
 ),
 variable = forcats::fct_recode(
   variable, 
   "Temps de port (min)" = "wear_time",
   "Total counts Axe 1 "= "total_counts_axis1",
   "Total counts VM" = "total_counts_vm",
   "Moyenne Axe 1 (counts/min)" = "axis1_per_min",
   "Moyenne VM (counts/min)" = "vm_per_min",
   "Temps SED (min)" = "minutes_SED", 
   "Temps LPA (min)" = "minutes_LPA", 
   "Temps MPA (min)" = "minutes_MPA",
   "Temps VPA (min)" = "minutes_VPA",
   "Temps MVPA (min)" = "minutes_MVPA",
   "Proportion du temps de port SED (%)" = "percent_SED",
   "Proportion du temps de port LPA (%)" = "percent_LPA",
   "Proportion du temps de port MPA (%)" = "percent_MPA",
   "Proportion du temps de port VPA (%)" = "percent_VPA",
   "Proportion du temps de port MVPA (%)" = "percent_MVPA",
   "Ratio MVPA / SED" = "ratio_mvpa_sed",
   "Total MET-hr" = "mets_hours_mvpa", 
   "Total kcal" = "total_kcal",
   "NAP" = "pal",
   "Total pas" = "total_steps",
   "Acc. pas max. 60 min (pas/min)" = "max_steps_60min",
   "Acc. pas max. 30 min (pas/min)" ="max_steps_30min",
   "Acc. pas max. 20 min (pas/min)" ="max_steps_20min",
   "Acc. pas max. 5 min (pas/min)" ="max_steps_5min", 
   "Acc. pas max. 1 min (pas/min)" ="max_steps_1min", 
   "Acc. pas pic 60 min (pas/min)" ="peak_steps_60min", 
   "Acc. pas pic 30 min (pas/min)" ="peak_steps_30min", 
   "Acc. pas pic 20 min (pas/min)" ="peak_steps_20min", 
   "Acc. pas pic 5 min (pas/min)" ="peak_steps_5min",
   "Acc. pas pic 1 min (pas/min)" ="peak_steps_1min",
   "Gradient d'intensité" = "ig",
   "M1/3 (counts/{{epoch_label}})"  := "M1/3",
   "M120 (counts/{{epoch_label}})"  := "M120",
   "M60 (counts/{{epoch_label}})"   := "M60", 
   "M30 (counts/{{epoch_label}})"   := "M30",
   "M15 (counts/{{epoch_label}})"   := "M15",
   "M5 (counts/{{epoch_label}})"    := "M5"
 )
 ) %>%
 dplyr::filter(variable %in% selected_metrics) %>%
 dplyr::mutate(value_maj = ifelse(is.na(value), 0, value)) %>%
 ggplot(aes(x = date, y = value_maj, fill = validity, color = validity)) +
 ggtitle(fig_title) +
 geom_bar(stat = "identity") +
 geom_point(size = 2, color = "red") +
 geom_line(aes(group = 1), linewidth = 0.7, color = "red") +
 geom_text(aes(label = ifelse(is.na(value), "NA", round(value, 1))), size = label_text_size, vjust = -0.6, color = "black") +
 labs(x = "Date", y = "", fill = "") +
 scale_y_continuous(expand = expansion(mult = c(.05, .2))) +
 scale_fill_manual(labels = c("Jour valide", 
                              paste0("Jour non valide (<", 
                                     minimum_wear_time_for_analysis, 
                                     " heures entre ", start_day_analysis, 
                                     " et ", end_day_analysis, ")")),
                   values = c("#00BE6C", "#FC717F")) +
 scale_color_manual(labels = c("Jour valide", 
                               paste0("Jour non valide(<", 
                                      minimum_wear_time_for_analysis, 
                                      " heures entre ", start_day_analysis, 
                                      " et ", end_day_analysis, ")")),
                    values = c("#00BE6C", "#FC717F")) +
 theme_bw() + 
 theme(legend.position = "bottom",
       legend.text = element_text(size = 15),
       legend.background = element_rect(fill = "beige", colour = "beige"),
       axis.title.x = element_blank(),
       axis.text.x = element_text(size = title_text_size, angle = 45, hjust = 1, vjust = 1.1),
       axis.text.y = element_blank(),
       axis.ticks = element_blank(),
       plot.background = element_rect(fill = "beige", color = "beige"),
       plot.margin = margin(1, 1, 0.5, 1, "cm"),
       strip.text.x = element_text(size = strip_text),
       plot.title = element_text(size = 15, color = "grey30", face = "bold")) +
 facet_wrap(. ~ variable, scales = "free_y", ncol = n_col) +
 guides(color = "none")

# Return plot
return(plot)
  
  }
}