#' Summarize results by day
#' 
#' This function summarizes various accelerometer metrics for each day of the measurement period.
#' 
#' The following metrics are computed from epochs corresponding to valid wear time: 
#' \itemize{
#'   \item \strong{wear_time:} total wear time computed using the daily period defined in the function
#'   \item \strong{total_counts_axis1:} total counts for the vertical axis
#'   \item \strong{total_counts_vm:} total counts for the vector magnitude
#'   \item \strong{axis1_per_min:} mean of the counts per minute for the vertical axis
#'   \item \strong{vm_per_min:} mean of the counts per minute for the vector magnitude
#'   \item \strong{minutes_SED:} total minutes spent in SED behavior
#'   \item \strong{minutes_LPA:} total minutes spent in LPA behavior
#'   \item \strong{minutes_MPA:} total minutes spent in MPA behavior
#'   \item \strong{minutes_VPA:} total minutes spent in VPA behavior
#'   \item \strong{minutes_MVPA:} total minutes spent in MVPA behavior
#'   \item \strong{percent_SED:} proportion of wear time spent in SED behavior
#'   \item \strong{percent_LPA:} proportion of wear time spent in LPA behavior
#'   \item \strong{percent_MPA:} proportion of wear time spent in MPA behavior
#'   \item \strong{percent_VPA:} proportion of wear time spent in VPA behavior
#'   \item \strong{percent_MVPA:} proportion of wear time spent in MVPA behavior
#'   \item \strong{ratio_mvpa_sed:} ratio between MVPA and SED times (`minutes_MVPA` / `minutes_SED`)
#'   \item \strong{mets_hours_mvpa:} total MET-hours spent in MVPA behavior
#'   \item \strong{total_kcal:} total kilocalories
#'   \item \strong{PAL:} physical activity level
#'   \item \strong{total_steps:} total step count
#'   \item \strong{max_steps_60min:} best step accumulation per minute averaged over a window of 60 continuous minutes
#'   \item \strong{max_steps_30min:} best step accumulation per minute averaged over a window of 30 continuous minutes
#'   \item \strong{max_steps_20min:} best step accumulation per minute averaged over a window of 20 continuous minutes
#'   \item \strong{max_steps_5min:} best step accumulation per minute averaged over a window of 5 continuous minutes
#'   \item \strong{max_steps_1min:} best step accumulation per minute over a window of 1 minute
#'   \item \strong{peak_steps_60min:} step accumulation per minute averaged over the best 60 continuous or discontinuous minutes
#'   \item \strong{peak_steps_30min:} step accumulation per minute averaged over the best 30 continuous or discontinuous minutes
#'   \item \strong{peak_steps_20min:} step accumulation per minute averaged over the best 20 continuous or discontinuous minutes
#'   \item \strong{peak_steps_5min:} step accumulation per minute averaged over the best 5 continuous or discontinuous minutes
#'   \item \strong{peak_steps_1min:} step accumulation per minute over the best minute (same result as for `max_steps_1min`)
#'   \item \strong{ig:} intensity gradient
#'   \item \strong{M1/3:} the count value (in counts/epoch duration) above which the most active 8h were accumulated over the day
#'   \item \strong{M120:} the count value (in counts/epoch duration) above which the most active 120 minutes were accumulated over the day
#'   \item \strong{M60:} the count value (in counts/epoch duration) above which the most active 60 minutes were accumulated over the day
#'   \item \strong{M30:} the count value (in counts/epoch duration) above which the most active 30 minutes were accumulated over the day
#'   \item \strong{M15:} the count value (in counts/epoch duration) above which the most active 15 minutes were accumulated over the day
#'   \item \strong{M5:} the count value (in counts/epoch duration) above which the most active 5 minutes were accumulated over the day
#'}
#'
#'PAL is computed by dividing total energy expenditure (TEE) by BMR. TEE is obtained by summing 
#'    the kilocalories computed for wear time epochs and the kilocalories related to BMR theoretically expended 
#'    during nonwear time epochs (it is assumed that the periods where the device was not worn corresponded to 
#'    sleeping periods, during which energy expenditure is near of BMR), and by multiplying this sum
#'    by 10/9 to take into account the thermic effect of food. Of course, such calculations may
#'    conduct to underestimate TEE and PAL if the device was removed during prolonged periods of
#'    physical activity. Moreover, even if the device was correctly worn, the estimate of PAL is very
#'    approximate since both BMR and kilocalories are estimated using methods
#'    that may not be accurate at the individual level.
#'    
#'The intensity gradient and the MX metrics are obtained using the \code{\link{compute_intensity_distri_metrics}} function.
#'   
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, and then the \code{\link{mark_intensity}} functions.
#' @param col_axis A character value to indicate the name of the variable to be used to compute total time per bin of intensity and then intensity gradient.
#' @param col_time A character value indicating the name of the variable where time information is provided.
#' @param col_nonwear A character value to indicate the name of the variable used to count nonwear time.
#' @param col_wear A character value to indicate the name of the variable used to count wear time.
#' @param valid_wear_time_start A character value with the HH:MM:SS format to set the start of the daily period to consider for computing valid wear time.
#' @param valid_wear_time_end A character value with the HH:MM:SS format to set the end of the daily period to consider for computing valid wear time.
#' @param age A numeric value in yr.
#' @param weight A numeric value in kg.
#' @param sex A character value.
#' @param start_first_bin A numeric value to set the lower bound of the first bin of the intensity band (in counts/epoch duration).
#' @param start_last_bin A numeric value to set the lower bound of the last bin of the intensity band (in counts/epoch duration).
#' @param bin_width A numeric value to set the width of the bins of the intensity band (in counts/epoch duration).
#'
#' @return A list of objects: `df_all_metrics`, `p_band`, and `p_log`. 
#'     `df_all_metrics` is a dataframe containing all the metrics for each day.
#'     `p_band` is a figure that shows the distribution of time spent in the configured bins of intensity for each day of the dataset. 
#'     `p_log` is a figure that shows, for each day, the relationship between the natural log of time spent in each intensity bin with the natural 
#'     log of the middle values of the intensity bins. 

#' @export

#' @importFrom magrittr %>%
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
#' recap_by_day(
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
#'     )
#' 
recap_by_day <- function(
  data,
  col_axis = "vm",
  col_time = "time",
  col_nonwear = "non_wearing_count",
  col_wear = "wearing_count",
  valid_wear_time_start = "00:00:00",
  valid_wear_time_end = "23:59:59",
  age = 40, 
  weight = 70, 
  sex = c("male", "female", "intersex", "undefined", "prefer not to say"),
  start_first_bin = 0,
  start_last_bin = 10000,
  bin_width = 500
  ) {
  
  sex <- match.arg(sex)
  
  # Computing basal metabolic rate
    bmr_kcal_min <- suppressMessages(
      compute_bmr(age = age, sex = sex, weight = weight) / (24*60)
    )
  
  # Correction factor to get results in minutes during following analyzes
    cor_factor = 60 / (as.numeric(data[[col_time]][2] - data[[col_time]][1]))
  
  # Getting results by day
      
         # Getting volume metrics by day   
                    
           df_vol_metrics <-
             data %>%
             dplyr::group_by(date, .drop = FALSE) %>%
             dplyr::filter(
               .data[[col_time]] >= hms::as_hms(valid_wear_time_start) &
                 .data[[col_time]] <= hms::as_hms(valid_wear_time_end) &
                 wearing == "w") %>%
             dplyr::summarise(
               wear_time = sum(.data[[col_wear]], na.rm = TRUE) / cor_factor,
               total_counts_axis1 = sum(axis1, na.rm = TRUE),
               total_counts_vm = sum(vm, na.rm = TRUE),
               axis1_per_min = round(sum(axis1, na.rm = TRUE) / wear_time, 2),
               vm_per_min = round(sum(vm, na.rm = TRUE) / wear_time, 2),
               total_steps = sum(steps, na.rm = TRUE),
               total_kcal = round(sum(kcal, na.rm = TRUE) + bmr_kcal_min * (as.numeric(hms::as_hms(valid_wear_time_end) - hms::as_hms(valid_wear_time_start))/60 - wear_time), 2),
               minutes_SED = round(sum(SED, na.rm = TRUE) / cor_factor, 2),
               minutes_LPA = round(sum(LPA, na.rm = TRUE) / cor_factor, 2),
               minutes_MPA = round(sum(MPA, na.rm = TRUE) / cor_factor, 2),
               minutes_VPA = round(sum(VPA, na.rm = TRUE) / cor_factor, 2),
               minutes_MVPA = round(sum(MPA, na.rm = TRUE) / cor_factor + sum(VPA, na.rm = TRUE) / cor_factor, 2),
               percent_SED = round(minutes_SED / wear_time * 100, 2),
               percent_LPA = round(minutes_LPA / wear_time * 100, 2),
               percent_MPA = round(minutes_MPA / wear_time * 100, 2),
               percent_VPA = round(minutes_VPA / wear_time * 100, 2), 
               percent_MVPA = round(minutes_MVPA / wear_time * 100, 2),
               mets_hours_mvpa = round(sum(mets_hours_mvpa, na.rm = TRUE), 2),
               ratio_mvpa_sed = round(minutes_MVPA / minutes_SED, 2),
               
               # Computing physical activity level (PAL), that is, total EE / BMR. BMR is assigned to nonwear time; 
               # the term 10/9 is used to take into account the thermic effect of food
               pal = round(total_kcal * (10/9) / (bmr_kcal_min * (as.numeric(hms::as_hms(valid_wear_time_end) - hms::as_hms(valid_wear_time_start))/60)), 2)) %>%
             dplyr::ungroup()
           
           
           # Defining a function for getting a NA value when computing a step-based metric
           # from a vector with no non-missing values and the max() function
           # Retrieved from: https://stackoverflow.com/questions/24519794/r-max-function-ignore-na
           my_max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm = TRUE), NA)
           
           
           # Getting step-based metrics for each day of measurement (using a 1-min epoch)
           
           if (as.numeric(data[[col_time]][2] - data[[col_time]][1]) == 60) { 
             
             df_step_metrics <- 
               data %>%
               tidyr::separate("timestamp", c("date", "time"), sep = " ") %>%
               dplyr::mutate(date = as.Date(date), time = hms::as_hms(time)) %>%
               dplyr::select(
                 date, 
                 time, 
                 steps
               ) %>%
               dplyr::group_by(date, .drop = FALSE) %>%
               dplyr::filter(
                 time >= hms::as_hms(valid_wear_time_start) &
                   time <= hms::as_hms(valid_wear_time_end)
               ) %>%
               dplyr::summarise(
                 max_steps_60min = round(my_max(zoo::rollmean(steps, align = "center", k = 60L, fill = NA)), 2),
                 max_steps_30min = round(my_max(zoo::rollmean(steps, align = "center", k = 30L, fill = NA)), 2),
                 max_steps_20min = round(my_max(zoo::rollmean(steps, align = "center", k = 20L, fill = NA)), 2),
                 max_steps_5min = round(my_max(zoo::rollmean(steps, align = "center", k = 5L, fill = NA)), 2),
                 max_steps_1min = round(my_max(zoo::rollmean(steps, align = "center", k = 1L, fill = NA)), 2),
                 peak_steps_60min = round(mean(head(sort(steps, decreasing = TRUE), n = 60L), na.rm = TRUE), 2),
                 peak_steps_30min = round(mean(head(sort(steps, decreasing = TRUE), n = 30L), na.rm = TRUE), 2),
                 peak_steps_20min = round(mean(head(sort(steps, decreasing = TRUE), n = 20L), na.rm = TRUE), 2),
                 peak_steps_5min = round(mean(head(sort(steps, decreasing = TRUE), n = 5L), na.rm = TRUE), 2), 
                 peak_steps_1min = round(mean(head(sort(steps, decreasing = TRUE), n = 1L), na.rm = TRUE), 2),
               )
           } else {
             
             df_step_metrics <- 
               PhysicalActivity::dataCollapser(
                 dataset = data, 
                 TS = "timestamp", 
                 by = 60
               ) %>%
               tidyr::separate("timestamp", c("date", "time"), sep = " ") %>%
               dplyr::mutate(date = as.Date(date), time = hms::as_hms(time)) %>%
               dplyr::select(
                 date, 
                 time, 
                 steps
               ) %>%
               dplyr::group_by(date, .drop = FALSE) %>%
               dplyr::filter(
                 time >= hms::as_hms(valid_wear_time_start) &
                   time <= hms::as_hms(valid_wear_time_end)
               ) %>%
               dplyr::summarise(
                 max_steps_60min = round(my_max(zoo::rollmean(steps, align = "center", k = 60L, fill = NA)), 2),
                 max_steps_30min = round(my_max(zoo::rollmean(steps, align = "center", k = 30L, fill = NA)), 2),
                 max_steps_20min = round(my_max(zoo::rollmean(steps, align = "center", k = 20L, fill = NA)), 2),
                 max_steps_5min = round(my_max(zoo::rollmean(steps, align = "center", k = 5L, fill = NA)), 2),
                 max_steps_1min = round(my_max(zoo::rollmean(steps, align = "center", k = 1L, fill = NA)), 2),
                 peak_steps_60min = round(mean(head(sort(steps, decreasing = TRUE), n = 60L), na.rm = TRUE), 2),
                 peak_steps_30min = round(mean(head(sort(steps, decreasing = TRUE), n = 30L), na.rm = TRUE), 2),
                 peak_steps_20min = round(mean(head(sort(steps, decreasing = TRUE), n = 20L), na.rm = TRUE), 2),
                 peak_steps_5min = round(mean(head(sort(steps, decreasing = TRUE), n = 5L), na.rm = TRUE), 2), 
                 peak_steps_1min = round(mean(head(sort(steps, decreasing = TRUE), n = 1L), na.rm = TRUE), 2),
               )
           }
           
           
           # Getting intensity distribution metrics by day
           list_int_distri_merics <- 
             compute_intensity_distri_metrics(
               data = data,
               col_axis = col_axis,
               col_time = col_time,
               valid_wear_time_start = valid_wear_time_start,
               valid_wear_time_end = valid_wear_time_end,
               start_first_bin = start_first_bin,
               start_last_bin = start_last_bin,
               bin_width = bin_width
             )
         
           df_all_metrics <- 
               dplyr::left_join(df_vol_metrics, df_step_metrics, by = "date") %>% 
               dplyr::left_join(list_int_distri_merics$metrics, by = "date") %>% 
               dplyr::select(
                   date,
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
                   `M1/3`,
                   M120,
                   M60, 
                   M30,
                   M15,
                   M5
             )

           
        # Creating list of results
        res <- list(df_all_metrics = df_all_metrics, p_band = list_int_distri_merics$p_band, p_log = list_int_distri_merics$p_log)
         
        # Providing information about the parameters used for computing results
         message(paste0("You have computed results with the recap_by_day() function using the following inputs: 
         age = ", age, "
         weight = ", weight, "
         sex = ", sex))
    
  return(res)
}

