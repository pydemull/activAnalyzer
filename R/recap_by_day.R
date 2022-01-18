#' Summarize results by day
#' 
#' This function summarizes accelerometer results for each day of the measurement period.
#' 
#' The following metrics are computed: 
#' \itemize{
#'   \item \strong{wear_time:} total wear time computed using the daily period defined in the function
#'   \item \strong{total_counts_axis1:} total counts for the vertical axis
#'   \item \strong{total_counts_vm:} total counts for the vector magnitude
#'   \item \strong{axis1_per_min:} mean of the counts per minute for the vertical axis (during the considered wear time only)
#'   \item \strong{vm_per_min:} mean of the counts per minute for the vector magnitude (during the considered wear time only)
#'   \item \strong{total_steps:} total step count
#'   \item \strong{total_kcal:} total kilocalories
#'   \item \strong{minutes_SED:} total minutes spent in SED behavior
#'   \item \strong{minutes_LPA:} total minutes spent in LPA behavior
#'   \item \strong{minutes_MPA:} total minutes spent in MPA behavior
#'   \item \strong{minutes_VPA:} total minutes spent in VPA behavior
#'   \item \strong{minutes_MVPA:} total minutes spent in MVPA behavior
#'   \item \strong{percent_SED:} proportion of wear time (over the 24-h cycle) spent in SED behavior
#'   \item \strong{percent_LPA:} proportion of wear time (over the 24-h cycle) spent in LPA behavior
#'   \item \strong{percent_MPA:} proportion of wear time (over the 24-h cycle) spent in MPA behavior
#'   \item \strong{percent_VPA:} proportion of wear time (over the 24-h cycle) spent in VPA behavior
#'   \item \strong{percent_MVPA:} proportion of wear time (over the 24-h cycle) spent in MPVA behavior
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
#'   \item \strong{mets_hours_mvpa:} total MET-hours spent during MPVA behavior
#'   \item \strong{ratio_mvpa_sed:} ratio between MVPA and SED times (`minutes_MVPA` / `minutes_SED`)
#'}
#'
#'PAL is computed by dividing total energy expenditure (TEE) by BMR. TEE is obtained by summing 
#'    the kilocalories measured during wear time epochs and the kilocalories related to BMR expended during nonwear time 
#'    epochs (it is assumed that the periods where the device was not worn corresponded to 
#'    sleeping periods, during which energy expenditure is near of BMR), and by multiplying this sum
#'    by 10/9 to take into account the thermic effect of food. Of course, such calculations may
#'    conduct to underestimate TEE and PAL if the device was removed during prolonged periods of
#'    physical activity. Moreover, even if the device was correctly worn, the estimate of PAL is very
#'    approximate since both BMR and kilocalories are estimated using methods
#'    that may not be accurate at the individual level.
#'   
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, and then \code{\link{mark_intensity}} functions.
#' @param col_date A character value to indicate the name of the date variable.
#' @param col_time A character value indicating the name of the variable where time information is provided.
#' @param valid_wear_time_start A character value with the HH:MM:SS format to set the start of the daily period to consider for computing valid wear time.
#' @param valid_wear_time_end A character value with the HH:MM:SS format to set the end of the daily period to consider for computing valid wear time.
#' @param age A numeric value in yr.
#' @param weight A numeric value in kg.
#' @param sex A character value.
#'
#' @return A dataframe.
#' @export

#' @importFrom magrittr %>%
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
#' recap_by_day(
#'     data = mydata_with_intensity_marks, 
#'     age = 32, 
#'     weight = 67, 
#'     sex = "male",
#'     valid_wear_time_start = "07:00:00",
#'     valid_wear_time_end = "20:00:00"
#'     )
#' 
recap_by_day <- function(data, 
                         col_date = "date", 
                         col_time = "time",
                         valid_wear_time_start = "00:00:00",
                         valid_wear_time_end = "23:59:00",
                         age = 40, 
                         weight = 70, 
                         sex = c("male", "female", "undefined")) {
  
  # Computing basal metabolic rate
    bmr_kcal_min <- suppressMessages(
      compute_bmr(age = age, sex = sex, weight = weight) / (24*60)
    )
  
  # Getting results by day
      
         # Defining a function for getting a NA value when computing a step-based metric
         # from a vector with no non-missing values and the max() function
         # Retrieved from: https://stackoverflow.com/questions/24519794/r-max-function-ignore-na
           my_max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm = TRUE), NA)
   
    # Summarising results by day              
    df <-
      data %>%
      dplyr::group_by(.data[[col_date]], .drop = FALSE) %>%
      dplyr::filter(
        .data[[col_time]] >= hms::as_hms(valid_wear_time_start) &
          .data[[col_time]] <= hms::as_hms(valid_wear_time_end) &
          wearing == "w") %>%
      dplyr::summarise(
        wear_time = sum(wearing_count, na.rm = TRUE),
        total_counts_axis1 = sum(axis1, na.rm = TRUE),
        total_counts_vm = sum(vm, na.rm = TRUE),
        axis1_per_min = round(sum(axis1, na.rm = TRUE) / wear_time, 2),
        vm_per_min = round(sum(vm, na.rm = TRUE) / wear_time, 2),
        total_steps = sum(steps, na.rm = TRUE),
        total_kcal = round(sum(kcal, na.rm = TRUE) + bmr_kcal_min * (24*60 - wear_time), 2),
        minutes_SED = sum(SED, na.rm = TRUE),
        minutes_LPA = sum(LPA, na.rm = TRUE),
        minutes_MPA = sum(MPA, na.rm = TRUE),
        minutes_VPA = sum(VPA, na.rm = TRUE),
        minutes_MVPA = sum(MPA, na.rm = TRUE) + sum(VPA, na.rm = TRUE),
        percent_SED = round(minutes_SED / wear_time * 100, 2),
        percent_LPA = round(minutes_LPA / wear_time * 100, 2),
        percent_MPA = round(minutes_MPA / wear_time * 100, 2),
        percent_VPA = round(minutes_VPA / wear_time * 100, 2), 
        percent_MVPA = round(minutes_MVPA / wear_time * 100, 2),
        max_steps_60min = round(my_max(accum_steps_60min), 2),
        max_steps_30min = round(my_max(accum_steps_30min), 2),
        max_steps_20min = round(my_max(accum_steps_20min), 2),
        max_steps_5min = round(my_max(accum_steps_5min), 2),
        max_steps_1min = round(my_max(accum_steps_1min), 2),
        peak_steps_60min = round(mean(head(sort(steps, decreasing = TRUE), n = 60L), na.rm = TRUE), 2),
        peak_steps_30min = round(mean(head(sort(steps, decreasing = TRUE), n = 30L), na.rm = TRUE), 2),
        peak_steps_20min = round(mean(head(sort(steps, decreasing = TRUE), n = 20L), na.rm = TRUE), 2),
        peak_steps_5min = round(mean(head(sort(steps, decreasing = TRUE), n = 5L), na.rm = TRUE), 2), 
        peak_steps_1min = round(mean(head(sort(steps, decreasing = TRUE), n = 1L), na.rm = TRUE), 2),
        mets_hours_mvpa = round(sum(mets_hours_mvpa, na.rm = TRUE), 2),
        ratio_mvpa_sed = round(minutes_MVPA / minutes_SED, 2),
        
        # Computing physical activity level (PAL), that is, total EE / BMR. BMR is assigned to nonwear time; 
        # the term 10/9 is used to take into account the thermic effect of food
        pal = round(total_kcal * (10/9) / (bmr_kcal_min * (24*60)), 2)) %>%
      dplyr::ungroup()

    
    # Providing information about the parameters used for computing results
    message(paste0("You have computed results with the recap_by_day() function using the following inputs: 
    age = ", age, "
    weight = ", weight, "
    sex = ", sex))
    
  return(df)
}

