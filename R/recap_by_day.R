#' Summarize results by day
#' 
#' This function summarizes results for each day of the measurement period.
#' 
#' The following metrics are computed: 
#' \itemize{
#'   \item \strong{wear_time:} total wear time
#'   \item \strong{total_counts_axis1}: total counts for the vertical axis
#'   \item \strong{total_counts_vm:} total counts for the vector magnitude
#'   \item \strong{total_steps:} total step count
#'   \item \strong{total_kcal_wear_time:} total kilocalories
#'   \item \strong{minutes_SED:} total minutes spent in SED behavior
#'   \item \strong{minutes_LPA:} total minutes spent in LPA behavior
#'   \item \strong{minutes_MPA:} total minutes spent in MPA behavior
#'   \item \strong{minutes_VPA:} total minutes spent in VPA behavior
#'   \item \strong{minutes_MVPA:} total minutes spent in MVPA behavior
#'   \item \strong{percent_SED:} proportion of wear time spent in SED behavior
#'   \item \strong{percent_LPA:} proportion of wear time spent in LPA behavior
#'   \item \strong{percent_MPA:} proportion of wear time spent in MPA behavior
#'   \item \strong{percent_VPA:} proportion of wear time spent in VPA behavior
#'   \item \strong{percent_MVPA:} proportion of wear time spent in MPVA behavior
#'   \item \strong{mets_hours_mvpa:} total MET-hours spent during MPVA behavior
#'   \item \strong{ratio_mvpa_sed:} ratio between MVPA and SED times (minutes_MVPA / minutes_SED)
#'}
#'
#'PAL is computed as follows: total energy expenditure (TEE) is divided by BMR. TEE is obtained by summing 
#'    the kilocalories measured during wear time epochs and the kilocalories likely expended during nonwear time 
#'    epochs (that is, kilocalories associated to BMR, as it is assumed that the device was mainly not worn during 
#'    sleeping periods if any, periods during which energy expenditure is near of BMR), and by multiplying this sum
#'    by 10/9 to take into account the thermic effect of food. Of course, such calculations may
#'    conduct to underestimate TEE and PAL if the device was removed during prolonged periods of
#'    physical activity. Moreover, even if the device was correctly worn, the estimate of PAL is very
#'    approximate since both BMR and kilocalories related to wear time are estimated using methods
#'    that may not be accurate at the individual level.
#'   
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}}, \code{\link{mark_wear_time}}, and \code{\link{mark_intensity}} functions.
#' @param col_date A character value to indicate the name of the date variable.
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
#' mydata_with_wear_marks <- mark_wear_time(dataset = mydata)
#' mydata_with_intensity_marks <- mark_intensity(data = mydata_with_wear_marks, 
#'     equation = "Sasaki et al. (2011) [Adults]", age = 32, weight = 67, sex = "male")
#' recap_by_day(data = mydata_with_intensity_marks, col_date = "date", sex = "male")
#' 
recap_by_day <- function(data, col_date = "date", age = 40, weight = 70, sex = c("male", "female", "undefined")) {
  
  # Computing basal metabolic rate
    bmr_kcal_min <- compute_bmr(age = age, sex = sex, weight = weight) / (24*60)
  
  # getting results by day
  df <-
    data %>%
    dplyr::group_by( .data[[col_date]], .drop = FALSE) %>%
    dplyr::filter(wearing == "w") %>%
    dplyr::summarise(
      wear_time = sum(wearing_count),
      total_counts_axis1 = sum(axis1),
      total_counts_vm = sum(vm),
      total_steps = sum(steps),
      total_kcal_wear_time = round(sum(kcal), 2),
      minutes_SED = sum(SED),
      minutes_LPA = sum(LPA),
      minutes_MPA = sum(MPA),
      minutes_VPA = sum(VPA),
      minutes_MVPA = sum(MPA) + sum(VPA),
      percent_SED = round(minutes_SED / wear_time * 100, 2),
      percent_LPA = round(minutes_LPA / wear_time * 100, 2),
      percent_MPA = round(minutes_MPA / wear_time * 100, 2),
      percent_VPA = round(minutes_VPA / wear_time * 100, 2), 
      percent_MVPA = round(minutes_MVPA / wear_time * 100, 2),
      mets_hours_mvpa = round(sum(mets_hours_mvpa), 2),
      ratio_mvpa_sed = round(minutes_MVPA / minutes_SED, 2),
      
      # Computing physical activity level (PAL), that is, total EE / BMR. BMR is assigned to nonwear time; 
      # the term 10/9 is used to take into account the thermic effect of food
      pal = round((total_kcal_wear_time + bmr_kcal_min * (24*60 - wear_time)) * (10/9) / (bmr_kcal_min * (24*60)), 2)) %>%
    dplyr::ungroup()
  
  return(df)
}

