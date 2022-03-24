#' Add intensity metrics
#' 
#' This function adds several columns to a dataset that contains accelerometer counts data. 
#'     These columns concern respectively sedentary time (SED), light physical activity time (LPA), 
#'     moderate physical activity time (MPA), vigorous physical activity time (VPA), 
#'     metabolic equivalent of task (METs), kilocalories (kcal), and  MET-hours when 
#'     time is spent in moderate-to-vigorous physical activity. For the SED, LPA, MPA, 
#'     and VPA columns, the function provides, for each epoch, the numeric value 1 when 
#'     the value of the configured counts variable respectively fulfills the criteria of the 
#'     SED, LPA, MPA, and VPA category (e.g., for the SED column, 1 may be provided if VM counts are <150 counts/min); 
#'     otherwise 0 is provided.
#'     METs are computed using the \code{\link{compute_mets}} function. METs are computed using a published equation from one 
#'     of the following scientific articles: Sasaki et al. (2011; doi:10.1016/j.jsams.2011.04.003); Santos-Lozano et al. 
#'     (2013; 10.1055/s-0033-1337945); Freedson et al. (1998; doi: 10.1097/00005768-199805000-00021).
#'     Kilocalories are computed as follows. For non-SED epochs, MET values are multiplied by BMR expressed in kcal/min when using the Santos-Lozano et al. 
#'     (2013) equations since, in that study, METs were multiples of the measured (not standard) resting metabolic rate. When using the  Sasaki et al. (2011) 
#'     and Freedson et al. (1998) equations, the MET values are multiplied by weight and 1/60 since, in those studies, METs were multiples of standard resting 
#'     metabolic rate (i.e., 3.5 mLO2/min/kg) and a standard MET is approximately equivalent to 1 kcal/kg/h (Butte et al., 2012; doi: 10.1249/MSS.0b013e3182399c0e).  
#'     For SED epochs, BMR expressed in kcal/min is directly used. BMR is computed using the \code{\link{compute_bmr}} function that uses sex, age, and weight 
#'     inputs, and one of the equations retrieved from the paper by Henry et al. (2005; doi: 10.1079/PHN2005801). MET-hours are obtained by multiplying METs by time 
#'     related to each epoch (e.g., 1/60e of an hour for 1-min epochs), only when the MET value is  >=3.
#'
#' @param data A dataframe obtained using the \code{\link{prepare_dataset}} and then the \code{\link{mark_wear_time}} functions.
#' @param col_axis A character value to indicate the name of the variable to be used for determining intensity categories. 
#' @param equation A character string to indicate the equation to be used for estimating METs.
#' @param sed_cutpoint A numeric value below which time is considered as spent in sedentary behavior (in counts/min). 
#'                     You must convert to counts/min if you want to use a cut-point initially developped using an epoch shorter than 60 s. In the case where the epoch of the 
#'                     dataset would be shorter than 60 s, the function will multiply the counts data so that it corresponds to the cut-point expressed in counts/min.
#'                     You must provide a value inferior or equal to 60.
#' @param mpa_cutpoint A numeric value at and above which time is considered as spent in moderate-to-vigorous physical activity (in counts/min). 
#'                     You must convert to counts/min if you want to use a cut-point initially developped using an epoch shorter than 60 s. In the case where the epoch of the 
#'                     dataset would be shorter than 60 s, the function will multiply the counts data so that it corresponds to the cut-point expressed in counts/min.
#'                     You must provide a value inferior or equal to 60.
#' @param vpa_cutpoint A numeric value at and above which time is considered as spent in vigorous physical activity (in counts/min). 
#'                     You must convert to counts/min if you want to use a cut-point initially developped using an epoch shorter than 60 s. In the case where the epoch of the 
#'                     dataset would be shorter than 60 s, the function will multiply the counts data so that it corresponds to the cut-point expressed in counts/min.
#'                     You must provide a value inferior or equal to 60.
#' @param age A numeric value in yr.
#' @param weight A numeric value in kg.
#' @param sex A character value.
#' @param dates A character vector containing the dates to be retained for analyzis. The dates must be with the "YYYY-MM-DD" format.

#' @return A dataframe.
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
#'     sex = "male",
#'     )
#' head(mydata_with_intensity_marks)
#' 
mark_intensity <- function(data, 
                           col_axis = "vm", 
                           sed_cutpoint = 200, 
                           mpa_cutpoint = 2690, 
                           vpa_cutpoint = 6167, 
                           equation = c("Sasaki et al. (2011) [Adults]",
                                        "Santos-Lozano et al. (2013) [Adults]",
                                        "Freedson et al. (1998) [Adults]",
                                        "Santos-Lozano et al. (2013) [Older adults]"),
                           age = 40, 
                           weight = 70, 
                           sex = c("male", "female", "undefined"),
                           dates = NULL) {
  
  
  if (is.null(dates)) {selected_dates <- attributes(as.factor(data$date))$levels}
  else {selected_dates <- attributes(as.factor(dates))$levels}
  
  equation <- match.arg(equation)
  sex <- match.arg(sex)
  
  # Correction factor for determining intensity from cut-points and equations
    cor_factor = 60 / (as.numeric(data$time[2] - data$time[1]))
  
  # Computing basal metabolic rate
    bmr_kcal_min <- suppressMessages(
      compute_bmr(age = age, sex = sex, weight = weight) / (24*60)
  )
  
  # Adding metrics
    df <-
        data %>%
        dplyr::mutate(
         SED = ifelse(.data[[col_axis]] < sed_cutpoint / cor_factor, 1, 0),
         LPA = ifelse(.data[[col_axis]] >= sed_cutpoint / cor_factor & .data[[col_axis]] < mpa_cutpoint / cor_factor, 1, 0),
         MPA = ifelse(.data[[col_axis]] >= mpa_cutpoint / cor_factor & .data[[col_axis]] < vpa_cutpoint / cor_factor, 1, 0), 
         VPA = ifelse(.data[[col_axis]] >= vpa_cutpoint / cor_factor, 1, 0),
         METS = suppressMessages(compute_mets(data = 
                                                data %>% 
                                                dplyr::mutate(axis1 = axis1 * cor_factor,
                                                              vm = vm * cor_factor),
                                              equation = equation, 
                                              weight = weight, 
                                              sex = sex)
                                 ),
         kcal = dplyr::case_when(
         SED == 1 ~ bmr_kcal_min / cor_factor,
         equation == "Sasaki et al. (2011) [Adults]" | equation == "Freedson et al. (1998) [Adults]" ~ METS * weight * 1/60 / cor_factor,
         equation == "Santos-Lozano et al. (2013) [Adults]" | equation == "Santos-Lozano et al. (2013) [Older adults]" ~ METS * bmr_kcal_min / cor_factor
        ),
          

      # Computing MET-hr corresponding to MVPA only, for each epoch
        mets_hours_mvpa = ifelse(METS >= 3, METS * 1/60 / cor_factor, 0),

      ) %>%
      
      # Filtering data based on selected dates 
        dplyr::filter(date %in% as.Date(selected_dates))
      

  # Marking the bouts based on intensity categories
    df$intensity_category <- ifelse(df$non_wearing_count == 1, "Nonwear", 
                                    ifelse(df$SED == 1, "SED", 
                                           ifelse(df$LPA == 1, "LPA", "MVPA")))
    
        
      # Thanks to https://stackoverflow.com/questions/29661269/increment-by-1-for-every-change-in-column 
      # for the code block below
        df$intensity_category <- as.factor(df$intensity_category)
        df$intensity_category_num <- as.numeric(forcats::fct_recode(df$intensity_category , "0" = "Nonwear", "1" = "SED", "2" = "LPA", "3" = "MVPA"))
        df$bout <- cumsum(c(1, as.numeric(diff(df$intensity_category_num))!= 0))
    
    
    
    
  # Providing information about the parameters used for computing results
    message(paste0("You have computed intensity metrics with the mark_intensity() function using the following inputs: 
    axis = ", col_axis, "
    sed_cutpoint = ", sed_cutpoint, " counts/min", "
    mpa_cutpoint = ", mpa_cutpoint, " counts/min", "
    vpa_cutpoint = ", vpa_cutpoint, " counts/min", "
    equation = " , equation, "
    age = ", age, "
    weight = ", weight, "
    sex = ", sex))
   
  # Returning dataframe 
    return(df)
    
}
