#' A `tibble` of activity data exported by an ActiGraph device
#'
#' This tibble has several attributes, most importantly, `epochlength`. 
#' (Code is from actigraph.sleepr package <https://github.com/dipetkov/actigraph.sleepr/>. 
#' See LICENCE.note file in the app skeleton.)
#' 
#' @param data A data frame of raw activity counts.
#' @param settings A data frame of device settings.
#' @return A tibble containing accelerometer data and having measurement settings as attributes.
#' 
tbl_agd <- function(data, settings) {
  assertthat::assert_that(
    is.data.frame(data),
    is.data.frame(settings),
    assertthat::has_name(data, "axis1"),
    assertthat::has_name(data, "timestamp"),
    assertthat::has_name(settings, "epochlength")
  )
  
  for (key in names(settings)) {
    attr(data, key) <- settings[[key]]
  }
  structure(data, class = c("tbl_df", "tbl", "data.frame"))
}
