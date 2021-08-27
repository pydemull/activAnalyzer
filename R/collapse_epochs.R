# Functions retrieved from https://rdrr.io/github/dipetkov/actigraph.sleepr/

check_epochlen_is_60 <- function(agdb, algorithm) {
  assert_that(get_epoch_length(agdb) == 60,
              msg = paste0(
                algorithm, " assumes 60sec epochs. ",
                "Aggregate epochs with `collapse_epochs`."
              )
  )
}
check_no_missing_timestamps <- function(agdb) {
  assert_that(has_missing_epochs(agdb) == FALSE,
              msg = paste0(
                "Missing timestamps. ",
                "Epochs should be evenly spaced from ",
                "first(timestamp) to last(timestamp)."
              )
  )
}
check_no_missing_counts <- function(agdb, var) {
  assert_that(noNA(agdb[[var]]),
              msg = paste0(
                "Missing ", var, " counts. These ",
                "can be imputed with `impute_epochs`."
              )
  )
}
check_no_missing_state <- function(agdb) {
  assert_that(has_name(agdb, "sleep"),
              msg = paste0(
                "Missing asleep/awake (S/W) indicator column. ",
                "State can be inferred with `apply_sadeh` ",
                "or `apply_cole_kripke.`"
              )
  )
  assert_that(noNA(agdb[["sleep"]]),
              msg = "Missing asleep/awake values."
  )
}
check_args_collapse_method <- function(agdb, epoch_len_out) {
  check_no_missing_timestamps(agdb)
  check_no_missing_counts(agdb, "axis1")
  assert_that(epoch_len_out == 60,
              msg = "Use `collapse_epochs` to aggregate to 60s epochs."
  )
  assert_that(exact_division(epoch_len_out, get_epoch_length(agdb)),
              msg = paste0(
                "Output epoch length is not an exact multiple ",
                "of input epoch length."
              )
  )
}
collapse_epochs_updated <- function(agdb, epoch_len_out,
                            use_incomplete = TRUE) {
  check_args_collapse_method(agdb, epoch_len_out)
  collapse_factor <- epoch_len_out / get_epoch_length(agdb)
  if (collapse_factor == 1) {
    return(agdb)
  }
  
  # TODO: a more general approach to collapsing
  # might use the findInterval function
  # though care must be taken with "incomplete"
  # epochs at the start/end of the time series
  
  agdb %>%
    do(collapse_epochs_(., collapse_factor, use_incomplete))
}
exact_division <- function(a, b) {
  (a %% b) == 0
}
collapse_epochs_ <- function(data, collapse_factor, use_incomplete) {
  
  # Exclude lux which is summarised by `floor(mean(lux))`
  selected <- base::intersect(
    colnames(data),
    c(
      "axis1", "axis2", "axis3", "steps", "vm",
      "inclineoff", "inclinestanding",
      "inclinesitting", "inclinelying"
    )
  )
  
  data <- data %>%
    select_at(vars("timestamp", all_of(selected))) %>%
    mutate(timestamp = floor_date(.data$timestamp, "mins"), n = 1L) %>%
    group_by(.data$timestamp) %>%
    summarise_all(sum)
  
  if (!use_incomplete) {
    data <- data %>% filter(.data$n == collapse_factor)
  }
  
  data %>% select(-n)
}