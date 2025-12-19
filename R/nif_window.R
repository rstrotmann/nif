#' Calculate time deviations for observations
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Calculate the time difference in hours between the actual time after dose
#' (TAD) and the scheduled time (NTIME) of observations.
#'
#' If NTIME == 0, the data point is considered pre-dose, and instead of TAD, the
#' (negative) time difference to the next administration is calculated!
#'
#' @param obj A nif object.
#'
#' @returns A nif object with the 'TIME_DEV' field added
#' @export
#' @examples
#' library(dplyr)
#'
#' examplinib_poc_nif %>%
#'   add_time_deviation() %>%
#'   head()
#'
add_time_deviation <- function(obj) {
  # input validation
  validate_nif(obj)

  # ensure that TAD and NTIME fields are present and not NA
  required_fields <- c("TAD", "NTIME")
  missing_fields <- setdiff(required_fields, names(obj))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Missing ", plural("field", length(missing_fields) > 1),
      " in nif object: ", nice_enumeration(missing_fields)
    ))
  }

  # identify observations without NTIME
  temp <- obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    reframe(n_obs = n(), n_missing_ntime = sum(is.na(NTIME)), .by = "ANALYTE")

  # calculate time deviation
  out <- obj %>%
    arrange(.data$USUBJID, .data$DTC, -.data$EVID) %>%
    group_by(.data$PARENT) %>%
    # identify DTC of next administration
    mutate(next_admin = case_when(.data$EVID == 1 ~ .data$DTC)) %>%
    tidyr::fill("next_admin", .direction = "up") %>%
    # calculate time to next dose
    mutate(TTND = as.numeric(.data$DTC - .data$next_admin, units = "hours")) %>%
    ungroup() %>%
    # calculate time difference
    mutate(TIME_DEV = round(
      case_when(
        .data$NTIME == 0 ~ .data$TTND,
        .default = .data$TAD - .data$NTIME
      ),
      3
    )) %>%
    select(-c("TTND", "next_admin")) %>%
    nif()

  return(out)
}


#' Flag time window violations
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' .BEFORE and .AFTER are inclusive!
#'
#' @param obj A nif object.
#' @param analyte The analyte to apply the time window to.
#' @param window Time window definition as data frame.
#' @param use_minutes time window is given in minutes rather than hours,
#'   defaults to TRUE.
#' @param silent Suppress messages, as logical.
#'
#' @returns A nif object with TIME_DEV, EXCL, EXCL_REASON fields added.
#' @export
add_time_window_flag <- function(
  obj, window, analyte = NULL, use_minutes = TRUE, silent = NULL
) {
  # validate input
  validate_nif(obj)
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_logical_param(silent, "silent", allow_null = TRUE)

  if (!inherits(window, "data.frame")) {
    stop("'window' must be a data frame!")
  }

  if (is.null(analyte)) {
    analyte <- guess_analyte(obj)
    conditional_message(
      "No analyte specified, using ", analyte,
      silent = silent
    )
  }

  if (!analyte %in% analytes(obj)) {
    stop("analyte ", analyte, " not found in nif object!")
  }

  missing_fields <- setdiff(c("NTIME", "BEFORE", "AFTER"), names(window))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Missing ", plural("field", length(missing_fields) > 1),
      " in 'window': ", nice_enumeration(missing_fields)
    ))
  }

  if (!all(as.logical(lapply(window, is.numeric)))) {
    stop("All fields in 'window' must be numeric!")
  }

  if (any(window$BEFORE < 0 | any(window$AFTER < 0))) {
    stop("BEFORE and AFTER must be postivive numbers!")
  }

  # ensure EXCL, EXCL_REASON field are present
  if (!"EXCL" %in% names(obj)) {
    obj <- mutate(obj, EXCL = FALSE)
  }
  if (!"EXCL_REASON" %in% names(obj)) {
    obj <- mutate(obj, EXCL_REASON = "")
  }

  # rename columns to avoid any name collisions, delete any other columns
  temp <- window %>%
    select(NTIME, .BEFORE = .data$BEFORE, .AFTER = .data$AFTER)

  # ensure that window is in hours
  if (use_minutes == TRUE) {
    temp <- temp %>%
      mutate(across(c(".BEFORE", ".AFTER"), ~ .x / 60))
  }

  # flag time window violations
  out <- obj %>%
    add_time_deviation() %>%
    left_join(temp, by = "NTIME") %>%
    mutate(EXCL = case_when(
      # .data$ANALYTE == analyte &
      (.data$TIME_DEV < -1 * .data$.BEFORE | .data$TIME_DEV > .data$.AFTER) ~ TRUE,
      .default = .data$EXCL
    )) %>%
    mutate(EXCL_REASON = case_when(
      # .data$ANALYTE == analyte &
      (.data$TIME_DEV < -1 * .data$.BEFORE | .data$TIME_DEV > .data$.AFTER) ~ "time window violation",
      .default = .data$EXCL_REASON
    )) %>%
    select(-c(".BEFORE", ".AFTER"))

  return(out)
}

## to do
# simplify to only accept a data frame for window!
