#' Extract last observation time from observation tibble
#'
#' @param obs tibble as created with make_obs
#' @return A datetime object representing the last recorded observation time
#' @import dplyr
#' @keywords internal
#' @noRd
last_obs_dtc <- function(obs) {
  return(max(obs$DTC, na.rm = TRUE))
}


#' Last administration DTC
#'
#' @param ex The EX domain as data table.
#' @return The last administration in DTC format.
#' @keywords internal
#' @noRd
last_ex_dtc <- function(ex) {
  return(max(ex$EXENDTC, na.rm = TRUE))
}


#' Calculate time fields based on DTC
#'
#' @description
#' This function generates the following time fields:
#'
#' * 'TIME' is the time in hours relative to the subject's first record, be it
#' an administration or observation event.
#'
#' * 'TAFD' is the time in hours relative to the subject's first administration
#' of the respective parent. Note that if a subject has received multiple drugs
#' (parents), the 'TAFD' field refers to the respective first administration.
#'
#' * 'TAD' is the time in hours relative to the most recent administration of
#' the parent compound.
#'
#' @param obj A nif object.
#' @return A nif object with TIME, TAFD, and TAD fields added.
#' @export
make_time <- function(obj) {
  # Input validation
  validate_nif(obj)

  required_cols <- c("ID", "DTC", "ANALYTE", "PARENT", "EVID")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Handle empty data frame
  if (nrow(obj) == 0) {
    return(
      obj %>%
        mutate(
          TIME = numeric(0),
          TAFD = numeric(0),
          TAD = numeric(0)) %>%
        new_nif())
  }

  # Validate DTC column
  if (!is.POSIXct(obj$DTC)) {
    stop("DTC column must contain POSIXct datetime values")
  }

  # Calculate time fields
  result <- obj %>%
    # Group by ID to find first time point for each subject
    group_by(.data$ID) %>%
    mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
    ungroup() %>%
    # Group by ID and PARENT to find first administration for each drug
    group_by(.data$ID, .data$PARENT) %>%
    mutate(
      # Find first administration time for TAFD
      FIRSTADMIN = if (any(.data$EVID == 1)) {
        min(.data$DTC[.data$EVID == 1], na.rm = TRUE)
      } else {
        NA_POSIXct_
      }
    ) %>%
    ungroup() %>%
    # Calculate TIME and TAFD
    mutate(
      # TIME: time since first record (in hours)
      TIME = round(
        as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "hours")),
        digits = 3
      ),
      # TAFD: time since first administration of parent (in hours)
      TAFD = ifelse(
        is.na(.data$FIRSTADMIN),
        NA_real_,
        round(
          as.numeric(difftime(.data$DTC, .data$FIRSTADMIN, units = "hours")),
          digits = 3
        )
      )
    ) %>%
    # Remove temporary columns
    select(-c("FIRSTDTC", "FIRSTADMIN")) %>%
    # Convert to nif object before adding TAD
    new_nif()

  # Add the TAD field
  result <- add_tad(result)

  return(result)
}


#' Calculate time fields based on TIME
#'
#' @param obj A nif object.
#'
#' @return A nif object.
#' @noRd
make_time_from_TIME <- function(obj) {
  # input validation
  validate_nif(obj)

  obj %>%
    ensure_parent() %>%
    assertr::verify(assertr::has_all_names(
      "ID", "CMT", "EVID")) %>%
    group_by(.data$ID, .data$PARENT) %>%
    mutate(.first_time = min(.data$TIME, na.rm = TRUE)) %>%
    mutate(.first_admin = min(.data$TIME[.data$EVID == 1], na.rm = TRUE)) %>%
    mutate(TAFD = round(.data$TIME - .data$.first_admin, digits = 3)) %>%
    arrange(.data$ID, .data$TIME, -.data$EVID) %>%
    mutate(.admin_time = case_when(.data$EVID == 1 ~ .data$TIME)) %>%
    tidyr::fill(".admin_time", .direction = "down") %>%
    mutate(TAD = .data$TIME - .data$.admin_time) %>%
    ungroup() %>%
    select(-c(".first_time", ".first_admin", ".admin_time")) %>%
    new_nif()
}
