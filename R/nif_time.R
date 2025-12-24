#' DTC of first administration by subject
#'
#' @param x A nif object.
#'
#' @return A data frame.
#' @noRd
first_admin_dtc <- function(x) {
  x |>
    assertr::verify(assertr::has_all_names("USUBJID", "DTC", "EVID")) |>
    assertr::verify(is.POSIXct(.data$DTC)) |>
    filter(EVID == 1) |>
    dplyr::group_by(.data$USUBJID) |>
    dplyr::mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    distinct(.data$USUBJID, .data$FIRSTDTC)
}


#' Extract last observation time from observation tibble
#'
#' @param obs tibble as created with make_obs
#' @return A datetime object representing the last recorded observation time
#' @import dplyr
#' @keywords internal
#' @noRd
last_obs_dtc <- function(obs) {
  max(obs$DTC, na.rm = TRUE)
}


#' Last administration DTC
#'
#' @param ex The EX domain as data table.
#' @return The last administration in DTC format.
#' @keywords internal
#' @noRd
last_ex_dtc <- function(ex) {
  max(ex$EXENDTC, na.rm = TRUE)
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
      obj |>
        mutate(
          TIME = numeric(0),
          TAFD = numeric(0),
          TAD = numeric(0)
        ) |>
        new_nif()
    )
  }

  # Validate DTC column
  if (!is.POSIXct(obj$DTC)) {
    stop("DTC column must contain POSIXct datetime values")
  }

  # Calculate reference time fields
  result <- obj |>
    as.data.frame() |>
    # Group by ID to find first time point for each subject
    group_by(.data$ID) |>
    mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) |>
    ungroup() |>
    # Group by ID and PARENT to find first administration for each drug
    group_by(.data$ID, .data$PARENT) |>
    mutate(
      # Find first administration time for TAFD
      FIRSTADMIN = if (any(.data$EVID == 1)) {
        min(.data$DTC[.data$EVID == 1], na.rm = TRUE)
      } else {
        NA_POSIXct_
      }
    ) |>
    ungroup()

  # Calculate TIME and TAFD
  result <- result |>
    # TIME: time since first record (in hours)
    mutate(
      TIME = round(
        as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "hours")),
        digits = 3
      )
    ) |>
    # TAFD: time since first administration of parent (in hours)
    mutate(
      TAFD = ifelse(
        is.na(.data$FIRSTADMIN),
        NA_real_,
        round(
          as.numeric(difftime(.data$DTC, .data$FIRSTADMIN, units = "hours")),
          digits = 3
        )
      )
    ) |>
    # Remove temporary columns
    select(-c("FIRSTDTC", "FIRSTADMIN")) |>
    # Convert to nif object before adding TAD
    new_nif()

  # Add the TAD field
  add_tad(result)
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

  obj |>
    ensure_parent() |>
    assertr::verify(assertr::has_all_names(
      "ID", "CMT", "EVID"
    )) |>
    group_by(.data$ID, .data$PARENT) |>
    mutate(.first_time = min(.data$TIME, na.rm = TRUE)) |>
    mutate(.first_admin = min(.data$TIME[.data$EVID == 1], na.rm = TRUE)) |>
    mutate(TAFD = round(.data$TIME - .data$.first_admin, digits = 3)) |>
    arrange(.data$ID, .data$TIME, -.data$EVID) |>
    mutate(.admin_time = case_when(.data$EVID == 1 ~ .data$TIME)) |>
    tidyr::fill(".admin_time", .direction = "down") |>
    mutate(TAD = .data$TIME - .data$.admin_time) |>
    ungroup() |>
    select(-c(".first_time", ".first_admin", ".admin_time")) |>
    new_nif()
}


#' Add time-after-dose (TAD) field
#'
#' This function adds a time-after-dose (TAD) field to a NIF object. TAD
#' represents the time elapsed since the most recent administration of the
#' parent compound. For observations before the first dose, TAD will be
#' negative.
#'
#' @param nif A NIF object.
#' @return A NIF object with an added TAD column.
#' @export
#' @examples
#' # Add TAD to a NIF object
#' add_tad(examplinib_poc_nif)
add_tad <- function(nif) {
  # Input validation
  validate_nif(nif)

  required_cols <- c("ID", "TIME", "EVID", "PARENT")
  missing_cols <- setdiff(required_cols, names(nif))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Handle empty data frame
  if (nrow(nif) == 0) {
    return(mutate(nif, TAD = numeric(0)))
  }

  # Validate data types
  if (!is.numeric(nif$TIME)) {
    stop("TIME column must contain numeric values")
  }
  if (!is.numeric(nif$EVID)) {
    stop("EVID column must contain numeric values")
  }
  if (!is.numeric(nif$ID)) {
    stop("ID column must contain numeric values")
  }

  # Calculate TAD
  result <- nif |>
    as.data.frame() |>
    # Ensure proper ordering for fill operations
    arrange(.data$ID, .data$PARENT, .data$TIME, -.data$EVID) |>
    # Create admin_time column for dosing events
    mutate(admin_time = case_when(
      .data$EVID == 1 ~ .data$TIME,
      TRUE ~ NA_real_
    )) |>
    # Group by subject and parent compound
    group_by(.data$ID, .data$PARENT) |>
    # Fill admin_time forward within each group
    tidyr::fill(admin_time, .direction = "downup") |>
    # Calculate TAD
    mutate(TAD = .data$TIME - .data$admin_time) |>
    # Remove temporary column
    select(-"admin_time") |>
    ungroup()

  # Return as NIF object
  new_nif(result)
}


#' Add time after first dose column
#'
#' @param nif A NIF object.
#' @return A NIF object.
#' @export
#' @keywords internal
#' @examples
#' add_tafd(examplinib_poc_nif)
add_tafd <- function(nif) {
  # Input validation
  if (!inherits(nif, "nif")) {
    stop("Input must be a NIF object")
  }

  required_cols <- c("ID", "TIME", "EVID")
  missing_cols <- setdiff(required_cols, names(nif))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (any(is.na(nif$ID))) {
    stop("ID colum must not contain NA values!")
  }

  # Handle empty data frame
  if (nrow(nif) == 0) {
    return(mutate(nif, TAFD = numeric(0)))
  }

  # Validate data types
  if (!is.numeric(nif$TIME)) {
    stop("TIME column must contain numeric values")
  }
  if (!is.numeric(nif$EVID)) {
    stop("EVID column must contain numeric values")
  }
  if (!is.numeric(nif$ID)) {
    stop("ID column must contain numeric values")
  }

  # Check for dosing events
  if (nrow(filter(nif, .data$EVID == 1)) == 0) {
    stop("No dosing event, TAFD cannot be calculated")
  }

  # Safely ensure parent exists
  tryCatch(
    {
      nif <- ensure_parent(nif)
    },
    error = function(e) {
      stop("Failed to ensure PARENT column: ", e$message)
    }
  )

  nif |>
    arrange(.data$ID, .data$PARENT, .data$TIME) |>
    group_by(.data$ID, .data$PARENT) |>
    mutate(first_admin = min(.data$TIME[.data$EVID == 1])) |>
    mutate(TAFD = .data$TIME - .data$first_admin) |>
    select(-c("first_admin")) |>
    ungroup() |>
    new_nif()
}


#' Add treatment day ('TRTDY') column
#'
#' @param obj The NIF object as data frame.
#' @return The updated NIF object as data frame.
#' @export
#' @examples
#' head(add_trtdy(examplinib_poc_nif))
add_trtdy <- function(obj) {
  obj |>
    assertr::verify(assertr::has_all_names("ID", "DTC", "EVID")) |>
    assertr::verify(is.POSIXct(.data$DTC)) |>
    dplyr::group_by(.data$ID) |>
    dplyr::mutate(FIRSTTRTDTC = min(.data$DTC[.data$EVID == 1],
      na.rm = TRUE
    )) |>
    dplyr::ungroup() |>
    mutate(TRTDY = interval(
      date(.data$FIRSTTRTDTC),
      date(.data$DTC)
    ) / days(1)) |>
    mutate(TRTDY = case_when(.data$TRTDY < 0 ~ .data$TRTDY,
      .default = .data$TRTDY + 1
    )) |>
    select(-FIRSTTRTDTC) |>
    new_nif()
}
