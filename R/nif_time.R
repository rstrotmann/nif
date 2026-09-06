#' DTC of first administration by subject
#'
#' @param x A nif object.
#'
#' @return A data frame.
#' @noRd
first_admin_dtc <- function(x) {
  x |>
    filter(.data$EVID == 1) |>
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
  required_cols <- c("ID", "DTC", "ANALYTE", "PARENT", "EVID")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  if (nrow(obj) == 0) {
    return(
      mutate(
        obj,
        TIME = numeric(0),
        TAFD = numeric(0),
        TAD = numeric(0)
      ) |>
        dplyr::dplyr_reconstruct(obj)
    )
  }

  if (!is.POSIXct(obj$DTC)) {
    stop("DTC column must contain POSIXct datetime values")
  }

  obj |>
    mutate(.row = dplyr::row_number()) |>
    group_by(.data$ID) |>
    mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) |>
    ungroup() |>
    mutate(
      TIME = round(
        as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "hours")),
        digits = 3
      )
    ) |>
    group_by(.data$ID, .data$PARENT) |>
    mutate(
      FIRSTADMIN = if (any(.data$EVID == 1)) {
        min(.data$DTC[.data$EVID == 1], na.rm = TRUE)
      } else {
        NA_POSIXct_
      }
    ) |>
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
    arrange(.data$ID, .data$PARENT, .data$TIME, .data$EVID) |>
    mutate(
      admin_time = case_when(
        .data$EVID == 1 ~ .data$TIME,
        TRUE ~ NA_real_
      )
    ) |>
    tidyr::fill("admin_time", .direction = "downup") |>
    mutate(TAD = .data$TIME - .data$admin_time) |>
    ungroup() |>
    arrange(.data$.row) |>
    select(-c("FIRSTDTC", "FIRSTADMIN", "admin_time", ".row")) |>
    dplyr::dplyr_reconstruct(obj)
}


#' Calculate time fields based on TIME
#'
#' @param obj A nif object.
#'
#' @return A nif object.
#' @noRd
make_time_from_time <- function(obj) {
  # input validation
  validate_nif(obj)

  # Ensure PARENT column exists
  obj <- ensure_parent(obj)

  # Check for required columns
  required_cols <- c("ID", "TIME", "EVID")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Handle empty data frame
  if (nrow(obj) == 0) {
    return(
      mutate(
        obj,
        TAFD = numeric(0),
        TAD = numeric(0)
      ) |>
        dplyr::dplyr_reconstruct(obj)
    )
  }

  # Validate data types
  if (!is.numeric(obj$TIME)) {
    stop("TIME column must contain numeric values")
  }
  if (!is.numeric(obj$EVID)) {
    stop("EVID column must contain numeric values")
  }
  if (!is.numeric(obj$ID)) {
    stop("ID column must contain numeric values")
  }

  # Calculate time fields
  result <- obj |>
    mutate(.row = dplyr::row_number()) |>
    arrange(.data$ID, .data$PARENT, .data$TIME, -.data$EVID) |>
    group_by(.data$ID, .data$PARENT) |>
    mutate(.first_admin = if (any(.data$EVID == 1)) {
      min(.data$TIME[.data$EVID == 1], na.rm = TRUE)
    } else {
      NA_real_
    }) |>
    mutate(TAFD = ifelse(
      is.na(.data$.first_admin),
      NA_real_,
      round(.data$TIME - .data$.first_admin, digits = 3)
    )) |>
    mutate(.admin_time = case_when(
      .data$EVID == 1 ~ .data$TIME,
      TRUE ~ NA_real_
    )) |>
    tidyr::fill(".admin_time", .direction = "downup") |>
    mutate(TAD = .data$TIME - .data$.admin_time) |>
    ungroup() |>
    arrange(.data$.row) |>
    select(-c(".first_admin", ".admin_time", ".row")) |>
    dplyr::dplyr_reconstruct(obj)

  result
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
#' @noRd
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
    return(
      mutate(nif, TAD = numeric(0)) |>
        dplyr::dplyr_reconstruct(nif)
    )
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
    mutate(.row = dplyr::row_number()) |>
    arrange(.data$ID, .data$PARENT, .data$TIME, .data$EVID) |>
    mutate(admin_time = case_when(
      .data$EVID == 1 ~ .data$TIME,
      TRUE ~ NA_real_
    )) |>
    group_by(.data$ID, .data$PARENT) |>
    tidyr::fill("admin_time", .direction = "downup") |>
    mutate(TAD = .data$TIME - .data$admin_time) |>
    ungroup() |>
    arrange(.data$.row) |>
    select(-c("admin_time", ".row")) |>
    dplyr::dplyr_reconstruct(nif)

  result
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
    mutate(first_admin = safe_min(.data$TIME[.data$EVID == 1])) |>
    mutate(TAFD = .data$TIME - .data$first_admin) |>
    select(-c("first_admin")) |>
    ungroup() |>
    nif()
}


#' Add treatment day ('TRTDY') column
#'
#' @param obj The NIF object as data frame.
#' @return The updated NIF object.
#' @export
#' @examples
#' head(add_trtdy(examplinib_poc_nif))
add_trtdy <- function(obj) {
  validate_nif(obj)

  obj |>
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
    select(-c("FIRSTTRTDTC")) |>
    nif()
}

