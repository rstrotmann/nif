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


# ---- time calculation ----
#
# All derived time fields are calculated in one place:
#
# * `calc_tafd()` and `calc_tad()` are vector functions for the records of one
#   subject and one parent. They do not depend on the order of the records.
# * `add_time_fields()` is the only function that applies them, grouped by ID
#   and PARENT.
# * `add_time_from_dtc()` derives TIME from DTC.
# * `make_time()`, `make_time_from_time()`, `add_tad()` and `add_tafd()` only
#   validate their input and call these functions.
#
# Convention: an observation at the same time as an administration is a
# trough, i.e., it relates to the previous administration.


#' Time after first dose
#'
#' Calculates the time after the first administration for all records of one
#' subject and one parent. Row order does not matter.
#'
#' @param time Event times in hours, as numeric.
#' @param evid Event IDs, as numeric. Only `EVID == 1` is a dose.
#'
#' @returns The TAFD as numeric vector of the same length as `time`, rounded to
#'   three digits. NA if there is no administration.
#' @noRd
calc_tafd <- function(time, evid) {
  is_dose <- !is.na(evid) & evid == 1 & !is.na(time)

  if (!any(is_dose)) {
    return(rep(NA_real_, length(time)))
  }

  round(time - min(time[is_dose]), digits = 3)
}


#' Time after dose, independent of row order
#'
#' Calculates the time after the most recent administration for all records of
#' one subject and one parent. Row order does not matter.
#'
#' A sample taken at the same time as a dose is considered a trough, i.e., it
#' is related to the previous dose, not to the dose given at that time.
#' Administration records have TAD 0. Records without an earlier dose (before or
#' at the first dose) are related to the first dose, so TAD is negative before
#' and 0 at the first dose. Without any administration, TAD is NA.
#'
#' The function does not know about subjects or parents. Call it grouped, as
#' done in `add_time_fields()`.
#'
#' @param time Event times in hours, as numeric.
#' @param evid Event IDs, as numeric. Only `EVID == 1` is a dose.
#'
#' @returns The TAD as numeric vector of the same length as `time`.
#' @noRd
calc_tad <- function(time, evid) {
  is_dose <- !is.na(evid) & evid == 1
  dose_times <- sort(unique(time[is_dose & !is.na(time)]))

  if (length(dose_times) == 0) {
    return(rep(NA_real_, length(time)))
  }

  # number of dose times strictly before each record, i.e., coincident doses do
  # not count
  n_before <- findInterval(time, dose_times, left.open = TRUE)
  reference <- dose_times[pmax(n_before, 1L)]

  ifelse(is_dose & !is.na(time), 0, time - reference)
}


#' Add time fields relative to administrations
#'
#' The one place where TAFD and TAD are calculated. Fields are calculated
#' separately for every combination of ID and PARENT, independent of the order
#' of the records. The row order is not changed.
#'
#' The function does not validate its input. It requires ID, PARENT, TIME and
#' EVID.
#'
#' @param obj A nif object with ID, PARENT, TIME and EVID.
#' @param fields The fields to calculate, any of "TAFD" and "TAD".
#'
#' @returns A nif object with the requested fields added or replaced.
#' @noRd
add_time_fields <- function(obj, fields = c("TAFD", "TAD")) {
  fields <- match.arg(fields, c("TAFD", "TAD"), several.ok = TRUE)

  out <- group_by(obj, .data$ID, .data$PARENT)
  if ("TAFD" %in% fields) {
    out <- mutate(out, TAFD = calc_tafd(.data$TIME, .data$EVID))
  }
  if ("TAD" %in% fields) {
    out <- mutate(out, TAD = calc_tad(.data$TIME, .data$EVID))
  }

  out |>
    ungroup() |>
    dplyr::dplyr_reconstruct(obj)
}


#' Add TIME from DTC
#'
#' TIME is the time in hours relative to the subject's first record, rounded to
#' three digits.
#'
#' @param obj A nif object with ID and DTC as POSIXct.
#'
#' @returns A nif object with TIME added or replaced.
#' @noRd
add_time_from_dtc <- function(obj) {
  obj |>
    group_by(.data$ID) |>
    mutate(
      TIME = round(
        as.numeric(
          difftime(.data$DTC, min(.data$DTC, na.rm = TRUE), units = "hours")
        ),
        digits = 3
      )
    ) |>
    ungroup() |>
    dplyr::dplyr_reconstruct(obj)
}


#' Stop if columns are missing
#'
#' @param obj A data frame.
#' @param required Required column names.
#'
#' @returns Nothing or stop.
#' @noRd
check_time_columns <- function(obj, required) {
  missing_cols <- setdiff(required, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
}


#' Stop if columns are not numeric
#'
#' @param obj A data frame.
#' @param cols Column names, as character.
#'
#' @returns Nothing or stop.
#' @noRd
check_numeric_columns <- function(obj, cols = c("TIME", "EVID", "ID")) {
  for (col in cols) {
    if (!is.numeric(obj[[col]])) {
      stop(col, " column must contain numeric values")
    }
  }
}


#' Add empty time fields to an empty nif object
#'
#' @param obj An empty nif object.
#' @param fields The fields to add, as character.
#'
#' @returns The nif object with empty numeric fields.
#' @noRd
add_empty_time_fields <- function(obj, fields) {
  for (field in fields) {
    obj[[field]] <- numeric(0)
  }
  dplyr::dplyr_reconstruct(obj, obj)
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
#' the parent compound. An observation at the same time as an administration
#' is considered a trough, i.e., it relates to the previous administration. The
#' result does not depend on the order of the records. In the nif object, such
#' a sample is also placed before the dose (see [nif::nif()]), so that row
#' order and 'TAD' agree.
#'
#' @param obj A nif object.
#' @return A nif object with TIME, TAFD, and TAD fields added.
#' @export
make_time <- function(obj) {
  check_time_columns(obj, c("ID", "DTC", "ANALYTE", "PARENT", "EVID"))

  if (nrow(obj) == 0) {
    return(add_empty_time_fields(obj, c("TIME", "TAFD", "TAD")))
  }

  if (!is.POSIXct(obj$DTC)) {
    stop("DTC column must contain POSIXct datetime values")
  }

  obj |>
    add_time_from_dtc() |>
    add_time_fields(c("TAFD", "TAD"))
}


#' Calculate time fields based on TIME
#'
#' @param obj A nif object.
#'
#' @return A nif object.
#' @noRd
make_time_from_time <- function(obj) {
  validate_nif_argument(obj)
  obj <- ensure_parent(obj)

  check_time_columns(obj, c("ID", "TIME", "EVID"))

  if (nrow(obj) == 0) {
    return(add_empty_time_fields(obj, c("TAFD", "TAD")))
  }

  check_numeric_columns(obj)

  add_time_fields(obj, c("TAFD", "TAD"))
}


#' Add time-after-dose (TAD) field
#'
#' This function adds a time-after-dose (TAD) field to a NIF object. TAD
#' represents the time elapsed since the most recent administration of the
#' parent compound. For observations before the first dose, TAD will be
#' negative. An observation at the same time as an administration is
#' considered a trough, i.e., it relates to the previous administration.
#'
#' @param nif A NIF object.
#' @return A NIF object with an added TAD column.
#' @noRd
#' @examples
#' # Add TAD to a NIF object
#' add_tad(examplinib_poc_nif)
add_tad <- function(nif) {
  validate_nif_argument(nif)
  check_time_columns(nif, c("ID", "TIME", "EVID", "PARENT"))

  if (nrow(nif) == 0) {
    return(add_empty_time_fields(nif, "TAD"))
  }

  check_numeric_columns(nif)

  add_time_fields(nif, "TAD")
}


#' Add time after first dose column
#'
#' @param nif A NIF object.
#' @return A NIF object.
#' @noRd
#' @examples
#' add_tafd(examplinib_poc_nif)
add_tafd <- function(nif) {
  if (!inherits(nif, "nif")) {
    stop("Input must be a NIF object")
  }

  check_time_columns(nif, c("ID", "TIME", "EVID"))

  if (any(is.na(nif$ID))) {
    stop("ID colum must not contain NA values!")
  }

  if (nrow(nif) == 0) {
    return(add_empty_time_fields(nif, "TAFD"))
  }

  check_numeric_columns(nif)

  if (nrow(filter(nif, .data$EVID == 1)) == 0) {
    stop("No dosing event, TAFD cannot be calculated")
  }

  tryCatch(
    {
      out <- ensure_parent(nif)
    },
    error = function(e) {
      stop("Failed to ensure PARENT column: ", e$message)
    }
  )

  add_time_fields(out, "TAFD")
}


#' Add treatment day ('TRTDY') column
#'
#' @param obj The NIF object as data frame.
#' @return The updated NIF object.
#' @export
#' @examples
#' head(add_trtdy(examplinib_poc_nif))
add_trtdy <- function(obj) {
  validate_nif_argument(obj)

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
    # nif()
    arrange_and_add_ref() |>
    restore_nif(obj)
}
