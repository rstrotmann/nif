#' Guess the most likely PCSPEC
#'
#' The PC specimen is selected based on the likelihood in the order of 'plasma'
#' < 'serum' < 'blood'.
#'
#' @param pc A data frame.
#' @param silent Suppress messages. Defaults to nif_options if NULL.
#'
#' @return The imputed spec as character.
#' @keywords internal
guess_pcspec <- function(pc, silent = NULL) {
  # Input validation
  if (!is.data.frame(pc) || !"PCSPEC" %in% names(pc)) {
    stop("Input must be a data frame with PCSPEC column")
  }

  pcspecs <- unique(pc$PCSPEC)
  if (length(pcspecs) == 0) {
    stop("No PCSPEC values found")
  }

  standard_specs <- c("PLASMA", "SERUM", "BLOOD")
  temp <- match(toupper(pcspecs), standard_specs)

  # Handle case where no matches found
  if (all(is.na(temp))) {
    warning("No standard specimen types found. Using first available specimen.")
    spec <- pcspecs[1]
  } else {
    spec <- pcspecs[order(temp)][1]
  }

  conditional_message(
    "Selected specimen type: ", spec, "\n",
    silent = silent)
  return(spec)
}


#' Guess the most likely LBSPEC
#'
#' @param lb The LB SDTM domain as data frame.
#' @return the imputed LBSPEC as character.
#' @export
#' @keywords internal
guess_lbspec <- function(lb) {
  lbspecs <- unique(lb$LBSPEC)
  standard_specs <- c("SERUM", "URINE")
  temp <- match(toupper(lbspecs), standard_specs)
  spec <- lbspecs[order(temp)][1]
  conditional_message("No specimen specified. Set to '", spec,
                      "' as the most likely.", "\n")
  return(spec)
}


#' Extract last observation time from observation tibble
#'
#' @param obs tibble as created with make_obs
#' @return A datetime object representing the last recorded observation time
#' @import dplyr
#' @keywords internal
last_obs_dtc <- function(obs) {
  return(max(obs$DTC, na.rm = TRUE))
}


#' Last administration DTC
#'
#' @param ex The EX domain as data table.
#' @return The last administration in DTC format.
#' @keywords internal
last_ex_dtc <- function(ex) {
  return(max(ex$EXENDTC, na.rm = TRUE))
}


#' Add TIME field to table
#'
#' TIME is created as the difference between the DTC field and the first DTC
#' field on the USUBJID level. TIME is in hours, rounded by 3 digits.
#' @param x The table as data frame.
#' @return A data frame with FIRSTDTC and TIME added.
#' @keywords internal
add_time <- function(x) {
  x %>%
    assertr::verify(assertr::has_all_names("USUBJID", "DTC")) %>%
    assertr::verify(is.POSIXct(.data$DTC)) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TIME = round(
      as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "h")),
      digits = 3
    ))
}


#' Calculate time fields based on DTC
#'
#' @description
#' This function generates the following time fields:
#'
#' 'TIME' is the time in hours relative to the subject's first record, be it an
#' administration or observation event.
#'
#' 'TAFD' is the time in hours relative to the subject's first administration of
#' the respective parent. Note that if a subject has received multiple drugs
#' (parents), the 'TAFD' field refers to the respective first administration.
#'
#' 'TAD' is the time in hours relative to the most recent administration of the
#' parent compound.
#'
#' @param obj A nif object.
#' @return A nif object with TIME, TAFD, and TAD fields added.
make_time <- function(obj) {
  # Input validation
  if (!inherits(obj, "nif")) {
    stop("Input must be a NIF object")
  }

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

  # Calculate time fields in a single pipeline for efficiency
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
make_time_from_TIME <- function(obj) {
  # as.data.frame(obj) %>%
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
    # tidyr::fill(.data$.admin_time, .direction = "down") %>%
    tidyr::fill(".admin_time", .direction = "down") %>%
    mutate(TAD = .data$TIME - .data$.admin_time) %>%
    ungroup() %>%
    select(-c(".first_time", ".first_admin", ".admin_time")) %>%
    new_nif()
}


#' Sort nif object and add REF field
#'
#' The input data format expected by NONMEM requires all rows ordered by ID and
#' TIME, and indexed sequentially on a subject level with a REF field.
#' Re-indexing may be required if a NIF object is extended, e.g., by merging in
#' further data.
#'
#' @param nif NIF object.
#' @return The updated NIF dataset including an updated REF field.
#' @import dplyr
#' @keywords internal
index_nif <- function(nif) {
  nif %>%
    as.data.frame() %>%
    dplyr::arrange(ID, TIME, -EVID) %>%
    dplyr::mutate(REF = row_number()) %>%
    dplyr::relocate(REF) %>%
    new_nif()
}


#' DTC of first administration by subject
#'
#' @param x A nif object.
#'
#' @return A data frame.
first_admin_dtc <- function(x) {
  x %>%
    assertr::verify(assertr::has_all_names("USUBJID", "DTC", "EVID")) %>%
    assertr::verify(is.POSIXct(.data$DTC)) %>%
    filter(EVID == 1) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    distinct(.data$USUBJID, .data$FIRSTDTC)
}


#' Subset nif to rows with DTC before the last individual or global observation
#'
#' @param obj A nif object.
#' @param individual Apply by ID, as logical.
#' @param keep_no_obs_sbs Retain subjects without observations.
#'
#' @return A nif object.
#' @export
limit <- function(obj, individual = TRUE, keep_no_obs_sbs = FALSE) {
  max_or_inf <- function(x) {
    if(length(x) == 0) return(max(obj$DTC, na.rm = TRUE))
    return(max(x, na.rm = TRUE))
  }

  if(keep_no_obs_sbs == FALSE) {
    obj <- obj %>%
      group_by(.data$ID) %>%
      filter(sum(EVID == 0) > 0) %>%
      ungroup()
  }
  if(individual == TRUE) {
    obj %>%
      group_by(.data$ID) %>%
      mutate(LAST_OBS_DTC = max_or_inf(.data$DTC[EVID == 0])) %>%
      ungroup() %>%
      filter(.data$DTC <= .data$LAST_OBS_DTC) %>%
      select(-c("LAST_OBS_DTC")) %>%
      new_nif()
  } else {
    last_obs_dtc <- max(obj$DTC[obj$EVID == 0])
    obj %>%
      filter(.data$DTC <= last_obs_dtc) %>%
      new_nif()
  }
}


#' Normalize nif object
#'
#' Order nif object, index and fill missing fields, and reduce to essential
#' columns.
#'
#' @param obj A nif object.
#' @param keep Fields to explicitly keep, as character.
#' @param cleanup Remove non-essential fields, as logical.
#' @return A nif object.
#' @export
normalize_nif <- function(obj, cleanup = TRUE, keep = NULL) {
  selector <- unique(c(
    "REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE", "HEIGHT", "WEIGHT",
    "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD", "PCELTM", "EVID", "AMT",
    "ANALYTE", "CMT",  "PARENT", "TRTDY", "METABOLITE", "DOSE", "DV", "MDV",
    "ACTARMCD", "IMPUTATION", "FOOD", "PART", "PERIOD", "COHORT", "FASTED",
    "RICH_N", "DI", "TREATMENT"))

  obj %>%
    mutate(ID = as.numeric(factor(.data$USUBJID, unique(.data$USUBJID)))) %>%
    make_time() %>%
    arrange(.data$DTC) %>%
    index_nif() %>%
    group_by(.data$ID, .data$PARENT) %>%
    tidyr::fill(any_of(c("DOSE", "EPOCH", "PART", "COHORT", "FOOD", "FASTED",
                         starts_with("BL_"))),
                .direction = "downup") %>%
    ungroup() %>%
    nif_cleanup(keep = keep) %>%
    new_nif()
}


#' Remove non-essential fields
#'
#' @param nif A nif object.
#' @param keep Fields to explicitly keep, as character.
#'
#' @return A nif object
nif_cleanup <- function(nif, keep = NULL) {
  selector <- unique(c("REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE",
    "HEIGHT", "WEIGHT", "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD",
    "PCELTM", "EVID", "AMT", "ANALYTE", "CMT",  "PARENT", "TRTDY",
    "METABOLITE", "DOSE", "DV", "MDV", "ACTARMCD", "IMPUTATION",
    "FOOD", "PART", "PERIOD", "COHORT", "FASTED", "RICH_N", "DI",
    "TREATMENT", keep))
  selector <- selector[selector %in% names(nif)]
  nif %>%
    select(all_of(selector), starts_with("BL_"))
}




