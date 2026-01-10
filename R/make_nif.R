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
#' @noRd
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
    silent = silent
  )
  spec
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
  conditional_message(
    "No specimen specified. Set to '", spec,
    "' as the most likely.", "\n"
  )
  spec
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
  # Input validation
  validate_nif(obj)
  validate_logical_param(individual, "individual")
  validate_logical_param(keep_no_obs_sbs, "keep_no_obs_sbs")

  # Check for required fields
  required_fields <- c("DTC", "ID", "EVID")
  missing_fields <- setdiff(required_fields, names(obj))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Missing required fields in nif object: ",
      nice_enumeration(missing_fields)
    ))
  }

  max_dtc <- max(obj$DTC, na.rm = TRUE)

  max_or_inf <- function(x) {
    if (length(x) == 0) {
      return(max_dtc)
    }
    max(x, na.rm = TRUE)
  }

  if (keep_no_obs_sbs == FALSE) {
    obj <- obj |>
      group_by(.data$ID) |>
      filter(sum(.data$EVID == 0) > 0) |>
      ungroup()
  }

  if (individual == TRUE) {
    obj |>
      group_by(.data$ID) |>
      mutate(LAST_OBS_DTC = max_or_inf(.data$DTC[.data$EVID == 0])) |>
      ungroup() |>
      filter(.data$DTC <= .data$LAST_OBS_DTC) |>
      select(-c("LAST_OBS_DTC")) |>
      nif()
  } else {
    last_obs_dtc <- max(obj$DTC[obj$EVID == 0], na.rm = TRUE)
    obj |>
      filter(.data$DTC <= last_obs_dtc) |>
      nif()
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
#' @noRd
normalize_nif <- function(obj, cleanup = TRUE, keep = NULL) {
  # input validation
  validate_nif(obj)
  validate_logical_param(cleanup)
  validate_char_param(keep, "keep", allow_null = TRUE, allow_multiple = TRUE)

  out <- obj |>
    index_id() |>
    make_time() |>
    arrange(.data$DTC) |>
    group_by(.data$ID, .data$PARENT) |>
    tidyr::fill(
      any_of(
        c(
          "STUDYID", "AGE", "SEX", "RACE", "ETHNIC", "COUNTRY",
          "HEIGHT", "WEIGHT", "BMI", "ACTARMCD", "ARM", "PART", "COHORT",
          "DOSE", "EPOCH", "FASTED", "FOOD"
        )
      ),
      .direction = "downup"
    ) |>
    fill(any_of(c(starts_with("BL_"))), .direction = "downup") |>
    ungroup() |>
    nif_cleanup(keep = keep) |>
    arrange_and_add_ref()

  class(out) <- c("nif", "data.frame")
  order_nif_columns(out)
}


#' Remove non-essential fields
#'
#' @param nif A nif object.
#' @param keep Fields to explicitly keep, as character.
#'
#' @return A nif object
#' @noRd
nif_cleanup <- function(nif, keep = NULL) {
  selector <- unique(c(
    "REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE",
    "HEIGHT", "WEIGHT", "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD",
    "PCELTM", "EVID", "AMT", "ANALYTE", "CMT", "PARENT", "TRTDY",
    "METABOLITE", "DOSE", "DV", "MDV", "ACTARMCD", "IMPUTATION",
    "FOOD", "PART", "PERIOD", "COHORT", "FASTED", "RICH_N", "DI",
    "TREATMENT", keep
  ))
  selector <- selector[selector %in% names(nif)]
  nif |>
    select(all_of(selector), starts_with("BL_"))
}
