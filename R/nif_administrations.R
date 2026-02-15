#' Expand dates
#'
#' @param stdtc STDTC as character.
#' @param endtc ENDTC as character.
#' @param stdy STDY as numeric.
#' @param endy ENDY as numeric.
#'
#' @return Vector of lists: Dates and Days.
#' @noRd
date_list <- function(stdtc, endtc, stdy = NA, endy = NA) {
  tryCatch(
    {
      start_date <- as.Date(stdtc)
      end_date <- as.Date(endtc)
    },
    error = function(e) {
      warning("Failed to parse dates: ", stdtc, " to ", endtc)
    }
  )

  if (!is.na(end_date) && end_date < start_date) {
    stop(paste0(
      "End date before start date for: ", stdtc, " to ", endtc
    ))
  }

  if (!is.na(endy) && !is.na(stdy) && endy < stdy) {
    stop(paste0(
      "End day before start day for: ", stdy, " to ", endy
    ))
  }

  if (!is.na(endtc)) {
    dtc_list <- seq(start_date, end_date, by = "days")
  } else {
    dtc_list <- start_date
  }

  if (!is.na(stdy)) {
    if (!is.na(endy)) {
      dy_list <- seq(stdy, endy)
    } else {
      dy_list <- seq(stdy, stdy + length(dtc_list) - 1)
    }
  } else {
    dy_list <- rep(NA_integer_, length(dtc_list))
  }

  if (length(dtc_list) != length(dy_list)) {
    stop("DTC list and DY list have different lengths!")
  }

  c(list(dtc_list), list(dy_list))
}


#' Expand administration episodes
#'
#' @param ex The EX domain as data frame.
#'
#' @return A data frame.
#' @noRd
expand_ex <- function(ex) {
  # Input validation
  if (!is.data.frame(ex)) {
    stop("Input must be a data frame")
  }

  missing_ex_fields <- setdiff(
    c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"),
    names(ex)
  )

  if (length(missing_ex_fields) > 0) {
    stop(paste0(
      "Missing fields: ", nice_enumeration(missing_ex_fields), "!"
    ))
  }

  # Convert EXSTDY and EXENDY to numeric if they exist
  if ("EXSTDY" %in% names(ex)) {
    ex$EXSTDY <- as.numeric(ex$EXSTDY)
  }
  if ("EXENDY" %in% names(ex)) {
    ex$EXENDY <- as.numeric(ex$EXENDY)
  }

  # create IMPUTATION field if necessary
  if (!"IMPUTATION" %in% names(ex)) {
    ex <- mutate(ex, IMPUTATION = "")
  }

  # Expand days
  ex <- ex |>
    # expand dates
    # to do: implement dose frequencies other than QD (e.g., BID)
    decompose_dtc(c("EXSTDTC", "EXENDTC")) |>
    rowwise() |>
    mutate(DTC_date = date_list(.data$EXSTDTC_date, .data$EXENDTC_date)[1])

  # expand EXDY, if present
  if (all(c("EXSTDY", "EXENDY") %in% names(ex))) {
    ex <- ex |>
      mutate(EXDY = date_list(
        .data$EXSTDTC_date, .data$EXENDTC_date,
        .data$EXSTDY, .data$EXENDY
      )[2])
  }

  ex <- ex |>
    tidyr::unnest(any_of(c("DTC_date", "EXDY"))) |>
    mutate(DTC_time = NA) |>
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) |>
    mutate(IMPUTATION = "no time information") |>
    # basic assumption: in all treatment episodes, time is from EXSTDTC but for
    # the last day, where it is from EXENDTC. This can be overridden by the
    # ex_post_expansion imputation.
    derive_ex_dtc_time()

  return(ex)
}


#' Compile administration data frame
#'
#' @details
#' A discussion on EC vs EX is provided
#' [here](https://www.cdisc.org/kb/ecrf/exposure-collected)
#'
#' @details
#' # Time imputations and filtering
#'
#' The following time imputations and filters are applied in the given
#' order:
#'
#' ## 1. nif::impute_exendtc_to_rfendtc()
#'
#' If EXENDTC is missing in the last administration episode for a given subject,
#' it is replaced with DM.RFENDTC, if available.
#'
#' ## 2. nif::filter_EXENDTC_after_EXSTDTC()
#'
#' Administration episodes in which EXSTDTC is after EXENDT are deleted from the
#' data set.
#'
#' ## 3. nif::impute_exendtc_to_cutoff()
#'
#' If in the last administration episode per subject and treatment, EXENDTC is
#' missing, for example because the treatment is still ongoing at the time of
#' the SDTM generation, EXENDTC is replaced with the cut-off date.
#'
#' ## 4. nif::impute_missing_exendtc()
#'
#' If in any further episode, EXENDTC is missing, it is replaced with the day
#' before the subsequent administration episode start (EXSTDTC). It should be
#' understood that this reflects a rather strong assumption, i.e., that the
#' treatment was continued into the next administration episode. This imputation
#' therefore issues a warning that cannot be suppressed.
#'
#' ## 5. Expand administration episodes
#'
#' All administration episodes, i.e., the intervals between EXSTDTC and EXENDTC
#' for a given row in EX, are expanded into a sequence of rows with one
#' administration day per row. The administration times for all rows except for
#' the last are taken from the time information in EXSTDTD, whereas the time
#' for the last administration event in the respective episode is taken from the
#' time information in EXENDTC.
#'
#' **Development note:** In the present version of the function, once-daily (QD)
#' dosing is assumed. Multiple-daily dosings are not supported. In future
#' versions, the dosing frequency provided in `EXDOSFRQ` may be taken into
#' account to adequately handle multiple daily administrations.
#'
#' ## 6. nif::impute_admin_from_pcrftdtc()
#'
#' For administration days for which PK sampling events are recorded in PC, the
#' administration time is taken from PC.PCRFTDTC, if this field is available.
#'
#' **Development note:** This may be updated in future versions of the function
#' to work with multiple-daily administrations.
#'
#' ## 7. Carry forward time
#'
#' For all administration events per subject and treatment, missing time
#' information is finally carried forward from available time information.
#'
#' @param sdtm A sdtm object.
#' @param subject_filter The filtering to apply to the DM domain, as string,
#' @param extrt The EXTRT for the administration, as character.
#' @param analyte The name of the analyte as character.
#' @param cmt The compartment for the administration as numeric.
#' @param cut_off_date The data cut-off date as Posix date-time or character.
#' @param keep Columns to keep after cleanup, as character.
#' @param silent Suppress messages, defaults to nif_option standard, if NULL.
#' @param imputation The imputation rule set.
#'
#' @return A data frame.
#' @noRd
#' @import cli
#' @seealso [nif::add_administration()]
make_administration <- function(
  sdtm,
  extrt,
  analyte = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = "",
  imputation = imputation_standard,
  silent = NULL
) {
  # input validation
  validate_imputation_set(imputation)

  # extract domains
  dm <- domain(sdtm, "dm") |>
    lubrify_dates()
  ex <- domain(sdtm, "ex") |>
    lubrify_dates()
  vs <- NULL

  if (has_domain(sdtm, "vs")) {
    vs <- domain(sdtm, "vs")
  }

  # validate extrt
  if (!extrt %in% ex$EXTRT) {
    stop(paste0("Treatment '", extrt, "' not found in EXTRT!"))
  }

  # Impute very last EXENDTC for a subject and EXTRT to RFENDTC, if absent
  ex <- impute_exendtc_to_rfendtc(ex, dm, extrt, cut_off_date, silent = silent)

  if (is.null(analyte)) {
    analyte <- extrt
  }

  # generate data cut off date
  if (is.null(cut_off_date)) {
    cut_off_date <- last_ex_dtc(ex)

    conditional_cli({
      cli_alert_info(paste0(
        "A global cut-off-date of ",
        format(cut_off_date),
        " was automatically assigned!"
      ))
    },
    silent = silent)
  } else {
    if (!is.POSIXct(cut_off_date)) {
      cut_off_date <- as_datetime(cut_off_date, format = dtc_formats)
    }
  }

  # make subjects
  sbs <- make_subjects(dm, vs, subject_filter, keep)

  admin <- ex

  if ("EXSEQ" %in% names(ex)) {
    admin <- mutate(admin, SRC_SEQ = .data$EXSEQ)
  } else {
    admin <- mutate(admin, SRC_SEQ = NA)
  }

  # create imputation field
  if (!"IMPUTATION" %in% names(admin)) {
    admin <- mutate(admin, IMPUTATION = "")
  }

  admin <- admin |>
    mutate(SRC_DOMAIN = "EX") |>
    filter(.data$EXTRT == extrt) |>
    decompose_dtc("EXSTDTC")

  # apply data cut-off date
  cut_off_rows <- admin |>
    filter(.data$EXSTDTC > cut_off_date) |>
    select(any_of(c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC", "EXSEQ")))

  if (nrow(cut_off_rows) > 0) {
    conditional_cli({
      cli::cli_alert_warning("Cut off date applied!")
      cli::cli_text(paste0(
        nrow(cut_off_rows),
        " administrations episodes for ", extrt,
        " begin after the cut-off date (",
        format(cut_off_date), ") and were deleted from the data set:"
      ))
      cli::cli_verbatim(
        df_to_string(cut_off_rows, indent = 2)
      )
      cli::cli_text()
    },
    silent = silent)
  }

  admin <- admin |>
    filter(.data$EXSTDTC <= cut_off_date) |>
    # IMPUTATION 1: pre-expansion
    imputation[["admin_pre_expansion"]](
      sdtm,
      extrt,
      analyte,
      cut_off_date,
      silent = silent
    ) |>

    # make standard fields
    mutate(
      TIME = NA_integer_,
      NTIME = 0,
      ANALYTE = analyte,
      PARENT = analyte,
      METABOLITE = FALSE,
      DV = NA_real_,
      CMT = cmt,
      EVID = 1,
      MDV = 1,
      DOSE = .data$EXDOSE,
      AMT = .data$EXDOSE
    ) |>
    # expand administration episodes
    expand_ex() |>

    # IMPUTATION 2: post-expansion
    imputation[["admin_post_expansion"]](
      sdtm,
      extrt,
      analyte,
      cut_off_date,
      silent = silent
    ) |>

    inner_join(sbs, by = "USUBJID") |>
    group_by(.data$USUBJID) |>
    mutate(TRTDY = as.numeric(
      ## changed from RFXSTDTC to RFSTDTC. The difference between both dates is
      ## that RFXSTDTC includes any exposure captured in the EX domain, whereas
      ## RFSTDTC refers to the first exposure to study treatment.
      ## Reference: https://www.lexjansen.com/phuse-us/2020/ds/DS07.pdf
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days"
    ) + 1) |>
    ungroup() |>
    index_id() |>
    nif()

  return(admin)
}


#' Add administration events
#'
#' Add rows to a [nif::nif] object that represent drug administration events
#' (EVID of 1) This is usually the first step in the stepwise creation of NIF
#' data tables.
#'
#' Drug administration data is taken from the EX domain of the source
#' [nif::sdtm] object. The `extrt` argument specifies the drug name as
#' represented in EX. By default, this will also be the value of the 'ANALYTE'
#' column for the respective rows in the resulting [nif::nif] object.
#' Alternatively, a custom `analyte` name can be explicitly provided, e.g., to
#' match with the 'ANALYTE' name of the corresponding pharmacokinetic
#' observations.
#'
#' For administrations, a model compartment of 1 is selected by default and will
#' be the corresponding value of the 'CMT' column. A different compartment can
#' be explicitly specified by the `cmt` argument.
#'
#' For an overview on the representation of administration events in NONMEM
#' Input Format compliant data sets, see: Bauer, R.J. (2019), NONMEM Tutorial
#' Part I: Description of Commands and Options, With Simple Examples of
#' Population Analysis. CPT Pharmacometrics Syst. Pharmacol., 8: 525-537
#' \doi{10.1002/psp4.12404}.
#'
#' To add observation events to the [nif::nif] object, see
#' [nif::add_observation()].
#'
#' @param nif A nif object.
#' @param sdtm A sdtm object.
#' @param subject_filter The filtering to apply to the DM domain, as string,
#' @param extrt The EXTRT for the administration, as character.
#' @param analyte The name of the analyte as character.
#' @param cmt The compartment for the administration as numeric.
#' @param cut_off_date The data cut-off date as Posix date-time or character.
#' @param keep Columns to keep after cleanup, as character.
#' @param silent Suppress messages, defaults to nif_option standard, if NULL.
#' @param debug Include debug fields, as logical.
#' @param imputation The imputation rule set.
#' @return A nif object.
#' @export
#' @examples
#' add_administration(nif(), examplinib_sad, "EXAMPLINIB") |>
#' head()
#'
add_administration <- function(
  nif,
  sdtm,
  extrt,
  analyte = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = NULL,
  debug = FALSE,
  imputation = imputation_standard,
  silent = NULL
) {
  # validate input
  validate_min_nif(nif)
  validate_sdtm(sdtm, c("dm", "ex"))
  validate_char_param(extrt, "extrt")
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_numeric_param(cmt, "cmt")
  validate_char_param(subject_filter, "subject_filter")
  validate_char_param(cut_off_date, "cut_off_date", allow_null = TRUE)
  validate_char_param(keep, "keep", allow_null = TRUE, allow_multiple = TRUE)
  validate_logical_param(debug, "debug")
  validate_logical_param(silent, "silent", allow_null = TRUE)

  validate_imputation_set(imputation)

  conditional_cli(
    cli_alert_info(paste0(
      "Imputation model '", deparse(substitute(imputation)),
      "' applied to administration of ", extrt)),
    silent = silent
  )

  debug <- isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if (isTRUE(debug)) keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  bind_rows(
    nif,
    make_administration(
      sdtm, extrt, analyte, cmt, subject_filter, cut_off_date, keep,
      imputation = imputation, silent = silent
    )
  ) |>
    normalize_nif(keep = keep)
}
