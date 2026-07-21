#' Expand administration episodes
#'
#' Administration episodes are expanded between EXSTDTC and EXENDTC (and between
#' EXSTDY and EXENDY, if present). The resulting expanded date field is DTC_date.
#' In addition, DTC_time is taken from the time component of EXSTDTC and EXENDTC
#' for the episode start and end times, if available in EXSTDTC and EXENDTC.
#' For the other days, DTC_time is set to NA.
#'
#' The resulting data frame maintains the decomposed XX_date and XX_time fields,
#' and the generated DTC_data and DTC_time fields are not re-composed into the
#' DTC field. This is accomplished in `make_administration()`,
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
    names(ex))
  if (length(missing_ex_fields) > 0)
    stop(paste0(
      "Missing fields: ", nice_enumeration(missing_ex_fields), "!"))

  # prepare fields
  ex <- lubrify_dates(ex)

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

  has_study_days <- all(c("EXSTDY", "EXENDY") %in% names(ex))

  # Expand days (assuming QD administration)
  ex <- ex |>
    decompose_dtc(c("EXSTDTC", "EXENDTC")) |>
    mutate(
      .start_date = as.Date(.data$EXSTDTC_date),
      .end_date = if_else(
        is.na(.data$EXENDTC_date),
        as.Date(.data$EXSTDTC_date),
        as.Date(.data$EXENDTC_date)
      ),
      .n_days = as.integer(.data$.end_date - .data$.start_date) + 1L
    )

  # validation (vectorized)
  bad_dates <- which(ex$.n_days < 1L)
  if (length(bad_dates) > 0) {
    stop("End date before start date for row(s): ",
         paste(bad_dates, collapse = ", "))
  }

  if (has_study_days) {
    bad_days <- which(ex$EXENDY < ex$EXSTDY)
    if (length(bad_days) > 0) {
      stop("End day before start day for row(s): ",
           paste(bad_days, collapse = ", "))
    }
  }

  ex <- ex |>
    tidyr::uncount(.data$.n_days, .remove = FALSE) |>
    group_by(.data$USUBJID, .data$EXTRT, .data$EXSTDTC_date, .data$EXENDTC_date) |>
    mutate(
      .offset = row_number() - 1L,
      DTC_date = as.character(.data$.start_date + .data$.offset)
    )

  if (has_study_days) {
    ex <- mutate(ex, EXDY = .data$EXSTDY + .data$.offset)
  }

  ex <- ex |>
    select(-c(".start_date", ".end_date", ".n_days", ".offset")) |>
    ungroup()


  # unnest and annotate administrations
  ex |>
    # tidyr::unnest(any_of(c("DTC_date", "EXDY"))) |>
    group_by(.data$USUBJID, .data$EXTRT, .data$EXSTDTC_date, .data$EXENDTC_date) |>

    # make DTC_time field
    mutate(DTC_time = case_when(
      # first line
      row_number() == 1 & !is.na(.data$EXSTDTC_time) ~ .data$EXSTDTC_time,
      # last line
      row_number() == n() & !is.na(.data$EXENDTC_time) ~ .data$EXENDTC_time,
      # default
      .default = NA
    )) |>

    # make IMPUTATION field
    mutate(.expand_imp = case_when(
      row_number() == 1 & !is.na(.data$EXSTDTC_time) ~ "time copied from EXSTDTC",
      row_number() == n() & !is.na(.data$EXENDTC_time) ~ "time copied from EXENDTC",
      .default = ""
    )) |>
    mutate(IMPUTATION = trimws(
      paste(
      .data$IMPUTATION,
      .data$.expand_imp,
      sep = "; "),
      whitespace = "[; ]+")) |>
    select(-".expand_imp") |>

    ungroup()
}


#' Check and prepare ex for iv administrations
#'
#' @param admin The un-expanded EX domain
#' @param iv_admin Administration is iv, as boolean. If NULL, will be tested
#' automatically based on the EXROUTE field, if available.
#' @param duration The duration of the iv administration as numeric. If NULL,
#' will be determined automatically based on the EXDUR field, if available.
#' @param silent Suppress messages.
#'
#' @returns The updated EX domain.
create_iv_fields <- function(
    admin, iv_admin = NULL, duration = NULL, silent = NULL
  ) {
  # input validation
  if (!is.data.frame(admin)) stop("Input must be a data frame!")
  validate_argument(iv_admin, "logical", allow_null = TRUE)
  validate_argument(duration, "numeric", allow_null = TRUE)
  validate_argument(silent, "logical", allow_null = TRUE)


  # check whether iv administration
  if (is.null(iv_admin)) {
    if ("EXROUTE" %in% names(admin)) {
      if (any(toupper(admin$EXROUTE) %in% c("IV", "INTRAVENOUS"))) {
        iv_admin <- TRUE
        conditional_cli(
          cli_alert_info("IV administration detected"),
          silent = silent
        )
      }
    }
  }

  # prepare fields for iv administrations
  if (isTRUE(iv_admin)) {
    # create duration field (DUR)
    if ("EXDUR" %in% names(admin)) {
      if (any(is_iso8601_pt(admin$EXDUR) == FALSE, na.rm = TRUE)) {
        stop("EXDUR must be an ISO8601-formatted duration!")
      }
      if (is.null(duration)) {
        admin <- mutate(admin, DUR = pt_to_hours(EXDUR))
      } else {
        admin <- mutate(admin, DUR = duration)
        conditional_cli(cli_alert(paste0(
          "Treatment duration (EXDUR) was replaced with custom duration (",
          duration, ")"
        )),
        silent)
      }
    } else {
      if (is.null(duration)) {
        admin <- mutate(admin, DUR = 0)
        conditional_cli(cli_alert_warning(
          "No EXDUR in data set and no duration specified. DUR will be 0!"
          ),
          silent
        )
      } else {
        admin <- mutate(admin, DUR = duration)
      }
    }
  }

  return(admin)
}


#' Compile administration data frame
#'
#' @details
#' A discussion on EC vs EX is provided
#' [here](https://www.cdisc.org/kb/ecrf/exposure-collected).
#'
#' # Administration pipeline
#'
#' `make_administration()` builds dosing rows from EX in this order:
#'
#' 1. **RFENDTC imputation (always)** - If the last episode per subject and
#'    `extrt` has missing `EXENDTC`, fill from `DM.RFENDTC` when available.
#'    This step is hardcoded and not controlled by `imputation`.
#'
#' 2. **Cut-off date** - Use `cut_off_date`, or if `NULL`, derive one from EX
#'    via `last_ex_dtc()`.
#'
#' 3. **Subjects** - Build the subject table from DM (and VS if present) using
#'    `subject_filter` and `keep`.
#'
#' 4. **Pre-expansion rules** - If `imputation` has `admin_pre_expansion`,
#'    call it (e.g. cut-off filter, EXENDTC imputations, invalid-episode
#'    filter). See the chosen rule set for exact steps and order.
#'
#' 5. **Expand episodes** - Expand each EX row between `EXSTDTC` and `EXENDTC`
#'    to one row per day (QD only; `EXDOSFRQ` is not used). Start/end times
#'    come from `EXSTDTC` / `EXENDTC` when present; other days are `NA`.
#'
#' 6. **Post-expansion rules** - If `imputation` has `admin_post_expansion`,
#'    call it (e.g. NTIME back-calculation, PCRFTDTC). Again, details depend
#'    on the rule set.
#'
#' 7. **Carry forward** - Always fill missing `DTC_time` forward within
#'    subject and treatment, then compose `DTC`, join subjects, set `TRTDY`,
#'    and return a nif object.
#'
#' Default `imputation` is [nif::imputation_rules_standard()]. Use
#' [nif::imputation_rules_void()] or a custom list to change or skip rule slots.
#' RFENDTC imputation and carry-forward still run when rules are empty.
#'
#' @param sdtm A sdtm object.
#' @param subject_filter The filtering to apply to the DM domain, as string,
#' @param extrt The EXTRT for the administration, as character.
#' @param analyte The name of the analyte as character.
#' @param pctestcd The PCTESTCD of the pharmacokinetic analyte corresponding to
#' the administered drug. This is needed when administration times are imputed
#' from the PCRFTDTC field from the PC domain.
#' @param cmt The compartment for the administration as numeric.
#' @param cut_off_date The data cut-off date as Posix date-time or character.
#' @param keep Columns to keep after cleanup, as character.
#' @param silent Suppress messages, defaults to nif_option standard, if NULL.
#' @param imputation The imputation rule set.
#' @inheritParams create_iv_fields iv_admin duration
#'
#' @return A data frame.
#' @noRd
#' @import cli
#' @seealso [nif::add_administration()]
make_administration <- function(
  sdtm,
  extrt,
  analyte = NULL,
  pctestcd = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = "",
  imputation = imputation_rules_standard,
  duration = NULL,
  iv_admin = NULL,
  silent = NULL
) {
  # input validation
  if (!is.list(imputation))
    stop("imputation must be a list!")

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

  # # create iv fields, if needed
  # admin <- create_iv_fields(admin, iv_admin = iv_admin, duration = duration)


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

  # create iv fields, if needed
  admin <- create_iv_fields(admin, iv_admin = iv_admin, duration = duration)

  # apply data cut-off date
  # cut_off_rows <- admin |>
  #   filter(.data$EXSTDTC > cut_off_date) |>
  #   select(any_of(c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC", "EXSEQ")))
  #
  # if (nrow(cut_off_rows) > 0) {
  #   conditional_cli({
  #     cli::cli_alert_warning("Cut off date applied!")
  #     cli::cli_text(paste0(
  #       nrow(cut_off_rows),
  #       " administrations episodes for ", extrt,
  #       " begin after the cut-off date (",
  #       format(cut_off_date), ") and were deleted from the data set:"
  #     ))
  #     cli::cli_verbatim(
  #       df_to_string(cut_off_rows, indent = 2)
  #     )
  #     cli::cli_text()
  #   },
  #   silent = silent)
  # }

  # ensure analyte
  if (is.null(analyte)) {
    analyte <- extrt
  }

  # IMPUTATION 1: pre-expansion
  if ("admin_pre_expansion" %in% names(imputation)) {
    admin <- admin |>
      imputation[["admin_pre_expansion"]](
        sdtm,
        extrt,
        analyte,
        pctestcd,
        cut_off_date,
        silent = silent
      )
  }

  admin <- admin |>
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
    expand_ex()

  # IMPUTATION 2: post-expansion
  if ("admin_post_expansion" %in% names(imputation)) {
    admin <- admin |>
      imputation[["admin_post_expansion"]](
        sdtm,
        extrt,
        analyte,
        pctestcd,
        cut_off_date,
        silent = silent
      )
  }

  admin <- admin |>
    carry_forward_admin_time_imputations() |>
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) |>
    inner_join(sbs, by = "USUBJID") |>
    group_by(.data$USUBJID)

  if ("RFSTDTC" %in% names(admin)) {
    admin <- admin |>
      mutate(TRTDY = as.numeric(
        ## changed from RFXSTDTC to RFSTDTC. The difference between both dates is
        ## that RFXSTDTC includes any exposure captured in the EX domain, whereas
        ## RFSTDTC refers to the first exposure to study treatment.
        ## Reference: https://www.lexjansen.com/phuse-us/2020/ds/DS07.pdf
        difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
        units = "days"
      ) + 1)
  }

  admin |>
    ungroup() |>
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) |>
    select(-any_of(c("DTC_date", "DTC_time", "EXSTDTC_date", "EXSTDTC_time",
                     "EXENDTC_date", "EXENDTC_time", "TEST"))) |>
    index_id() |>
    nif()
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
#' @param pctestcd The PCTESTCD of the pharmacokinetic analyte corresponding to
#' the administered drug. This is needed when administration times are imputed
#' from the PCRFTDTC field from the PC domain.
#' @param cmt The compartment for the administration as numeric.
#' @param cut_off_date The data cut-off date as Posix date-time or character.
#' @param keep Columns to keep after cleanup, as character.
#' @param silent Suppress messages, defaults to nif_option standard, if NULL.
#' @param debug Include debug fields, as logical.
#' @inheritParams create_iv_fields iv_admin duration
#' @param imputation The imputation rule set.
#'
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
  pctestcd = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = NULL,
  debug = FALSE,
  imputation = imputation_rules_standard,
  duration = NULL,
  iv_admin = NULL,
  silent = NULL
) {
  # validate input
  validate_min_nif(nif)
  validate_sdtm(sdtm, c("dm", "ex"))
  validate_argument(extrt, "character")
  validate_argument(analyte, "character", allow_null = TRUE)
  validate_argument(cmt, "numeric")
  validate_argument(subject_filter, "character")
  validate_argument(cut_off_date, "character", allow_null = TRUE)
  validate_argument(keep, "character", allow_null = TRUE, allow_multiple = TRUE)
  validate_argument(debug, "logical")
  validate_argument(silent, "logical", allow_null = TRUE)

  if (!is.list(imputation))
    stop("imputation must be a list!")

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
      sdtm, extrt,
      analyte = analyte,
      pctestcd = pctestcd,
      cmt = cmt,
      subject_filter = subject_filter,
      cut_off_date = cut_off_date,
      keep = keep,
      imputation = imputation,
      iv_admin = iv_admin,
      duration = duration,
      silent = silent
    )
  ) |>
    normalize_nif(keep = keep)
}
