#' Expand dates
#'
#' @param stdtc STDTC as character.
#' @param endtc ENDTC as character.
#' @param stdy STDY as numeric.
#' @param endy ENDY as numeric.
#'
#' @return Vector of lists: Dates and Days.
#' @noRd
date_list <- function(stdtc, endtc, stdy=NA, endy=NA) {
  tryCatch({
    start_date <- as.Date(stdtc)
    end_date <- as.Date(endtc)
  },
  error = function(e) {
    warning("Failed to parse dates: ", stdtc, " to ", endtc)
  })

  if(!is.na(end_date) & end_date < start_date) {
    stop(paste0(
      "End date before start date for: ", stdtc, " to ", endtc
    ))
  }

  if(!is.na(endy) & !is.na(stdy) & endy < stdy) {
    stop(paste0(
      "End day before start day for: ", stdy, " to ", endy
    ))
  }

  if(!is.na(endtc)){
    dtc_list <- seq(start_date, end_date, by = "days")
  } else {
    dtc_list <- start_date
  }

  if(!is.na(stdy)) {
    if(!is.na(endy)) {
        dy_list <- seq(stdy, endy)
      } else {
        dy_list <- seq(stdy, stdy + length(dtc_list) - 1)
      }
  } else {
    dy_list <- rep(NA, length(dtc_list))
  }

  if(length(dtc_list) != length(dy_list)) {
    stop("DTC list and DY list have different lengths!")
  }

  return(c(list(dtc_list), list(dy_list)))
}


#' Expand administration episodes
#'
#' @param ex The EX domain as data frame.
#'
#' @return A data frame.
#' @noRd
expand_ex <- function(ex) {
  validate <- TRUE

  # Input validation
  if (validate) {
    if (!is.data.frame(ex)) stop("Input must be a data frame")
    ex %>%
      assertr::verify(assertr::has_all_names(
        "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"
    ))
  }

  # Convert EXSTDY and EXENDY to numeric if they exist
  if ("EXSTDY" %in% names(ex)) {
    ex$EXSTDY <- as.numeric(ex$EXSTDY)
  }
  if ("EXENDY" %in% names(ex)) {
    ex$EXENDY <- as.numeric(ex$EXENDY)
  }

  ex %>%
    assertr::verify(assertr::has_all_names(
      "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"
      )) %>%
    {if(!"IMPUTATION" %in% names(.))
      mutate(., IMPUTATION = "") else .} %>%
    decompose_dtc(c("EXSTDTC", "EXENDTC")) %>%

    # expand dates
    # to do: implement dose frequencies other than QD (e.g., BID)
    rowwise() %>%
    # mutate(DTC_date = date_list(.data$EXSTDTC_date, .data$EXENDTC_date)) %>%
    mutate(DTC_date = date_list(.data$EXSTDTC_date, .data$EXENDTC_date)[1]) %>%

    {if(all(c("EXSTDY", "EXENDY") %in% names(ex)))
      mutate(., EXDY = date_list(.data$EXSTDTC_date, .data$EXENDTC_date,
                                 .data$EXSTDY, .data$EXENDY)[2]) else .} %>%

    tidyr::unnest(any_of(c("DTC_date", "EXDY"))) %>%

    group_by(.data$USUBJID, .data$EXTRT, .data$EXENDTC_date) %>%

    mutate(DTC_time = case_when(
      row_number() == n() & !is.na(EXENDTC_time) ~ .data$EXENDTC_time,
      .default = .data$EXSTDTC_time
    )) %>%

    # make imputation field
    mutate(IMPUTATION = case_when(
      row_number() == n() & !is.na(EXENDTC_time) ~ .data$IMPUTATION, # no comment
      row_number() == n() & row_number() != 1 & is.na(EXENDTC_time) &
        !is.na(EXSTDTC_time) ~ "time carried forward",
      row_number() == 1 & !is.na(EXSTDTC_time) ~ .data$IMPUTATION, # no comment
      !is.na(EXSTDTC_time) ~ "time carried forward",
      .default = "no time information"
    )) %>%

    ungroup() %>%
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    select(-c("EXSTDTC_date", "EXSTDTC_time", "EXENDTC_date", "EXENDTC_time",
              "DTC_date", "DTC_time"))
}


#' Compile administration data frame
#'
#' @details
#' A discussion on EC vs EX is provided [here](https://www.cdisc.org/kb/ecrf/exposure-collected#:~:text=In%20the%20SDTMIG%2C%20the%20Exposure,data%20collected%20on%20the%20CRF.)
#'
#' @details
#' # Time imputations and filtering
#'
#' The following time imputations and filters are applied in the given
#' order:
#'
#' ## 1. [impute_exendtc_to_rfendtc()]
#'
#' If EXENDTC is missing in the last administration episode for a given subject,
#' it is replaced with DM.RFENDTC, if available.
#'
#' ## 2. [filter_EXSTDTC_after_EXENDTC()]
#'
#' Administration episodes in which EXSTDTC is after EXENDT are deleted from the
#' data set.
#'
#' ## 3. [impute_exendtc_to_cutoff()]
#'
#' If in the last administration episode per subject and treatment, EXENDTC is
#' missing, for example because the treatment is still ongoing at the time of
#' the SDTM generation, EXENDTC is replaced with the cut-off date.
#'
#' ## 4. [impute_missing_exendtc()]
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
#' ## 6. [impute_admin_times_from_pcrftdtc()]
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
#' @param cut_off_date The data cut-off date as Posix date-time.
#' @param keep Columns to keep after cleanup, as character.
#' @param silent Suppress messages, defaults to nif_option standard, if NULL.
#'
#' @return A data frame.
#' @export
#' @importFrom assertthat assert_that
#' @keywords internal
#' @seealso [nif::add_administration()]
make_administration <- function(
    sdtm,
    extrt,
    analyte = NULL,
    cmt = 1,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    cut_off_date = NULL,
    keep = "",
    silent = NULL
) {
  dm <- domain(sdtm, "dm") %>% lubrify_dates()
  ex <- domain(sdtm, "ex") %>% lubrify_dates()
  vs <- NULL

  if(has_domain(sdtm, "vs")) {
    vs <- domain(sdtm, "vs")
  }

  ex <- impute_exendtc_to_rfendtc(ex, dm, silent = silent)

  assertthat::assert_that(
    extrt %in% ex$EXTRT,
    msg = paste0("Treatment '", extrt, "' not found in EXTRT!")
  )

  if(is.null(analyte)) {analyte <- extrt}
  if(is.null(cut_off_date)) cut_off_date <- last_ex_dtc(ex)

  sbs <- make_subjects(dm, vs, subject_filter, keep)

  admin <- ex %>%
    mutate(SRC_DOMAIN = "EX") %>%
    {if("EXSEQ" %in% names(ex)) mutate(., SRC_SEQ = EXSEQ) else
      mutate(., SRC_SEQ = NA)} %>%

    {if(!"IMPUTATION" %in% names(.))
      mutate(., IMPUTATION = "") else .} %>%

    filter(.data$EXTRT == extrt) %>%
    filter(.data$EXSTDTC <= cut_off_date) %>%
    decompose_dtc("EXSTDTC") %>%

    # impute_exendtc_to_rfendtc(dm) %>%
    filter_EXSTDTC_after_EXENDTC(dm) %>%

    # time imputations
    impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) %>%
    impute_missing_exendtc() %>%
    decompose_dtc("EXENDTC") %>%

    # make generic fields
    mutate(TIME = NA, NTIME = 0, ANALYTE = analyte, PARENT = analyte,
           METABOLITE = FALSE, DV = NA, CMT = cmt, EVID = 1, MDV = 1,
           DOSE = EXDOSE, AMT = EXDOSE) %>%
    expand_ex()

  # impute missing administration times from PCRFTDTC
  if("pc" %in% names(sdtm$domains)) {
    pc <- domain(sdtm, "pc") %>% lubrify_dates()
    admin <- admin %>%
      {if("PCRFTDTC" %in% names(pc))
        impute_admin_times_from_pcrftdtc(
          ., pc, analyte, analyte, silent = silent) else .}
  }

  admin <- admin %>%
    # carry forward missing administration times
    decompose_dtc("DTC") %>%
    arrange(.data$USUBJID, .data$ANALYTE, .data$DTC) %>%
    mutate(IMPUTATION = case_when(
      is.na(.data$DTC_time) == TRUE ~ "time carried forward",
      .default = .data$IMPUTATION)) %>%
    group_by(.data$USUBJID, .data$ANALYTE) %>%
    tidyr::fill("DTC_time", .direction = "down") %>%
    ungroup() %>%

    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    select(-c("DTC_date", "DTC_time")) %>%
    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      # difftime(date(.data$DTC), date(safe_min(.data$RFXSTDTC))),
      ## changed from RFXSTDTC to RFSTDTC. The difference between both dates is
      ## that RFXSTDTC includes any exposure captured in the EX domain, whereas
      ## RFSTDTC refers to the first exposure to study treatment.
      ## Reference: https://www.lexjansen.com/phuse-us/2020/ds/DS07.pdf
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    new_nif()

  return(admin)
}


#' Append administration events
#'
#' Drug administration data is taken from the EX domain of the sdtm object. The
#' 'extrt' field specifies the drug name as represented in 'EX', however, a
#' different 'analyte' name can be assigned to match with that of the
#' pharmacokinetic observations for the parent drug in plasma.
#'
#' For an overview on the representation of administration events in NONMEM
#' Input Format compliant data sets, see Bauer, R.J. (2019), NONMEM Tutorial
#' Part I: Description of Commands and Options, With Simple Examples of
#' Population Analysis. CPT Pharmacometrics Syst. Pharmacol., 8: 525-537.
#' [https://doi.org/10.1002/psp4.12404](https://doi.org/10.1002/psp4.12404)
#'
#' @param nif A nif object.
#' @inheritParams make_administration
#' @param debug Include debug fields, as logical.
#' @param silent Suppress messages, defaults to nif_option standard when NULL.
#'
#' @return A nif object.
#' @export
#' @examples
#' add_administration(new_nif(), examplinib_sad, "EXAMPLINIB")
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
    silent = NULL) {
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

  debug = isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if(isTRUE(debug))
    keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")
  bind_rows(
    nif,
    make_administration(
      sdtm, extrt, analyte, cmt, subject_filter, cut_off_date, keep,
      silent = silent)) %>%
    normalize_nif(keep = keep)
}




