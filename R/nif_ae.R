
#' Make AE observation
#'
#' This function adds AE events as observations with the time of the AE onset
#' as the observation time point and the severity grade as the dependent
#' variable if the field AETOXGR is included. If AETOXGR is not included in the
#' AE domain, a coding table must be provided to translate any other field into
#' a numeric DV column, e.g.,
#' `coding_table = data.frame( AESEV = c("MILD", "MODERATE", "SEVERE"), DV = c(1, 2, 3)`
#'
#' @param sdtm A sdtm object.
#' @param ae_term The AE term as character.
#' @param ae_field The field in which the AE term is specified in. Can be:
#'
#' * AEDECOD:  Dictionary-derived term
#' * AELLT:    Lowest-level term
#' * AEHLT:    High-level term
#' * AEHLGT:   High-level group term
#' * AESOC:    System organ class
#' * AEBODSYS: Body system or organ class
#'
#' or any other field from the 'AE' domain
#' @param analyte The name for the AE observation, defaults to 'AE_xx' with xx
#' the ae_term.
#' @param parent The parent compound as character.
#' @param cmt The compartment as numeric.
#' @param subject_filter A subject filter term.
#' @param observation_filter An observation filter term.
#' @param coding_table A DV coding table as data frame. The coding table translates
#' columns included in the AE domain into a DV field.
#' @param keep Columns to keep, as character.
#'
#' @return A nif object.
#' @keywords internal
#' @export
make_ae <- function(
    sdtm,
    ae_term,
    ae_field = "AEDECOD",
    analyte = NULL,
    parent = "",
    cmt = NA,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    coding_table = NULL,
    keep = NULL) {
  # Input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  # Validate ae_term parameter
  # if (!is.character(ae_term) || length(ae_term) != 1) {
  #   stop("ae_term must be a single character string")
  # }
  # if (is.na(ae_term) || nchar(trimws(ae_term)) == 0) {
  #   stop("ae_term cannot be NA or empty")
  # }
  validate_char_param(ae_term, "ae_term")
  validate_char_param(ae_field, "ae_field")
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(parent, "parent", allow_empty = TRUE)
  validate_char_param(subject_filter, "subject_filter")
  validate_char_param(observation_filter, "observation_filter")
  validate_char_param(keep, "keep", allow_null = TRUE, allow_multiple = TRUE)

  if(is.null(analyte)) {
    analyte <- paste0("AE_", gsub(" ", "_", ae_term))
  }

  expected_domains <- c("DM", "VS", "AE")
  missing_domains <- setdiff(expected_domains, toupper(names(sdtm$domains)))
  if(length(missing_domains) > 0)
    stop(paste0(plural("Domain", length(missing_domains)>1), " ",
                nice_enumeration(missing_domains)," not found!"))

  sbs <- make_subjects(
    domain(sdtm, "dm"), domain(sdtm, "vs"), subject_filter, keep = keep)

  obj <- domain(sdtm, "ae") %>%
    mutate(SRC_DOMAIN = "AE") %>%
    {if("AESEQ" %in% names(.))
      mutate(., SRC_SEQ = .data[["AESEQ"]]) else
        mutate(., SRC_SEQ = NA)} %>%
    lubrify_dates()

  # Validate coding table
  if(!is.null(coding_table)) {
    if(!inherits(coding_table, "data.frame"))
      stop("coding table must be a data frame!")
    temp <- names(coding_table)
    temp <- temp[temp != "DV"]
    unknown_fields <- setdiff(temp, names(obj))
    if(length(unknown_fields) > 0)
      stop(paste0(
        "Fields ", nice_enumeration(unknown_fields),
        " not found in AE domain!"
      ))
    if(!"DV" %in% names(coding_table))
      stop("coding table must include a DV column!")
    if(!is.numeric(coding_table$DV))
      stop("DV in the coding table must be numeric!")
    obj <- obj %>%
      left_join(coding_table, by = temp)
  } else {
    if(!"AETOXGR" %in% names(obj))
      stop("AETOXGR not found in AE. Use a coding table!")
    obj <- obj %>%
      mutate(DV = as.numeric(.data$AETOXGR))
  }

  # Validate required fields exist in AE domain
  required_fields <- c("AESTDTC", ae_field)
  missing_fields <- required_fields[!required_fields %in% names(obj)]
  if(length(missing_fields) > 0)
    stop(paste0("Required field(s) missing in AE domain: ",
                nice_enumeration(missing_fields)))

  # Validate keep parameter columns exist in AE domain
  if(length(keep) > 0) {
    missing_keep_fields <- keep[!keep %in% names(obj)]
    if(length(missing_keep_fields) > 0)
      stop(paste0("Column(s) specified in 'keep' not found in AE domain: ",
                  nice_enumeration(missing_keep_fields)))
  }


  obj %>%
    filter(eval(parse(text = observation_filter))) %>%
    filter(.data[[ae_field]] == ae_term) %>%
    mutate(
      DTC = .data[["AESTDTC"]],
      ) %>%
    select(any_of(c("USUBJID", "DTC", "DV", "SRC_SEQ", "SRC_DOMAIN", keep))) %>%
    mutate(
      # ANALYTE = paste0("AE_", gsub(" ", "_", ae_term)),
      ANALYTE = analyte,
      TIME = NA,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = FALSE,
      EVID = 0,
      MDV = 0,
      IMPUTATION = "") %>%
    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    new_nif()
}


#' Add AE observation
#'
#' This function adds AE events as observations with the time of the AE onset
#' as the observation time point and the severity grade as the dependent
#' variable if the field AETOXGR is included. If AETOXGR is not included in the
#' AE domain, a coding table must be provided to translate any other field into
#' a numeric DV column, e.g.,
#' `coding_table = data.frame( AESEV = c("MILD", "MODERATE", "SEVERE"), DV = c(1, 2, 3)`
#'
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams make_ae
#' @param nif A nif object.
#' @param debug Include debug fields, as logical.
#' @param silent Suppress messages, defaults to nif_option setting, if NULL.
#'
#' @return A nif object.
#' @export
add_ae_observation <- function(
    nif,
    sdtm,
    ae_term,
    ae_field = "AEDECOD",
    analyte = NULL,
    parent = NULL,
    cmt = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    coding_table = NULL,
    keep = NULL,
    debug = FALSE,
    silent = NULL) {
  debug = isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if(isTRUE(debug)) keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  validate_char_param(analyte, "analyte", allow_null = TRUE)
  if(is.null(analyte)) {
    analyte <- paste0("AE_", gsub(" ", "_", ae_term))
  }

  if(is.null(parent)) {
    parent <- guess_parent(nif)
    conditional_message(
      # "Parent for ", ae_term, " was set to ", parent, "!",
      "Parent for ", analyte, " was set to ", parent, "!",
      silent = silent)
  }

  if(is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_message(paste0(
      # "Compartment for AE_", ae_term,
      "Compartment for ", analyte,
      " was not specified and has been set to ", cmt), silent = silent)
  }

  ae <- make_ae(
      sdtm, ae_term, ae_field, analyte, parent, cmt, subject_filter,
      observation_filter, coding_table, keep) %>%
    filter(.data$USUBJID %in% subjects(nif)$USUBJID)

  bind_rows(
    nif, ae) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    mutate(ID = as.numeric(as.factor(.data$USUBJID))) %>%
    normalize_nif(keep = keep)
}

