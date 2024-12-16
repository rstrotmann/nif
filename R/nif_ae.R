
#' Make AE observation
#'
#' This function adds AE events as observations with the time of the AE onset
#' as the observation time point and the severity grade as the dependent
#' variable.
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
#' @param parent The parent compound as character.
#' @param cmt The compartment as numeric.
#' @param subject_filter A subject filter term.
#' @param observation_filter An observation filter term.
#' @param keep Columns to keep, as character.
#'
#' @return A nif object.
make_ae <- function(
    sdtm,
    ae_term,
    ae_field = "AEDECOD",
    parent = NULL,
    cmt = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    keep = "") {
  sbs <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"), subject_filter,
                       keep)
  if(!"ae" %in% names(sdtm$domains))
    stop("Domain AE not included in sdtm object!")

  obj <- domain(sdtm, "ae") %>%
    lubrify_dates()

  obj %>%
    mutate(SRC_DOMAIN = "AE") %>%
    # mutate(SRC_SEQ = .data[[paste0(toupper(domain), "SEQ")]]) %>%
    {if("AESEQ" %in% names(obj))
      mutate(., SRC_SEQ = .data[["AESEQ"]]) else
        mutate(., SRC_SEQ = NA)} %>%
    filter(eval(parse(text = observation_filter))) %>%
    filter(.data[[ae_field]] == ae_term) %>%
    mutate(
      DTC = .data[["AESTDTC"]],
      DV = as.numeric(.data[["AETOXGR"]])) %>%
    select("USUBJID", "DTC", "DV", "SRC_SEQ", "SRC_DOMAIN") %>%
    mutate(
      ANALYTE = paste0("AE_", gsub(" ", "_", ae_term)),
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
      # difftime(date(.data$DTC), date(safe_min(.data$RFXSTDTC))),
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    new_nif()
}


#' Add AE observation
#'
#' This function adds AE events as observations with the time of the AE onset
#' as the observation time point and the severity grade as the dependent
#' variable.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @inheritParams make_ae
#' @param nif A nif object.
#' @param debug Include debug fields, as logical.
#'
#' @return A nif object.
#' @export
add_ae_observation <- function(
    nif,
    sdtm,
    ae_term,
    ae_field = "AEDECOD",
    parent = NULL,
    cmt = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    keep = "",
    debug = FALSE) {
  debug = isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if(isTRUE(debug)) keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  if(is.null(parent)) {
      parent <- guess_parent(nif)
      conditional_message(paste0("Parent for ", ae_term, " was set to ",
                                 parent, "!"))
  }
  if(is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_message(paste0(
      "Compartment for AE_", ae_term,
      " was not specified and has been set to ", cmt))
  }

  ae <- make_ae(sdtm, ae_term, ae_field, parent, cmt, subject_filter,
                observation_filter, keep) %>%
    filter(.data$USUBJID %in% subjects(nif)$USUBJID)

  bind_rows(
    nif, ae) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    mutate(ID = as.numeric(as.factor(.data$USUBJID))) %>%
    normalize_nif(keep = keep)
}


