#' Make event observation
#'
#' @param sdtm A sdtm object.
#' @param domain The domain code as character.
#' @param testcd The testcode from xxTESTCD where xx is the domain code, as
#'   character. Not used if NULL.
#' @param event_filter A filter term to characterize the events to extract, as
#'   character.
#' @param analyte The name for the analyte. Defaults to the 'EV_testcd', if
#'   NULL.
#' @param parent The name of the parent analyte for the observation as
#'   character. Defaults to the value of 'analyte' if NULL.
#' @param metabolite observation is a metabolite, as logical.
#' @param cmt The compartment for the observation, as numeric.
#' @param subject_filter The filter to be applied to the DM domain, as
#'   character.
#' @param observation_filter The filtering to apply to the observation source
#'   data, as character.
#' @param DTC_field The field to use as the date-time code for the observation.
#'   Defaults to 'xxDTC', with xx the domain name, if NULL.
#' @param keep Columns to keep, as character.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#' @param event_diff Only retain events where there is a change from the event
#'   filter evaluating to TRUE after being FALSE in the previous observation,
#'   i.e., apply time differentiation to the observation. As logical. Defaults
#'   to FALSE.
#'
#' @return A data frame.
#' @importFrom stats as.formula
#' @export
#' @keywords internal
make_event <- function(
    sdtm,
    domain,
    testcd,
    event_filter,
    event_diff = FALSE,
    analyte = NULL,
    parent = NULL,
    metabolite = FALSE,
    cmt = NA,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    DTC_field = NULL,
    keep = NULL,
    silent = NULL) {

  # Validate inputs
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  domain_name <- tolower(domain)
  if (!domain_name %in% names(sdtm$domains)) {
    stop(paste0("Domain '", domain_name, "' not found in sdtm object"))
  }

  if(is.null(analyte)) analyte <- paste0("EV_", testcd)
  if(is.null(parent)) parent <- analyte

  # Create fields
  if(is.null(DTC_field))
    DTC_field <- paste0(toupper(domain), "DTC")

  # Get subject data
  tryCatch({
    sbs <- make_subjects(
      domain(sdtm, "dm"), domain(sdtm, "vs"), subject_filter, keep)
  }, error = function(e) {
    stop(paste0("Error getting subject data: ", e$message))
  })

  obj <- domain(sdtm, domain_name) %>%
    lubrify_dates()

  # check and apply observation filter
  if(!is_valid_filter(obj, observation_filter))
    stop(paste0("observation filter '", observation_filter, "' is invalid!"))

  filtered_obj <- obj %>%
    mutate(SRC_DOMAIN = .data$DOMAIN) %>%
    {if(paste0(toupper(domain), "SEQ") %in% names(obj))
      mutate(., SRC_SEQ = .data[[paste0(toupper(domain), "SEQ")]]) else
        mutate(., SRC_SEQ = NA)} %>%
    filter(eval(parse(text = observation_filter)))

  # Add warning if observation_filter returns no entries
  if (nrow(filtered_obj) == 0) {
    stop("The observation_filter '", observation_filter, "' returned no entries.")
  }

  # filter for testcd
  if(!is.null(testcd)){
    testcd_field <- paste0(toupper(domain), "TESTCD")
    if(!testcd %in% unique(filtered_obj[[testcd_field]]))
      stop(paste0(
        "testcd ", testcd,
        " not found after filtering for observation_filter!"))

    filtered_obj <- filtered_obj %>%
      filter(.data[[testcd_field]] == testcd)
  }

  # check and apply event filter
  if(!is_valid_filter(filtered_obj, event_filter))
    stop(paste0("event filter '", event_filter, "' is not a valid filter!"))

  # flag marks the event condition, dflag marks a change in the event condition
  # ev_flag marks the attainment of the condition
  temp <- filtered_obj %>%
    mutate(flag = case_when(
      stats::as.formula(paste0(event_filter, "~ 1")),
      .default = 0)) #%>%
    # mutate(ev_flag = flag)

  # Apply event differentiation, if event_diff == TRUE
  if(event_diff == TRUE) {
    temp <- temp %>%
      mutate(dflag = case_when(
        .data$flag != lag(.data$flag) ~ 1, .default = 0)) %>%
      # mutate(ev_flag = .data$flag == 1 & .data$dflag == 1)
      mutate(flag = case_when(
        .data$flag == 1 & .data$dflag == 1 ~ 1,
        .default = 0)) %>%
      # filter(flag == 1) %>%
      select(-c("dflag"))
  }

  out <- temp %>%
    mutate(DTC = .data[[DTC_field]]) %>%
    # filter(.data$ev_flag == TRUE) %>%
    # filter(.data$flag == TRUE) %>%
    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    filter(!is.na(.data$DTC)) %>%
    mutate(
      ANALYTE = analyte,
      # DV = 1,
      DV = flag,
      TIME = NA,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = metabolite,
      EVID = 0,
      MDV = as.numeric(is.na(DV)),
      IMPUTATION = "") %>%
    # select(-c("flag", "dflag", "ev_flag"))
    select(-c("flag"))

  return(out)
}


#' Append event observations
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param nif A nif object.
#' @inheritParams make_event
#'
#' @return A nif object.
#' @export
add_event_observation <- function(
    nif,
    sdtm,
    domain,
    testcd = NULL,
    event_filter,
    event_diff = FALSE,
    analyte = NULL,
    parent = NULL,
    metabolite = FALSE,
    cmt = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    # TESTCD_field = NULL,
    DTC_field = NULL,
    keep = NULL,
    debug = FALSE,
    silent = NULL
) {
  # Validate inputs
  if (!inherits(nif, "nif")) {
    stop("nif must be an nif object")
  }

  # Validate metabolite parameter
  if (!is.logical(metabolite) || length(metabolite) != 1) {
    stop("metabolite must be a single logical value")
  }

  debug = isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if(isTRUE(debug))
    keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  if(is.null(analyte))
    analyte <- paste0("EV_", testcd)

  nif <- nif %>%
    ensure_analyte()

  if(length(parents(nif)) == 0)
    stop("Please add at least one administration first!")

  # Test if compartment is already assigned
  if(!is.null(cmt))
    if(cmt %in% unique(nif$CMT))
      warning(paste0("Compartment ", cmt, " is already assigned!"))

  # Assign compartment for observation if CMT == NULL
  if(is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_message(
      paste0("Compartment for ", analyte,
             " was not specified and has been set to ", cmt),
      silent = silent)
  }

  if(is.null(parent)) {
    parent <- guess_parent(nif)
    if(is.null(parent)) {
      stop(paste0(
        "A parent could not be automatically determined. ",
        "Please specify a parent value explicitly."))
    }
    conditional_message(
      paste0("Parent for ", analyte, " was set to ", parent, "!"),
      silent = silent)
  }

  event_obs <- make_event(
    sdtm,
    domain,
    testcd,
    event_filter,
    event_diff,
    analyte = analyte,
    parent = parent,
    metabolite = metabolite,
    cmt = cmt,
    subject_filter = subject_filter,
    observation_filter = observation_filter,
    DTC_field = DTC_field,
    keep = keep) %>%
    select(any_of(c(standard_nif_fields, "IMPUTATION", keep)))

  obj <- bind_rows(
    nif,
    event_obs) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    mutate(ID = as.numeric(as.factor(.data$USUBJID))) %>%
    make_time() %>%
    normalize_nif(keep = keep)

  return(obj)
}
