make_event <- function(
    sdtm,
    domain,
    testcd,
    event_filter,
    analyte = NULL,
    parent = NULL,
    metabolite = FALSE,
    cmt = NA,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    # TESTCD_field = NULL,
    DTC_field = NULL,
    # DV_field = NULL,
    # coding_table = NULL,
    # NTIME_lookup = NULL,
    # ntime_method = NULL,
    keep = NULL,
    # include_day_in_ntime = FALSE,
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
  testcd_field <- paste0(toupper(domain), "TESTCD")
  if(!testcd %in% unique(filtered_obj[[testcd_field]]))
    stop(paste0("testcd ", testcd, " not found after filtering for obsrvation_filter!"))

  testcd_obj <- filtered_obj %>%
    filter(.data[[testcd_field]] == testcd)

  # check and apply event filter
  if(!is_valid_filter(testcd_obj, event_filter))
    stop(paste0("event filter '", event_filter, "' is not a valid filter!"))

  # flag marks the event condition, dflag marks a change in the event condition
  # ev_flag marks the attainment of the condition
  temp <- testcd_obj %>%
    mutate(flag = case_when(
      as.formula(paste0(event_filter, "~ 1")),
      .default = 0)) %>%
    mutate(dflag = case_when(flag != lag(flag) ~ 1, .default = 0)) %>%
    mutate(ev_flag = flag == 1 & dflag == 1)

  out <- temp %>%
    mutate(DTC = .data[[DTC_field]]) %>%
    filter(ev_flag == TRUE) %>%
    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    filter(!is.na(.data$DTC)) %>%
    mutate(
      ANALYTE = analyte,
      DV = 1,
      TIME = NA,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = metabolite,
      EVID = 0,
      MDV = as.numeric(is.na(DV)),
      IMPUTATION = "") %>%
    select(-c(flag, dflag, ev_flag))

  return(out)
}
