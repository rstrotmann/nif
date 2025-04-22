#' Make nominal time
#'
#' Return NTIME lookup table or NULL if the xxELTM field is not included in
#' the input data frame.
#'
#' @param obj The input as data table.
#' @param include_day include_day Include time component of treatment day, as
#'   logical.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#' @param domain The domain name as character.
#'
#' @return A data frame.
#' @export
make_ntime <- function(
    obj,
    domain,
    include_day = FALSE,
    silent = NULL) {

  pull_column <- function(col_tail) {
    temp <- obj %>%
      select(ends_with(col_tail))
    if(length(temp) == 0)
      return(NULL)
    return(temp)
  }

  eltm_name <- paste0(toupper(domain), "ELTM")
  dy_name <- paste0(toupper(domain), "DY")

  eltm <- pull_column(eltm_name)
  dy <- pull_column(dy_name)

  if(is.null(eltm)) {
    conditional_message(
      "ELTM is not defined. Provide a NTIME lookup",
      "table to define nominal time!",
      silent = silent)
    return(NULL)
  }
  if(length(eltm) > 1) {
    stop("Multiple columns ending in ELTM")
  }

  if(include_day == TRUE) {
    out <- data.frame(
      eltm,
      dy
    ) %>%
      mutate(NTIME = pt_to_hours(.[[1]]) + trialday_to_day(.[[2]]) * 24)
  } else {
    out <- data.frame(
      eltm
    ) %>%
      mutate(NTIME = pt_to_hours(.[[1]]))
  }

  return(distinct(out))
}


#' Compile observation data frame
#'
#' @description
#' Create a data frame of observations from a SDTM domain specified by 'domain'
#' where the dependent variable comes from the 'DV_field' parameter and the
#' timing information from the 'DTC_field' parameter.
#'
#' The 'TIME' in the output is `NA` throughout and needs to be calculated based
#' on administration time point information provided separately.
#'
#' If the 'NTIME_lookup' parameter is provided, 'NTIME' can be derived from a
#' field contained in the input data set, e.g., 'PCELTM' (see the code
#' examples). Otherwise, 'NTIME' will be `NA`.
#'
#' @param sdtm A sdtm object. Needs at least the 'DM' and 'VS' domains, and the
#'   domain the observations come from.
#' @param domain The domain as character.
#' @param testcd The observation variable, as character.
#' @param analyte The name for the analyte. Defaults to the 'testcd', if NULL.
#' @param parent The name of the parent analyte for the observation as
#'   character. Defaults to the value of 'analyte' if NULL.
#' @param metabolite Observation is a metabolite, as logical.
#' @param cmt The compartment for the observation as numeric.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param observation_filter The filtering to apply to the observation source
#'   data.
#' @param TESTCD_field The xxTESTCD field. Defaults to the two-character domain
#'   name followed by 'TESTCD', if NULL.
#' @param DTC_field The field to use as the date-time code for the observation.
#'   Defaults to the two-character domain name followed by 'DTC', if NULL.
#' @param DV_field the field to use as the dependent variable. Defaults to the
#'   two-character domain name followed by 'STRESN', if NULL.
#' @param coding_table Coding table to translate a categorical values into
#'   numerical values, as data frame. The data frame must have at least one
#'   column that matches a column in the domain, and a numerical 'DV' column
#'   that provides the recoding result.
#' @param factor Multiplier for the DV field, as numeric.
#' @param NTIME_lookup A data frame with two columns, a column that defines the
#'   custom nominal time information in the target domain (e.g., 'PCELTM'), and
#'   'NTIME'. This data frame is left_joined into the observation data frame
#'   to provide the NTIME field.
#' @param keep Columns to keep, as character.
#' @param include_day_in_ntime Include treatment day in the calculation of
#'   NTIME, as logical.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#'
#' @return A data frame.
#' @keywords internal
#' @import stringr
#'
make_observation <- function(
    sdtm,
    domain,
    testcd,
    analyte = NULL,
    parent = NULL,
    metabolite = FALSE,
    cmt = NA,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    TESTCD_field = NULL,
    DTC_field = NULL,
    DV_field = NULL,
    coding_table = NULL,
    factor = 1,
    NTIME_lookup = NULL,
    keep = NULL,
    include_day_in_ntime = FALSE,
    # duplicate_function = NULL,
    silent = NULL
) {
  # Validate inputs
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  # domain_name <- str_to_lower(domain)
  domain_name <- tolower(domain)
  if (!domain_name %in% names(sdtm$domains)) {
    stop(paste0("Domain '", domain_name, "' not found in sdtm object"))
  }

  # Create fields
  if(is.null(DTC_field))
    DTC_field <- paste0(toupper(domain), "DTC")

  if(is.null(DV_field))
    DV_field <- paste0(toupper(domain), "STRESN")

  if(is.null(TESTCD_field))
    TESTCD_field <- paste0(toupper(domain), "TESTCD")

  if(is.null(analyte)) analyte <- testcd
  if(is.null(parent)) parent <- analyte

  # Get subject data
  tryCatch({
    sbs <- make_subjects(
      domain(sdtm, "dm"), domain(sdtm, "vs"), subject_filter, keep)
  }, error = function(e) {
    stop(paste0("Error getting subject data: ", e$message))
  })

  obj <- domain(sdtm, domain_name) %>%
    lubrify_dates()

  # Check if required fields exist
  required_fields <- c(TESTCD_field, DTC_field)
  missing_fields <- required_fields[!required_fields %in% names(obj)]
  if (length(missing_fields) > 0) {
    stop(paste0("Required field(s) missing in domain '", domain_name, "': ",
                paste(missing_fields, collapse = ", ")))
  }

  # Check if DV field exists when no coding table
  if (!DV_field %in% names(obj) && is.null(coding_table)) {
    stop(paste0("DV field '", DV_field, "' not found in domain and no coding table provided"))
  }

  # Create NTIME lookup table if not provided
  if(is.null(NTIME_lookup)) {
    NTIME_lookup = make_ntime(
      obj, domain, include_day = FALSE, silent = silent)
    if(is.null(NTIME_lookup)) {
      conditional_message(
        "No NTIME_lookup could be created, NTIME will be NA",
        silent = silent)
    }
  } else {
    # Validate NTIME_lookup structure
    if(!is.data.frame(NTIME_lookup)) {
      stop("NTIME_lookup must be a data frame")
    }
    if(!"NTIME" %in% names(NTIME_lookup)) {
      stop("NTIME_lookup must contain a 'NTIME' column")
    }
    if(length(intersect(names(NTIME_lookup), names(obj))) < 1) {
      stop("NTIME_lookup must contain at least one column that matches a column in the domain data")
    }
  }

  # apply coding table, if not NULL
  if(!is.null(coding_table)) {
    if(!any(names(coding_table) %in% names(obj))){
      stop("Coding table cannot be applied to data set!")
    }
    if(!is.numeric(coding_table$DV)){
      stop("DV field in coding table must be numeric!")
    }
    # Capture warnings instead of suppressing them
    join_msgs <- capture.output(type = "message", {
      obj <- obj %>%
        left_join(coding_table)
    })
    if(length(join_msgs) > 0) {
      conditional_message(
        "Warnings during coding table join: ",
        paste(join_msgs, collapse = "\n"),
        silent = silent)
    }
  } else {
    obj <- obj %>%
      mutate(DV = .data[[DV_field]] * factor)
  }

  filtered_obj <- obj %>%
    mutate(SRC_DOMAIN = .data$DOMAIN) %>%
    {if(paste0(toupper(domain), "SEQ") %in% names(obj))
      mutate(., SRC_SEQ = .data[[paste0(toupper(domain), "SEQ")]]) else
        mutate(., SRC_SEQ = NA)} %>%
    filter(eval(parse(text = observation_filter)))

  # Add warning if subject_filter returns no entries
  if (nrow(filtered_obj) == 0) {
    stop("The observation_filter '", observation_filter, "' returned no entries.")
  }

  out <- filtered_obj %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    mutate(
      DTC = .data[[DTC_field]],
      ANALYTE = analyte,
      TIME = NA,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = metabolite,
      EVID = 0,
      MDV = as.numeric(is.na(DV)),
      IMPUTATION = "")

  join_variables <- intersect(names(NTIME_lookup), names(obj))

  if(!is.null(NTIME_lookup)) {
      out <- left_join(out, NTIME_lookup, by = join_variables)
  } else {
    out <- out %>%
      mutate(NTIME = NA)
  }

  # Include day into NTIME
  if(include_day_in_ntime == TRUE) {
    dy_name <- paste0(toupper(domain), "DY")
    if(!dy_name %in% names(out)) {
      stop(paste0(dy_name, " not found in domain, day cannot be included in observation"))
    }
    out <- out %>%
      mutate(NTIME = NTIME + trialday_to_day(out[[dy_name]]) * 24)
  }

  out <- out %>%
    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    filter(!is.na(.data$DTC))

  return(out)
}


#' Append observation events
#'
#' Observations can be pharmacokinetic observations (i.e., from the PC domain),
#' or any other class of observation from any other SDTM domain. The 'testcd'
#' specifies the value of the respective __TESTCD__ field (e.g., 'PCTESTCD',
#' 'VSTESTCD' or 'LBTESTCD') that defines the observation. Observation events
#' can be attached to an administered drug by specifying the 'parent' field.
#' This is required for, e.g., the time-after-dose ('TAD') and
#' time-after-first-dose ('TAFD') time calculation.
#'
#' Observations can be further specified with the 'observation_filter' term. The
#' filter term can refer to any field of the respective SDTM domain.
#'
#' A PK/PD model compartment can be specified with 'cmt' or will be
#' automatically assigned if `cmt = NULL`.
#'
#' @param nif A nif object.
#' @inheritParams make_observation
#' @param debug Include debug fields, as logical.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#' @param duplicates Selection how to deal with duplicate observations with
#'   respect to the USUBJID, ANALYTE and DTC fields:
#'   * 'stop': Stop execution and produce error message
#'   * 'ignore': Include duplicates in the data set
#'   * 'identify': Return a list of duplicate entries
#'   * 'resolve': Resolve duplicates, applying the `duplicate_function` to the
#'   duplicate entries.
#' @param duplicate_function Function to resolve duplicate values, defaults to
#'   `mean`.
#' @param na.rm Logical indicating whether to remove NA values when applying the
#'   duplicate_function. Defaults to TRUE.
#'
#' @return A nif object.
#' @seealso [add_administration()]
#' @export
add_observation <- function(
    nif,
    sdtm,
    domain,
    testcd,
    analyte = NULL,
    parent = NULL,
    metabolite = FALSE,
    cmt = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    TESTCD_field = NULL,
    DTC_field = NULL,
    DV_field = NULL,
    coding_table = NULL,
    factor = 1,
    NTIME_lookup = NULL,
    keep = NULL,
    debug = FALSE,
    include_day_in_ntime = FALSE,
    silent = NULL,
    duplicates = "stop",
    duplicate_function = mean,
    na.rm = TRUE
) {
  debug = isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if(isTRUE(debug)) keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  valid_duplicate_values <- c("stop", "ignore", "identify", "resolve")
  if(!duplicates %in% valid_duplicate_values)
    stop(paste0("Invalid value for 'duplicates' - must be one of ",
                nice_enumeration(valid_duplicate_values, conjunction = "or")))

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
      paste0("Compartment for ", testcd,
             " was not specified and has been set to ", cmt),
      silent = silent)
  }

  if(is.null(analyte)) analyte <- testcd

  imp <- nif %>%
    # ensure_analyte() %>%
    # as.data.frame() %>%
    filter(EVID == 1) %>%
    distinct(ANALYTE) %>%
    pull(ANALYTE)

  if(is.null(parent)) {
    if(analyte %in% imp) {
      parent <- analyte
    } else {
      parent <- guess_parent(nif)
      if(is.null(parent)) {
        stop("No suitable parent could be determined. Please specify a parent value explicitly.")
      }
      conditional_message(
        paste0("Parent for ", analyte, " was set to ", parent, "!"),
        silent = silent)
    }
  }

  observation <- make_observation(
    sdtm, domain, testcd, analyte, parent, metabolite, cmt, subject_filter,
    observation_filter, TESTCD_field, DTC_field, DV_field,
    coding_table, factor, NTIME_lookup, keep,
    include_day_in_ntime = include_day_in_ntime, silent = silent) %>%
    select(any_of(c(standard_nif_fields, keep)))

  # Duplicate handling
  dupl_fields <- c("USUBJID", "ANALYTE", "DTC")
  n_dupl <- find_duplicates(observation, fields = dupl_fields, count_only = TRUE)

  if(n_dupl != 0){
    if(duplicates == "stop") {
      stop(paste0(
        n_dupl, " duplicate observations found with respect to ",
        nice_enumeration(dupl_fields), ".\n",
        "Identify the duplicates using the `duplicates = 'identify'` parameter,\n",
        "or have duplicates automatically resolved using `duplicates = 'resolve'`\n",
        "where the resolution function is specified by the `duplicate_function`\n",
        "parameter (default is `mean`)."
      ))
    }

    if(duplicates == "identify") {
      message(paste0(
        n_dupl, " duplicate observations found with respect to ",
        nice_enumeration(dupl_fields), ". ",
        "Only duplicate observations returned!"))
      return(find_duplicates(observation, fields = dupl_fields) #%>%
               # relocate(count)
             )
    }

    if(duplicates == "resolve") {
      observation = resolve_duplicates(
        observation,
        fields = dupl_fields,
        duplicate_function = duplicate_function,
        na.rm = na.rm)
      conditional_message(
        "In observations for ", testcd, " (analyte '", analyte, "'), ",
        n_dupl, " duplicates were resolved!",
        silent = silent
      )
    }

    if(duplicates == "ignore") {
      conditional_message(
        "In observations for ", testcd, " (analyte '", analyte, "'), ",
        n_dupl, " duplicates were found but kept in the data set!",
        silent = silent
      )
    }
  }


  obj <- bind_rows(
    nif,
    observation
    ) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    mutate(ID = as.numeric(as.factor(.data$USUBJID))) %>%
    group_by(.data$USUBJID, .data$PARENT) %>%
    mutate(NO_ADMIN_FLAG = case_when(sum(EVID == 1) == 0 ~ TRUE,
                                     .default = FALSE)) %>%
    ungroup()

  n_no_admin <- sum(obj$NO_ADMIN_FLAG == TRUE)
  if(n_no_admin != 0) {
    conditional_message(
      paste0("Missing administration information in ",
             n_no_admin, " observations (did you set a\n",
             "parent for these observations?):\n",
             df_to_string(
               obj %>%
                 filter(.data$NO_ADMIN_FLAG == TRUE) %>%
                 group_by(.data$USUBJID, .data$PARENT, .data$ANALYTE) %>%
                 mutate(N = sum(EVID == 0)) %>%
                 ungroup() %>%
                 distinct(.data$USUBJID, .data$PARENT, .data$ANALYTE, N),
               indent = 2), "\n"),
      silent = silent)

    obj <- obj %>%
      filter(.data$NO_ADMIN_FLAG == 0)
  }

  obj %>%
    select(-c("NO_ADMIN_FLAG")) %>%
    new_nif() %>%
    normalize_nif(keep = keep)
}


#' Add observation from non-SDTM-formatted data table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param nif A nif object.
#' @param raw The raw observation data frame.
#' @param analyte The analyte name as character.
#' @param parent The parent as character.
#' @param cmt The compartment for the analte as numeric.
#' @param observation_filter Filter term, as character.
#' @param USUBJID_field The field specifying the USUBJID, as character.
#' @param DTC_field The field specifying the DTC, as character.
#' @param NTIME_field The field specifying the NTIME, as character.
#' @param DV_field The field specifying the dependent variable, as character.
#' @param keep Columns to keep, as character.
#' @param debug Keep debug information.
#' @param silent Suppress messages, defaults to nif_option setting, if NULL.
#'
#' @return A nif object.
#' @export
import_observation <- function(
    nif,
    raw,
    analyte,
    parent = NULL,
    cmt = NULL,
    observation_filter = "TRUE",
    USUBJID_field = "USUBJID",
    DTC_field = NULL,
    NTIME_field = NULL,
    DV_field = NULL,
    keep = NULL,
    debug = FALSE,
    silent = NULL
  ) {
  # Validate inputs
  if (!inherits(nif, "nif")) {
    stop("nif must be an nif object")
  }

  debug = isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if(isTRUE(debug)) keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  nif <- nif %>%
    ensure_parent()

  # if(length(parents(nif)) == 0)
  #   stop("Please add at least one administration first!")

  if(!all(c(DV_field, USUBJID_field) %in% names(raw)))
    stop(paste0(
      "ERROR: DV field (", DV_field, ") and USUBJID field (", USUBJID_field,
      ") must both be present in the input data frame!"))

  if(!any(c(NTIME_field, DTC_field) %in% names(raw)))
    stop(paste0(
      "ERROR: One of the time fields (",
      nice_enumeration(c(NTIME_field, DTC_field), conjunction = "or"),
      ") must be present in the input data frame!"))

  if(is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_message(
      "Compartment was not specified and has been set to ", cmt,
      silent = silent)
    }

  imp <- nif %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    distinct(ANALYTE) %>%
    pull(ANALYTE)

  if(is.null(parent)) {
    if(analyte %in% imp) {
      parent <- analyte
    } else {
      parent <- guess_parent(nif)
      conditional_message(
        "Parent for ", analyte, " was set to ", parent, "!",
        silent = silent)
    }
  }

  sbs <- nif %>%
    # as.data.frame() %>%
    filter(EVID == 1) %>%
    select("USUBJID", "ID", any_of(fillable_nif_fields), starts_with("BL_"),
           any_of(keep)) %>%
    select(!any_of(c("SRC_DOMAIN", "SRC_SEQ"))) %>%
    mutate(IMPUTATION = "") %>%
    distinct()

  filtered_raw <- raw %>%
    filter(eval(parse(text = observation_filter)))

  # Add warning if subject_filter returns no entries
  if (nrow(filtered_raw) == 0) {
    stop("The observation_filter '", observation_filter, "' returned no entries.")
  }

  obs <- filtered_raw %>%
    mutate(USUBJID = .data[[USUBJID_field]]) %>%
    {if(!is.na(NTIME_field)) mutate(., NTIME = .data[[NTIME_field]]) else .} %>%
    mutate(DV = .data[[DV_field]]) %>%
    select(any_of(c("USUBJID", "NTIME", "DV", DTC_field))) %>%
    mutate(
      ANALYTE = analyte,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = FALSE,
      EVID = 0,
      MDV = as.numeric(is.na(DV))) %>%
    inner_join(sbs, by = "USUBJID") %>%
    mutate(
      SRC_DOMAIN = "IMPORT",
      SRC_SEQ = NA) #%>%
    # as.data.frame()

  # derive time from DTC, if present, or generate DTC from first administration
  #   time in nif object
  if(!is.null(DTC_field)) {
    obs <- obs %>%
      mutate(DTC = .data[[DTC_field]]) %>%
      lubrify_dates()
  } else {
    obs <- obs %>%
      left_join(first_admin_dtc(nif), by = "USUBJID") %>%
      mutate(DTC = .data$FIRSTDTC + duration(hours = NTIME)) %>%
      select(!all_of("FIRSTDTC")) %>%
      mutate(IMPUTATION = paste0("DTC derived from ", NTIME_field))
  }

  bind_rows(
    # as.data.frame(nif),
    nif,
    obs
  ) %>%
    normalize_nif(keep = keep)
  # return(out)
}
