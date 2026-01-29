#' Convert time point text to numeric time values
#'
#' @description
#' Extracts numeric time values (in hours) from time point text descriptions.
#' This function handles various formats including pre-dose notations, hour and
#' minute specifications, time ranges, and day information.
#'
#' @param obj A data frame containing time point text descriptions.
#' @param domain The domain code as character (default: "PC" for
#'   pharmacokinetic). Used to determine the column name containing time point
#'   descriptions.
#'
#' @details
#' The function recognizes and processes the following patterns:
#' * Pre-dose notations (e.g., "PRE-DOSE", "PREDOSE") are converted to 0
#' * Hour specifications (e.g., "1H POST-DOSE", "2.5 HOURS POST DOSE")
#' * Minute specifications (e.g., "30 MIN POST DOSE") are converted to hours
#' * Time ranges (e.g., "0.5H TO 2H POST-DOSE") - the later time is used
#' * Day information (e.g., "DAY1 - 2 HOURS POST ADMINISTRATION")
#'
#' @return A data frame with a column representing the unique values of the
#'   xxTPT variable and a NTIME column with the time in hours.
#'
#' @examples
#' df <- data.frame(PCTPT = c("PRE-DOSE", "1H POST-DOSE", "2.5 HRS POST-DOSE"))
#' make_ntime_from_tpt(df) # Returns c(0, 1, 2.5)
#' @noRd
make_ntime_from_tpt <- function(obj, domain = NULL) {
  if (is.null(domain)) {
    domain <- unique(obj$DOMAIN)[1]
  }
  tpt_name <- paste0(toupper(domain), "TPT")

  # Extract the TPT values
  tpt <- unique(obj[[tpt_name]])
  if (all(is.null(tpt))) {
    return(NULL)
  }

  # Create a function to process each TPT value
  extract_ntime <- function(x) {
    if (is.na(x)) {
      return(NA)
    }

    # Handle PRE-DOSE variations
    if (stringr::str_detect(x, "^PRE[- ]?DOSE$")) {
      return(0)
    }

    # Handle ranges like "0.5H TO 2H POST-DOSE" - take the later time
    if (stringr::str_detect(x, "TO")) {
      range_match <- stringr::str_extract(
        x,
        "TO\\s*([0-9]+\\.?[0-9]*)\\s*(H|HR|HRS|HOUR|HOURS)",
        group = 1
      )
      if (!is.na(range_match)) {
        return(range_match)
      }
    }

    # Extract hour information
    hour_match <- stringr::str_extract(
      x,
      "([0-9]+\\.?[0-9]*)\\s*(H|HR|HRS|HOUR|HOURS)",
      group = 1
    )
    if (!is.na(hour_match)) {
      return(as.numeric(hour_match))
    }

    # Extract minute information
    min_match <- stringr::str_extract(
      x,
      "([0-9]+\\.?[0-9]*)\\s*(M|MIN|MINS|MINUTE|MINUTES)",
      group = 1
    )
    if (!is.na(min_match)) {
      return(as.numeric(min_match) / 60)
    }

    # Default to NA if no pattern matches
    NA
  }

  out <- data.frame(
    tpt,
    as.numeric(sapply(tpt, extract_ntime))
  )
  colnames(out) <- c(tpt_name, "NTIME")

  out
}


#' Make ntime lookup table from TPTNUM field
#'
#' @param obj A data frame containing time point text descriptions.
#' @param domain The domain code as character (default: "PC" for
#'   pharmacokinetic). Used to determine the column name containing time point
#'   descriptions.
#'
#' @returns A data frame with a column representing the unique values of the
#'   xxTPT variable and a NTIME column with the time in hours.
#' @noRd
make_ntime_from_tptnum <- function(obj, domain = NULL) {
  if (is.null(domain)) {
    domain <- unique(obj$DOMAIN)[1]
  }
  tpt_name <- paste0(toupper(domain), "TPTNUM")

  # Extract the TPT values
  tptnum <- unique(obj[[tpt_name]])
  if (all(is.null(tptnum))) {
    return(NULL)
  }

  out <- data.frame(
    tpt_name = tptnum,
    NTIME = tptnum
  ) |>
    arrange(tpt_name)

  colnames(out) <- c(tpt_name, "NTIME")

  out
}


#' Make ntime lookup table from DY field
#'
#' @param obj A data frame.
#' @param domain the domain as character.
#'
#' @returns A data frame.
#' @noRd
make_ntime_from_dy <- function(obj, domain = NULL) {
  validate_char_param(domain, "domain", allow_null = TRUE)

  if (is.null(domain)) {
    domain <- unique(obj$DOMAIN)[1]
  }

  tpt_name <- paste0(toupper(domain), "DY")
  if (!tpt_name %in% names(obj)) {
    stop(paste0(tpt_name, " not found in input data!"))
  }

  tpt <- unique(obj[[tpt_name]])

  if (!is.numeric(tpt)) {
    stop(paste0(tpt_name, " must be numeric!"))
  }

  out <- data.frame(
    x = tpt,
    NTIME = (tpt - 1) * 24
  ) |>
    arrange(tpt_name)

  colnames(out) <- c(tpt_name, "NTIME")
  out
}


#' Make ntime lookup table from VISITDY field
#'
#' @param obj A data frame.
#' @param domain the domain as character.
#'
#' @returns A data frame.
#' @noRd
make_ntime_from_visitdy <- function(obj, domain = NULL) {
  validate_char_param(domain, "domain", allow_null = TRUE)

  if (!"DOMAIN" %in% names(obj))
    stop("Missing DOMAIN field!")

  if (is.null(domain)) {
    domain <- unique(obj$DOMAIN)[1]
  }

  tpt_name <- "VISITDY"
  if (!tpt_name %in% names(obj)) {
    stop(paste0(tpt_name, " not found in input data!"))
  }

  tpt <- unique(obj[[tpt_name]])

  out <- data.frame(
    x = tpt,
    NTIME = (tpt - 1) * 24
  ) |>
    arrange(tpt_name)

  colnames(out) <- c(tpt_name, "NTIME")
  out
}


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
#' @noRd
make_ntime <- function(
  obj,
  domain = NULL,
  include_day = FALSE,
  silent = NULL
) {
  pull_column <- function(col_tail) {
    temp <- obj |>
      select(ends_with(col_tail))

    if (ncol(temp) == 0) {
      return(NULL)
    }

    if (ncol(temp) > 1) {
      stop(paste0("multiple columns ending with ", col_tail, " found!"))
    }

    temp <- obj |>
      pull(ends_with(col_tail))
    if (length(temp) == 0) {
      return(NULL)
    }
    temp
  }

  # input validation
  if (!inherits(obj, "data.frame")) {
    stop("object must be a data frame!")
  }

  # determine domain if not provided
  if (is.null(domain)) {
    if (!"DOMAIN" %in% names(obj)) {
      stop("domain not provided and DOMAIN field not found in object!")
    }
    domain <- unique(obj$DOMAIN)
    if (length(domain) != 1) {
      stop("DOMAIN must be a single value!")
    }
  }

  eltm_name <- paste0(toupper(domain), "ELTM")
  dy_name <- paste0(toupper(domain), "DY")

  eltm <- pull_column(eltm_name)
  dy <- pull_column(dy_name)

  if (is.null(eltm)) {
    conditional_message(
      eltm_name,
      " is not defined. Provide a NTIME lookup table to define nominal time!",
      silent = silent
    )
    return(NULL)
  }

  if (include_day == TRUE) {
    out <- data.frame(
      eltm,
      dy
    ) |>
      mutate(NTIME = pt_to_hours(eltm) + trialday_to_day(.data$dy) * 24)
    names(out) <- c(eltm_name, dy_name, "NTIME")
  } else {
    out <- data.frame(eltm) |>
      mutate(NTIME = pt_to_hours(eltm))
    names(out) <- c(eltm_name, "NTIME")
  }

  distinct(out)
}


#' Compile observation data frame
#'
#' @description
#' Create a data frame of observations from a SDTM domain specified by the
#' `domain` argument where the dependent variable comes from the `dv_field`
#' argument and the timing information from the `dtc_field` argument.
#'
#' The 'TIME' in the output is `NA` throughout and needs to be calculated based
#' on administration time point information provided separately.
#'
#' If the 'ntime_lookup' parameter is provided, 'NTIME' can be derived from a
#' field contained in the input data set, e.g., 'PCELTM' (see the code
#' examples). Otherwise, 'NTIME' will be `NA`.
#'
#' @param sdtm A sdtm object. Needs at least the 'DM' and 'VS' domains, and the
#'   domain the observations come from.
#' @param domain The domain as character.
#' @param testcd The observation variable, as character.
#' @param analyte The name for the analyte. Defaults to the 'testcd', if NULL.
#' @param parent The name of the parent analyte for the observation as
#'   character. Defaults to the respective treatment administered before the
#'   observation, if NULL.
#' @param metabolite Observation is a metabolite, as logical.
#' @param cmt The compartment for the observation as numeric.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param observation_filter The filtering to apply to the observation source
#'   data.
#' @param testcd_field The xxTESTCD field. Defaults to the two-character domain
#'   name followed by 'TESTCD', if NULL.
#' @param dtc_field The field to use as the date-time code for the observation.
#'   Defaults to the two-character domain name followed by 'DTC', if NULL.
#' @param dv_field the field to use as the dependent variable. Defaults to the
#'   two-character domain name followed by 'STRESN', if NULL.
#' @param coding_table Coding table to translate a categorical values into
#'   numerical values, as data frame. The data frame must have at least one
#'   column that matches a column in the domain, and a numerical 'DV' column
#'   that provides the recoding result.
#' @param factor Multiplier for the DV field, as numeric.
#' @param ntime_lookup A data frame with two columns, a column that defines the
#'   custom nominal time information in the target domain (e.g., 'PCELTM'), and
#'   'NTIME'. This data frame is left_joined into the observation data frame
#'   to provide the NTIME field.
#' @param keep Columns to keep, as character.
#' @param silent Suppress messages, as logical. Defaults to nif_option setting
#'   if NULL.
#' @param ntime_method the field to derive the nominal time from. Allowed values
#'   are 'TPT', 'TPTNUM', 'ELTM', 'VISITDY' and 'DY'. Defaults to xxTPT where xx
#'   is the domain name.
#' @param include_day_in_ntime as logical.
#' @param cat xxCAT filter to apply, as character.
#' @param scat xxSCAT filter to apply, as character.
#' @param omit_not_done Delete rows where xxSTAT is "NOT DONE, as logical.
#' @param na_to_zero Set all NA values of DV to 0, as logical.
#'
#' @return A data frame.
#' @keywords internal
#' @export
#' @import stringr
#' @import cli
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
  cat = NULL,
  scat = NULL,
  testcd_field = NULL,
  dtc_field = NULL,
  dv_field = NULL,
  coding_table = NULL,
  factor = 1,
  ntime_lookup = NULL,
  ntime_method = "TPT",
  keep = NULL,
  include_day_in_ntime = FALSE,
  omit_not_done = TRUE,
  silent = NULL,
  na_to_zero = FALSE
) {
  # validate inputs
  validate_char_param(domain, "domain")
  validate_sdtm(sdtm, domain)
  validate_char_param(testcd, "testcd")
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(parent, "parent", allow_null = TRUE)

  validate_char_param(cat, "cat", allow_null = TRUE)
  validate_char_param(scat, "scat", allow_null = TRUE)

  validate_logical_param(omit_not_done, "omit_not_done")
  # other validations not implemented - add_observation takes care of that.
  validate_logical_param(na_to_zero, "na_to_zero")

  domain_name <- tolower(domain)

  # validate ntime method
  allowed_ntime_method <- c("TPT", "TPTNUM", "ELTM", "VISITDY", "DY")
  if (!ntime_method %in% allowed_ntime_method) {
    stop(paste0(
      "ntime_method must be one of ",
      nice_enumeration(allowed_ntime_method, conjunction = "or")
    ))
  }

  # Create fields
  if (is.null(dtc_field)) dtc_field <- paste0(toupper(domain), "DTC")
  if (is.null(dv_field)) dv_field <- paste0(toupper(domain), "STRESN")
  if (is.null(testcd_field)) testcd_field <- paste0(toupper(domain), "TESTCD")

  cat_field <- paste0(toupper(domain), "CAT")
  scat_field <- paste0(toupper(domain), "SCAT")

  if (is.null(analyte)) analyte <- testcd
  if (is.null(parent)) parent <- analyte

  # Get subject data
  tryCatch(
    {
      sbs <- make_subjects(
        domain(sdtm, "dm"), domain(sdtm, "vs"), subject_filter, keep
      )
    },
    error = function(e) {
      stop(paste0("Error getting subject data: ", e$message))
    }
  )

  # Get observation data
  obj <- domain(sdtm, domain_name) |>
    lubrify_dates()

  # Check whether required fields exist
  required_fields <- c(testcd_field, dtc_field)
  missing_fields <- required_fields[!required_fields %in% names(obj)]
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Required field(s) missing in domain '", domain_name, "': ",
      nice_enumeration(missing_fields)
    ))
  }

  # Check if DV field exists when no coding table
  if (!dv_field %in% names(obj) && is.null(coding_table)) {
    stop(paste0(
      "DV field '", dv_field,
      "' not found in domain and no coding table provided"
    ))
  }

  # Create NTIME lookup table if not provided
  if (is.null(ntime_lookup)) {
    if (ntime_method == "TPT") {
      ntime_lookup <- make_ntime_from_tpt(obj, domain)
    }
    if (ntime_method == "TPTNUM") {
      ntime_lookup <- make_ntime_from_tptnum(obj, domain)
    }
    if (ntime_method == "ELTM") {
      ntime_lookup <- make_ntime(
        obj, domain,
        include_day = FALSE, silent = silent
      )
    }
    if (ntime_method == "VISITDY") {
      ntime_lookup <- make_ntime_from_visitdy(obj, domain)
    }
    if (ntime_method == "DY") {
      ntime_lookup <- make_ntime_from_dy(obj, domain)
    }
    if (is.null(ntime_lookup)) {
      conditional_cli(
        cli_alert_warning("No ntime_lookup could be created, NTIME will be NA"),
        silent = silent
      )
    }
  } else { # in case a lookup table is provided
    # Validate ntime_lookup structure
    if (!is.data.frame(ntime_lookup)) {
      stop("ntime_lookup must be a data frame")
    }
    if (!"NTIME" %in% names(ntime_lookup)) {
      stop("ntime_lookup must contain a 'NTIME' column")
    }
    if (length(
      intersect(
        names(ntime_lookup),
        names(obj)
      )
    ) < 1
    ) {
      stop(paste(
        "ntime_lookup must contain at least one column that matches",
        "a column in the domain data"
      ))
    }
  }

  # apply coding table, if not NULL
  if (!is.null(coding_table)) {
    if (!any(names(coding_table) %in% names(obj))) {
      stop("Coding table cannot be applied to data set!")
    }
    if (!is.numeric(coding_table$DV)) {
      stop("DV field in coding table must be numeric!")
    }
    # Capture warnings instead of suppressing them
    join_msgs <- capture.output(type = "message", {
      obj <- obj |>
        left_join(coding_table)
    })
    if (length(join_msgs) > 0) {
      conditional_message(
        "Warnings during coding table join: ",
        paste(join_msgs, collapse = "\n"),
        silent = silent
      )
    }
  } else { # proceed without coding table
    obj <- obj |>
      mutate(DV = .data[[dv_field]] * factor)
  }

  # set NA values to zero, if flag is set
  if (na_to_zero == TRUE) {
    obj <- obj |>
      mutate(DV = case_when(is.na(.data$DV) ~ 0, .default = .data$DV))
  }

  # apply observation filter, add debug fields
  filtered_obj <- obj |>
    mutate(SRC_DOMAIN = .data$DOMAIN) |>
    filter(eval(parse(text = observation_filter)))

  if (paste0(toupper(domain), "SEQ") %in% names(obj)) {
    filtered_obj <- mutate(
      filtered_obj,
      SRC_SEQ = .data[[paste0(toupper(domain), "SEQ")]]
    )
  } else {
    filtered_obj <- mutate(filtered_obj, SRC_SEQ = NA)
  }

  # Raise error if observation_filter returns no entries
  if (nrow(filtered_obj) == 0) {
    stop("No entries found after applying observation filter!")
  }

  # filter for NOT DONE
  stat_field <- paste0(toupper(domain), "STAT")
  if (omit_not_done == TRUE && stat_field %in% names(filtered_obj)) {
    not_done <- filtered_obj |>
      filter(.data[[stat_field]] == "NOT DONE")
    if (nrow(not_done) > 0) {
      conditional_cli(
        cli({
          cli_alert_info(paste0(
            nrow(not_done), " observations for ", analyte, " with ", stat_field,
            " of 'NOT DONE' omitted"
          ))
        }),
        silent = silent
      )
    }
    filtered_obj <- filtered_obj |>
      filter(.data[[stat_field]] != "NOT DONE")

    if (nrow(filtered_obj) == 0) {
      stop("No entries found after omitting 'NOT DONE' entries")
    }
  }

  filtered_obj <- filtered_obj |>
    apply_cat_filter(cat, cat_field) |>
    apply_cat_filter(scat, scat_field)

  if (nrow(filtered_obj) == 0)
    stop("No entries found after applying cat and scat filters")

  # create further fields
  out <- filtered_obj |>
    filter(.data[[testcd_field]] == testcd) |>
    mutate(
      DTC = .data[[dtc_field]],
      ANALYTE = analyte,
      TIME = NA,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = metabolite,
      EVID = 0,
      MDV = as.numeric(is.na(.data$DV)),
      IMPUTATION = ""
    )

  join_variables <- intersect(names(ntime_lookup), names(obj))

  # apply NTIME lookup table
  if (!is.null(ntime_lookup)) {
    out <- left_join(out, ntime_lookup, by = join_variables)
  } else {
    out <- out |>
      mutate(NTIME = NA)
  }

  # Include day into NTIME
  if (include_day_in_ntime == TRUE) {
    dy_name <- paste0(toupper(domain), "DY")
    if (!dy_name %in% names(out)) {
      stop(paste0(
        dy_name, " not found in domain, day cannot be included in observation"
      ))
    }
    out <- out |>
      mutate(NTIME = .data$NTIME + trialday_to_day(out[[dy_name]]) * 24)
  }

  # merge subject-level information
  if ("STUDYID" %in% names(sbs) && "STUDYID" %in% names(out)) {
    out <- inner_join(out, sbs, by = c("USUBJID", "STUDYID"))
  } else {
    out <- inner_join(out, sbs, by = c("USUBJID"))
  }

  out |>
    group_by(.data$USUBJID) |>
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFSTDTC))),
      units = "days"
    ) + 1) |>
    ungroup() |>
    filter(!is.na(.data$DTC))
}


#' Add observation events to nif
#'
#' Add rows to a [nif::nif] object that represent observation events, i.e., with
#' EVID values of 0. This is usually the second step in the creation of NIF data
#' tables, after [nif::add_administration()].
#'
#' Observations can be pharmacokinetic observations (i.e., from the PC domain),
#' or any other type of observation from any other SDTM domain. The `domain` and
#' `testcd` arguments specify the source of the observation events.
#'
#' In general, the dependent variable and the observation time stamp are taken
#' from the 'xxSTRESN' and 'xxDTC' fields of the source domain (where xx refers
#' to the domain code). Differing fields can be specified by the `dv_field` and
#' `dtc_field` arguments
#'
#' Observation events can be attached to an administered drug by the `parent`
#' argument. Specifying the respective parent ANALYTE code links these
#' observations to the respective drug administration event. This is required
#' for the calculation of time-after-dose ('TAD') and time-after-first-dose
#' ('TAFD') for observations. If no `parent` is specified, the most likely
#' parent is automatically selected. This usually works well for studies where
#' only one treatment is administered. In other cases, it may be beneficial to
#' explicitly define a parent for all observations.
#'
#' Observations can be further specified with `cat` and `scat` arguments that
#' refer to the 'xxCAT' and 'xxSCAT' fields of the source domain (xx), and the
#' `observation_filter` argument. This may be necessary, when observations
#' defined by the `testcd` alone are ambiguous, e.g., when for pharmacokinetic
#' observations, both BLOOD and URINE observations are included in the PC domain
#' data.
#'
#' A model compartment can be specified by the `cmt` argument, or will be
#' automatically assigned otherwise.
#'
#' ## Nominal time
#'
#' While the actual time fields (i.e., TIME, TAFD, TAD) for the observation
#' events are automatically derived from the date-time stamp (the xxDTC field),
#' the nominal time of the observation is often not unambiguously represented in
#' the source SDTM data. Different methods are provided to derive the nominal
#' time (NTIME) field and can be selected using the `ntime_method` argument.
#'
#' * 'TPT' attempts to extract the nominal time from the 'xxTPT' field of the
#' SDTM data
#' * 'TPTNUM' interprets the 'xxTPTNUM' field as the numerical representation of
#' the
#' nominal observation time in hours. Note that depending on the SDTM generation
#' method, this may or may not be correct
#' * 'ELTM' uses the xxELTM (elapsed time) column, if available
#' * 'VISITDY' uses the VISIDY column to derive the nominal time. Note that the
#' (planned) visit day may differ from the actual day of the observation. As
#' this column has not time information, the granularity of the observation is
#' full days.
#' * 'DY' uses the 'xxDY' columns of the original SDTM data. As this
#' column has not time information, the granularity of the observation is full
#' days.
#'
#' Alternatively, a lookup table data frame can be provided by the
#' `ntime_lookup` argument that associates any column in the source SDTM data
#' with the respective NTIME.
#'
#' ## Recoding
#'
#' For some observation types, the source SDTM data may not include a dependent
#' variable in numerical form (i.e., for AE observations when AETOXGR is
#' provided in text form). In such cases, a `coding_table` can be provided that
#' links the values of an arbitrary columns to the expected 'DV' value.
#'
#' ## Duplicate observations
#'
#' SDTM data may contain duplicate observations, most often because of
#' incomplete data cleaning. [nif::add_observation()] provides methods to deal
#' with duplicates, see the description of the `duplicates` argument.
#'
#' ## Further information
#'
#' For an overview on the representation of observation events in NONMEM
#' Input Format compliant data sets, see Bauer, R.J. (2019), NONMEM Tutorial
#' Part I: Description of Commands and Options, With Simple Examples of
#' Population Analysis. CPT Pharmacometrics Syst. Pharmacol., 8: 525-537.
#' <https://doi.org/10.1002/psp4.12404>.
#'
#' To add administration events to the [nif::nif] object, see
#' [nif::add_administration()].
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
#' @param duplicate_identifier Fields by which duplicates are identified (after
#'   addition of the observations to the nif object), defaults to "DTC".
#'   Consider also "NTIME", or any other custom field.
#' @param na_rm Logical indicating whether to remove NA values when applying the
#'   duplicate_function. Defaults to TRUE.
#'
#' @return A nif object.
#' @seealso [nif::add_administration()]
#' @export
#' @import cli
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
  cat = NULL,
  scat = NULL,
  testcd_field = NULL,
  dtc_field = NULL,
  dv_field = NULL,
  coding_table = NULL,
  factor = 1,
  ntime_lookup = NULL,
  ntime_method = "TPT",
  keep = NULL,
  debug = FALSE,
  include_day_in_ntime = FALSE,
  silent = NULL,
  duplicates = "stop",
  duplicate_function = mean,
  duplicate_identifier = "DTC",
  omit_not_done = TRUE,
  na_rm = TRUE,
  na_to_zero = FALSE
) {
  # validate inputs
  validate_min_nif(nif)
  validate_sdtm(sdtm)
  validate_testcd(sdtm, testcd, domain)

  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(parent, "parent", allow_null = TRUE)
  validate_logical_param(metabolite, "metabolite")
  validate_numeric_param(cmt, "cmt", allow_null = TRUE)
  validate_char_param(subject_filter, "subject_filter")
  validate_char_param(observation_filter, "observation_filter")
  validate_char_param(testcd_field, "testcd_field", allow_null = TRUE)
  validate_char_param(dtc_field, "dtc_field", allow_null = TRUE)
  validate_char_param(dv_field, "dv_field", allow_null = TRUE)
  validate_numeric_param(factor, "factor")
  validate_char_param(ntime_method, "ntime_method", allow_null = TRUE)
  validate_char_param(keep, "keep", allow_null = TRUE, allow_multiple = TRUE)
  validate_logical_param(debug, "debug")
  validate_logical_param(include_day_in_ntime, "include_day_in_ntime")
  validate_logical_param(silent, "silent", allow_null = TRUE)
  validate_char_param(duplicates, "duplicates")
  validate_char_param(duplicate_identifier, "duplicate_identifier",
                      allow_multiple = TRUE)

  validate_logical_param(na_to_zero, "na_to_zero")

  debug <- isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if (isTRUE(debug)) {
    keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")
  }

  # validate duplicate handler arguments
  valid_duplicate_values <- c("stop", "ignore", "identify", "resolve")
  if (!duplicates %in% valid_duplicate_values) {
    stop(paste0(
      "Invalid value for 'duplicates' - must be one of ",
      nice_enumeration(valid_duplicate_values, conjunction = "or")
    ))
  }

  missing_dupl_id <- setdiff(duplicate_identifier, names(nif))
  if (length(missing_dupl_id) > 0) {
    stop(paste0(
      "Missing ", plural("field", length(missing_dupl_id) > 1),
      " in input: ", nice_enumeration(missing_dupl_id)
    ))
  }

  # ensure that keep includes all fields already present in the nif
  keep <- unique(c(keep, names(nif), duplicate_identifier))

  # ensure analytes
  nif <- nif |>
    ensure_analyte()

  if (length(parents(nif)) == 0) {
    stop("Please add at least one administration first!")
  }

  # Test if compartment is already assigned
  if (!is.null(cmt)) {
    if (cmt %in% unique(nif$CMT)) {
      warning(paste0("Compartment ", cmt, " is already assigned!"))
    }
  }

  # Assign compartment for observation if CMT == NULL
  if (is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_cli(
      cli_alert_info(paste0(
        "Compartment for ", testcd, " set to ", cmt
      )),
      silent = silent
    )
  }

  if (is.null(analyte)) {
    analyte <- testcd
  }

  # If null, set parent to placeholder. The parent field will be later completed
  # based on the last administration before the observation
  if (is.null(parent)) {
    parent <- "."
  }

  observation <- make_observation(
    sdtm, domain, testcd, analyte, parent, metabolite, cmt, subject_filter,
    observation_filter, cat, scat, testcd_field, dtc_field, dv_field,
    coding_table, factor, ntime_lookup, ntime_method, keep,
    include_day_in_ntime = include_day_in_ntime, omit_not_done = omit_not_done,
    silent = silent, na_to_zero = na_to_zero
  ) |>
    select(any_of(c(standard_nif_fields, "IMPUTATION", keep)))

  # Duplicate handling
  dupl_fields <- c("USUBJID", "ANALYTE", duplicate_identifier)

  n_dupl <- find_duplicates(observation, fields = dupl_fields,
                            count_only = TRUE)

  if (n_dupl != 0) {
    if (duplicates == "stop") {
      stop(paste0(
        n_dupl, " duplicate ",
        plural("observation", n_dupl > 1), " found with respect to ",
        nice_enumeration(dupl_fields), ".\n\n",
        "Identify duplicates using the `duplicates = 'identify'` parameter, ",
        "or have duplicates automatically resolved with ",
        "`duplicates = 'resolve'` where the resolution function is specified ",
        "by the `duplicate_function` parameter (default is `mean`)."
      ))
    }

    if (duplicates == "identify") {
      cli::cli({
        cli::cli_alert_danger("Only duplicate observations returned")
        cli::cli_text(paste0(
          n_dupl, " duplicate observations found with respect to ",
          nice_enumeration(dupl_fields), "."
        ))
        cli_text()
      })

      d <- find_duplicates(observation, fields = dupl_fields)
      original_domain <- domain(sdtm, domain)

      temp <- original_domain |>
        mutate(DTC = .data[[paste0(toupper(domain), "DTC")]]) |>
        mutate(ANALYTE = .data[[paste0(toupper(domain), "TESTCD")]]) |>
        lubrify_dates() |>
        inner_join(d[, dupl_fields], by = dupl_fields) |>
        distinct()

      return(temp)
    }

    if (duplicates == "resolve") {
      observation <- resolve_duplicates(
        observation,
        fields = dupl_fields,
        duplicate_function = duplicate_function,
        na_rm = na_rm
      )
      conditional_cli(
        cli::cli({
          cli_alert_warning(paste0(
            n_dupl, " duplicate observations for ", testcd,
            " (analyte ", analyte, ") resolved, applying ",
            function_name(duplicate_function)
          ))
        }),
        silent = silent
      )
    }

    if (duplicates == "ignore") {
      conditional_cli(
        {
          cli_alert_warning("Duplicates in the data set!")
          cli_text(paste0(
            n_dupl, " duplicates in ", testcd, " (", analyte, ") observations ",
            "and kept in the data set as duplicates!)"
          ))
        },
        silent = silent
      )
    }
  }

  obj <- bind_rows(nif, observation) |>

    arrange(.data$USUBJID, .data$DTC) |>
    mutate(ID = as.numeric(as.factor(.data$USUBJID))) |>

    mutate(PARENT = case_when(.data$PARENT == "." ~ NA,
                              .default = .data$PARENT)) |>
    mutate(.current_admin = case_when(.data$EVID == 1 ~ .data$ANALYTE,
                                      .default = NA)) |>
    group_by(.data$USUBJID) |>
    fill(".current_admin", .direction = "downup") |>
    ungroup() |>
    mutate(PARENT = case_when(is.na(.data$PARENT) ~ .current_admin,
                              .default = .data$PARENT)) |>
    select(-c(".current_admin")) |>
    group_by(.data$USUBJID, .data$PARENT) |>
    mutate(NO_ADMIN_FLAG = case_when(
      sum(.data$EVID == 1) == 0 ~ TRUE,
      .default = FALSE
    )) |>
    ungroup()

  n_no_admin <- sum(obj$NO_ADMIN_FLAG == TRUE)
  if (n_no_admin != 0) {
    conditional_cli(
      {
        cli_alert_warning("Missing parent!")
        cli_text(paste0(
          "Missing administration information in ",
          n_no_admin, " observations (did you set a ",
          "parent for these observations?)"
        ))
        cli_verbatim(df_to_string(
          obj |>
            filter(.data$NO_ADMIN_FLAG == TRUE) |>
            group_by(.data$USUBJID, .data$PARENT, .data$ANALYTE) |>
            mutate(N = sum(.data$EVID == 0)) |>
            ungroup() |>
            distinct(.data$USUBJID, .data$PARENT, .data$ANALYTE, .data$N),
          indent = 2, abbr_lines = 5, abbr_threshold = 20
        ))
        cli_text()
      },
      silent = silent
    )

    obj <- obj |>
      filter(.data$NO_ADMIN_FLAG == 0)
  }

  obj |>
    select(-c("NO_ADMIN_FLAG")) |>
    index_id() |>
    nif() |>
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
#' @param cmt The compartment for the analyte as numeric.
#' @param observation_filter Filter term, as character.
#' @param usubjid_field The field specifying the USUBJID, as character.
#' @param dtc_field The field specifying the DTC, as character.
#' @param ntime_field The field specifying the NTIME, as character.
#' @param dv_field The field specifying the dependent variable, as character.
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
  usubjid_field = "USUBJID",
  dtc_field = NULL,
  ntime_field = NULL,
  dv_field = NULL,
  keep = NULL,
  debug = FALSE,
  silent = NULL
) {
  # validate inputs
  validate_min_nif(nif)
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(parent, "parent", allow_null = TRUE)
  validate_numeric_param(cmt, "cmt", allow_null = TRUE)
  validate_char_param(observation_filter, "observation_filter")
  validate_char_param(dtc_field, "dtc_field", allow_null = TRUE)
  validate_char_param(ntime_field, "ntime_field", allow_null = TRUE)
  validate_char_param(dv_field, "dv_field", allow_null = TRUE)
  validate_char_param(keep, "keep", allow_null = TRUE, allow_multiple = TRUE)
  validate_logical_param(debug, "debug")
  validate_logical_param(silent, "silent", allow_null = TRUE)

  debug <- isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if (isTRUE(debug)) keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  nif <- nif |>
    ensure_parent()

  if (!all(c(dv_field, usubjid_field) %in% names(raw))) {
    stop(paste0(
      "ERROR: DV field (", dv_field, ") and USUBJID field (", usubjid_field,
      ") must both be present in the input data frame!"
    ))
  }

  if (!any(c(ntime_field, dtc_field) %in% names(raw))) {
    stop(paste0(
      "ERROR: One of the time fields (",
      nice_enumeration(c(ntime_field, dtc_field), conjunction = "or"),
      ") must be present in the input data frame!"
    ))
  }

  if (is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_cli(
      {
        cli_alert_info(paste0(
          "Compartment for ", analyte, " set to ", cmt
        ))
      },
      silent = silent
    )
  }

  imp <- nif |>
    as.data.frame() |>
    filter(.data$EVID == 1) |>
    distinct(.data$ANALYTE) |>
    pull(.data$ANALYTE)

  if (is.null(parent)) {
    if (analyte %in% imp) {
      parent <- analyte
    } else {
      parent <- guess_parent(nif)
      conditional_cli(
        cli_inform(paste0("Parent for ", analyte, " set to ", parent)),
        silent = silent
      )
    }
  }

  sbs <- nif |>
    filter(.data$EVID == 1) |>
    select(
      # "USUBJID", "ID", any_of(fillable_nif_fields),
      any_of(c("ID", "USUBJID", "AGE", "SEX", "RACE", "HEIGHT", "WEIGHT",
               "BMI")),
      starts_with("BL_"),
      any_of(keep)
    ) |>
    select(!any_of(c("SRC_DOMAIN", "SRC_SEQ"))) |>
    mutate(IMPUTATION = "") |>
    distinct()

  filtered_raw <- raw |>
    filter(eval(parse(text = observation_filter)))

  # Add warning if subject_filter returns no entries
  if (nrow(filtered_raw) == 0) {
    stop("The observation_filter '", observation_filter,
         "' returned no entries.")
  }

  obs <- filtered_raw |>
    mutate(USUBJID = .data[[usubjid_field]])

  if (!is.null(ntime_field))
    obs <- mutate(obs, NTIME = .data[[ntime_field]])

  obs <- obs |>
    mutate(DV = .data[[dv_field]]) |>
    select(any_of(c("USUBJID", "NTIME", "DV", dtc_field, keep))) |>
    mutate(
      ANALYTE = analyte,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = FALSE,
      EVID = 0,
      MDV = as.numeric(is.na(.data$DV))
    ) |>
    inner_join(sbs, by = "USUBJID") |>
    mutate(
      SRC_DOMAIN = "IMPORT",
      SRC_SEQ = NA
    ) |>
    distinct()

  # derive time from DTC, if present, or generate DTC from first administration
  #   time in nif object
  if (!is.null(dtc_field)) {
    obs <- obs |>
      mutate(DTC = .data[[dtc_field]]) |>
      lubrify_dates()
  } else {
    obs <- obs |>
      left_join(first_admin_dtc(nif), by = "USUBJID") |>
      mutate(DTC = .data$FIRSTDTC + duration(hours = .data$NTIME)) |>
      select(!all_of("FIRSTDTC")) |>
      mutate(IMPUTATION = paste0("DTC derived from ", ntime_field))
  }

  bind_rows(
    nif,
    obs
  ) |>
    normalize_nif(keep = keep)
}
