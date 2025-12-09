#' SDTM class constructor, creating a sdtm object from a set of SDTM domains
#'
#' @param sdtm_data The SDTM domains as list of data frames.
#'
#' @import dplyr
#' @return A sdtm object.
#' @export
new_sdtm <- function(
    sdtm_data#,
    # analyte_mapping = data.frame(),
    # metabolite_mapping = data.frame(),
    # parent_mapping = data.frame(),
    # time_mapping = data.frame()
  ) {
  domains <- sdtm_data

  analyte_mapping <- data.frame()
  metabolite_mapping <- data.frame()
  parent_mapping <- data.frame()
  time_mapping <- data.frame()

  temp <- list(
    domains = domains,
    analyte_mapping = analyte_mapping,
    metabolite_mapping = metabolite_mapping,
    parent_mapping = parent_mapping,
    time_mapping = time_mapping
  )

  class(temp) <- c("sdtm", "list")
  temp
}


#' Trial title as reported in TS domain
#'
#' @param obj A sdtm object.
#'
#' @returns The title or NULL.
#' @export
#'
#' @examples
#' trial_title(examplinib_sad)
trial_title <- function(obj) {
  validate_sdtm(obj)

  domains <- toupper(names(obj$domains))
  if(!"TS" %in% domains) return(NULL)
  ts <- domain(obj, "ts")
  if(!"TSPARMCD" %in% names(ts)) return(NULL)
  if(!"TITLE" %in% unique(ts$TSPARMCD)) return(NULL)

  title <- ts %>%
    filter(.data$TSPARMCD == "TITLE") %>%
    select(matches("^TSVAL[1-8]?$")) %>%
    as.character() %>%
    paste(collapse = " ") %>%
    str_trim()

  return(title)
}


#' Trial data cut-off date as reported in TS domain
#'
#' @param obj A sdtm object.
#'
#' @returns A POSIXct object or NULL.
#' @export
trial_dco <- function(obj) {
  validate_sdtm(obj)

  domains <- toupper(names(obj$domains))
  if(!"TS" %in% domains) return(NULL)
  ts <- domain(obj, "ts")
  if(!"TSPARMCD" %in% names(ts)) return(NULL)
  if(!"DCUTDTC" %in% unique(ts$TSPARMCD)) return(NULL)
  dco <- ts$TSVAL[ts$TSPARMCD == "DCUTDTC"]
  if(length(dco) > 1) warning("TS domain inclucdes multiple data cut-off dates!")
  return(lubridate::as_datetime(dco, format = dtc_formats))
}


#' SDTM summary
#'
#' This function returns a named list of properties of the SDTM object:
#'
#' * `study` The study identifier as character.
#' * `subjects` The USUBJIDs as character.
#' * `domains` The included domains by their SDTM code and the number of
#'   respective unique USUBJID, as data frame,
#' * `treatments` The unique `EXTRT` as character.
#' * `arms` The unique `ACTAMCD` and `ACTARM` as data frame.
#' * `doses` The unique `EXTRT` and `EXDOSE` as data frame.
#' * `specimens` The unique `PCSPEC` as character.
#' * `analytes` The unique `PCTEST` and `PCTESTCD` as data frame.
#' * `pc_timepoints` The unique `PCTPT` and `PCTPTNUM` as data frame.
#' * `analyte_mapping` The analyte mapping as data frame.
#' * `metabolite_mapping` The metabolite mapping as data frame.
#' * `time_mapping` The time mapping as data frame.
#'
#' @param object A SDTM object.
#' @param ... Further parameters.
#' @return A sdtm_summary object.
#' @export
#' @keywords internal
#' @examples
#' summary(examplinib_poc)
summary.sdtm <- function(object, ...) {
  # validate input
  validate_sdtm(object)

  # Initialize empty output
  out <- list(
    study = character(0),
    subjects = character(0),
    treatments = character(0),
    arms = data.frame(ACTARMCD = character(0), ACTARM = character(0)),
    doses = data.frame(EXTRT = character(0), EXDOSE = numeric(0)),
    specimens = character(0),
    analytes = data.frame(PCTEST = character(0), PCTESTCD = character(0)),
    analyte_mapping = object$analyte_mapping,
    metabolite_mapping = object$metabolite_mapping,
    time_mapping = object$time_mapping,
    hash = hash(object),
    last = last_dtc(object),
    title = trial_title(object),
    dco = trial_dco(object)
  )

  # Numbers of subjects and observations by domain
  out$disposition <- purrr::map(
    object$domains,
    # function(x){
    #   data.frame(
    #     SUBJECTS = length(unique(x$USUBJID)),
    #     OBSERVATIONS = dim(x)[1]
    #   )
    # }

    function(x){
      if("USUBJID" %in% names(x)){
        data.frame(
          SUBJECTS = length(unique(x$USUBJID)),
          OBSERVATIONS = dim(x)[1]
        )
      } else {
        data.frame(SUBJECTS = 0, OBSERVATIONS = 0)
      }
    }

  ) %>% purrr::list_rbind() %>%
    mutate(DOMAIN = names(object$domains)) %>%
    select(c("DOMAIN", "SUBJECTS", "OBSERVATIONS"))

  # Get data for DM domain if it exists
  if(has_domain(object, "dm")) {
    dm <- domain(object, "dm")

    # Get study ID if STUDYID exists
    if("STUDYID" %in% colnames(dm)) {
      out$study <- unique(dm$STUDYID)
    }

    # Get subjects if USUBJID exists
    if("USUBJID" %in% colnames(dm)) {
      out$subjects <- unique(dm$USUBJID)
    }

    # Get arms if ACTARMCD and ACTARM exist
    if(all(c("ACTARMCD", "ACTARM") %in% colnames(dm))) {
      out$arms <- unique(dm[c("ACTARMCD", "ACTARM")])
    }
  }

  # Get data for PC domain if it exists
  if(has_domain(object, "pc")) {
    pc <- domain(object, "pc")

    # Get specimens if PCSPEC exists
    if("PCSPEC" %in% colnames(pc)) {
      out$specimens <- unique(pc$PCSPEC)
    }

    # Get analytes if PCTEST and PCTESTCD exist
    if(all(c("PCTEST", "PCTESTCD") %in% colnames(pc))) {
      out$analytes <- unique(pc[c("PCTEST", "PCTESTCD")])
    }

    # Get PC timepoints if PCTPT and PCTPTNUM exist
    if(all(c("PCTPT", "PCTPTNUM") %in% colnames(pc))) {
      out$pc_timepoints <- unique(pc[c("PCTPT", "PCTPTNUM")])
    }
  }

  # Get data for EX domain if it exists
  if(has_domain(object, "ex")) {
    ex <- domain(object, "ex")

    # Get treatments if EXTRT exists
    if("EXTRT" %in% colnames(ex)) {
      out$treatments <- unique(ex$EXTRT)
    }

    # Get doses if EXTRT and EXDOSE exist
    if(all(c("EXTRT", "EXDOSE") %in% colnames(ex))) {
      out$doses <- unique(ex[c("EXTRT", "EXDOSE")])
    }
  }

  class(out) <- "summary_sdtm"
  return(out)
}


#' print SDTM summary
#'
#' @param x SDTM object
#' @param ... Further parameters
#' @return none
#' @export
#' @noRd
print.summary_sdtm <- function(x, color = FALSE, ...) {
  indent <- 2
  hline <- paste0(rep("-", 8), collapse = "")
  spacer <- paste0(rep(" ", indent), collapse = "")

  cat(paste(hline, "SDTM data set summary", hline, "\n"))
  cat(paste(
    plural("Study", length(x$study) > 1),
    nice_enumeration(x$study),
    "\n"
  ))

  if(!is.null(x$title))
    cat(paste0("\n", str_wrap(x$title, width = 80), "\n"))

  if(!is.null(x$dco))
    cat(paste0("\nDCO: ", x$dco, "\n"))

  cat("\nData disposition\n")
  cat(df_to_string(x$disposition,
                   color = color, indent = indent))
  cat("\n\n")

  cat("Arms (DM):\n")
  cat(paste0(df_to_string(x$arms,
    color = color, indent = indent,
    show_none = TRUE
  ), "\n\n"))

  if("ex" %in% tolower(x$disposition$DOMAIN)) {
    cat("Treatments (EX):\n")
    temp <- paste0(
      str_trim(x$treatments),
      collapse = ", ")
    temp <- ifelse(temp == "", "none", temp)
    cat(paste0(spacer, temp, "\n\n"))
  }

  if("pc" %in% tolower(x$disposition$DOMAIN)) {
    cat("PK sample specimens (PC):\n")
    temp <- paste0(x$specimens, collapse = ", ")
    temp <- ifelse(temp == "", "none", temp)
    cat(paste0(spacer, temp, "\n\n"))

    cat("PK analytes (PC):\n")
    cat(df_to_string(x$analytes,
      color = color, indent = indent,
      show_none = TRUE
    ), "\n\n")
  }

  # if("analyte_mapping" %in% names(x)) {
  #   if (nrow(x$analyte_mapping) != 0) {
  #     cat("Treatment-to-analyte mappings:\n")
  #     cat(df_to_string(x$analyte_mapping,
  #       color = color, indent = indent,
  #       show_none = TRUE
  #     ), "\n\n")
  #   }
  # }
  #
  # if("metabolite_mapping" %in% names(x)) {
  #   if (nrow(x$metabolite_mapping) != 0) {
  #     cat("Parent-to-metabolite mappings:\n")
  #     cat(df_to_string(x$metabolite_mapping,
  #       color = color, indent = indent,
  #       show_none = TRUE
  #     ), "\n\n")
  #   }
  # }
  #
  # if("time_mapping" %in% names(x)) {
  #   if (nrow(x$time_mapping) != 0) {
  #     cat("Time mappings:\n")
  #     cat(df_to_string(x$time_mapping,
  #       color = color, indent = indent,
  #       show_none = TRUE
  #     ), "\n\n")
  #   }
  # }

  cat(paste0("Hash: ", x$hash, "\n"))
  cat(paste0("Last DTC: ", x$last))

  invisible(x)
}


#' print() implementation for a sdtm object
#'
#' @param x A SDTM object.
#' @param ... Further parameters
#' @export
#' @noRd
print.sdtm <- function(x, ...) {
  print(summary(x))
}


#' Check whether a domain is present in an SDTM object
#'
#' @param obj The sdtm object.
#' @param name The domain name(s) to check as a character string or vector.
#' @return Logical indicating whether all specified domains exist in the SDTM object.
#' @export
#' @keywords internal
#' @examples
#' # Check if DM domain exists
#' has_domain(examplinib_fe, "dm")
#'
#' # Check if a non-existent domain exists
#' has_domain(examplinib_fe, "xyz")
#'
#' # Check if multiple domains exist
#' has_domain(examplinib_fe, c("dm", "vs"))
has_domain <- function(obj, name) {
  # Input validation
  if (!inherits(obj, "sdtm")) {
    stop("'obj' must be an SDTM object")
  }

  if (!is.character(name) || length(name) == 0) {
    stop("'name' must be a non-empty character vector")
  }

  # Normalize domain names to lowercase
  names_lower <- tolower(name)

  # Check if all domains exist
  all(names_lower %in% names(obj$domains))
}


#' Baseline details for specific subjects
#'
#' @param obj The object, either an SDTM or NIF object.
#' @param id The ID or USUBJID as numeric or character.
#' @export
#' @examples
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
#' subject_info(examplinib_poc_nif, 1)
#' unclass(subject_info(examplinib_poc_nif, 1))
#' subject_info(examplinib_poc_nif, 1)$administrations
subject_info <- function(obj, id) {
  UseMethod("subject_info")
}


#' Baseline details for specific subjects
#'
#' @inheritParams subject_info
#' @export
#' @keywords internal
#' @examples
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
subject_info.sdtm <- function(obj, id) {
  validate_sdtm(obj, "dm")
  validate_char_param(id, "id", allow_multiple = FALSE)

  temp <- obj %>%
    domain("dm") %>%
    dplyr::filter(.data$USUBJID %in% id) %>%
    as.list()
  class(temp) <- c("subject_info", "data.frame")
  return(temp)
}


#' Suggest data programming steps to generate a nif object from an sdtm object
#'
#' @param obj A sdtm object
#' @param consider_nif_auto Include suggestions regarding parent or metabolite
#'   mappings to the sdtm object, as logical.
#' @return Nothing.
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' suggest(examplinib_poc)
suggest <- function(obj, consider_nif_auto = FALSE) {
  # input validation
  validate_sdtm(obj)

  message_code <- function(fct, data, header="", footer="", collapse = "%>%") {
    # if(header != "") header <- paste0(indent_string(indent + 2), header, "\n")
    # if(footer != "") footer <- paste0("\n", indent_string(indent + 2), footer)

    lines <- sapply(data, fct)
    cli::cli_code(lines)
  }

  if(!has_domain(obj, c("dm", "ex", "pc")))
    stop("Domains DM, EX and PC must be present!")

  # # Domains
  pc <- domain(obj, "pc")

  # Function body
  n_suggestion <- 1

  sdtm_summary <- summary(obj)

  # Treatments
  cli::cli_h1(paste0("{n_suggestion}. Treatments"))
  n_suggestion <- n_suggestion + 1
  cli::cli_text(paste0("There are {length(sdtm_summary$treatments)} treatments ",
    "(EXTRT) in EX: {nice_enumeration(sdtm_summary$treatments)}. ",
    "Consider adding them to the nif object using `add_administration()`, ",
    "see the code snippet below (replace 'sdtm' with the name of your sdtm ",
    "object):"))
  cli::cli_text()

  message_code(
    function(x) {paste0( "  add_administration(sdtm, '", x, "') %>%")},
    sdtm_summary$treatments
  )

  # PK observations
  cli::cli_h1("{n_suggestion}. Pharmacokinetic observations")
  n_suggestion <- n_suggestion +1
  if(nrow(sdtm_summary$analytes) == 1) {
    cli::cli_text(paste0("There is one pharmacokinetic analyte: "))
  } else {
    cli::cli_text(paste0("There are {nrow(sdtm_summary$analytes)} ",
                         "pharmacokinetic analytes: "))
  }
  cli::cli_text()
  cli::cli_verbatim(df_to_string(sdtm_summary$analytes, indent = 2))
  cli::cli_text()
  cli::cli_text(paste0(
    "Consider adding them to the nif object using `add_observation()`, see the ",
    "code snippet below (replace 'sdtm' with the name of your sdtm object):"
  ))
  cli::cli_text()
  message_code(
    function(x) {paste0( "  add_observation(sdtm, 'pc', ", x, "') %>%")},
    sdtm_summary$analytes$PCTESTCD
  )
  # cli::cli_text()

  # PK category
  if("PCCAT" %in% names(pc)){
    cats <- filter(pc, .data$PCCAT != "") %>%
      distinct(.data$PCCAT)
    if(nrow(cats) > 1){
      cli::cli_h2("Pharmacokinetic categories")
      cli::cli_alert_warning(paste0(
        "Note that there are different analytical categories (PCCAT) defined in ",
        "PC: "))
      cli::cli_text()
      cli::cli_verbatim(df_to_string(pc %>% distinct(PCCAT), indent = 2))
      cli::cli_text()
      cli::cli_text(paste0(
        "Consider filtering for specific PCCAT (and PCSCAT, if applicable) ",
        "using the 'cat' and 'scat' arguments to 'add_observation()'!"
      ))
    }
  }

  # PK specimens
  specimens <- filter(pc, PCSPEC != "") %>%
    distinct(PCSPEC)
  if (nrow(specimens) > 1) {
    # cli::cli_alert_warning("Multiple specimens!")
    cli::cli_h2("Pharmacokinetic specimens")
    cli::cli_alert_warning(paste0(
      "Note that there are data from {nrow(specimens)} different PK sample ",
      "specimen types in 'PC' ({nice_enumeration(specimens$PCSPEC)}). ",
      "When calling `add_observation()`, consider filtering for a specific ",
      "specimen using the 'observation_filter' argument to ",
      "'add_observation()'!"))
  }

  # NTIME
  time_fields <- distinct(pc, across(any_of(
    c("PCTPT", "PCTPTNUM", "PCELTM"))))
  cli::cli_h2("NTIME definition")
  cli::cli_text(
    "The PC domain contains multiple fields that the nominal sampling time ",
    "can be derived from: ")
  cli::cli_text()
  cli::cli_verbatim(df_to_string(
    time_fields, indent = 2, abbr = 5, abbr_threshold = 10))
  cli::cli_text()
  cli::cli_text(paste0(
    "Consider specifying a suitabe 'ntime_method' argument to ",
    "'add_observation()'. By default, the function will attempt to extract ",
    "time information from the PCTPT field."
  ))

  # Trial arms
  if(nrow(sdtm_summary$arms) > 1) {
    cli::cli_h1("{n_suggestion}. Study arms")
    n_suggestion <- n_suggestion + 1
    cli::cli_text(paste0(
      "There are {nrow(sdtm_summary$arms)} study arms defined in DM:"))
    cli::cli_text()
    cli::cli_verbatim(df_to_string(arrange(sdtm_summary$arms, ACTARMCD), indent = 2))
    cli::cli_text()
    cli::cli_text(paste0(
      "Consider defining a PART or ARM variable, filtering for a particular ",
      "arm, or defining a covariate based on ACTARMCD."))
  }
}




#' Unique subjects within a sdtm object
#'
#' @param obj The sdtm object.
#'
#' @return A data frame.
#' @export
#' @noRd
#' @examples
#' subjects(examplinib_poc)
subjects.sdtm <- function(obj) {
  # input validation
  validate_sdtm(obj, "dm")

  obj %>%
    domain("dm") %>%
    as.data.frame() %>%
    distinct(.data$USUBJID)
}


#' Analytes in a sdtm object

#' @param obj The sdtm object.
#'
#' @return Character.
#' @export
#' @keywords internal
#'
#' @examples
#' analytes(examplinib_sad)
analytes.sdtm <- function(obj) {
  # validate input
  validate_sdtm(obj, "pc")

  unique(domain(obj, "pc")$PCTESTCD)
}


#' Doses in a sdtm object
#'
#' @param obj A sdtm object.
#'
#' @return Numeric.
#' @export
#' @keywords internal
#'
#' @examples
#' doses(examplinib_poc)
doses.sdtm <- function(obj) {
  # validate input
  validate_sdtm(obj, "ex")

  unique(domain(obj, "ex")$EXDOSE)
}


#' Treatments in a sdtm object
#'
#' @param obj A sdtm object.
#'
#' @return Character.
#' @export
#' @keywords internal
#'
#' @examples
#' treatments(examplinib_poc)
treatments.sdtm <- function(obj) {
  unique(domain(obj, "ex")$EXTRT)
}


#' Keep only selected USUBJID in the data set
#'
#' @param obj The input, either a `nif` or `sdtm` object.
#' @param usubjid The USUBJID as character.
#' @return The filtered object.
#' @export
#' @examples
#' filter_subject(examplinib_poc, subjects(examplinib_poc)[1, "USUBJID"])
#' filter_subject(examplinib_poc_nif, subjects(examplinib_poc_nif)[1, "USUBJID"])
filter_subject <- function(obj, usubjid) {
  UseMethod("filter_subject")
}


#' Filter sdtm object for specific USUBJID
#'
#' @param obj The sdtm object.
#' @param usubjid The USUBJID as character
#' @return A sdtm object.
#' @export
#' @noRd
#' @examples
#' filter_subject(examplinib_poc, subjects(examplinib_poc)[1, "USUBJID"])
filter_subject.sdtm <- function(obj, usubjid) {
  temp <- lapply(
    obj$domains,
    function(x) {
      if("USUBJID" %in% names(x))
        filter(x, .data$USUBJID %in% usubjid)
      else
        x
    }
  )

  return(new_sdtm(temp))
}


#' Make subject data frame from SDTM object
#'
#' @param obj A sdtm object.
#' @param ... Further arguments.
#'
#' @return A data frame.
#' @export
make_subjects_sdtm <- function(obj, ...) {
  make_subjects(domain(obj, "dm"), domain(obj, "vs"), ...)
}



#' Guess NTIME from PCTPT
#'
#' @param sdtm A sdtm object.
#'
#' @return A data frame.
#' @export
#' @keywords internal
#'
#' @examples
#' guess_ntime(examplinib_poc)
guess_ntime <- function(sdtm) {
  if (!has_domain(sdtm, "pc")) {
    stop("PC domain not found in SDTM object")
  }
  pc_domain <- domain(sdtm, "pc")

  # Check if PCTPT column exists in the PC domain
  if (!"PCTPT" %in% colnames(pc_domain)) {
    stop("PCTPT column not found in PC domain. Cannot extract nominal times.")
  }

  # Check if any PCTPT entries are in ISO 8601 date format
  iso_date_entries <- is_iso8601_date(pc_domain$PCTPT)
  if(any(iso_date_entries, na.rm = TRUE)) {
    iso_date_values <- unique(pc_domain$PCTPT[iso_date_entries])
    warning("Some PCTPT entries are in ISO 8601 date format (e.g., ",
            paste(head(iso_date_values, 3), collapse = ", "),
            if(length(iso_date_values) > 3) "..." else "",
            "). These date-only values may not have extractable time information.")
  }

  pc_domain %>%
    distinct(.data$PCTPT) %>%
    mutate(
      # Extract time using multiple patterns
      time = case_when(
        # Pattern 1: Number followed by h, hr, hrs, hour, hours (with optional space)
        !is.na(str_extract(tolower(.data$PCTPT),
                           "([0-9.]+)\\s*(?:h|hr|hrs|hour|hours)",
                           group = 1)) ~
          str_extract(tolower(.data$PCTPT),
                     "([0-9.]+)\\s*(?:h|hr|hrs|hour|hours)",
                     group = 1),

        # Pattern 2: HOUR(S) followed by number
        !is.na(str_extract(tolower(.data$PCTPT),
                           "(?:hour|hours)\\s*([0-9.]+)",
                           group = 1)) ~
          str_extract(tolower(.data$PCTPT),
                     "(?:hour|hours)\\s*([0-9.]+)",
                     group = 1),

        # Default: No time found
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(pre = str_match(tolower(.data$PCTPT), "pre") == "pre") %>%
    mutate(NTIME = case_when(
      is.na(.data$time) & pre == TRUE ~ 0,
      !is.na(.data$time) & pre == TRUE ~ -as.numeric(.data$time),
      is.na(.data$time) & is.na(.data$pre) ~ NA,
      .default = as.numeric(.data$time)
    )) %>%
    select(-c("time", "pre"))
}


#' Calculate SLD for SDTM.TR domain
#'
#' Calculate the sum of longest diameters (SLD) and number of lesions by subject
#' and time point, and add as rows. If a SDTM.TR domain is not included in the
#' input SDTM object, the SDTM object is returned as-is.
#'
#' literature: https://www.lexjansen.com/phuse/2018/ds/DS03.pdf
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param sdtm_obj A SDTM object.
#' @param observation_filter A filter term, as character.
#' @param testcd The TRTESTCD to select for SLD calculation, as character.
#' @param group Grouping variables, as character.
#'
#' @return A SDTM object.
#' @export
#'
derive_sld <- function(
    sdtm_obj,
    testcd = "DIAMETER",
    group = c("TRMETHOD", "TRGRPID"),
    observation_filter = "TRGRPID == 'TARGET'") {

  # Validate inputs
  if (!inherits(sdtm_obj, "sdtm")) {
    stop("Input must be a sdtm object")
  }

  if (!"tr" %in% names(sdtm_obj$domains)) {
    warning("TR domain not in SDTM object, returning unchanged.")
    return(sdtm_obj)
  }

  # Get TR domain
  tr <- domain(sdtm_obj, "tr")

  # Validate required columns
  required_cols <- c("USUBJID", "TRTESTCD", "TRSTRESN", "TRDTC")
  missing_cols <- setdiff(required_cols, names(tr))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in TR domain: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Validate filter expression
  tryCatch(
    {
      filter_expr <- rlang::parse_expr(observation_filter)
      filtered_tr <- filter(tr, !!filter_expr)
    },
    error = function(e) {
      stop("Invalid filter expression: ",
           observation_filter, "\nError: ", e$message)
    }
  )

  # Validate testcd exists
  if (!testcd %in% tr$TRTESTCD) {
    stop("Test code '", testcd, "' not found in TR domain")
  }

  # Group by variables that define a unique assessment
  group_vars <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID", "SUBJID", "TRDTC", "TRDY",
    "VISITNUM", "VISIT", "EPOCH", "TREVAL",
    "TRREFID",
    "TRSTRESU",
    "VISITDY",
    "TRCAT",
    group
  )

  # Keep only group variables that exist in the data
  group_vars <- intersect(group_vars, names(tr))

  # Calculate SLD
  diameter_data <- filtered_tr %>%
    filter(.data$TRTESTCD == testcd)

  sld_data <- diameter_data %>%
    reframe(
      N_TARGET = n(),
      SLD = sum(.data$TRSTRESN, na.rm = TRUE),
      .by = any_of(group_vars)
    ) %>%
    distinct() %>%
    tidyr::pivot_longer(
      cols = c("N_TARGET", "SLD"),
      names_to = "TRTESTCD",
      values_to = "TRSTRESN"
    ) %>%
    {if("TRTEST" %in% names(tr))
        mutate(., TRTEST = case_match(
          .data$TRTESTCD,
          "SLD" ~ "Sum of longest diameters",
          "N_TARGET" ~ "Number of target lesions used for SLD calculation"))
        else .}

  tr <- tr %>%
    add_row(sld_data) %>%
    arrange(.data$USUBJID, .data$TRDTC)

  temp <- sdtm_obj
  temp$domains[["tr"]] <- tr
  return(temp)
}


#' Extract TESTCD fields by domain from a sdtm object
#'
#' @param obj A sdtm object.
#' @param domain Domains to select, as character. Defaults to all domains, if
#' NULL.
#'
#' @returns A data frame with columns DOMAIN and TESTCD. Returns an empty data
#' frame if no TESTCD columns are found.
#'
#' @export
testcd <- function(obj, domain = NULL) {
  # Validate inputs
  if (!inherits(obj, "sdtm")) {
    stop("Input must be a sdtm object")
  }

  validate_char_param(domain, "domain", allow_null = TRUE, allow_multiple = TRUE)
  if(is.null(domain)) {
    domain <- names(obj$domains)
  }

  domain <- tolower(domain)

  missing_domains <- setdiff(domain, names(obj$domains))
  n_missing = length(missing_domains)
  if(n_missing > 0) {
    conditional_message(
      plural("Domain", n_missing > 1), " ", nice_enumeration(missing_domains),
      " not found in sdtm object!"
    )
    domain <- intersect(domain, names(obj$domains))
  }

  purrr::reduce(
    obj$domains[domain],
    function(acc, x) {
      if (!"DOMAIN" %in% names(x)) {
        # warning("Domain data frame missing DOMAIN column")
        return(acc)
      }

      domain <- unique(toupper(x$DOMAIN))
      if (length(domain) == 0) {
        warning("DOMAIN column is empty")
        return(acc)
      }

      testcd_col <- paste0(domain, "TESTCD")
      out <- NULL
      if(testcd_col %in% names(x)){
        out <- data.frame(
          DOMAIN = domain,
          TESTCD = as.character(unique(x[[testcd_col]]))
        )
      }
      return(bind_rows(acc, out))},
    .init = data.frame(DOMAIN = character(), TESTCD = character())
  )
}


#' Generate the XXH128 hash of a sdtm object
#'
#' @param obj A sdtm object.
#'
#' @returns The XXH128 hash of the sdtm object as character.
#' @export
#' @importFrom rlang hash
#' @keywords internal
#'
#' @examples
#' hash(examplinib_sad)
hash.sdtm <- function(obj) {
  validate_sdtm(obj)
  rlang::hash(obj$domains)
}


#' Last date in SDTM domain object
#'
#' @param obj A nif object.
#'
#' @returns A POSIXct scalar.
#' @export
#' @keywords internal
#'
#' @examples
#' last_dtc(examplinib_sad)
last_dtc.sdtm <- function(obj) {
  validate_sdtm(obj)

  # temp <- lapply(obj$domains, last_dtc.data.frame)
  temp <- lapply(obj$domains, last_dtc_data_frame)
  if(is.null(unlist(temp)))
    return(NULL)

  out <- as.POSIXct(max(unlist(temp), na.rm = TRUE))

  return(out)
}
