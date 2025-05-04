#' Check whether fields in domain are compliant with SDTM
#'
#' @param domain The SDTM domain as data frame.
#' @param silent Suppress optional messages, as logical. Defaults to global
#'   nif_options if NULL.
#'
#' @return Invisibly returns TRUE if validation passes, or stops with an error
#'  if required columns are missing.
#' @export
#'
#' @examples validate_domain(domain(examplinib_sad, "dm"))
validate_domain <- function(domain, silent = NULL) {
  # Input validation
  if (!is.data.frame(domain)) {
    stop("The 'domain' parameter must be a data frame")
  }

  if (!"DOMAIN" %in% colnames(domain)) {
    stop("The data frame must have a 'DOMAIN' column")
  }

  # Check if domain$DOMAIN is empty
  if (length(domain$DOMAIN) == 0) {
    stop("The 'DOMAIN' column is empty")
  }

  # Get unique domain names and handle multiple values
  domain_names <- unique(domain$DOMAIN)
  if (length(domain_names) > 1) {
    warning("Multiple domain values found: ", paste(domain_names, collapse = ", "),
            ". Using first value: ", domain_names[1])
    domain_name <- toupper(domain_names[1])
  } else {
    domain_name <- toupper(domain_names)
  }

  if(!domain_name %in% unique(domain_model$DOMAIN)) {
    warning("Unknown domain '", domain_name, "' cannot be validated!")
    return(invisible(TRUE))
  } else {
    temp <- domain_model %>%
      dplyr::filter(.data$DOMAIN == domain_name)

    required_names <- temp %>%
      dplyr::filter(.data$CORE == "Req") %>%
      dplyr::pull(.data$VARNAM)

    expected_names <- temp %>%
      dplyr::filter(.data$CORE == "Exp") %>%
      dplyr::pull(.data$VARNAM)

    permitted_names <- temp %>%
      dplyr::filter(.data$CORE == "Perm") %>%
      dplyr::pull(.data$VARNAM)

    missing_req <- setdiff(required_names, colnames(domain))
    missing_exp <- setdiff(expected_names, colnames(domain))
    missing_perm <- setdiff(permitted_names, colnames(domain))

    if (length(missing_req) > 0) {
      conditional_message(
        "The following required columns are missing in ",
        domain_name, ": ",
        paste(missing_req, collapse = ", "),
        silent = silent)
    }

    if (length(missing_exp) > 0) {
      conditional_message(
        "The following expected columns are missing in domain ",
        domain_name, ": ",
        paste(missing_exp, collapse = ", "),
        silent = silent)
    }

    if (length(missing_perm) > 0) {
      conditional_message(
        "The following permitted columns are missing in domain ",
        domain_name, ": ",
        paste(missing_perm, collapse = ", "),
        silent = silent)
    }

    return(invisible(TRUE))
  }
}


#' Check whether sdtm object is compliant with SDTM standard
#'
#' @param sdtm SDTM object.
#' @param silent Suppress optional messages, as logical. Defaults to global
#'   nif_options if NULL.
#'
#' @return Invisibly returns TRUE if validation passes, or stops with an error
#'  if required columns are missing.
#' @export
validate_sdtm <- function(sdtm, silent = NULL) {
  for(d in sdtm$domains) {
    validate_domain(d, silent = silent)
  }
}


#' SDTM class constructor, creating a sdtm object from a set of SDTM domains
#'
#' @param sdtm_data The SDTM domains as list of data frames.
#' @param analyte_mapping The analyte mapping as data frame.
#' @param metabolite_mapping The metabolite mapping as data frame.
#' @param parent_mapping The parent mapping as data frame.
#' @param time_mapping The time mapping as data frame.
#'
#' @import dplyr
#' @returns A sdtm object.
#' @export
new_sdtm <- function(sdtm_data,
                     analyte_mapping = data.frame(),
                     metabolite_mapping = data.frame(),
                     parent_mapping = data.frame(),
                     time_mapping = data.frame()) {
  domains <- sdtm_data

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
#' * `specimems` The unique `PCSPEC` as character.
#' * `analytes` The unique `PCTEST` and `PCTESTCD` as data frame.
#' * `pc_timepoints` The unique `PCTPT` and `PCTPTNUM` as data frame.
#' * `analyte_mapping` The analyte mapping as data frame (see
#'   [add_analyte_mapping()]).
#' * `metabolite_mapping` The metabolite mapping as data frame (see
#'   [add_metabolite_mapping()]).
#' * `time_mapping` The time mapping as data frame (see [add_time_mapping()]).
#'
#' @param object A SDTM object.
#' @param ... Further parameters.
#' @return A sdtm_summary object.
#' @export
#' @examples
#' summary(examplinib_poc)
summary.sdtm <- function(object, ...) {
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
    time_mapping = object$time_mapping
  )

  # Numbers of subjects and observations by domain
  out$disposition <- purrr::map(
    object$domains,
    function(x){
      data.frame(
        SUBJECTS = length(unique(x$USUBJID)),
        OBSERVATIONS = dim(x)[1]
      )
    }
  ) %>% purrr::list_rbind() %>%
    mutate(DOMAIN = names(object$domains)) %>%
    select(DOMAIN, SUBJECTS, OBSERVATIONS)

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


#' Return a specific domain from a sdtm object
#'
#' @param obj The sdtm object.
#' @param name The domain to return as a single character string.
#' @return The specified domain as data frame. Issues a warning if the domain
#'   does not exist and returns NULL.
#' @export
#' @examples
#' head(domain(examplinib_fe, "dm"), 3)
domain <- function(obj, name) {
  # Input validation
  if (!inherits(obj, "sdtm")) {
    stop("'obj' must be an SDTM object")
  }

  if (!is.character(name) || length(name) == 0) {
    stop("'name' must be a non-empty character vector")
  }

  if (length(name) > 1) {
    stop("'name' must be a single domain name, not a vector of multiple names")
  }

  # Normalize domain name to lowercase
  name_lower <- tolower(name)

  # Check if domain exists
  if(!has_domain(obj, name_lower)) {
    stop("Domain '", name, "' not found in SDTM object")
  }

  # Extract domain safely
  temp <- obj$domains[[name_lower]]

  # Return the domain
  return(temp)
}


#' Check whether a domain is present in an SDTM object
#'
#' @param obj The sdtm object.
#' @param name The domain name(s) to check as a character string or vector.
#' @return Logical indicating whether all specified domains exist in the SDTM object.
#' @export
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
#' @examples
#' subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
subject_info.sdtm <- function(obj, id) {
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
#'
#' @import dplyr
#' @export
#'
#' @examples
#' suggest(examplinib_poc)
suggest <- function(obj, consider_nif_auto = FALSE) {

  ## Helper functions
  message_block <- function(...) {
    args <- lapply(list(...), as.character)
    message(paste0(
      str_wrap(paste0(args, collapse = ""),
               width = 80, indent = 3, exdent = 3)), "\n")
  }

  message_df <- function(df) {
    message(paste0(
      df_to_string(df, indent = 6, header_sep = TRUE), "\n"))
  }

  message_code <- function(fct, data, header = "", footer = "", indent = 3,
                           collapse = " %>%") {
    if(header != "") header <- paste0(indent_string(indent + 2), header, "\n")
    if(footer != "") footer <- paste0("\n", indent_string(indent + 2), footer)

    lines <- paste0(indent_string(indent + 2), sapply(data, fct))

    message(paste0(
      indent_string(indent), "---\n",
      header,
      paste0(lines, collapse = paste0(collapse, "\n")),
      footer,
      "\n",
      indent_string(indent), "---\n"))
  }

  suggest_out <- function(n, ..., table = NULL) {
    args <- lapply(list(...), as.character)
    text <- paste0(args, collapse = "")
    out <- paste0(str_wrap(paste0(n, ". ", text),
      width = 80, indent = 0, exdent = 3), "\n")
    if (!is.null(table)) {
      out <- paste0(out, "\n", df_to_string(table, indent = 7), "\n")}
    message(out)
    return(n + 1)
  }

  if(!has_domain(obj, c("dm", "ex", "pc")))
    stop("Domains DM, EX and PC must be present!")

  # Domains
  dm <- domain(obj, "dm")
  ex <- domain(obj, "ex")
  pc <- domain(obj, "pc")

  # Function body
  n_suggestion <- 1

  # treatments <- distinct(obj$ex, EXTRT)
  treatments <- distinct(ex, EXTRT)
  n_trt <- length(treatments$EXTRT)

  # analytes <- distinct(obj$pc, PCTEST, PCTESTCD)
  analytes <- distinct(pc, PCTEST, PCTESTCD)
  n_analytes <- length(analytes$PCTESTCD)

  # 1. Analyte mappings
  if(isTRUE(consider_nif_auto)) {
    n_suggestion <- suggest_out(n_suggestion,
      "There are ", nrow(treatments), " treatments (EXTRT) in 'EX':")
    message_df(treatments)
    message_block("There are ", nrow(analytes),
                  " pharmacokinetic analytes (PCTESTCD) in 'PC':")
    message_df(analytes)
    message_block(
      "To associate treatments with their respective parent analyte, consider ",
      "adding analyte mapping(s) to the sdtm object using the below code ",
      "snippet (replace 'x' with the corresponding PCTESTCD):")
    message_code(function(x) {
      paste0("add_analyte_mapping('", x, "', 'x')")}, treatments$EXTRT)
    message_block(
      "For further information see the documentation to 'add_analyte_mapping ",
      "('?add_analyte_mapping').")
  }

  # 2. Metabolite mappings
  if(isTRUE(consider_nif_auto)){
    if (n_analytes > n_trt) {
      n_suggestion <- suggest_out(n_suggestion, paste0(
        "Only needed if you want to generate nif objects automatically (using ",
        "'nif_auto()'): ",
        "There are more pharmacokinetic analytes in 'PC' than treatments in ",
        "'EX'. If you want to include pharmacokinetic observations of ",
        "metabolites, you can add metabolite mapping(s) to the sdtm object ",
        "using 'add_metabolite_mapping' (see ?add_metabolite_mapping for ",
        "further information)."
      ))
    }
  }

  # 3. Treatments
  if (n_trt > 0) {
    n_suggestion <- suggest_out(n_suggestion,
      "There are ", n_trt, " different treatments in 'EX' (see ",
      "below).")
    message_df(treatments)
    message_block(
      "Consider adding them to the nif object using `add_administration()`, ",
      "see the code snippet below (replace 'sdtm' with the name of your sdtm ",
      "object):")
    message_code(
      function(x) {paste0( "  add_administration(sdtm, '", x, "')")},
      treatments$EXTRT,
      header = "%>%")
    }

  # 4. PK observations
  if(n_analytes > 0) {
    n_suggestion <- suggest_out(n_suggestion,
      "There are ", n_analytes, " different pharmacokinetic analytes in ",
      "'PC':")
    message_df(analytes)
    message_block(
      "Consider adding them to the nif object using `add_observation()`. ",
      "Replace 'sdtm' with the name of your sdtm object and 'y' with the ",
      "respective treatment code (",
      nice_enumeration(unique(treatments$EXTRT), conjunction = "or"), "):")
    message_code(
      function(x) {
        paste0("  add_observation(sdtm, 'pc', '", x, "', parent = 'y')")},
      analytes$PCTESTCD,
      header = "%>%")}

  # 5. PK specimens
  specimens <- filter(pc, PCSPEC != "") %>%
    distinct(PCSPEC)

  if (nrow(specimens) > 1) {
    n_suggestion <- suggest_out(n_suggestion,
      "There are data from ", nrow(specimens), " different sample specimen ",
      "types in 'PC'. ",
      "When calling `add_observation()`, consider filtering for a specific ",
      "specimen using the 'observation_filter' parameter.")
    message_block(
      "For further information see the documentation to `add_observation()`")
  }

  # 6. NTIME
  if (!("PCELTM" %in% names(pc))) {
    temp <- guess_ntime(obj) %>%
      mutate(out = paste0('"', .data$PCTPT, '", ', .data$NTIME, ","))
    n_suggestion <- suggest_out(n_suggestion,
      "By default, `add_observation()` takes the nominal sampling time from ",
      "the (permissible) field PCELTM. However, in this data set, PCELTM ",
      "is not defined, and the nominal time must be manually derived from, e.g., ",
      "PCTPT. Consider providing the NTIME_lookup parameter to ",
      "`add_observation()` as the below data frame (make sure to review the ",
      "suggested NTIME values):")
    message_code(function(x) {
      paste0("  ", x)},
    temp$out,
    header = "NTIME_lookup <- tribble(\n       ~PCTPT, ~NTIME,",
    footer = ")",
    collapse = "")
  }

  # 7. Trial arms
  arms <- dm %>%
    filter(ACTARMCD != "") %>%
    distinct(ACTARM, ACTARMCD)

  if (nrow(arms) > 1) {
    n_suggestion <- suggest_out(n_suggestion,
      "There are ", nrow(arms), " arms defined in DM (see ",
      "below). Consider defining a PART or ARM variable in the nif dataset, ",
      "filtering for a particular arm, or defining a covariate based on ",
      "ACTARMCD.")
    message_df(arms)
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
  obj %>%
    domain("dm") %>%
    as.data.frame() %>%
    distinct(.data$USUBJID)
}


#' Analytes in a sdtm object
#'
#' @param obj The sdtm object.
#'
#' @return Character.
#' @export
#'
#' @examples
#' analytes(examplinib_sad)
analytes.sdtm <- function(obj) {
  unique(domain(obj, "pc")$PCTESTCD)
}


#' Doses in a sdtm object
#'
#' @param obj A sdtm object.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' doses(examplinib_poc)
doses.sdtm <- function(obj) {
  unique(domain(obj, "ex")$EXDOSE)
}


#' Treatments in a sdtm object
#'
#' @param obj A sdtm object.
#'
#' @return Character.
#' @export
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
  temp <- lapply(obj$domains, function(x) filter(x, .data$USUBJID %in% usubjid))
  new_sdtm(
    sdtm_data = temp, analyte_mapping = obj$analyte_mapping,
    metabolite_mapping = obj$metabolite_mapping,
    time_mapping = obj$time_mapping
  )
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


#' Generic function testcd
#'
#' @param obj A sdtm object.
#'
#' @returns A data frame.
testcd <- function(obj) {
  UseMethod("testcd")
}


#' Extract TESTCD fields by domain from a sdtm object
#'
#' @param sdtm A sdtm object.
#'
#' @returns A data frame with columns DOMAIN and TESTCD. Returns an empty data frame
#'   if no TESTCD columns are found.
#'
#' @export
#'
#' @examples
#' testcd(examplinib_sad)
testcd.sdtm <- function(obj) {
  # Validate inputs
  if (!inherits(obj, "sdtm")) {
    stop("Input must be a sdtm object")
  }

  purrr::reduce(
    obj$domains,
    function(acc, x) {
      if (!"DOMAIN" %in% names(x)) {
        warning("Domain data frame missing DOMAIN column")
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
          TESTCD = unique(x[[testcd_col]])
        )
      }
      return(bind_rows(acc, out))},
    .init = data.frame(DOMAIN = character(), TESTCD = character())
  )
}


