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
      stop("The following required columns are missing in ", domain_name, ": ",
           paste(missing_req, collapse = ", "))
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
    # pc = domains[["pc"]],
    # dm = domains[["dm"]],
    # ex = domains[["ex"]],
    # vs = domains[["vs"]],
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
  out <- list(
    study = unique(domain(object, "dm")$STUDYID),
    subjects = unique(domain(object, "pc")$USUBJID),
    # pc_timepoints = unique(domain(object, "pc")[c("PCTPT", "PCTPTNUM")]),
    domains = data.frame(
      DOMAIN = names(object$domains),
      N = as.numeric(lapply(
        object$domains, function(x) {
          length(unique(x$USUBJID))
        }
      ))
    ),
    treatments = unique(domain(object, "ex")$EXTRT),
    arms = unique(domain(object, "dm")[c("ACTARMCD", "ACTARM")]),
    doses = unique(domain(object, "ex")[c("EXTRT", "EXDOSE")]),
    specimems = unique(domain(object, "pc")$PCSPEC),
    analytes = unique(domain(object, "pc")[c("PCTEST", "PCTESTCD")]),
    analyte_mapping = object$analyte_mapping,
    metabolite_mapping = object$metabolite_mapping,
    time_mapping = object$time_mapping
  )
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
  hline <- paste0(rep("-", 8), collapse = "")
  indent <- 1

  n <- filter(x$domains, DOMAIN == "pc")$N
  if (length(n) == 0) n <- 0

  cat(paste(hline, "SDTM data set summary", hline, "\n"))
  cat(paste("Study", x$study))
  cat(paste(
    " with", n,
    "subjects providing PC data.\n"
  ))

  cat("\nSubjects per domain:\n")
  temp <- x$domains
  cat(paste0(df_to_string(x$domains,
    color = color, indent = indent,
    show_none = TRUE
  ), "\n\n"))

  cat("Arms (DM):\n")
  cat(paste0(df_to_string(x$arms,
    color = color, indent = indent,
    show_none = TRUE
  ), "\n\n"))

  cat("Treatments (EX):\n")
  temp <- paste0(" ", x$treatments, collapse = ", ")
  temp <- ifelse(temp == "", " (none)", temp)
  cat(temp, "\n\n")

  cat("PK sample specimens (PC):\n")
  temp <- paste0(x$specimens, collapse = ", ")
  temp <- ifelse(temp == "", " (none)", temp)
  cat(temp, "\n\n")

  cat("PK analytes (PC):\n")
  cat(df_to_string(x$analytes,
    color = color, indent = indent,
    show_none = TRUE
  ), "\n\n")

  if (nrow(x$analyte_mapping) != 0) {
    cat("Treatment-to-analyte mappings:\n")
    cat(df_to_string(x$analyte_mapping,
      color = color, indent = indent,
      show_none = TRUE
    ), "\n\n")
  }

  if (nrow(x$metabolite_mapping) != 0) {
    cat("Parent-to-metabolite mappings:\n")
    cat(df_to_string(x$metabolite_mapping,
      color = color, indent = indent,
      show_none = TRUE
    ), "\n\n")
  }

  if (nrow(x$time_mapping) != 0) {
    cat("Time mappings:\n")
    cat(df_to_string(x$time_mapping,
      color = color, indent = indent,
      show_none = TRUE
    ), "\n\n")
  }

  invisible(x)
}


#' Attach a treatment-analyte mapping to SDTM object
#'
#' In some studies, multiple drugs are co-administered, and there may be analyte
#' data related to different parent drugs. In order to appropriately correlate
#' observations with administrations, the [nif_auto()] function needs to know
#' which analyte (`PCTESTCD`) belongs to which drug (`EXTRT`). Multiple mappings
#' can be provided.
#'
#' @param obj A SDTM object.
#' @param extrt The treatment as defined in EX.
#' @param pctestcd The analyte as character (as in 'PCTESTCD' for PK observations
#' @param analyte The analyte name to be used in the nif object, as character.
#' @seealso [nif_auto()]
#' @keywords internal
#' @export
#' @examples
#' sdtm_object <- add_analyte_mapping(examplinib_fe, "EXAMPLINIB", "RS2023")
add_analyte_mapping <- function(obj, extrt, pctestcd, analyte = NULL) {
  if (is.null(analyte)) analyte <- pctestcd
  obj$analyte_mapping <- rbind(
    obj$analyte_mapping,
    data.frame("EXTRT" = extrt, "PCTESTCD" = pctestcd, "ANALYTE" = analyte)
  )
  return(obj)
}


#' Attach parent mapping to SDTM object
#'
#'
#' @param obj A sdtm object.
#' @param analyte The analyte as character.
#' @param parent The parent as character.
#'
#' @return A sdtm object.
#' @keywords internal
#' @export
add_parent_mapping <- function(obj, analyte, parent) {
  obj$parent_mapping <- rbind(
    obj$parent_mapping,
    data.frame(ANALYTE = analyte, PARENT = parent)
  )
  return(obj)
}


#' Attach a parent-metabolite mapping to a SDTM object
#'
#' In case multiple analytes are measured for a specific administered drug, some
#' functions need that information to correlate plasma concentrations with
#' administrations.
#'
#' @param obj The SDTM object.
#' @param pctestcd_parent The PCTESTCD of the parent compound.
#' @param pctestcd_metabolite The PCTESTCD of the metabolite.
#' @return The SDTM object.
#' @export
#' @keywords internal
#' @seealso [nif_auto()]
#' @examples
#' add_metabolite_mapping(examplinib_fe, "RS2023", "RS2023487A")
add_metabolite_mapping <- function(obj, pctestcd_parent, pctestcd_metabolite) {
  obj$metabolite_mapping <- rbind(
    obj$metabolite_mapping,
    data.frame(
      "PCTESTCD_parent" = pctestcd_parent,
      "PCTESTCD_metab" = pctestcd_metabolite
    )
  )
  return(obj)
}


#' Attach a time mapping to an sdtm object
#'
#' The nominal time of observations (e.g., `PCTPT`) is not required
#' to follow a strict format and is in most cases provided as a composite
#' string. This function can be used to explicitly define the nominal
#' observation times (in hours) for the values of, e.g., `PCTPT`.
#'
#' @param obj The SDTM object.
#' @param ... Mappings in the form '"<PCTPT>"=<NTIME>' with multiple mappings
#'   separated by commas. <PCTPT> corresponds to the value in the PCTPT fiels,
#'   and NTIME corresponds to the nominal time in hours.
#' @return The SDTM object
#' @export
#' @keywords internal
#' @seealso [suggest()]
#' @examples
#' sdtm_object <- add_time_mapping(examplinib_fe,
#'   "PREDOSE" = 0,
#'   "HOUR 0.5" = 0.5,
#'   "HOUR 1" = 1,
#'   "HOUR 1.5" = 1.5,
#'   "HOUR 2" = 2,
#'   "HOUR 3" = 3,
#'   "HOUR 4" = 4,
#'   "HOUR 6" = 6,
#'   "HOUR 8" = 8,
#'   "HOUR 10" = 10,
#'   "HOUR 12" = 12,
#'   "HOUR 24" = 24,
#'   "HOUR 48" = 48,
#'   "HOUR 72" = 72,
#'   "HOUR 96" = 96,
#'   "HOUR 144" = 144,
#'   "HOUR 168" = 168
#' )
add_time_mapping <- function(obj, ...) {
  temp <- unlist(c(as.list(environment())[-1], list(...)))
  mapping <- data.frame(PCTPT = names(temp), NTIME = as.numeric(temp))
  obj$time_mapping <- rbind(obj$time_mapping, mapping)
  return(obj)
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
  # if (!name_lower %in% names(obj$domains)) {
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
#' @param name The domain name to check as a single character string.
#' @return Logical indicating whether the domain exists in the SDTM object.
#' @export
#' @examples
#' # Check if DM domain exists
#' has_domain(examplinib_fe, "dm")
#'
#' # Check if a non-existent domain exists
#' has_domain(examplinib_fe, "xyz")
has_domain <- function(obj, name) {
  # Input validation
  if (!inherits(obj, "sdtm")) {
    stop("'obj' must be an SDTM object")
  }

  if (!is.character(name) || length(name) == 0) {
    stop("'name' must be a non-empty character vector")
  }

  # Ensure name is a single value
  if (length(name) > 1) {
    stop("'name' must be a single domain name, not a vector of multiple names")
  }

  # Normalize domain name to lowercase
  name_lower <- tolower(name)

  # Check if domain exists
  return(name_lower %in% names(obj$domains))
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


#' Suggest data programming steps for a sdtm object
#'
#' @param consider_nif_auto Include suggestions regarding parent or metabolite
#'   mappings to the sdtm object, as logical.
#' @param obj A sdtm object
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

  # Domains
  dm <- domain(obj, "dm")
  ex <- domain(obj, "ex")
  pc <- domain(obj, "pc")

  # Function body
  # col <- 34
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
    # n_trt <- length(unique(obj$ex$EXTRT))
    # n_trt <- length(treatments$EXTRT)
    # n_analytes <- length(unique(obj$pc$PCTESTCD))
    # n_analytes <- length(analytes$PCTESTCD)
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
  # treatments <- distinct(obj$ex, EXTRT)
  # if (nrow(treatments) > 0) {
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
  # obs <- distinct(obj$pc, PCTESTCD)
  # if (nrow(obs) > 0) {
  if(n_analytes > 0) {
    n_suggestion <- suggest_out(n_suggestion,
      "There are ", n_analytes, " different pharmacokinetic analytes in ",
      "'PC':")
    message_df(analytes)
    message_block(
      "Consider adding them to the nif object using `add_observation()`. ",
      "Replace 'sdtm' with the name of your sdtm object and 'y' with the",
      "respective treatment code (",
      nice_enumeration(unique(treatments$EXTRT), conjunction = "or"), "):")
    message_code(
      function(x) {
        paste0("  add_observation(sdtm, 'pc', '", x, "', parent = 'y')")},
      # obs$PCTESTCD,
      analytes$PCTESTCD,
      header = "%>%")}

  # 5. PK specimems
  # specimems <- filter(obj$pc, PCSPEC != "") %>%
  specimems <- filter(pc, PCSPEC != "") %>%
    distinct(PCSPEC)

  if (nrow(specimems) > 1) {
    n_suggestion <- suggest_out(n_suggestion,
      "There are data from ", nrow(specimems), " different sample specimem ",
      "types in 'PC'. ",
      "When calling `add_observation()`, consider filtering for a specific ",
      "specimem using the 'observation_filter' parameter.")
    message_block(
      "For further information see the documentation to `add_observation()`")
  }

  # 6. NTIME
  # if (!("PCELTM" %in% names(obj$pc))) {
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
  # arms <- obj$dm %>%
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
  # Use domain() function instead of direct access to sdtm$pc
  if (!has_domain(sdtm, "pc")) {
    stop("PC domain not found in SDTM object")
  }
  
  domain(sdtm, "pc") %>%
    distinct(.data$PCTPT) %>%
    mutate(time = str_extract(tolower(.data$PCTPT),
      "([0-9.]+)\\s*(h)",
      group = 1
    )) %>%
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
