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
#' @examples
#' validate_domain(domain(examplinib_sad, "dm"))
validate_domain <- function(domain, silent = NULL) {
  # Input validation
  if (!is.data.frame(domain)) {
    stop("The 'domain' parameter must be a data frame")
  }

  if (!"DOMAIN" %in% colnames(domain)) {
    stop("The data frame must have a DOMAIN column")
  }

  # Check if domain$DOMAIN is empty
  if (length(domain$DOMAIN) == 0 | all(domain$DOMAIN == "")) {
    stop("The DOMAIN column is empty")
  }

  # Get unique domain names and handle multiple values
  domain_name <- unique(domain$DOMAIN)
  # if (length(domain_names) > 1) {
  #   warning("Multiple domain values found: ", paste(domain_names, collapse = ", "),
  #           ". Using first value: ", domain_names[1])
  #   domain_name <- toupper(domain_names[1])
  # } else {
  #   domain_name <- toupper(domain_names)
  # }
  if (length(domain_name) > 1) {
    stop(paste0(
      "Multiple domain values found: ", nice_enumeration(domain_name)))
  }

  if(!domain_name %in% unique(domain_model$DOMAIN)) {
    # warning("Unknown domain '", domain_name, "' cannot be validated!")
    conditional_message(
      "Unknown domain '", domain_name, "' cannot be validated!", silent = silent)
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


#' Check whether domains of sdtm object are compliant with SDTM standard
#'
#' @param sdtm SDTM object.
#' @param silent Suppress optional messages, as logical. Defaults to global
#'   nif_options if NULL.
#'
#' @return Invisibly returns TRUE if validation passes, or stops with an error
#'  if required columns are missing.
#' @export
validate_sdtm_domains <- function(sdtm, silent = NULL) {
  for(d in sdtm$domains) {
    validate_domain(d, silent = silent)
  }
}


#' Validate sdtm object
#'
#' @param obj A stdm object.
#' @param expected_domains Expected domains as character.
#'
#' @returns Nothing or stop.
validate_sdtm <- function(
    obj,
    expected_domains = NULL) {
  validate_char_param(expected_domains, "expected_domains", allow_null = TRUE,
                      allow_multiple = TRUE)

  if (!inherits(obj, "sdtm")) {
    stop("Input must be a sdtm object")
  }

  if(!is.null(expected_domains)) {
    expected_domains <- tolower(expected_domains)
    missing_domains <- setdiff(expected_domains, names(obj$domains))
    if(length(missing_domains) > 0) {
      stop(paste0(
        "Expected ", plural("domain", length(missing_domains) > 1),
        " missing in sdtm object: ", nice_enumeration(missing_domains)
      ))
    }
  }
}





#' Generic function parameter validation
#'
#' @param type Parameter type (string, logical or numeric)
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#' @param allow_na Allow NA value, as logical.
#'
#' @returns Nothing or stop.
validate_param <- function(
    type = c("string", "logical", "numeric"),
    param,
    param_name,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE,
    allow_na = FALSE) {

  # Validate type parameter
  type <- match.arg(type)

  # Check for NULL first
  if (is.null(param)) {
    if (allow_null) {
      return(invisible(NULL))
    } else {
      stop(paste0(param_name, " must not be NULL"))
    }
  }

  # Check for NA values
  if(!allow_na && any(is.na(param))) {
    stop(paste0(param_name, " must not contain NA"))
  }

  # Type checking
  if(
    (type == "string" && !is.character(param)) ||
    (type == "logical" && !is.logical(param)) ||
    # (type == "numeric" && !(is.numeric(param) | is.na(param)))) {
    (type == "numeric" && !is.numeric(param))) {
    stop(paste0(param_name, " must be a ", type, " value"))
  }

  # Length checking
  if(length(param) != 1 && !allow_multiple) {
    stop(paste0(param_name, " must be a single value"))
  }

  # Empty string check (only for character types)
  if (type == "string" && !allow_empty && length(param) > 0 && any(nchar(param) == 0)) {
    stop(paste0(param_name, " must be a non-empty string"))
  }

  return(invisible(NULL))
}


#' Validate character parameter
#'
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#'
#' @returns Nothing or stop.
validate_char_param <- function(
    param,
    param_name,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE) {
  validate_param(
    "string",
     param,
     param_name,
     allow_null,
     allow_empty,
     allow_multiple)
}

#' Validate logical parameter
#'
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#'
#' @returns Nothing or stop.
validate_logical_param <- function(
    param,
    param_name,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE) {
  validate_param(
    "logical",
    param,
    param_name,
    allow_null,
    allow_empty,
    allow_multiple)
}

#' Validate numeric parameter
#'
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#' @param allow_na Allow NA values, as logical.
#'
#' @returns Nothing or stop.
validate_numeric_param <- function(
    param,
    param_name,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE,
    allow_na = FALSE) {
  validate_param(
    "numeric",
    param,
    param_name,
    allow_null,
    allow_empty,
    allow_multiple,
    allow_na)
}


#' Validate nif object parameter
#'
#' @param obj A nif object.
#'
#' @returns Nothing or stop.
#'
validate_nif <- function(obj) {
  if (!inherits(obj, "nif")) {
    stop("Input must be a nif object")
  }
}


#' Validate domain object parameter
#'
#' @param obj A domain object.
#'
#' @returns Nothing or stop.
validate_domain_param <- function(obj) {
  if(!inherits(obj, "domain")) {
    stop("Input must be a domain object")
  }
}



#' Validate nif object with minimally required fields
#'
#' @param obj A nif object.
#' @param additional_fields Additional required fields as character.
#'
#' @returns Nothing or stop.
#'
validate_min_nif <- function(obj, additional_fields = NULL) {
  validate_nif(obj)

  missing_fields <- setdiff(
    c(minimal_nif_fields, additional_fields),
    names(obj))

  if(length(missing_fields) > 0) {
    stop(paste0(
      "missing required ", plural("field", length(missing_fields) > 1), ": ",
      nice_enumeration(missing_fields)
    ))
  }
}


#' Validate that one or multiple testcd are included in a sdtm object
#'
#' @param sdtm A sdtm object.
#' @param testcd Testcode to validate as character.
#' @param domain Domain as character
#'
#' @returns Validated testcode(s) as character.
validate_testcd <- function(sdtm, testcd, domain = NULL) {
  # input validation
  validate_char_param(domain, "domain", allow_null = TRUE)
  validate_char_param(testcd, "testcd", allow_multiple = TRUE)

  if(!is.null(domain)) {
    domain <- tolower(domain)
    if(!domain %in% names(sdtm$domains))
      stop(paste0(
        "Domain ", domain, " not found in sdtm object!"))

    temp <- domain(sdtm, domain)
    testcd_field <- paste0(toupper(domain), "TESTCD")

    if(!testcd_field %in% names(temp))
      stop(paste0(
        toupper(domain), " has no ", testcd_field, " field!"))

    missing_testcd <- testcd[!testcd %in% unique(temp[[testcd_field]])]
    if(length(missing_testcd) > 0)
      stop(paste0(
        "Testcd ", nice_enumeration(missing_testcd),
        " not found in domain ", toupper(domain), "!"))
  }

  missing_testcd <- testcd[!testcd %in% testcd(sdtm)$TESTCD]
  if(length(missing_testcd) > 0)
    stop(paste0(
      "Testcd ", nice_enumeration(missing_testcd), " not found in sdtm!"))

  return(testcd)
}



