#' Generic function parameter validation
#'
#' @param type Parameter type (string, logical or numeric)
#' @param param The parameter to be tested.
#' @param param_name The parameter name as character.
#' @param allow_null Allow NULL values, as logical.
#' @param allow_empty Allow empty parameter, as logical.
#' @param allow_multiple Allow vector of the specified type, as logical.
#'
#' @returns Nothing or stop.
validate_param <- function(
    type = c("string", "logical", "numeric"),
    param,
    param_name,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE) {

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
  if(any(is.na(param))) {
    stop(paste0(param_name, " must not contain NA"))
  }

  # Type checking
  if(
    (type == "string" && !is.character(param)) ||
    (type == "logical" && !is.logical(param)) ||
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
#'
#' @returns Nothing or stop.
validate_numeric_param <- function(
    param,
    param_name,
    allow_null = FALSE,
    allow_empty = FALSE,
    allow_multiple = FALSE) {
  validate_param(
    "numeric",
    param,
    param_name,
    allow_null,
    allow_empty,
    allow_multiple)
}


#' Validate nif parameter
#'
#' @param obj A nif object.
#'
#' @returns Nothin or stop.
validate_nif <- function(obj) {
  if (!inherits(obj, "nif")) {
    stop("Input must be a nif object")
  }
}


