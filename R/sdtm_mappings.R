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
#' @return A SDTM object.
#' @seealso [nif_auto()]
#' @keywords internal
#' @import stringr
add_analyte_mapping <- function(obj, extrt, pctestcd, analyte = NULL) {
  # Input validation
  if (!inherits(obj, "sdtm")) {
    stop("'obj' must be an SDTM object")
  }

  if (!is.character(extrt))
    stop("'extrt' must be a character string")

  if (!is.character(pctestcd))
    stop("'pctestcd' must be a character string")

  # Handle vector inputs
  if (length(extrt) > 1) {
    warning("'extrt' has length > 1, using only the first element")
    extrt <- extrt[1]
  }

  if (length(pctestcd) > 1) {
    warning("'pctestcd' has length > 1, using only the first element")
    pctestcd <- pctestcd[1]
  }

  # Trim whitespace using stringr or base R depending on availability
  if (requireNamespace("stringr", quietly = TRUE)) {
    extrt <- stringr::str_trim(extrt)
    pctestcd <- stringr::str_trim(pctestcd)
  } else {
    extrt <- trimws(extrt)
    pctestcd <- trimws(pctestcd)
  }

  # Check for NA values
  if (is.na(extrt)) {
    stop("'extrt' cannot be NA")
  }

  if (is.na(pctestcd)) {
    stop("'pctestcd' cannot be NA")
  }

  # Check for empty strings
  if (nchar(extrt) == 0) {
    stop("'extrt' must be a non-empty character string")
  }

  if (nchar(pctestcd) == 0) {
    stop("'pctestcd' must be a non-empty character string")
  }

  # Validate analyte parameter if provided
  if (!is.null(analyte)) {
    if (!is.character(analyte)) {
      stop("'analyte' must be a character string")
    }

    if (length(analyte) > 1) {
      warning("'analyte' has length > 1, using only the first element")
      analyte <- analyte[1]
    }

    if (requireNamespace("stringr", quietly = TRUE)) {
      analyte <- stringr::str_trim(analyte)
    } else {
      analyte <- trimws(analyte)
    }

    if (is.na(analyte)) {
      stop("'analyte' cannot be NA")
    }

    if (nchar(analyte) == 0) {
      stop("'analyte' must be a non-empty character string")
    }
  } else {
    analyte <- pctestcd
  }

  # Initialize analyte_mapping if it doesn't exist
  if (is.null(obj$analyte_mapping)) {
    obj$analyte_mapping <- data.frame(
      EXTRT = character(0),
      PCTESTCD = character(0),
      ANALYTE = character(0),
      stringsAsFactors = FALSE
    )
  }

  # Check if EXTRT already exists in the mapping (simplified check)
  if (extrt %in% obj$analyte_mapping$EXTRT) {
    stop("EXTRT '", extrt, "' already exists in analyte mapping. EXTRT must be unique.")
  }

  # Add the new mapping
  obj$analyte_mapping <- rbind(
    obj$analyte_mapping,
    data.frame(
      EXTRT = extrt,
      PCTESTCD = pctestcd,
      ANALYTE = analyte,
      stringsAsFactors = FALSE
    )
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
add_parent_mapping <- function(obj, analyte, parent) {
  # Input validation
  if (!inherits(obj, "sdtm")) {
    stop("'obj' must be an SDTM object")
  }

  if (!is.character(analyte))
    stop("'analyte' must be a character string")

  if (!is.character(parent))
    stop("'parent' must be a character string")

  # Handle vector inputs
  if (length(analyte) > 1) {
    warning("'analyte' has length > 1, using only the first element")
    analyte <- analyte[1]
  }

  if (length(parent) > 1) {
    warning("'parent' has length > 1, using only the first element")
    parent <- parent[1]
  }

  # Trim whitespace using stringr or base R depending on availability
  if (requireNamespace("stringr", quietly = TRUE)) {
    analyte <- stringr::str_trim(analyte)
    parent <- stringr::str_trim(parent)
  } else {
    analyte <- trimws(analyte)
    parent <- trimws(parent)
  }

  # Check for NA values
  if (is.na(analyte)) {
    stop("'analyte' cannot be NA")
  }

  if (is.na(parent)) {
    stop("'parent' cannot be NA")
  }

  # Check for empty strings
  if (nchar(analyte) == 0) {
    stop("'analyte' must be a non-empty character string")
  }

  if (nchar(parent) == 0) {
    stop("'parent' must be a non-empty character string")
  }

  # Initialize parent_mapping if it doesn't exist
  if (is.null(obj$parent_mapping)) {
    obj$parent_mapping <- data.frame(
      ANALYTE = character(0),
      PARENT = character(0),
      stringsAsFactors = FALSE
    )
  }

  obj$parent_mapping <- rbind(
    obj$parent_mapping,
    data.frame(ANALYTE = analyte, PARENT = parent, stringsAsFactors = FALSE)
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
#' @keywords internal
#' @seealso [nif_auto()]
add_metabolite_mapping <- function(obj, pctestcd_parent, pctestcd_metabolite) {
  # Input validation
  if (!inherits(obj, "sdtm")) {
    stop("'obj' must be an SDTM object")
  }

  if (!is.character(pctestcd_parent))
    stop("'pctestcd_parent' must be a character string")

  if (!is.character(pctestcd_metabolite))
    stop("'pctestcd_metabolite' must be a character string")

  # Handle vector inputs
  if (length(pctestcd_parent) > 1) {
    warning("'pctestcd_parent' has length > 1, using only the first element")
    pctestcd_parent <- pctestcd_parent[1]
  }

  if (length(pctestcd_metabolite) > 1) {
    warning("'pctestcd_metabolite' has length > 1, using only the first element")
    pctestcd_metabolite <- pctestcd_metabolite[1]
  }

  # Trim whitespace using stringr or base R depending on availability
  if (requireNamespace("stringr", quietly = TRUE)) {
    pctestcd_parent <- stringr::str_trim(pctestcd_parent)
    pctestcd_metabolite <- stringr::str_trim(pctestcd_metabolite)
  } else {
    pctestcd_parent <- trimws(pctestcd_parent)
    pctestcd_metabolite <- trimws(pctestcd_metabolite)
  }

  # Check for NA values
  if (is.na(pctestcd_parent)) {
    stop("'pctestcd_parent' cannot be NA")
  }

  if (is.na(pctestcd_metabolite)) {
    stop("'pctestcd_metabolite' cannot be NA")
  }

  # Check for empty strings
  if (nchar(pctestcd_parent) == 0) {
    stop("'pctestcd_parent' must be a non-empty character string")
  }

  if (nchar(pctestcd_metabolite) == 0) {
    stop("'pctestcd_metabolite' must be a non-empty character string")
  }

  # Initialize metabolite_mapping if it doesn't exist
  if (is.null(obj$metabolite_mapping) | length(obj$metabolite_mapping) == 0) {
    obj$metabolite_mapping <- data.frame(
      PCTESTCD_parent = character(0),
      PCTESTCD_metab = character(0)#,
      # stringsAsFactors = FALSE
    )
  }

  # Check if mapping already exists
  existing_mapping <- obj$metabolite_mapping %>%
    dplyr::filter(.data$PCTESTCD_parent == pctestcd_parent &
                  .data$PCTESTCD_metab == pctestcd_metabolite)

  if (nrow(existing_mapping) > 0) {
    stop("Metabolite mapping for parent '", pctestcd_parent,
         "' to metabolite '", pctestcd_metabolite, "' already exists.")
  }

  obj$metabolite_mapping <- rbind(
    obj$metabolite_mapping,
    data.frame(
      PCTESTCD_parent = pctestcd_parent,
      PCTESTCD_metab = pctestcd_metabolite#,
      # stringsAsFactors = FALSE
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
#'   separated by commas. <PCTPT> corresponds to the value in the PCTPT fields,
#'   and NTIME corresponds to the nominal time in hours.
#' @return The SDTM object
#' @keywords internal
#' @seealso [suggest()]
add_time_mapping <- function(obj, ...) {
  temp <- unlist(c(as.list(environment())[-1], list(...)))
  mapping <- data.frame(PCTPT = names(temp), NTIME = as.numeric(temp))
  obj$time_mapping <- rbind(obj$time_mapping, mapping)
  return(obj)
}
