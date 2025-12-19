#' Ensure that the ANALYTE field is present
#'
#' If 'ANALYTE' is not in the column names, the field is created based on the
#' compartment (CMT).
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @noRd
ensure_analyte <- function(obj) {
  # Validate input is a NIF object
  if (!inherits(obj, "nif")) {
    stop("Input must be a NIF object")
  }

  # If ANALYTE already exists, return as is
  if ("ANALYTE" %in% names(obj)) {
    return(obj)
  }

  # Validate CMT exists when needed
  if (!"CMT" %in% names(obj)) {
    stop("CMT column is required when ANALYTE is not present")
  }

  # Create ANALYTE from CMT with proper NA handling
  obj %>%
    mutate(ANALYTE = case_when(
      is.na(CMT) ~ NA_character_,
      TRUE ~ paste0("CMT", as.character(CMT))
    )) %>%
    new_nif() # Ensure return value is a NIF object
}


#' Ensure that the DOSE field is present
#'
#' This function ensures that the DOSE field exists in the NIF object. If it doesn't exist,
#' it creates it by:
#' 1. Using AMT values for dosing events (EVID == 1)
#' 2. Filling missing values using forward and backward fill within each subject
#'
#' @param obj A NIF object
#' @return A NIF object with the DOSE field
#' @keywords internal
#' @noRd
ensure_dose <- function(obj) {
  # Input validation
  if (!inherits(obj, "nif")) {
    stop("Input must be a NIF object")
  }

  # Check for required columns
  required_cols <- c("ID", "EVID", "AMT")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # If DOSE already exists, return as is
  if ("DOSE" %in% names(obj)) {
    return(obj)
  }

  # Create DOSE field
  result <- obj %>%
    # Ensure proper ordering for fill operations
    arrange(.data$ID, .data$TIME) %>%
    # Create DOSE from AMT for dosing events
    mutate(DOSE = case_when(
      .data$EVID == 1 ~ .data$AMT,
      TRUE ~ NA_real_
    )) %>%
    # Fill DOSE within each subject
    group_by(.data$ID) %>%
    tidyr::fill(DOSE, .direction = "downup") %>%
    ungroup()

  # Return as NIF object
  return(new_nif(result))
}


#' Ensure that the PARENT field is present in a NIF file.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @noRd
ensure_parent <- function(obj) {
  # Validate input is a NIF object
  validate_nif(obj)

  # Validate required columns exist
  required_cols <- c("EVID", "CMT")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Get administration CMT values
  admin_cmt <- obj %>%
    as.data.frame() %>%
    filter(.data$EVID == 1) %>%
    distinct(.data$CMT)

  # Handle case where there are no administrations
  if (nrow(admin_cmt) == 0) {
    warning("No administration records found (EVID == 1)")
    return(obj)
  }

  # If PARENT column doesn't exist, create it
  if (!"PARENT" %in% names(obj)) {
    # Use the most common CMT value with EVID == 1 for administrations
    most_common_cmt <- obj %>%
      filter(.data$EVID == 1) %>%
      count(.data$CMT) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(.data$CMT)

    obj <- obj %>%
      mutate(PARENT = as.character(most_common_cmt))
  }

  # Ensure return value is a NIF object
  return(new_nif(obj))
}


#' Ensure that the METABOLITE field is present in a NIF file.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @noRd
ensure_metabolite <- function(obj) {
  obj %>%
    {
      if (!"METABOLITE" %in% names(obj)) {
        mutate(., METABOLITE = FALSE)
      } else {
        .
      }
    }
}


#' Ensure that TAD is present in the NIF object
#'
#' This function ensures that the time-after-dose (TAD) field exists in the NIF object.
#' If it doesn't exist, it creates it by calling add_tad(). TAD represents the time
#' elapsed since the most recent administration of the parent compound.
#'
#' @param obj A NIF object.
#' @return A NIF object with the TAD field.
#' @keywords internal
#' @noRd
ensure_tad <- function(obj) {
  # Validate input is a NIF object
  if (!inherits(obj, "nif")) {
    stop("Input must be a NIF object")
  }

  # If TAD already exists, return as is
  if ("TAD" %in% names(obj)) {
    return(obj)
  }

  # Check for required columns
  required_cols <- c("ID", "TIME", "EVID")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns for TAD calculation: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Ensure PARENT field exists
  obj <- ensure_parent(obj)

  # Add TAD field
  result <- add_tad(obj)

  # Validate TAD was added
  if (!"TAD" %in% names(result)) {
    stop("Failed to add TAD field")
  }

  return(result)
}


#' Ensure that TAFD is present in the NIF object
#'
#' This function ensures that the time-after-first-dose (TAFD) field exists in the NIF object.
#' If it doesn't exist, it creates it by calling add_tafd(). TAFD represents the time
#' elapsed since the first administration of the parent compound.
#'
#' @param obj A NIF object.
#' @return A NIF object with the TAFD field.
#' @keywords internal
#' @noRd
ensure_tafd <- function(obj) {
  # Validate input is a NIF object
  if (!inherits(obj, "nif")) {
    stop("Input must be a NIF object")
  }

  # If TAFD already exists, return as is
  if ("TAFD" %in% names(obj)) {
    return(obj)
  }

  # Check for required columns
  required_cols <- c("ID", "TIME", "EVID")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns for TAFD calculation: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Add TAFD field
  tryCatch(
    {
      result <- add_tafd(obj)
    },
    error = function(e) {
      stop("Failed to add TAFD field: ", e$message)
    }
  )

  # Validate TAFD was added
  if (!"TAFD" %in% names(result)) {
    stop("Failed to add TAFD field")
  }

  return(result)
}


#' Ensure that all time fields are in the nif object
#'
#' This function ensures that the necessary time fields (TIME, TAD, TAFD) exist in the NIF object.
#' If any of these fields are missing, it will create them based on available data:
#' - If DTC (datetime) is available, it uses make_time() to calculate all time fields
#' - If only TIME is available, it uses make_time_from_TIME() to derive TAD and TAFD
#'
#' @param obj A nif object.
#'
#' @return A nif object with TIME, TAD, and TAFD fields.
#' @keywords internal
#' @noRd
ensure_time <- function(obj) {
  # Validate input is a NIF object
  if (!inherits(obj, "nif")) {
    stop("Input must be a NIF object")
  }

  # If all required time fields already exist, return object unchanged
  if (all(c("TIME", "TAD", "TAFD") %in% names(obj))) {
    return(obj)
  }

  # Check for required columns based on available data
  if ("DTC" %in% names(obj)) {
    # DTC available - use make_time to generate all time fields
    result <- tryCatch(
      {
        make_time(obj)
      },
      error = function(e) {
        stop("Failed to calculate time fields from DTC: ", e$message)
      }
    )
  } else if ("TIME" %in% names(obj)) {
    # TIME available but missing TAD/TAFD - use make_time_from_TIME
    result <- tryCatch(
      {
        make_time_from_TIME(obj)
      },
      error = function(e) {
        stop("Failed to calculate TAD/TAFD from TIME: ", e$message)
      }
    )
  } else {
    stop("Missing required columns: Either DTC or TIME is required to calculate time fields")
  }

  # Validate that all time fields are now present
  missing_time_fields <- setdiff(c("TIME", "TAD", "TAFD"), names(result))
  if (length(missing_time_fields) > 0) {
    stop(
      "Failed to generate all required time fields: ",
      paste(missing_time_fields, collapse = ", ")
    )
  }

  return(result)
}


ensure_cfb <- function(obj) {
  obj <- obj %>%
    {
      if (!"DVCFB" %in% names(obj)) add_cfb(.) else .
    }
}
