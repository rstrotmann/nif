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
  obj |>
    mutate(ANALYTE = case_when(
      is.na(CMT) ~ NA_character_,
      TRUE ~ paste0("CMT", as.character(CMT))
    )) |>
    nif() # Ensure return value is a NIF object
}


#' Ensure that the DOSE field is present
#'
#' This function ensures that the DOSE field exists in the NIF object. If it
#' doesn't exist, it creates it by:
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
  result <- obj |>
    # Ensure proper ordering for fill operations
    arrange(.data$ID, .data$TIME) |>
    # Create DOSE from AMT for dosing events
    mutate(DOSE = case_when(
      .data$EVID == 1 ~ .data$AMT,
      TRUE ~ NA_real_
    )) |>
    # Fill DOSE within each subject
    group_by(.data$ID) |>
    tidyr::fill("DOSE", .direction = "downup") |>
    ungroup()

  # Return as NIF object
  nif(result)
}


#' Ensure that the PARENT field is present in a NIF file.
#'
#' The PARENT is defined as the "ANALYTE" of the administered treatment. If
#' there are multiple treatments, there will be likely different parents (e.g.,
#' "placebo" and "examplinib").
#'
#' If the "PARENT" field is already present in the input nif object, the input
#' will be returned unchanged. If not, the most likely parent for each analyte
#' is guessed based on simple heuristics:
#'
#' * If there is only one treatment in the input data set, and there are
#' observations of the same ANALYTE name, that ANALYTE will be considered the
#' PARENT for all observations.
#' * If there is only one treatment but no observation with the same ANALYTE
#' name, the analyte with the lowest compartment (CMT) number is considered the
#' analyte corresponding the PARENT for all observations. In these cases a
#' message is issued to inform that this imputation was made.
#' * If there are multiple treatments in the input data set, however with
#' individual subjects receiving only one of the treatments, then the PARENT is
#' imputed as the ANALYTE of the respective treatment.
#' * If there are multiple treatments in the input data set, and individual
#' subjects have received multiple treatments, the PARENT for all observations
#' is imputed as the observation with the lowest compartment (CMT) number. In
#' these cases, a warning is issued that the parent could not be clearly
#' determined but was based on assumptions.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @noRd
ensure_parent_old <- function(obj) {
  # Validate input is a NIF object
  validate_nif(obj)

  # Validate required columns exist
  required_cols <- c("EVID", "CMT")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Handle empty data frame
  if (nrow(obj) == 0) {
    return(
      obj |>
        mutate(PARENT = numeric(0)) |>
        nif()
    )
  }

  obj <- obj |>
    ensure_analyte()

  # Get treatments early since it's needed in both code paths
  treatments <- treatments(obj)

  # Get administration CMT values
  admin_cmt <- obj |>
    as.data.frame() |>
    filter(.data$EVID == 1) |>
    distinct(.data$CMT)

  # If PARENT already exists, return early
  if ("PARENT" %in% names(obj)) {
    return(nif(obj))
  }

  # Handle case where there are no administrations
  if (nrow(admin_cmt) == 0) {
    # Use observation records to determine most likely parent
    cmt_heuristics <- filter(obj, .data$EVID == 0) |>
      reframe(.data$ANALYTE, n = n(), .by = "CMT") |>
      distinct() |>
      arrange(desc(n))

    # Check if we have any observations to determine parent
    if (nrow(cmt_heuristics) == 0) {
      stop("Cannot determine PARENT: no administration records (EVID == 1) and no observation records (EVID == 0)")
    }

    most_likely_parent <- cmt_heuristics[1, "ANALYTE"]

    obj <- obj |>
      mutate(
        PARENT = case_when(
          .data$EVID == 1 ~ .data$ANALYTE,
          .data$EVID == 0 & .data$ANALYTE %in% treatments ~ .data$ANALYTE,
          .default = most_likely_parent
        )
      )

    # Ensure return value is a NIF object
    return(nif(obj))
  }

  # Handle case where administrations exist
  admins <- filter(obj, .data$EVID == 1)
  cmt_heuristics <- admins |>
    reframe(.data$ANALYTE, n = n(), .by = "CMT") |>
    distinct() |>
    arrange(desc(n))

  # This should not happen if admin_cmt has rows, but check for safety
  if (nrow(cmt_heuristics) == 0) {
    stop("Cannot determine PARENT: administration records found but no valid ANALYTE values")
  }

  most_likely_parent <- cmt_heuristics[1, "ANALYTE"]

  obj <- obj |>
    mutate(
      PARENT = case_when(
        .data$EVID == 1 ~ .data$ANALYTE,
        .data$EVID == 0 & .data$ANALYTE %in% treatments ~ .data$ANALYTE,
        .default = most_likely_parent
      )
    )

  # Ensure return value is a NIF object
  nif(obj)
}


#' Ensure that the PARENT field is present in a NIF file.
#'
#' The PARENT is defined as the "ANALYTE" of the administered treatment. If
#' there are multiple treatments, there will be likely different parents (e.g.,
#' "placebo" and "examplinib").
#'
#' If the "PARENT" field is already present in the input nif object, the input
#' will be returned unchanged. If not, the most likely parent for each analyte
#' is guessed based on simple heuristics:
#'
#' * If there is only one treatment in the input data set, and there are
#' observations of the same ANALYTE name, that ANALYTE will be considered the
#' PARENT for all observations.
#' * If there is only one treatment but no observation with the same ANALYTE
#' name, the analyte with the lowest compartment (CMT) number is considered the
#' analyte corresponding the PARENT for all observations. In these cases a
#' message is issued to inform that this imputation was made.
#' * If there are multiple treatments in the input data set, however with
#' individual subjects receiving only one of the treatments, then the PARENT is
#' imputed as the ANALYTE of the respective treatment.
#' * If there are multiple treatments in the input data set, and individual
#' subjects have received multiple treatments, the PARENT for all observations
#' is imputed as the observation with the lowest compartment (CMT) number. In
#' these cases, a warning is issued that the parent could not be clearly
#' determined but was based on assumptions.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @noRd
ensure_parent <- function(obj) {
  # Validate input is a NIF object
  validate_nif(obj)

  # Validate required columns exist
  required_cols <- c("EVID", "CMT", "ID")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Handle empty data frame
  if (nrow(obj) == 0) {
    return(
      obj |>
        mutate(PARENT = character(0)) |>
        nif()
    )
  }

  # Ensure ANALYTE exists
  obj <- obj |>
    ensure_analyte()

  # If PARENT already exists, return early
  if ("PARENT" %in% names(obj)) {
    return(nif(obj))
  }

  # Get treatments (ANALYTE values from administration records)
  trts <- treatments(obj)

  # Get all unique analytes from observations
  obs_analytes <- analytes(obj)

  # Case 1: Single treatment
  if (length(trts) == 1) {
    single_treatment <- trts[1]

    # Check if there are observations with the same ANALYTE name as the treatment
    if (single_treatment %in% obs_analytes) {
      # Case 1a: Single treatment + observations with same ANALYTE
      obj <- obj |>
        mutate(
          PARENT = case_when(
            .data$EVID == 1 ~ .data$ANALYTE,
            TRUE ~ single_treatment
          )
        )
    } else {
      # Case 1b: Single treatment but no matching ANALYTE in observations
      # Find analyte with lowest CMT number
      lowest_cmt_analyte <- obj |>
        filter(.data$EVID == 0) |>
        arrange(.data$CMT) |>
        slice(1) |>
        pull(.data$ANALYTE)

      if (length(lowest_cmt_analyte) == 0 || is.na(lowest_cmt_analyte)) {
        stop("Cannot determine PARENT: no observation records (EVID == 0) found")
      }

      message(
        "PARENT field imputed: Single treatment '", single_treatment,
        "' found but no observations with matching ANALYTE. ",
        "Using analyte '", lowest_cmt_analyte,
        "' (lowest CMT number) as PARENT for all observations."
      )

      obj <- obj |>
        mutate(
          PARENT = case_when(
            .data$EVID == 1 ~ .data$ANALYTE,
            TRUE ~ lowest_cmt_analyte
          )
        )
    }
  } else if (length(trts) > 1) {
    # Case 2: Multiple treatments
    # Check if individual subjects received multiple treatments
    subjects_multiple_treatments <- obj |>
      filter(.data$EVID == 1) |>
      group_by(.data$ID) |>
      summarise(n_treatments = n_distinct(.data$ANALYTE), .groups = "drop") |>
      filter(.data$n_treatments > 1) |>
      nrow()

    if (subjects_multiple_treatments == 0) {
      # Case 2a: Multiple treatments but subjects only receive one each
      # Create mapping of treatment ANALYTE per subject
      subject_treatment_map <- obj |>
        filter(.data$EVID == 1) |>
        group_by(.data$ID) |>
        summarise(SUBJECT_PARENT = first(.data$ANALYTE), .groups = "drop")

      obj <- obj |>
        left_join(subject_treatment_map, by = "ID") |>
        mutate(
          PARENT = case_when(
            .data$EVID == 1 ~ .data$ANALYTE,
            TRUE ~ .data$SUBJECT_PARENT
          )
        ) |>
        select(-.data$SUBJECT_PARENT)
    } else {
      # Case 2b: Multiple treatments and subjects received multiple treatments
      # Find analyte with lowest CMT number
      lowest_cmt_info <- obj |>
        filter(.data$EVID == 0) |>
        arrange(.data$CMT) |>
        slice(1)

      if (nrow(lowest_cmt_info) == 0) {
        stop("Cannot determine PARENT: no observation records (EVID == 0) found")
      }

      lowest_cmt_analyte <- lowest_cmt_info |>
        pull(.data$ANALYTE)

      warning(
        "PARENT field imputed: Multiple treatments found and some subjects ",
        "received multiple treatments. Parent could not be clearly determined. ",
        "Using analyte '", lowest_cmt_analyte,
        "' (lowest CMT number) as PARENT for all observations based on assumptions."
      )

      obj <- obj |>
        mutate(
          PARENT = case_when(
            .data$EVID == 1 ~ .data$ANALYTE,
            TRUE ~ lowest_cmt_analyte
          )
        )
    }
  } else {
    # No treatments found
    stop("Cannot determine PARENT: no treatment records (EVID == 1) found")
  }

  # Ensure return value is a NIF object
  nif(obj)
}


#' Ensure that the METABOLITE field is present in a NIF file.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @noRd
ensure_metabolite <- function(obj) {
  if (!"METABOLITE" %in% names(obj))
    mutate(obj, METABOLITE = FALSE)
  else
    obj
}


#' Ensure that TAD is present in the NIF object
#'
#' This function ensures that the time-after-dose (TAD) field exists in the NIF
#' object. If it doesn't exist, it creates it by calling add_tad(). TAD
#' represents the time elapsed since the most recent administration of the
#' parent compound.
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
  # if (!"TAD" %in% names(result)) {
  #   stop("Failed to add TAD field")
  # }

  result
}


#' Ensure that TAFD is present in the NIF object
#'
#' This function ensures that the time-after-first-dose (TAFD) field exists in
#' the NIF object. If it doesn't exist, it creates it by calling add_tafd().
#' TAFD represents the time elapsed since the first administration of the parent
#' compound.
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
  # if (!"TAFD" %in% names(result)) {
  #   stop("Failed to add TAFD field")
  # }

  result
}


#' Ensure that all time fields are in the nif object
#'
#' This function ensures that the necessary time fields (TIME, TAD, TAFD) exist
#' in the NIF object. If any of these fields are missing, it will create them
#' based on available data:
#'
#' - If DTC (datetime) is available, it uses make_time() to calculate all time
#' fields
#' - If only TIME is available, it uses make_time_from_TIME() to derive TAD and
#' TAFD
#'
#' @param obj A nif object.
#'
#' @return A nif object with TIME, TAD, and TAFD fields.
#' @keywords internal
#' @noRd
ensure_time <- function(obj) {
  # Validate input is a NIF object
  # if (!inherits(obj, "nif")) {
  #   stop("Input must be a NIF object")
  # }

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
    stop(paste(
      "Missing required columns: Either DTC or TIME is required to calculate",
      "time fields"
    ))
  }

  # Validate that all time fields are now present
  # missing_time_fields <- setdiff(c("TIME", "TAD", "TAFD"), names(result))
  # if (length(missing_time_fields) > 0) {
  #   stop(
  #     "Failed to generate all required time fields: ",
  #     paste(missing_time_fields, collapse = ", ")
  #   )
  # }

  result
}


ensure_cfb <- function(obj) {
  if (!"DVCFB" %in% names(obj))
    derive_cfb(obj)
  else
    obj
}
