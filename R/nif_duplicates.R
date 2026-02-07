#' Find duplicate rows in a data frame
#'
#' This function identifies duplicate rows in a data frame based on specified
#' fields. It returns a data frame containing the duplicate rows and their
#' counts. The 'ID' field must always be present.
#'
#' @param df A data frame to check for duplicates
#' @param fields A character vector of field names to check for duplicates. If
#' NULL, defaults to c("ID", "TIME", "ANALYTE") for NIF data.
#' @param count_only Logical indicating whether to return only the count of
#' duplicates (default: FALSE)
#'
#' @return A data frame containing the duplicate rows and their counts, or just
#' the count if count_only is TRUE
#' @noRd
find_duplicates <- function(
  df,
  fields = NULL,
  count_only = FALSE
) {
  ## input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame!")
  }
  validate_char_param(
    fields, "fields",
    allow_multiple = TRUE, allow_null = TRUE
  )
  validate_logical_param(count_only, "count_only")

  if (is.null(fields)) {
    fields <- c("ID", "TIME", "ANALYTE")
  }

  # Check if all specified fields exist in the data frame
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0) {
    stop(paste0(
      plural("Field", length(missing_fields) > 1),
      " not found in input: ", nice_enumeration(missing_fields)
    ))
  }

  # preserve baseline fields
  baseline_fields <- identify_baseline_columns(df)
  index_fields <- unique(c(setdiff(fields, baseline_fields), "ID", "USUBJID"))
  index_fields <- intersect(names(df), index_fields)

  # if MDV is present, delete observations with MDV == 1
  if ("MDV" %in% names(df)) {
    df <- df |>
      filter(.data$MDV != 1)
  }

  duplicates <- df |>
    group_by(across(all_of(index_fields))) |>
    mutate(count = n()) |>
    ungroup() |>
    dplyr::filter(.data$count > 1)

  if (count_only) {
    return(nrow(duplicates))
  }

  if (nrow(duplicates) == 0) {
    return(NULL)
  }

  as.data.frame(duplicates)
}


#' Resolve duplicate rows by averaging DV and setting conflicting fields to NA
#'
#' This function identifies duplicate rows based on specified identifier fields
#' and resolves them by:
#' - Averaging the DV field across duplicates
#' - Keeping other fields as-is if they have the same value across duplicates
#' - Setting fields to NA if they have multiple different values within
#' duplicates
#'
#' @param df A data frame to resolve duplicates from
#' @param fields A character vector of field names to identify duplicates. These
#'   fields are used to group rows that are considered duplicates.
#' @param duplicate_function A function to apply to duplicate values. Default is
#' mean. The function should take a vector and return a single value.
#' @param dependent_variable The name of the field to apply the
#' duplicate_function to. Defaults to "DV".
#' @param na_rm Logical indicating whether to remove NA values when applying the
#' duplicate_function. Defaults to TRUE.
#'
#' @return A data frame with duplicate rows resolved. The DV field contains the
#'   average of duplicate values, and other fields are kept as-is if consistent
#'   or set to NA if inconsistent within duplicate groups.
#' @noRd
resolve_duplicates <- function(
  df,
  fields = "TIME",
  dependent_variable = "DV",
  duplicate_function = mean,
  na_rm = TRUE
) {
  ## input validation
  if (!is.data.frame(df)) {
    stop("df must be a data frame!")
  }

  validate_char_param(fields, "fields", allow_multiple = TRUE,
                      allow_null = FALSE)

  # Check if all specified fields exist in the data frame
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0) {
    stop(paste0(
      plural("Field", length(missing_fields) > 1),
      " not found in input: ", nice_enumeration(missing_fields)
    ))
  }

  # Check if dependent_variable exists in the data frame
  if (!dependent_variable %in% names(df)) {
    stop(paste(
      "The dependent variable", dependent_variable,
      "does not exist in the data frame"
    ))
  }

  # Validate that duplicate_function is a function
  if (!is.function(duplicate_function)) {
    stop("duplicate_function must be a function")
  }

  # Remove DV from fields if present (it will be handled separately)
  fields <- setdiff(fields, dependent_variable)

  # Check that we still have at least one field to group by
  if (length(fields) == 0) {
    stop(paste("At least one field (other than DV) must be provided to",
               "identify duplicates"))
  }

  ## business logic
  # if MDV is present, delete observations with MDV == 1
  if ("MDV" %in% names(df)) {
    df <- df |>
      filter(.data$MDV != 1)
  }

  # Get all columns except the grouping fields and DV
  other_cols <- setdiff(names(df), c(fields, dependent_variable))

  # Helper function to check if all values in a vector are the same
  # Returns the unique value if all are the same, NA otherwise
  # Preserves the original type of the vector when returning NA
  all_same <- function(x) {
    x_clean <- x[!is.na(x)]
    if (length(x_clean) == 0) {
      # All NAs - return NA of the same type as the original vector
      return(switch(class(x)[1],
        character = NA_character_,
        numeric = NA_real_,
        integer = NA_integer_,
        logical = NA,
        NA
      ))
    }
    if (length(unique(x_clean)) == 1) {
      return(x_clean[1])
    }
    # Multiple different values - return NA of the same type
    switch(class(x)[1],
      character = NA_character_,
      numeric = NA_real_,
      integer = NA_integer_,
      logical = NA,
      NA
    )
  }

  f <- function(x) {
    if (na_rm == TRUE) {
      x_filtered <- x[!is.na(x)]
      if (length(x_filtered) == 0) {
        # All values are NA - return NA instead of NaN
        return(NA)
      }
      duplicate_function(x_filtered)
    } else {
      duplicate_function(x)
    }
  }

  # Group by fields and summarize
  df |>
    group_by(across(all_of(fields))) |>
    reframe(
      .dependent_variable = f(.data[[dependent_variable]]),
      across(all_of(other_cols), function(.x) all_same(.x))
    ) |>
    rename_with(~dependent_variable, ".dependent_variable") |>
    relocate(any_of(names(df))) |>
    as.data.frame()
}



#' Consolidate multiplicate observations
#'
#' Identify multiplicate observations over ID, CMT and 'fields' and integrate
#' the DV value for them using the 'duplicate_function'. A common application is
#' the averaging of triplicate ECG parameter observations by NTIME.
#'
#' Observations with MDV == 1 are excluded from the process.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param id_field The field(s) over which to identify duplicates (in addition
#' to ID ANALYTE and CMT which are automatically considered).
#' @param silent Suppress messages.
#' @param duplicate_function A function by which to consolidate the
#' multiplicates
#' @param na_rm Remove NA values.
#'
#' @returns A nif object.
#' @export
gather_duplicates <- function(
  obj,
  id_field = "NTIME",
  duplicate_function = mean,
  na_rm = TRUE,
  silent = NULL
) {
  # validate input
  validate_nif(obj)
  validate_argument(id_field, "character", allow_multiple = TRUE)
  validate_argument(na_rm, "logical")
  validate_argument(silent, "logical", allow_null = TRUE)

  group_fields <- c("ID", "CMT", "AMT", "EVID", "ANALYTE", id_field)
  validate_fields(obj, group_fields)

  if (!is.function(duplicate_function)) {
    stop("duplicate_function must be a function")
  }

  # if MDV is present, delete observations with MDV == 1
  if ("MDV" %in% names(obj)) {
    obj <- filter(obj, .data$MDV != 1)
  }

  # baseline fields by ID
  bl_col <- setdiff(
    identify_baseline_columns(obj),
    c("EVID", "AMT", "NTIME", "TIME", "DV")
  )
  bl <- obj |>
    as.data.frame() |>
    distinct(across(all_of(c("ID", bl_col))))

  duplicate_bl_ids <- bl$ID[duplicated(bl$ID)]
  if (length(duplicate_bl_ids) > 0) {
    stop(
      "Baseline (",
      nice_enumeration(bl_col),
      ") must be unique per ID. Inconsistent baseline for ID(s): ",
      nice_enumeration(unique(duplicate_bl_ids))
    )
  }

  # identify duplicates
  n_dupl <- find_duplicates(obj, group_fields, count_only = TRUE)
  conditional_cli(
    cli_alert_info(paste0(
      n_dupl, " duplicate observation gathered using '",
      deparse(substitute(duplicate_function)), "'!"
    )),
    silent = silent
  )

  # enhanced duplicate_function
  f <- function(x) {
    if (na_rm == TRUE) {
      x_filtered <- x[!is.na(x)]
      if (length(x_filtered) == 0) {
        # All values are NA - return NA instead of NaN
        return(NA)
      }
      duplicate_function(x_filtered)
    } else {
      duplicate_function(x)
    }
  }

  obj |>
    reframe(
      across(any_of(
        setdiff(c("DV", "TAFD", "TAD", "NTIME", "TIME"), id_field)
      ), f),
      .by = any_of(group_fields)
    ) |>
    left_join(bl, by = "ID") |>
    nif()
}
