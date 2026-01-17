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
