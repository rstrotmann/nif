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


#' Resolve source-data duplicates in observation data frames
#'
#' Collapses rows that are duplicated with respect to the specified identifier
#' fields (e.g., USUBJID, ANALYTE, and DTC) during NIF construction. This
#' addresses unintended duplicates in the source data, such as repeated lab
#' results at the same datetime.
#'
#' The dependent variable (default: DV) is aggregated using
#' `duplicate_function`. All other (non-grouping) columns are kept if their
#' values are consistent within each duplicate group, or set to NA if they
#' conflict. This means baseline covariates may be lost if they differ across
#' duplicate rows.
#'
#' Rows with MDV == 1 are excluded before duplicate detection.
#'
#' @param df A data frame (typically an intermediate observation table during
#'   NIF construction).
#' @param fields A character vector of column names that define duplicate
#'   groups. Rows sharing the same values in these fields are considered
#'   duplicates. Must contain at least one field other than the dependent
#'   variable.
#' @param dependent_variable The column to aggregate across duplicates.
#'   Defaults to "DV".
#' @param duplicate_function A function applied to the dependent variable
#'   within each duplicate group (e.g., `mean`, `median`, `max`). Must accept
#'   a numeric vector and return a single value. Defaults to `mean`.
#' @param na_rm Logical. Remove NA values before applying
#'   `duplicate_function`? Defaults to TRUE.
#'
#' @return A data frame with one row per unique combination of `fields`.
#'   The dependent variable is aggregated; other columns retain their value
#'   if uniform within the group or are set to NA if conflicting.
#'
#' @seealso [nif::gather_duplicates()] for consolidating multiplicate measurements
#'   in a finished nif object with baseline-safe handling.
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


#' Consolidate multiplicate observations in a nif object
#'
#' Collapses replicate measurements (e.g., triplicate ECG readings) that share
#' the same subject, analyte, compartment, and nominal time point into a single
#' row by applying `duplicate_function` to the dependent variable and
#' time-related fields.
#'
#' Unlike [nif::resolve_duplicates()], baseline covariates are preserved safely:
#' they are separated before aggregation and re-joined by ID afterward, so
#' they are never set to NA due to row-level inconsistencies.
#'
#' Duplicate groups are defined by ID, CMT, AMT, EVID, ANALYTE, and
#' `id_field`. The aggregation function is applied to DV, TIME, NTIME, TAD,
#' and TAFD (except those used as grouping fields). Rows with MDV == 1 are
#' excluded before processing.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param id_field Character vector of additional field(s) used (together with
#'   ID, CMT, AMT, EVID, and ANALYTE) to define duplicate groups. Defaults to
#'   "NTIME".
#' @param duplicate_function A function applied to numeric columns within each
#'   duplicate group (e.g., `mean`, `median`, `sum`). Defaults to `mean`.
#' @param na_rm Logical. Remove NA values before applying
#'   `duplicate_function`? Defaults to TRUE.
#' @param silent Logical or NULL. Suppress informational messages? NULL uses
#'   the package default.
#'
#' @returns A nif object with multiplicate observations consolidated.
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
