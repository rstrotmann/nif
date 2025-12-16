#' Find duplicate rows in a data frame
#'
#' This function identifies duplicate rows in a data frame based on specified
#' fields. It returns a data frame containing the duplicate rows and their counts.
#'
#' @param df A data frame to check for duplicates
#' @param fields A character vector of field names to check for duplicates. If
#' NULL, defaults to c("USUBJID", "TIME", "ANALYTE") for NIF data.
#' @param count_only Logical indicating whether to return only the count of
#' duplicates (default: FALSE)
#' @param return_all_cols Logical indicating whether to return all columns from
#' the original data frame (default: TRUE)
#' @param additional_cols Character vector of additional columns to include in
#' the output when return_all_cols is FALSE
#'
#' @return A data frame containing the duplicate rows and their counts, or just
#' the count if count_only is TRUE
#' @export
find_duplicates <- function(
    df,
    fields = NULL,
    count_only = FALSE,
    return_all_cols = TRUE,
    additional_cols = NULL) {
  ## TO DO
  ##
  ## REWRITE THIS FUNCTION TO MATCH
  ## RESOLVE DUPLICATES
  ##
  # input validation
  if(!is.data.frame(df)) {
    stop("df must be a data frame!")
  }
  validate_char_param(fields, "fields",
                      allow_multiple = TRUE, allow_null = TRUE)
  validate_logical_param(count_only, "count_only")
  validate_logical_param(return_all_cols, "return_all_cols")
  validate_char_param(additional_cols, "additional_cols",
                      allow_multiple = TRUE, allow_null = TRUE)

  if(is.null(fields)) {
    fields <- c("ID", "TIME", "ANALYTE")
  }

  # Check if all specified fields exist in the data frame
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0) {
    stop(paste0(
      plural("Field", length(missing_fields) > 1),
      " not found in input: ", nice_enumeration(missing_fields)))
  }

  # check additional_cols
  missing_additional <- setdiff(additional_cols, names(df))
  if(length(missing_additional) > 0) {
    stop(paste0(
      plural("Field", length(missing_additional) > 1),
      "for 'additional_cols' not found in input: ",
      nice_enumeration(missing_additional)))
  }

  # Group by specified fields and count occurrences
  duplicates <- df %>%
    dplyr::group_by(across(all_of(fields))) %>%
    dplyr::summarize(count = n(), .groups = "drop") %>%
    dplyr::filter(count > 1)

  if (count_only) {
    return(nrow(duplicates))
  }

  # Join back with original data to get all columns
  if (nrow(duplicates) > 0) {
    if (return_all_cols) {
      result <- duplicates %>%
        dplyr::left_join(df, by = fields) %>%
        dplyr::arrange(across(all_of(fields)))
    } else {
      # If additional_cols is NULL, use fields
      cols_to_keep <- unique(c(fields, additional_cols))
      result <- duplicates %>%
        dplyr::left_join(
          df %>% dplyr::select(all_of(cols_to_keep)),
          by = fields
        ) %>%
        dplyr::arrange(across(all_of(fields)))
    }
    return(as.data.frame(result))
  } else {
    return(NULL)
  }
}


#' Remove duplicate rows from a data frame
#'
#' This function removes duplicate rows from a data frame based on specified
#' fields, applying a function to handle duplicate values in the dependent
#' variable.
#'
#' @param df A data frame to remove duplicates from
#' @param fields A character vector of field names to check for duplicates. If
#' NULL, defaults to c("USUBJID", "TIME", "ANALYTE") for NIF data.
#' @param duplicate_function A function to apply to duplicate values. Default is
#' mean. The function should take a vector and return a single value.
#' @param dependent_variable The name of the field to apply the
#' duplicate_function to. Defaults to "DV".
#' @param na.rm Logical indicating whether to remove NA values when applying the
#' duplicate_function. Defaults to TRUE.
#'
#' @return A data frame with duplicate rows removed
#' @export
resolve_duplicates <- function(
    df,
    fields = "TIME",
    duplicate_function = mean,
    dependent_variable = "DV",
    na.rm = TRUE) {
  if(is.null(fields))
    fields <- c("ID", "TIME", "ANALYTE")

  # Check if all specified fields exist in the data frame
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0)
    stop(paste("The following fields do not exist in the data frame:",
               paste(missing_fields, collapse = ", ")))

  # Check if dependent_variable exists in the data frame
  if (!dependent_variable %in% names(df)) {
    stop(paste("The dependent variable", dependent_variable,
               "does not exist in the data frame"))
  }

  # Validate that duplicate_function is a function
  if (!is.function(duplicate_function)) {
    stop("duplicate_function must be a function")
  }

  # preserve baseline fields
  baseline_fields <- identify_baseline_columns(df)
  index_fields <- setdiff(c(fields, "ID", "ANALYTE"), baseline_fields)

  baseline <- df %>%
    select(all_of(setdiff(c("ID", baseline_fields), "DV"))) %>%
    distinct()

  f <- function(x) {
    if(na.rm == TRUE){
      duplicate_function(x[!is.na(x)])
    } else {
      duplicate_function(x)
    }
  }

  # if MDV is present, delete observations with MDV == 1
  if("MDV" %in% names(df))
    df <- df %>%
    filter(.data$MDV != 1)

  result <- df %>%
    reframe(
      !!dependent_variable := f(.data[[dependent_variable]]),
      .by = any_of(index_fields)) %>%
    left_join(baseline, by = "ID") %>%
    relocate(any_of(names(df)))

  return(as.data.frame(result))
}
