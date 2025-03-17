#' Improved guess parent function
#'
#' Guess the most likely parent based on its prevalence in the NIF object.
#' This improved version addresses several issues with the original function:
#' 1. Consistent return types (returns NA instead of NULL when no parent is found)
#' 2. Optional fallback to administration data when no observations exist
#' 3. Improved warnings and messages for better debugging
#' 4. Option to specify a default value to return when no parent can be determined
#'
#' @param obj A nif object.
#' @param fallback_to_admin Logical. If TRUE and no observations exist, the function
#'   will try to determine the parent from administration data (EVID = 1).
#' @param default_value Value to return if no parent can be determined. If NULL,
#'   NA will be returned.
#' @param warn Logical. If TRUE, warning messages will be issued when no parent
#'   can be determined.
#'
#' @return The parent as character, or NA/default_value if no parent can be determined.
#' @export
improved_guess_parent <- function(
    obj, fallback_to_admin = TRUE, default_value = NA, warn = TRUE) {
  # Early return for empty dataset
  if (nrow(obj) == 0) {
    if (warn) warning("Empty dataset: No observations to determine parent.")
    return(default_value)
  }

  # Preprocess data for observations
  temp <- obj %>%
    as.data.frame() %>%
    ensure_analyte() %>%
    ensure_metabolite() %>%
    ensure_parent() %>%
    as.data.frame() %>%
    filter(.data$EVID == 0)

  # Check if observations exist
  if (nrow(temp) == 0) {
    if (warn) warning("No observations (EVID = 0) found in dataset.")

    # Try fallback to administration data if requested
    if (isTRUE(fallback_to_admin)) {
      admin_data <- obj %>%
        as.data.frame() %>%
        ensure_analyte() %>%
        ensure_parent() %>%
        filter(.data$EVID == 1)

      if (nrow(admin_data) > 0) {
        admin_result <- admin_data %>%
          group_by(.data$PARENT) %>%
          summarize(n = n(), .groups = "drop") %>%
          filter(n == max(.data$n)) %>%
          arrange(.data$PARENT) %>%
          slice(1) %>%
          pull("PARENT")

        if (length(admin_result) > 0) {
          if (warn) message("Parent determined from administration data: ", admin_result)
          return(admin_result)
        }
      }

      if (warn) warning("No administration data (EVID = 1) found either.")
    }

    return(default_value)
  }

  # Check if any non-metabolite observations exist
  non_metabolites <- temp %>%
    filter(.data$METABOLITE == FALSE)

  if (nrow(non_metabolites) == 0) {
    if (warn) warning("Only metabolite observations found, cannot determine parent.")
    return(default_value)
  }

  # Proceed with normal parent determination based on frequency
  result <- non_metabolites %>%
    group_by(.data$PARENT) %>%
    summarize(n = n(), .groups = "drop") %>%
    filter(n == max(.data$n)) %>%
    arrange(.data$PARENT) %>%
    slice(1) %>%
    pull("PARENT")

  # Check for NA or empty result
  if (length(result) == 0 || is.na(result) || result == "") {
    if (warn) warning("Could not determine a valid parent from observations.")
    return(default_value)
  }

  return(result)
}
