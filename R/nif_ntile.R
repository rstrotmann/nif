#' Add n-tiles (quantiles) for a specified column across all subjects
#'
#' This function calculates n-tiles for a specified column's values across all
#' subjects. The quantiles are calculated across all subjects, and each subject
#' gets the same n-tile value across all their rows. The input column must have
#' exactly one distinct value per subject (e.g., age, weight, baseline values).
#'
#' @param nif A nif object
#' @param input_col The column name to calculate n-tiles for (must have one
#'   distinct entry per subject)
#' @param n The number of quantiles (n-tiles) to generate (default = 4)
#' @param ntile_name Custom name for the output column. If NULL, uses `x_NTILE`
#'   format where x is the name of the input column
#'
#' @return A nif object with a new column containing the n-tile values (1 to n),
#'   named either `x_NTILE` (default) or the custom name specified in
#'   `ntile_name`
#'
#' @import dplyr
#' @export
#'
#' @seealso [dplyr::ntile()] for the underlying n-tile calculation
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' examplinib_sad_nif %>%
#'   add_ntile("WEIGHT") %>%
#'   plot(dose_norm = TRUE, facet = "WEIGHT_NTILE")
#'
#' examplinib_poc_nif %>%
#'   add_ntile("WEIGHT", n = 5) %>%
#'   distinct(ID, WEIGHT, WEIGHT_NTILE) %>%
#'   ggplot(aes(x = WEIGHT_NTILE, y = WEIGHT)) +
#'   geom_point() +
#'   labs(title = "Plasma concentrations by WEIGHT quartiles")
#' theme_bw()
add_ntile <- function(nif, input_col, n = 4, ntile_name = NULL) {
  # Validate that input is a nif object
  if (!inherits(nif, "nif")) {
    stop("Input must be a nif object")
  }

  # Validate that input_col parameter is a character string
  if (!is.character(input_col) || length(input_col) != 1) {
    stop("input_col must be a single character string")
  }

  # Validate that n is a positive integer between 2 and 100
  if (!is.numeric(n) || length(n) != 1 || n < 2 || n > 100 ||
    n != as.integer(n)) {
    stop("n must be a positive integer between 2 and 100")
  }

  # Validate that ntile_name is either NULL or a valid character string
  if (!is.null(ntile_name)) {
    if (!is.character(ntile_name) || length(ntile_name) != 1) {
      stop("ntile_name must be a single character string or NULL")
    }
  }

  # Check that required columns exist: ID, input_col
  required_cols <- c("ID", input_col)
  missing_cols <- setdiff(required_cols, names(nif))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns: ",
      nice_enumeration(missing_cols)
    )
  }

  # Validate data types (input_col should be numeric)
  if (!is.numeric(nif[[input_col]])) {
    stop("Column '", input_col, "' must contain numeric values")
  }

  # Validate that input_col has exactly one distinct value per subject (ID)
  subject_values <- nif %>%
    as.data.frame() %>%
    group_by(.data$ID) %>%
    summarise(
      n_distinct_values = n_distinct(.data[[input_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # Check if any subject has multiple distinct values
  subjects_with_multiple <- subject_values %>%
    filter(.data$n_distinct_values > 1)

  if (nrow(subjects_with_multiple) > 0) {
    stop(
      "Column '", input_col, "' must have exactly one distinct value per ",
      "subject. Found multiple values for subjects: ",
      nice_enumeration(subjects_with_multiple$ID)
      # paste(subjects_with_multiple$ID, collapse = ", ")
    )
  }

  # Extract unique subject-level values for n-tile calculation
  subject_level_data <- nif %>%
    as.data.frame() %>%
    group_by(.data$ID) %>%
    summarise(
      value = first(.data[[input_col]]),
      .groups = "drop"
    )

  # Handle cases where there are insufficient observations for n-tile calculation
  if (nrow(subject_level_data) < n) {
    stop(
      "Insufficient subjects (", nrow(subject_level_data),
      ") for calculating ", n, " n-tiles. Need at least ", n, " subjects."
    )
  }

  # Calculate n-tiles across all subjects (not within each subject)
  subject_level_data <- subject_level_data %>%
    mutate(ntile_value = ntile(.data$value, n = n))

  # Dynamic column naming logic
  column_name <- if (is.null(ntile_name)) {
    paste0(input_col, "_NTILE")
  } else {
    ntile_name
  }

  # Add the new column with the determined name to the nif object
  result <- nif %>%
    as.data.frame() %>%
    left_join(
      subject_level_data %>% select("ID", "ntile_value"),
      by = "ID"
    ) %>%
    rename(!!column_name := "ntile_value")

  # Ensure the result is a proper nif object using new_nif()
  return(new_nif(result))
}
