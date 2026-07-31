#' Add quantiles for a subject-level covariate
#'
#' Assigns n-tile bins for a subject-level numeric column. Bins are computed
#' from the distinct non-missing values across subjects (via [dplyr::ntile()]),
#' then mapped back so identical values always share the same bin. Each subject
#' gets the same n-tile across all their rows. The input column must have
#' exactly one distinct value per subject, including `NA` (e.g., age, weight,
#' baseline values). If there are fewer distinct values than `n`, fewer than `n`
#' bins are used.
#'
#' @param nif A nif object
#' @param input_col The column name to calculate n-tiles for (must have one
#'   distinct entry per subject)
#' @param n The number of quantiles (n-tiles) to generate (default = 4)
#' @param ntile_name Custom name for the output column. If NULL, uses `x_NTILE`
#'   format where x is the name of the input column
#' @param silent Suppress messages.
#'
#' @return A nif object with a new column containing the n-tile values (1 to at
#'   most `n`), named either `x_NTILE` (default) or the custom name specified in
#'   `ntile_name`. Subjects with a missing input value receive `NA`.
#'
#' @import dplyr
#' @export
#'
#' @seealso [dplyr::ntile()] used on distinct subject values
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' examplinib_sad_nif |>
#'   add_ntile("WEIGHT") |>
#'   plot(dose_norm = TRUE, facet = "WEIGHT_NTILE")
#'
#' examplinib_poc_nif |>
#'   add_ntile("WEIGHT", n = 5) |>
#'   distinct(ID, WEIGHT, WEIGHT_NTILE) |>
#'   ggplot(aes(x = WEIGHT_NTILE, y = WEIGHT)) +
#'   geom_point() +
#'   labs(title = "WEIGHT by n-tile") +
#'   theme_bw()
add_ntile <- function(
    nif,
    input_col,
    n = 4,
    ntile_name = NULL,
    silent = NULL
    ) {
  # input validation
  validate_nif(nif)
  validate_argument(input_col, "character")
  validate_argument(n, "numeric")

  if (n < 2 || n > 100)  stop("n must be a positive integer between 2 and 100")
  validate_argument(ntile_name, "character", allow_null = TRUE)

  if (n %% 1 != 0) {
    stop("n must be an integer value!")
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
  multiple_baseline_id <- nif |>
    reframe(n = n_distinct(.data[[input_col]], na.rm = FALSE), .by = "ID") |>
    filter(n > 1)

  if (nrow(multiple_baseline_id) > 0) {
    stop(paste0(
      "Some subjects do not have unique values for ", input_col, ":\n",
      df_to_string(multiple_baseline_id, indent = 2)
    ))
  }

  column_name <- ifelse(
    is.null(ntile_name),
    paste0(input_col, "_NTILE"),
    ntile_name
    )

  if (column_name %in% names(nif)) {
    conditional_cli(
      cli_alert_warning(paste0(
        "Column ", column_name, " will be replaced!"
      )),
      silent = silent
    )
    nif <- nif |>
      select(-all_of(column_name))
  }

  # One value per subject, then n-tiles on distinct values (ties stay together)
  subject_level_data <- nif |>
    reframe(value = first(.data[[input_col]], na_rm = TRUE), .by = "ID")

  temp <- subject_level_data |>
    filter(!is.na(.data$value)) |>
    distinct(.data$value) |>
    mutate(ntile = ntile(.data$value, n = n)) |>
    rename_with(~ input_col, "value") |>
    rename_with(~ column_name, "ntile")

  nif |>
    left_join(
      temp,
      by = input_col
    ) |>
    nif()
}
