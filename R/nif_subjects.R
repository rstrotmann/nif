#' Calculate age from birthday and reference date
#'
#' @param df A data frame containing at least BRTHDTC and reference date
#'   columns.
#' @param ref_date_col The name of the reference date column, default is
#'   "RFICDTC".
#' @param preserve_age Whether to preserve existing AGE values, default is TRUE.
#'
#' @return A data frame with AGE column added or updated.
#' @keywords internal
#' @noRd
calculate_age <- function(df, ref_date_col = "RFICDTC", preserve_age = TRUE) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  if (!("BRTHDTC" %in% colnames(df) && ref_date_col %in% colnames(df))) {
    return(df) # Return unchanged if required columns not present
  }

  df <- df |>
    lubrify_dates() |>
    mutate(age_brthdtc = round(as.numeric(
      lubridate::as.duration(.data[[ref_date_col]] - .data$BRTHDTC), "years"
    ), 0))

  if (preserve_age && "AGE" %in% names(df)) {
    df <- df |>
      mutate(AGE = case_when(
        is.na(.data$AGE) ~ .data$age_brthdtc,
        .default = .data$AGE
      ))
  } else {
    df <- df |>
      mutate(AGE = .data$age_brthdtc)
  }

  df |>
    select(-"age_brthdtc")
}


#' Compile subject information
#'
#' @param dm The DM domain as data table.
#' @param vs The VS domain as data table or NULL.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param keep Columns to keep, as character vector.
#' @param silent Suppress messages, as logical.
#' @param bl_covariates The TESTCD to keep as baseline covariates, as character.
#' Defaults to WEIGHT and HEIGHT.
#' @param duplicate_function The summary function to calculate the baseline
#'in case of duplicate values. Defaults to mean.
#'
#' @return A data frame
#' @import tidyselect
#' @import dplyr
#' @import lubridate
#' @keywords internal
#' @noRd
#' @examples
#' make_subjects(domain(examplinib_poc, "dm"), domain(examplinib_poc, "vs"))
#'
make_subjects <- function(
  dm,
  vs = NULL,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  bl_covariates = c("WEIGHT", "HEIGHT"),
  duplicate_function = mean,
  keep = NULL,
  silent = NULL
) {
  # input validation
  validate_df_argument(dm, expected_fields = c("USUBJID", "SEX", "ACTARMCD"))
  validate_argument(subject_filter, "character")
  validate_argument(keep, "character", allow_null = TRUE, allow_multiple = TRUE)
  validate_df_argument(
    vs, allow_null = TRUE,
    expected_fields = c("USUBJID", "VSTESTCD", "VSSTRESN")
  )

  # If vs is provided, validate required fields
  if (!is.null(vs)) {
    if (!"VSBLFL" %in% names(vs) && !"VSDTC" %in% names(vs)) {
      stop("When VSBLFL is not available, VSDTC must be present for baseline determination!")
    }
  }

  # business logic
  dm <- calculate_age(dm)

  if (!is.null(vs)) {
    # Check if RFSTDTC exists in dm when needed for baseline calculations
    if (!"VSBLFL" %in% names(vs) && !"RFSTDTC" %in% colnames(dm)) {
      stop("Baseline covariates cannot be determined because VS has no VSBLFL field, and DM has no RFSTDTC.")
    }

    baseline_covariates <- vs |>
      lubrify_dates() |>
      left_join(
        select(dm, c("USUBJID", "RFSTDTC")), by = "USUBJID"
      )

    if ("VSBLFL" %in% names(vs)) {
      baseline_covariates <- filter(baseline_covariates, .data$VSBLFL == "Y")
    } else {
      baseline_covariates <- filter(baseline_covariates,
                                    .data$VSDTC < .data$RFSTDTC)
    }

    # deal with duplicates and make wide table
    baseline_covariates <- baseline_covariates |>
      filter(.data$VSTESTCD %in% bl_covariates) |>
      reframe(value = duplicate_function(.data$VSSTRESN, na.rm = TRUE),
              .by = c("USUBJID", "VSTESTCD")) |>
      pivot_wider(names_from = "VSTESTCD", values_from = "value")

    # calculate BMI
    if ("HEIGHT" %in% colnames(baseline_covariates) &&
          "WEIGHT" %in% colnames(baseline_covariates)) {
      baseline_covariates <- baseline_covariates |>
        mutate(BMI = calculate_bmi(.data$HEIGHT, .data$WEIGHT))
    }
  } else {
    baseline_covariates <- distinct(dm, .data$USUBJID)
  }

  # Apply filter to dm and prepare output
  subject_expr <- validate_filter(subject_filter, data = dm)
  filtered_dm <- dm |>
    lubrify_dates() |>
    filter(rlang::eval_tidy(subject_expr, data = pick(everything())))

  # Add warning if subject_filter returns no entries
  if (nrow(filtered_dm) == 0) {
    warning("The subject_filter '", subject_filter, "' returned no entries.")
  }

  # Only join with baseline_covariates if vs is not NULL
  if (!is.null(vs)) {
    out <- filtered_dm |>
      left_join(baseline_covariates, by = "USUBJID") |>
      recode_sex()
  } else {
    out <- filtered_dm |>
      recode_sex()
  }

  # Generate sequential IDs instead of NA values
  out |>
    mutate(ID = row_number()) |>
    relocate("ID") |>
    select(any_of(c(
      "ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE",
      "BMI", "ACTARMCD", "RFXSTDTC", "RFSTDTC", bl_covariates, keep
    )))
}
