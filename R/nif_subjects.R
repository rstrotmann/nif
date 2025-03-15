#' Compile subject information
#'
#' @param dm The DM domain as data table.
#' @param vs The VS domain as data table.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param keep Columns to keep, as character vector.
#'
#' @return A data table.
#' @import tidyselect
#' @import dplyr
#' @import lubridate
#' @keywords internal
make_subjects <- function(
    dm,
    vs = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    keep = character(0)) {

  # Input validation
  if (!is.data.frame(dm)) {
    stop("The 'dm' parameter must be a data frame")
  }

  required_cols <- c("USUBJID", "SEX", "ACTARMCD")
  missing_cols <- setdiff(required_cols, colnames(dm))
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from the 'dm' data frame: ",
         paste(missing_cols, collapse = ", "))
  }

  # If vs is provided, validate it's a data frame
  if (!is.null(vs)) {
    if (!is.data.frame(vs)) {
      stop("The 'vs' parameter must be a data frame or NULL")
    }

    # Check for required columns in vs
    vs_required_cols <- c("USUBJID", "VSTESTCD", "VSSTRESN")
    vs_missing_cols <- setdiff(vs_required_cols, colnames(vs))
    if (length(vs_missing_cols) > 0) {
      stop("The following required columns are missing from the 'vs' data frame: ",
           paste(vs_missing_cols, collapse = ", "))
    }
    
    # Check for VSDTC when needed for baseline determination
    if (!"VSBLFL" %in% names(vs) && !"VSDTC" %in% names(vs)) {
      stop("When 'VSBLFL' is not available in vs, 'VSDTC' must be present for baseline determination")
    }
  }

  # Check if the required helper functions exist
  for (func in c("lubrify_dates", "recode_sex")) {
    if (!exists(func, mode = "function")) {
      stop(paste0("Required function '", func, "' not found"))
    }
  }
  
  # Check if calculate_bmi is needed and exists
  if (!is.null(vs) && !exists("calculate_bmi", mode = "function")) {
    stop("Required function 'calculate_bmi' not found")
  }

  # if AGE is not present in DM, calculate age from birthday and informed
  #   consent signature date
  if ("RFICDTC" %in% colnames(dm) && "BRTHDTC" %in% colnames(dm)) {
    dm <- dm %>%
      lubrify_dates() %>%
      mutate(age_brthdtc = round(as.numeric(
        lubridate::as.duration(.data$RFICDTC - .data$BRTHDTC), "years"), 0)) %>%
      {if("AGE" %in% names(dm))
        mutate(., AGE = case_when(
          is.na(.data$AGE) ~ .data$age_brthdtc,
          .default = .data$AGE))
        else
          mutate(., AGE = .data$age_brthdtc)} %>%
      select(-"age_brthdtc")
  }

  if(!is.null(vs)){
    # Check if RFSTDTC exists in dm when needed for baseline calculations
    if (!"VSBLFL" %in% names(vs) && !"RFSTDTC" %in% colnames(dm)) {
      stop("When 'VSBLFL' is not available in vs, 'RFSTDTC' must be present in dm for baseline determination")
    }

    baseline_covariates <- vs %>%
      lubrify_dates() %>%
      left_join(select(dm, c("USUBJID", "RFSTDTC")), by = "USUBJID") %>%
      {if("VSBLFL" %in% names(vs)) filter(., VSBLFL == "Y") else
        filter(., VSDTC < RFSTDTC)} %>%
      filter(VSTESTCD %in% c("WEIGHT", "HEIGHT")) %>%
      group_by(.data$USUBJID, .data$VSTESTCD) %>%
      summarize(mean = mean(.data$VSSTRESN), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "VSTESTCD", values_from = "mean")

    if ("HEIGHT" %in% colnames(baseline_covariates) &&
        "WEIGHT" %in% colnames(baseline_covariates)) {
      baseline_covariates <- baseline_covariates %>%
        mutate(BMI = calculate_bmi(.data$HEIGHT, .data$WEIGHT))
    }
  } else {
    baseline_covariates <- distinct(dm, .data$USUBJID)
  }

  # Apply filter to dm and prepare output
  filtered_dm <- dm %>%
    lubrify_dates() %>%
    filter(eval(parse(text = subject_filter)))

  # Only join with baseline_covariates if vs is not NULL
  if (!is.null(vs)) {
    out <- filtered_dm %>%
      left_join(baseline_covariates, by = "USUBJID") %>%
      recode_sex()
  } else {
    out <- filtered_dm %>%
      recode_sex()
  }
  
  # Generate sequential IDs instead of NA values
  out <- out %>%
    mutate(ID = row_number()) %>%
    relocate("ID") %>%
    select(., any_of(c(
      "ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "HEIGHT",
      "WEIGHT", "BMI", "ACTARMCD", "RFXSTDTC", "RFSTDTC", keep)))
  
  return(out)
}
