#' Add baseline and change from baseline fields
#'
#' @details
#' Output fields:
#' * `DVBL` Baseline value for the dependent variable DV.
#' * `DVCFB` Change from baseline for the dependent variable DV.
#' @details The Baseline is calculated as the median (or the summary function
#' output) of the DV field for all time points identified by the
#' baseline_filter' term.
#'
#' @param obj A NIF object.
#' @param baseline_filter A filter term to identify the baseline condition.
#' @param summary_function The function to derive the baseline. This function is
#'   applied over the DV values identified by the 'baseline_filter' term. The
#'   default function is `median`. Alternatively, `mean`, `min` or `max` can be
#'   considered.
#' @param silent Suppress messages, defaults to nif_option setting if NULL.
#' @return A NIF object
#' @importFrom stats na.omit
#' @export
#' @examples
#' head(add_cfb(examplinib_poc_nif))
#' head(add_cfb(examplinib_poc_min_nif))
add_cfb <- function(
    obj,
    baseline_filter = "TIME <= 0",
    summary_function = median,
    silent = NULL) {

  # Validate required columns
  required_cols <- c("ID", "DV", "TIME")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Validate data types
  if (!is.numeric(obj$DV)) {
    stop("DV column must contain numeric values")
  }
  if (!is.numeric(obj$TIME)) {
    stop("TIME column must contain numeric values")
  }
  if (!is.numeric(obj$ID)) {
    stop("ID column must contain numeric values")
  }

  # Validate summary function
  if (!is.function(summary_function)) {
    stop("summary_function must be a function")
  }

  # Safe evaluation of filter
  tryCatch(
    {
      filter_expr <- parse(text = baseline_filter)
      test_eval <- eval(filter_expr, envir = obj)
      if (!is.logical(test_eval)) {
        stop("baseline_filter must evaluate to logical values")
      }
      if (length(test_eval) != nrow(obj)) {
        stop("baseline_filter must return a logical vector with length equal to number of rows")
      }
    },
    error = function(e) {
      stop("Invalid baseline_filter expression: ", e$message)
    }
  )

  # Ensure ANALYTE column exists and handle NA values
  obj <- obj %>%
    ensure_analyte() %>%
    as.data.frame()

  # Check for NA values in grouping columns
  na_ids <- obj$ID[is.na(obj$ID)]
  if (length(na_ids) > 0) {
    conditional_message(
      "Found NA values in ID column.",
      "These rows will be excluded from calculations.",
      silent = silent)
  }

  na_analytes <- obj$ANALYTE[is.na(obj$ANALYTE)]
  if (length(na_analytes) > 0) {
    conditional_message(
      "Found NA values in ANALYTE column.",
      "These rows will be excluded from calculations.",
      silent = silent)
  }

  # Filter out NA values in grouping columns before calculations
  obj %>%
    filter(!is.na(.data$ID), !is.na(.data$ANALYTE)) %>%
    group_by(.data$ID, .data$ANALYTE) %>%
    mutate(DVBL = summary_function(
      na.omit(.data$DV[eval(parse(text = baseline_filter))])
    )) %>%
    mutate(DVCFB = .data$DV - .data$DVBL) %>%
    new_nif()
}


#' Derive a new analyte with change from baseline from an existing analyte
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param source_analyte The original analyte.
#' @param analyte The name of the derived analyte. Defaults to "CFB_xx" with xx
#'   the original analyte name.
#' @param baseline_filter A filter term to identify the baseline condition.
#' @param summary_function The function to derive the baseline. This function is
#'   applied over the DV values identified by the 'baseline_filter' term. The
#'   default function is `median`. Alternatively, `mean`, `min` or `max` can be
#'   considered.
#' @param silent Suppress messages, defaults to nif_option setting if NULL.
#'
#' @returns A nif object.
#' @export
derive_cfb_analyte <- function(
    obj, source_analyte, analyte = NULL,
    baseline_filter = "TIME <= 0",
    summary_function = median,
    silent = NULL) {
  # input validation
  validate_nif(obj)
  validate_char_param(source_analyte, "source_analyte")
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_logical_param(silent, "silent", allow_null = TRUE)

  if(!source_analyte %in% analytes(obj))
    stop(paste0("Analyte ", source_analyte, " not found in analytes!"))

  if(is.null(analyte))
    analyte <- paste0("CFB_", source_analyte)

  if(analyte %in% analytes(obj))
    stop(paste0("Analyte ", analyte, " already in data set!"))

  # Validate summary function
  if (!is.function(summary_function)) {
    stop("summary_function must be a function")
  }

  # Safe evaluation of filter
  tryCatch({
      filter_expr <- parse(text = baseline_filter)
      test_eval <- eval(filter_expr, envir = obj)
      if (!is.logical(test_eval)) {
        stop("baseline_filter must evaluate to logical values")
      }
      if (length(test_eval) != nrow(obj)) {
        stop("baseline_filter must return a logical vector with length equal to number of rows")
      }
    },
    error = function(e) {
      stop("Invalid baseline_filter expression: ", e$message)
    }
  )

  # cmt
  cmt <- max(obj$CMT) + 1
  conditional_message(
    "Compartment for ", analyte, " set to ", cmt,
    silent = silent)

  # make new analyte
  temp <- obj %>%
    filter(.data$EVID == 0) %>%
    filter(.data$ANALYTE == source_analyte) %>%
    group_by(.data$ID) %>%
    mutate(.BL = summary_function(
      na.omit(.data$DV[eval(parse(text = baseline_filter))])
    )) %>%
    ungroup() %>%
    mutate(DV = .data$DV - .data$.BL) %>%
    select(-all_of(c(".BL", "REF"))) %>%
    mutate(ANALYTE = analyte) %>%
    mutate(CMT = cmt)

  out <- bind_rows(obj, temp) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    index_nif()

  return(out)
}




#' Add baseline and relative-to-baseline fields
#'
#' @details
#' Output fields:
#' * `DVBL` Baseline value for the dependent variable DV.
#' * `DVRTB` Relative-to-baseline value for the dependent variable DV.
#' @details The Baseline is calculated as the median (or the summary function
#' output) of the DV field for all time points identified by the
#' baseline_filter' term.
#'
#' @param obj A NIF object.
#' @param baseline_filter A filter term to identify the baseline condition.
#' @param summary_function The function to derive the baseline. This function is
#'   applied over the DV values identified by the 'baseline_filter' term. The
#'   default function is `median`. Alternatively, `mean`, `min` or `max` can be
#'   considered.
#' @return A NIF object
#' @importFrom stats na.omit
#' @export
#' @examples
#' head(add_rtb(examplinib_poc_nif))
#' head(add_rtb(examplinib_poc_min_nif))
add_rtb <- function(obj, baseline_filter = "TIME <= 0",
                    summary_function = median) {
  obj %>%
    ensure_analyte() %>%
    as.data.frame() %>%
    group_by(.data$ID, .data$ANALYTE) %>%
    mutate(DVBL = summary_function(
      na.omit(.data$DV[eval(parse(text = baseline_filter))])
    )) %>%
    mutate(DVRTB = .data$DV / .data$DVBL) %>%
    new_nif()
}
