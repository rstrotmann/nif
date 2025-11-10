#' Attach baseline covariate
#'
#' Baseline covariates, as specified by the 'testcd' field, can come from any
#' SDTM domain. By default, the baseline value is identified by `xxBLFL == "Y"`
#' in the respective SDTM domain. Alternatively, a custom observation filter can
#' be defined. The name of the baseline covariate is the 'testcd', prefixed with
#' 'BL_'.
#'
#' @param nif A nif object.
#' @param sdtm A sdtm object.
#' @param domain The domain as character.
#' @param testcd The covariate variable name as character.
#' @param DV_field The name of the DV field as character.
#' @param TESTCD_field The name of the TESTCD field. defaults to xxTESTCD with
#'   xx the domain name, as character.
#' @param observation_filter A filter term for the `domain`, as character. Note:
#'   if the filter term includes date comparisons, make sure to represent the
#'   date as, datetime object e.g.,  `lubridate::as_datetime()`.
#' @param baseline_filter A filter term to identify the baseline condition.
#'   within the `domain`. Defaults to either "xxBLFL == 'Y'" or
#'   "xxLOBXFL == 'Y'" (with xx the domain code), whichever is found first in
#'   the domain data.
#' @param summary_function The summary function to summarize multiple baseline
#'   values. Defaults to `mean`.
#' @param silent Suppress messages, defaults to nif_option setting if NULL.
#' @param coding_table A recoding table as data frame, or NULL. If present, the
#'   table needs to have a field that matches a column in the domain, and a
#'   field 'DV' that provides the re-coded value.
#' @param name The column label, as character.
#'
#' @return A nif object.
#' @importFrom stats na.omit
#' @export
#' @examples
#' add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT")
#' add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT",
#'   baseline_filter = "VSBLFL == 'Y'")
add_baseline <- function(
    nif,
    sdtm,
    domain,
    testcd,
    name = NULL,
    DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = "TRUE",
    baseline_filter = NULL,
    coding_table = NULL,
    summary_function = mean,
    silent = NULL) {
  # input validation
  validate_nif(nif)
  validate_sdtm(sdtm)
  validate_testcd(sdtm, testcd, domain)

  # validate_char_param(testcd, "testcd")
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(DV_field, "DV_field", allow_null = TRUE)
  validate_char_param(TESTCD_field, "TESTCD_field", allow_null = TRUE)
  validate_char_param(observation_filter, "observation_filter")
  validate_char_param(baseline_filter, "baseline_filter", allow_null = TRUE)
  validate_logical_param(silent, "silent", allow_null = TRUE)

  # validate coding table
  if(!is.null(coding_table)) {
    if (!inherits(coding_table, "data.frame"))
      stop("coding table must be a data frame!")
    if(!"DV" %in% names(coding_table))
      stop("Coding table must include a numeric 'DV' field!")
    if(!is.numeric(coding_table$DV))
      stop("DV field in coding table must be numeric!")
  }

  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(
    str_to_upper(domain), "TESTCD")

  # Set domain data
  domain_data <- domain(sdtm, str_to_lower(domain))

  # Validate required fields exist in domain data
  required_fields <- c("USUBJID", TESTCD_field, DV_field)
  missing_fields <- required_fields[!required_fields %in% names(domain_data)]
  if (length(missing_fields) > 0) {
    stop(paste0("Required fields missing in domain data: ",
                paste(missing_fields, collapse = ", ")))
  }

  # Validate testcd exists in the domain
  if (!testcd %in% domain_data[[TESTCD_field]]) {
    stop(paste0("Test code '", testcd, "' not found in domain '", domain, "'"))
  }

  if(is.null(name)) {
    bl_field <- paste0("BL_", testcd)
  } else {
    bl_field <- name
  }

  # generate baseline filter
  if(is.null(baseline_filter)){
    blcol <- intersect(c(
      paste0(str_to_upper(domain), "BLFL"),
      paste0(str_to_upper(domain), "LOBXFL")
    ),
    names(domain_data))
    if(length(blcol) == 0) {
      stop(
        "No baseline flag column identified. Please provide a baseline_filter"
      )
    }
    baseline_filter = paste0(
      blcol, " == 'Y'"
    )
    conditional_message(
      "baseline_filter for ", bl_field, " set to ",
      baseline_filter,
      silent = silent
    )
  }

  join_fields <- intersect(names(coding_table),
                           names(domain(sdtm, str_to_lower(domain))))

  if(!is.null(coding_table) & length(join_fields) == 0) {
    stop("Coding table cannot be applied - no valid data column!")
  } else {
    if(!is.null(coding_table)) {
      conditional_message(
        "Recoding from ", join_fields,
        silent = silent)}
  }

  filtered_domain <- domain(sdtm, str_to_lower(domain)) %>%
    lubrify_dates() %>%
    filter(eval(parse(text = observation_filter))) %>%
    filter(.data[[TESTCD_field]] %in% testcd) %>%
    filter(eval(parse(text = baseline_filter)))

  # Check if any data remains after filtering
  if (nrow(filtered_domain) == 0) {
    stop(paste0("No data found for test code '", testcd,
                "' after applying baseline filter"))
  }

  baseline <- filtered_domain %>%
    {if(is.null(coding_table)) mutate(., DV = .data[[DV_field]]) else
      left_join(., coding_table, by = join_fields)} %>%
    tidyr::pivot_wider(names_from = all_of(TESTCD_field),
                       values_from = DV) %>%
    select(all_of(c("USUBJID", {{testcd}}))) %>%
    group_by(.data$USUBJID) %>%
    summarize(across(
      all_of(testcd),
      ~ summary_function(na.omit(.x)))) %>%
    rename_with(~bl_field, .cols = all_of(testcd)) %>%
    ungroup()

  # Check if all baseline values are NA
  if (all(is.na(baseline[[bl_field]]))) {
    stop(paste0("No valid baseline values found for test code '", testcd,
                "'. Data was found but all values are NA after processing."))
  }

  # Check if any baseline values are NA and warn user
  if (any(is.na(baseline[[bl_field]]))) {
    conditional_message(
      "Some subjects have missing baseline values for test code '",
      testcd, "'. These will be NA in the output.",
      silent = silent
    )
  }

  out <- nif %>%
    coalesce_join(baseline, by = "USUBJID", join = 'left_join')

  return(out)
}


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
