#' Add a baseline covariate
#'
#' Add a column to a [nif::nif] object that represents the baseline value for a
#' subject-level covariate.
#'
#' The source of the baseline covariate is specified by the `domain` and
#' `testcd` arguments. The baseline condition is defined by the
#' `baseline_filter` argument. If none is provided, the baseline filter defaults
#' to `xxBLFL == "Y"` where 'xx' is the domain code. In addition, a custom
#' `observation_filter` can be defined to further specify the observation. This
#' may be necessary, when observations defined by the `testcd` alone are
#' ambiguous, e.g., when for pharmacokinetic baseline observations, both BLOOD
#' and URINE observations are included in the PC domain data.
#'
#' The name of the baseline column defaults to the 'testcd', prefixed with
#' 'BL_', e.g., BL_WEIGHT. A specific name can be defined by the `name`
#' argument. Note that baseline WEIGHT, HEIGHT and BMI (if applicable) are
#' automatically included during the generation of a nif object.
#'
#'
#' @param nif A nif object.
#' @param sdtm A sdtm object.
#' @param domain The domain as character.
#' @param testcd The covariate variable name as character.
#' @param dv_field The name of the DV field as character.
#' @param testcd_field The name of the TESTCD field. defaults to xxTESTCD with
#'   xx the domain name, as character.
#' @param observation_filter A filter term for the `domain`, as character. Note:
#'   if the filter term includes date comparisons, make sure to represent the
#'   date as, datetime object e.g.,  `lubridate::as_datetime()`.
#' @param cat xxCAT filter to apply, as character.
#' @param scat xxSCAT filter to apply, as character.
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
#' @param factor A multiplier for the baseline value, defaults to 1.
#'
#' @return A nif object.
#' @importFrom stats na.omit
#' @export
#' @examples
#' add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT")
#' add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT",
#'   baseline_filter = "VSBLFL == 'Y'"
#' )
add_baseline <- function(
  nif,
  sdtm,
  domain,
  testcd,
  name = NULL,
  dv_field = NULL,
  testcd_field = NULL,
  observation_filter = "TRUE",
  cat = NULL,
  scat = NULL,
  baseline_filter = NULL,
  coding_table = NULL,
  summary_function = mean,
  factor = 1,
  silent = NULL
) {
  # input validation
  validate_nif(nif)
  validate_sdtm(sdtm)
  validate_testcd(sdtm, testcd, domain)
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(dv_field, "dv_field", allow_null = TRUE)
  validate_char_param(testcd_field, "testcd_field", allow_null = TRUE)
  validate_char_param(observation_filter, "observation_filter")
  validate_char_param(cat, "cat", allow_null = TRUE)
  validate_char_param(scat, "scat", allow_null = TRUE)
  validate_char_param(baseline_filter, "baseline_filter", allow_null = TRUE)
  validate_numeric_param(factor, "factor")
  validate_logical_param(silent, "silent", allow_null = TRUE)

  # create fields
  cat_field <- paste0(toupper(domain), "CAT")
  scat_field <- paste0(toupper(domain), "SCAT")

  # validate coding table
  if (!is.null(coding_table)) {
    if (!inherits(coding_table, "data.frame")) {
      stop("coding table must be a data frame!")
    }
    if (!"DV" %in% names(coding_table)) {
      stop("Coding table must include a numeric 'DV' field!")
    }
    if (!is.numeric(coding_table$DV)) {
      stop("DV field in coding table must be numeric!")
    }
  }

  if (is.null(dv_field)) dv_field <- paste0(str_to_upper(domain), "STRESN")
  if (is.null(testcd_field)) {
    testcd_field <- paste0(
      str_to_upper(domain), "TESTCD"
    )
  }

  # Set domain data
  domain_data <- domain(sdtm, str_to_lower(domain))

  # Validate required fields exist in domain data
  required_fields <- c("USUBJID", testcd_field, dv_field)
  missing_fields <- required_fields[!required_fields %in% names(domain_data)]
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Required fields missing in domain data: ",
      paste(missing_fields, collapse = ", ")
    ))
  }

  # Validate testcd exists in the domain
  if (!testcd %in% domain_data[[testcd_field]]) {
    stop(paste0("Test code '", testcd, "' not found in domain '", domain, "'"))
  }

  if (is.null(name)) {
    bl_field <- paste0("BL_", testcd)
  } else {
    bl_field <- name
  }

  # generate baseline filter
  if (is.null(baseline_filter)) {
    blcol <- intersect(
      c(
        paste0(str_to_upper(domain), "BLFL"),
        paste0(str_to_upper(domain), "LOBXFL")
      ),
      names(domain_data)
    )
    if (length(blcol) == 0) {
      stop(
        "No baseline flag column identified. Please provide a baseline_filter"
      )
    }
    baseline_filter <- paste0(
      blcol, " == 'Y'"
    )
    conditional_message(
      "baseline_filter for ", bl_field, " set to ",
      baseline_filter,
      silent = silent
    )
  }

  join_fields <- intersect(
    names(coding_table),
    names(domain(sdtm, str_to_lower(domain)))
  )

  if (!is.null(coding_table) && length(join_fields) == 0) {
    stop("Coding table cannot be applied - no valid data column!")
  } else {
    if (!is.null(coding_table)) {
      conditional_message(
        "Recoding from ", join_fields,
        silent = silent
      )
    }
  }

  filtered_domain <- domain(sdtm, str_to_lower(domain)) |>
    lubrify_dates() |>
    apply_cat_filter(cat, cat_field) |>
    apply_cat_filter(scat, scat_field)

  if (nrow(filtered_domain) == 0) {
    stop(paste0("No data after applying cat and scat filters!"))
  }

  # apply observation and baseline filters
  filtered_domain <- filtered_domain |>
    filter(eval(parse(text = observation_filter))) |>
    filter(.data[[testcd_field]] %in% testcd) |>
    filter(eval(parse(text = baseline_filter)))

  # Check if any data remains after filtering
  if (nrow(filtered_domain) == 0) {
    stop(paste0(
      "No data found for test code '", testcd,
      "' after applying baseline filter"
    ))
  }

  # apply coding table, if available
  if (is.null(coding_table)) {
    baseline <- mutate(filtered_domain, DV = .data[[dv_field]])
  } else {
    baseline <- left_join(filtered_domain, coding_table, by = join_fields)
  }

  baseline <- baseline |>
    mutate(DV = .data$DV * factor) |>
    tidyr::pivot_wider(
      names_from = all_of(testcd_field),
      values_from = "DV"
    ) |>
    select(all_of(c("USUBJID", {{ testcd }}))) |>
    group_by(.data$USUBJID) |>
    summarize(across(
      all_of(testcd),
      ~ summary_function(na.omit(.x))
    )) |>
    rename_with(~bl_field, .cols = all_of(testcd)) |>
    ungroup()

  # Check if all baseline values are NA
  if (all(is.na(baseline[[bl_field]]))) {
    stop(paste0(
      "No valid baseline values found for test code '", testcd,
      "'. Data was found but all values are NA after processing."
    ))
  }

  # Check if any baseline values are NA and warn user
  if (any(is.na(baseline[[bl_field]]))) {
    conditional_message(
      "Some subjects have missing baseline values for test code '",
      testcd, "'. These will be NA in the output.",
      silent = silent
    )
  }

  out <- nif |>
    coalesce_join(baseline, by = "USUBJID", join = "left_join")

  out
}


#' Extract the individual baseline value for an analyte
#'
#' @param obj A nif object.
#' @param analyte The analyte to derive the baseline for, as character. Defaults
#' to all analytes if NULL.
#' @param baseline_filter The baseline condition as character, defaults to
#' `TAFD <= 0`.
#' @param summary_function A function to reduce multiple baseline values,
#' defaults to `median`.
#' @param default_baseline The default value if the baseline filter computes to
#' NA.
#' @param silent Suppress messages, as logical.
#'
#' @returns A nif object with the DVBL field added for the specified analyte.
#' @export
#'
#' @examples
#' head(derive_baseline(examplinib_sad_nif, "RS2023"))
#'
derive_baseline <- function(
  obj,
  analyte = NULL,
  baseline_filter = "TAFD <= 0",
  summary_function = median,
  default_baseline = NA_real_,
  silent = NULL
) {
  obj <- ensure_analyte(obj)

  # Validate required columns
  required_cols <- c("ID", "DV", "TIME", "ANALYTE", "EVID")
  missing_cols <- setdiff(required_cols, names(obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # create empty DVBL column if needed
  if (!"DVBL" %in% names(obj)) {
    obj <- mutate(obj, DVBL = NA_real_)
  }

  # validate analyte
  if (is.null(analyte)) {
    analyte <- analytes(obj)
  }
  validate_analyte(obj, analyte)

  validate_numeric_param(default_baseline, "default_baseline", allow_na = TRUE)

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
        stop(paste(
          "baseline_filter must return a logical vector with length",
          "equal to number of rows"
        ))
      }
    },
    error = function(e) {
      stop("Invalid baseline_filter expression: ", e$message)
    }
  )

  temp <- obj |>
    filter(.data$ANALYTE %in% analyte)

  # Check for NA values in grouping columns
  na_ids <- temp$ID[is.na(temp$ID)]
  if (length(na_ids) > 0) {
    conditional_message(
      "Found NA values in ID column.",
      "These rows will be excluded from calculations.",
      silent = silent
    )
  }

  na_analytes <- temp$ANALYTE[is.na(temp$ANALYTE)]
  if (length(na_analytes) > 0) {
    conditional_message(
      "Found NA values in ANALYTE column.",
      "These rows will be excluded from calculations.",
      silent = silent
    )
  }

  # Helper function to calculate baseline for a group
  calc_baseline <- function(group_data, filter_expr, summary_fun, default) {
    filtered_dv <- na.omit(group_data$DV[eval(filter_expr, envir = group_data)])
    if (length(filtered_dv) == 0) {
      return(default)
    }
    result <- summary_fun(filtered_dv)
    if (is.na(result) || is.nan(result)) {
      return(default)
    }
    result
  }

  filter_expr <- parse(text = baseline_filter)

  bl <- temp |>
    filter(!is.na(.data$ID)) |>
    filter(!is.na(.data$ANALYTE)) |>
    filter(.data$EVID == 0) |>
    group_by(.data$ID, .data$ANALYTE) |>
    summarize(
      DVBL = calc_baseline(
        pick(everything()),
        filter_expr,
        summary_function,
        default_baseline
      ),
      .groups = "drop"
    ) |>
    as.data.frame()

  obj |>
    coalesce_join(
      bl,
      by = c("ID", "ANALYTE"), join = "left_join", keep = "right"
    )
}


#' Calculate change from baseline
#'
#' Extract the individual baseline value and change from baseline for an analyte
#'
#' @param obj A nif object.
#' @param analyte The analyte to derive the baseline for, as character. Defaults
#' to all analytes if NULL.
#' @param baseline_filter The baseline condition as character, defaults to
#' `TAFD <= 0`.
#' @param summary_function A function to reduce multiple baseline values,
#' defaults to `median`.
#' @param default_baseline The default value if the baseline filter computes to
#' NA.
#' @param silent Suppress messages, as logical.
#'
#' @returns A nif object with the DVBL field added for the specified analyte.
#' @export
#'
#' @examples
#' head(derive_cfb(examplinib_sad_nif))
#'
derive_cfb <- function(
  obj,
  analyte = NULL,
  baseline_filter = "TIME <= 0",
  summary_function = median,
  default_baseline = NA_real_,
  silent = NULL
) {
  derive_baseline(
    obj,
    analyte = analyte, baseline_filter = baseline_filter,
    summary_function = summary_function, default_baseline = default_baseline,
    silent = silent
  ) |>
    mutate(DVCFB = .data$DV - .data$DVBL)
}


#' Add baseline and change from baseline fields
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Pleas use [nif::derive_cfb()] instead.
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
add_cfb <- function(
  obj,
  baseline_filter = "TIME <= 0",
  summary_function = median,
  silent = NULL
) {
  lifecycle::deprecate_warn("0.61.1", "add_cfb()", "derive_cfb()")

  derive_cfb(obj,
    baseline_filter = baseline_filter,
    summary_function = summary_function,
    silent = silent
  )
}


#' Derive a new analyte with change from baseline from an existing analyte
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
  silent = NULL
) {
  # input validation
  validate_nif(obj)
  validate_char_param(source_analyte, "source_analyte")
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_logical_param(silent, "silent", allow_null = TRUE)

  if (!source_analyte %in% analytes(obj)) {
    stop(paste0("Analyte ", source_analyte, " not found in analytes!"))
  }

  if (is.null(analyte)) {
    analyte <- paste0("CFB_", source_analyte)
  }

  if (analyte %in% analytes(obj)) {
    stop(paste0("Analyte ", analyte, " already in data set!"))
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
        stop(paste(
          "baseline_filter must return a logical vector with length",
          "equal to number of rows"
        ))
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
    silent = silent
  )

  # make new analyte
  temp <- obj |>
    filter(.data$EVID == 0) |>
    filter(.data$ANALYTE == source_analyte) |>
    group_by(.data$ID) |>
    mutate(.BL = summary_function(
      na.omit(.data$DV[eval(parse(text = baseline_filter))])
    )) |>
    ungroup() |>
    mutate(DV = .data$DV - .data$.BL) |>
    select(-all_of(c(".BL", "REF"))) |>
    mutate(ANALYTE = analyte) |>
    mutate(CMT = cmt)

  out <- bind_rows(obj, temp) |>
    arrange(.data$USUBJID, .data$DTC) |>
    # index_nif()
    arrange_and_add_ref()

  out
}


#' Add baseline and relative-to-baseline fields
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @noRd
add_rtb <- function(obj, baseline_filter = "TIME <= 0",
                    summary_function = median) {
  obj |>
    ensure_analyte() |>
    as.data.frame() |>
    group_by(.data$ID, .data$ANALYTE) |>
    mutate(DVBL = summary_function(
      na.omit(.data$DV[eval(parse(text = baseline_filter))])
    )) |>
    mutate(DVRTB = .data$DV / .data$DVBL) |>
    ungroup() |>
    nif()
}


#' Add baseline creatinine
#'
#' @param obj A nif data set.
#' @param sdtm Source sdtm data object.
#' @param baseline_filter The filter term to identify baseline conditions.
#' @param silent Suppress messages.
#' @param observation_filter An observation filter term as character.
#'
#' @returns A nif object with the BL_CREAT column added, if possible. Otherwise
#' the unchanged input.
#' @export
add_bl_creat <- function(
  obj,
  sdtm,
  baseline_filter = NULL,
  observation_filter = "TRUE",
  silent = NULL
) {
  # input validation
  if (!"lb" %in% names(sdtm$domains)) {
    stop("LB domain not found!")
  }

  lb <- domain(sdtm, "lb")

  if (!"CREAT" %in% unique(lb$LBTESTCD)) {
    stop("No CREAT data found!")
  }

  micromolar_units <- c("umol/L", "umol/l", "micromol/l", "micromol/L")
  mg_units <- c("mg/dl", "mg/dL")

  # baseline filter
  if (is.null(baseline_filter)) {
    blcol <- intersect(c("LBBLFL", "LBLOBXFL"), names(lb))
    if (length(blcol) == 0) {
      stop(
        "No baseline flag column identified. Please provide a baseline_filter"
      )
    }
    baseline_filter <- paste0(
      blcol, " == 'Y'"
    )
  }

  if (!is_valid_filter(lb, baseline_filter)) {
    conditional_cli(
      cli_alert_warning("Invalid baseline filter, baseline CREAT not added"),
      silent = silent
    )
    return(obj)
  }

  # convert all CREAT values to micromolar concentrations
  temp_lb <- lb |>
    filter(.data$LBTESTCD == "CREAT")

  if (!"LBSTRESU" %in% names(lb)) {
    median_creat <- median(temp_lb$LBSTRESN, na.rm = TRUE)
    creat_unit <- ifelse(median_creat > 10, "umol/L", "mg/dL")
    temp_lb <- mutate(temp_lb, LBSTRESU = creat_unit)
    conditional_cli(
      cli_alert_warning(paste0("LBSTESU field not found. Values assumed in ",
                               creat_unit)),
      silent = silent
    )
  }

  temp_lb <- temp_lb|>
    mutate(.to_convert = .data$LBSTRESU %in% mg_units) |>
    mutate(.no_unit = !.data$LBSTRESU %in% c(mg_units, micromolar_units)) |>
    mutate(LBSTRESN = case_when(
      .data$.to_convert == TRUE ~ .data$LBSTRESN * 88.4,
      .default = .data$LBSTRESN
    )) |>
    mutate(LBSTRESU = case_when(
      .data$.to_convert == TRUE ~ "umol/L",
      .default = .data$LBSTRESU))

  if (any(temp_lb$.to_convert == TRUE))
    conditional_cli(
      cli_alert_info("BL_CREAT values converted to umol/L!"),
      silent = silent
    )

  if (any(temp_lb$.no_unit == TRUE))
    conditional_cli(
      cli_alert_warning("Some CREAT values had no unit and were deleted!"),
      silent = silent
    )

  temp_lb <- temp_lb |>
    filter(.data$.no_unit == FALSE)

  if (nrow(temp_lb) == 0) {
    conditional_cli(
      cli_alert_danger("No baseline CREAT data after filtering!")
    )
    return(obj)
  }

  temp_sdtm <- sdtm
  temp_sdtm$domains[["lb"]] <- temp_lb

  obj |>
    add_baseline(temp_sdtm, "lb", "CREAT",
      baseline_filter = baseline_filter,
      observation_filter = observation_filter
    )
}


#' Add baseline creatinine clearance field.
#'
#' The function expects BL_CREAT to be in umol/L units.
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#' @param molar Convert to molar units, as logical.
#'   Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
#' @return A NIF object with the baseline creatinine clearance (BL_EGFR) field
#' added,
#' @seealso [nif::egfr_mdrd()]
#' @seealso [nif::egfr_cg()]
#' @seealso [nif::egfr_raynaud()]
#' @export
#' @examples
#' head(add_bl_crcl(examplinib_poc_nif))
add_bl_crcl <- function(obj, method = egfr_cg, molar = TRUE) {
  missing_columns <- setdiff(
    c("BL_CREAT", "AGE", "SEX", "RACE", "WEIGHT"),
    names(obj)
  )
  if (length(missing_columns) > 0) {
    stop(paste0(
      "Missing coluns: ", nice_enumeration(missing_columns), "!"
    ))
  }

  if ("BL_CREAT" %in% colnames(obj)) {
    obj |>
      mutate(BL_CRCL = method(
        .data$BL_CREAT, .data$AGE, .data$SEX, .data$RACE, .data$WEIGHT,
        molar = molar
      ))
  } else {
    obj |>
      mutate(BL_CRCL = as.numeric(NA))
  }
}


#' Add baseline renal function class
#'
#' If baseline creatinine clearance (BL_CRCL) is not included in the input, it
#' will be calculated first.
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#' @param molar Use molar concentrations.
#' Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
#' @return A NIF object.
#' @export
#' @examples
#' head(add_bl_renal(examplinib_poc_nif), 5)
add_bl_renal <- function(obj, method = egfr_cg, molar = FALSE) {
  if (!"BL_CRCL" %in% names(obj)) {
    obj <- add_bl_crcl(obj, method = method, molar = molar)
  }

  obj |>
    mutate(BL_RENAL = as.character(
      cut(.data$BL_CRCL,
        breaks = c(0, 30, 60, 90, Inf),
        labels = c("severe", "moderate", "mild", "normal")
      )
    )) |>
    mutate(BL_RENAL = factor(.data$BL_RENAL,
      levels = c("normal", "mild", "moderate", "severe")
    ))
}


#' Add baseline lean body mass (LBM)
#'
#' @param obj A nif object.
#' @param method The function to calculate LBM, i.e., [nif::lbm_boer()],
#' [nif::lbm_hume()] or [nif::lbm_peters()].
#'
#' @return A nif object.
#' @seealso [nif::lbm_hume()]
#' @seealso [nif::lbm_boer()]
#' @seealso [nif::lbm_peters()]
#' @export
add_bl_lbm <- function(obj, method = lbm_boer) {
  # input validation
  validate_nif(obj)
  missing_fields <- setdiff(c("WEIGHT", "HEIGHT", "SEX"), names(obj))
  if (length(missing_fields) > 0) {
    stop(paste0("Missing fields: ", nice_enumeration(missing_fields)))
  }

  obj |>
    mutate(BL_LBM = method(.data$WEIGHT, .data$HEIGHT, .data$SEX))
}


#' Add baseline hepatic function class
#'
#' Based on the NCI ODWG criteria with TB the total (direct and indirect) serum
#' bilirubin, and AST aspartate aminotransferase.
#'
#' * normal: TB & AST ≤ upper limit of normal (ULN)
#' * mild hepatic dysfunction: TB > ULN to 1.5 x ULN or AST > ULN
#' * moderate hepatic dysfunction: TB >1.5–3 x ULN, any AST
#' * severe hepatic dysfunction: TB >3 - 10 x ULN, any AST
#'
#' @param obj A nif object.
#' @param sdtm The corresponding sdtm object.
#' @param baseline_filter A filter term to identify the baseline condition, as
#'   character.
#' @param summary_function The summary function to summarize multiple baseline
#'   values. Defaults to `mean`
#' @param observation_filter The filter term for the observation source data, as
#'   character.
#' @param silent Suppress messages.
#'
#' @return A nif object.
#' @export
add_bl_odwg <- function(
  obj,
  sdtm,
  observation_filter = NULL,
  baseline_filter = NULL,
  summary_function = mean,
  silent = NULL
) {
  # input validation
  validate_char_param(observation_filter, "observation_filter",
                      allow_null = TRUE)
  validate_char_param(baseline_filter, "baseline_filter", allow_null = TRUE)

  if (!"lb" %in% names(sdtm$domains)) {
    conditional_cli(
      cli_alert_warning("LB domain not found, BL_ODWG could not be added!"),
      silent = silent
    )
    return(obj)
  }

  lb <- sdtm |>
    domain("lb")

  # hepatic function markers
  missing_params <- setdiff(c("BILI", "AST"), unique(lb$LBTESTCD))
  if (length(missing_params) > 0) {
    conditional_cli(
      cli_alert_warning(
        "Missing hepatic function markers: ",
        nice_enumeration(missing_params)
      ),
      silent = silent
    )
    return(obj)
  }

  # baseline filter
  if (is.null(baseline_filter)) {
    blcol <- intersect(c("LBBLFL", "LBLOBXFL"), names(lb))
    if (length(blcol) == 0) {
      conditional_cli({
        cli_alert_warning("No baseline flag column found!")
        cli_text(
          "Please provide an explicit baseline filter to calculate BL_ODWG!")
        cli_text()
      }, silent = silent
      )
      return(obj)
    }

    baseline_filter <- paste0(
      blcol[1], " == 'Y'"
    )
  }

  # observation filter
  if (is.null(observation_filter)) {
    observation_filter <- "TRUE"
    if ("LBSPEC" %in% names(lb)) {
      if ("URINE" %in% unique(lb$LBSPEC)) {
        observation_filter <- "LBSPEC != 'URINE'"
      }
    }
  }

  # required fields
  missing_fields <- setdiff(c("LBSTRESN", "LBSTNRHI"), names(lb))
  if (length(missing_fields) > 0) {
    conditional_cli(
      cli_alert_warning(paste0(
        "Missing required field: ", nice_enumeration(missing_fields)
      )),
      silent = silent
    )
    return(obj)
  }

  # apply filters
  lb1 <- lb |>
    filter(eval(parse(text = baseline_filter))) |>
    filter(eval(parse(text = observation_filter))) |>
    filter(.data$LBTESTCD %in% c("AST", "BILI"))

  if (nrow(lb1) == 0) {
    conditional_cli({
      cli_alert_warning(
        "No hepatic function markers after baseline and observation filtering!")
      cli_text("BL_ODWG could not be derived!")
    },
    silent = silent)
    return(obj)
  }

  lb1 <- lb1 |>
    mutate(
      LBTESTCD = paste0(.data$LBTESTCD, "_X_ULN"),
      LBSTRESN = .data$LBSTRESN / .data$LBSTNRHI
    )

  sdtm$domains[["lb"]] <- lb1

  obj |>
    add_baseline(sdtm, "lb", "BILI_X_ULN", baseline_filter = "TRUE",
                 summary_function = summary_function) |>
    add_baseline(sdtm, "lb", "AST_X_ULN", baseline_filter = "TRUE",
                 summary_function = summary_function) |>
    mutate(BL_ODWG = case_when(
      .data$BL_BILI_X_ULN > 3 & .data$BL_BILI_X_ULN <= 10 ~ "severe",
      .data$BL_BILI_X_ULN > 1.5 & .data$BL_BILI_X_ULN <= 3 ~ "moderate",
      (.data$BL_BILI_X_ULN > 1 & .data$BL_BILI_X_ULN <= 1.5) |
        .data$BL_AST_X_ULN > 1 ~ "mild",
      .data$BL_BILI_X_ULN <= 1 & .data$BL_BILI_X_ULN <= 1 &
        .data$BL_AST_X_ULN <= 1 ~ "normal",
      .default = NA
    )) |>
    mutate(BL_ODWG = factor(.data$BL_ODWG,
      levels = c("normal", "mild", "moderate", "severe")
    ))
}
