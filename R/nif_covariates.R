#' Attach time-varying covariate
#'
#' A time-varying covariate is added as a new field with daily time granularity
#' and carried forward for missing entries. The name of the covariate can be
#' specified by 'covariate'. By default, it is set to the 'testcd' (without any
#' prefix).
#'
#' @param nif A nif object.
#' @param sdtm The corresponding sdtm object.
#' @param domain The domain as character.
#' @param testcd The xxTESTCD with xx the domain name, as character.
#' @param DV_field The name of the DV field as character.
#' @param TESTCD_field The name of the TESTCD field. defaults to xxTESTCD (with
#'   xx the domain code), as character.
#' @param observation_filter A filter term for the `domain`, as character.
#' @param duplicate_function The function to apply if multiple covariate values
#'   are found by day.
#' @param covariate The name of the covariate, defaults to the testcd if NULL.
#' @param DTC_field The field to use as the date-time code for the observation.
#'   Defaults to the two-character domain name followed by 'DTC', if NULL.
#' @param silent Suppress messages, defaults to nif_option setting if NULL.
#'
#' @return A nif object with a new column added that contains the time-varying covariate values.
#'   The name of this column is determined by the `covariate` parameter (or defaults to
#'   the value of `testcd` if not specified). The covariate values are matched to the nif
#'   object by USUBJID and date. For each subject, missing covariate values are filled
#'   using the last observed value (carrying forward).
#'
#' @seealso add_baseline()
#' @export
#'
#' @examples
#' add_covariate(examplinib_poc_nif, examplinib_poc, "vs", "WEIGHT",
#'   covariate = "wt")
add_covariate <- function(
    nif,
    sdtm,
    domain = NULL,
    testcd,
    covariate = NULL,
    DTC_field = NULL,
    DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = "TRUE",
    duplicate_function = mean,
    silent = NULL
) {
  # input validation
  validate_nif(nif)
  validate_sdtm(sdtm)
  if(is.null(TESTCD_field)) {
    validate_testcd(sdtm, testcd, domain)
  } else {
    validate_char_param(domain, "domain")
    if(!TESTCD_field %in% names(domain(sdtm, domain)))
      stop(paste0(
        "Testcode field ", TESTCD_field, " not found in domain ", domain, "!"))
    if(!testcd %in% unique(domain(sdtm, domain)[[TESTCD_field]]))
      stop(paste0(
        "Testcd ", testcd, " not found in ",
        toupper(domain), "$", TESTCD_field, "!"))
  }

  # validate_char_param(domain, "domain")

  # # Validate domain exists in sdtm
  # if (!domain %in% names(sdtm$domains)) {
  #   stop(paste0("Domain '", domain, "' not found in sdtm object"))}

  # validate_char_param(testcd, "testcd")

  validate_char_param(covariate, "covariate", allow_null = TRUE)
  validate_char_param(DTC_field, "DTC_field", allow_null = TRUE)
  validate_char_param(DV_field, "DV_field", allow_null = TRUE)
  validate_char_param(TESTCD_field, "TESTCD_field", allow_null = TRUE)
  validate_char_param(observation_filter, "observation_filter")
  validate_logical_param(silent, "silent", allow_null = TRUE)

  # Set up field names
  if(is.null(DTC_field)) DTC_field <- paste0(str_to_upper(domain), "DTC")
  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field))
    TESTCD_field <- paste0(str_to_upper(domain), "TESTCD")
  if(is.null(covariate)) covariate <- str_to_upper(testcd)

  # check whether covariate name is already taken
  if(covariate %in% names(nif)) {
    stop(paste0(
      "Covariate ", covariate, " is already in nif, please provide a ",
      "unique name in the 'covariate' parameter!"
    ))
  }

  COV_field <- covariate

  # Get domain data
  domain_data <- domain(sdtm, str_to_lower(domain))

  # Validate required fields exist in domain data
  required_fields <- c("USUBJID", DTC_field, DV_field, TESTCD_field)
  missing_fields <- required_fields[!required_fields %in% names(domain_data)]
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Required fields missing in domain data: ",
      nice_enumeration(missing_fields)
    ))
  }

  # Validate testcd exists in the domain
  if (!testcd %in% domain_data[[TESTCD_field]]) {
    stop(paste0("Test code '", testcd, "' not found in domain '", domain, "'"))
  }

  # Validate subjects exist in both datasets
  if (!any(domain_data$USUBJID %in% nif$USUBJID)) {
    stop("No matching subjects found between SDTM domain and NIF object")
  }

  filtered_cov <- domain_data %>%
    filter(.data$USUBJID %in% unique(nif$USUBJID)) %>%
    lubrify_dates() %>%
    filter(eval(parse(text = observation_filter))) %>%
    filter(.data[[TESTCD_field]] == testcd)

  # Check if any data remains after filtering
  if (nrow(filtered_cov) == 0) {
    stop(paste0(
      "No data found for test code '", testcd,
      "' after applying observation filter"))
  }

  cov <- filtered_cov %>%
    # filter(.data[[TESTCD_field]] == testcd) %>%
    tidyr::pivot_wider(
      names_from = all_of(TESTCD_field),
      values_from = all_of(DV_field),
      values_fn = duplicate_function
    ) %>%
    rename("DTC" = all_of(DTC_field)) %>%
    rename_with(~COV_field, all_of(testcd)) %>%
    decompose_dtc("DTC") %>%
    select(!all_of(c("DTC", "DTC_time"))) %>%
    select(all_of(c("USUBJID", "DTC_date", COV_field))) %>%
    distinct()

  temp <- nif %>%
    mutate(original = TRUE) %>%
    decompose_dtc("DTC") %>%
    full_join(cov, by = c("USUBJID", "DTC_date")) %>%
    arrange(.data$USUBJID, .data$DTC_date) %>%
    group_by(.data$USUBJID) %>%
    tidyr::fill(!!COV_field) %>%
    ungroup() %>%
    filter(.data$original == TRUE) %>%
    select(!any_of(c("original", "DTC_date", "DTC_time"))) %>%
    new_nif()

  return(temp)
}


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

  # validate_char_param(domain, "domain")

  # # Validate domain exists in sdtm
  # if (!domain %in% names(sdtm$domains)) {
  #   stop(paste0("Domain '", domain, "' not found in sdtm object"))}

  # validate_char_param(testcd, "testcd")
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(DV_field, "DV_field", allow_null = TRUE)
  validate_char_param(TESTCD_field, "TESTCD_field", allow_null = TRUE)
  validate_char_param(observation_filter, "observation_filter")
  validate_char_param(baseline_filter, "baseline_filter", allow_null = TRUE)
  validate_logical_param(silent, "silent", allow_null = TRUE)

  # # ensure testcd is valid
  # if(!testcd %in% testcd(sdtm)$TESTCD) {
  #   msg <- paste0("Testcd ", testcd, " not found in sdtm")
  #   if(toupper(testcd) %in% testcd(sdtm)$TESTCD)
  #     msg <- paste0(msg, " (did you mean ", toupper(testcd), "?)")
  #   stop(msg)
  # }

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
    # summarize(across(
    #   all_of(testcd),
    #   ~ summary_function(na.omit(.x, na.rm = TRUE)))) %>%

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
