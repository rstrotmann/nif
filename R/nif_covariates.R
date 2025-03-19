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
#' @param covariate The name of the covariate, defaults to the testcd if 'NULL'.
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
    nif, sdtm, domain, testcd,
    covariate = NULL,
    DTC_field = NULL,
    DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = "TRUE",
    duplicate_function = mean,
    silent = NULL
) {
  # Validate that nif is a nif object
  if (!inherits(nif, "nif")) {
    stop("First argument must be a nif object")
  }

  # Validate that sdtm object exists
  if (missing(sdtm)) {
    stop("SDTM object is required")
  }

  # Validate domain exists in sdtm
  if (!domain %in% names(sdtm)) {
    stop(paste0("Domain '", domain, "' not found in SDTM object"))
  }

  # Set up field names
  if(is.null(DTC_field)) DTC_field <- paste0(str_to_upper(domain), "DTC")
  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(str_to_upper(domain),
                                                   "TESTCD")
  if(is.null(covariate)) covariate <- str_to_upper(testcd)
  COV_field <- covariate

  # Get domain data
  domain_data <- domain(sdtm, str_to_lower(domain))

  # Validate required fields exist in domain data
  required_fields <- c("USUBJID", DTC_field, DV_field, TESTCD_field)
  missing_fields <- required_fields[!required_fields %in% names(domain_data)]
  if (length(missing_fields) > 0) {
    stop(paste0("Required fields missing in domain data: ",
                paste(missing_fields, collapse = ", ")))
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
    filter(eval(parse(text = observation_filter)))

  # Check if any data remains after filtering
  if (nrow(filtered_cov) == 0) {
    stop(paste0("No data found for test code '", testcd,
                   "' after applying observation filter"))
    # return(nif)
  }

  cov <- filtered_cov %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    tidyr::pivot_wider(names_from = all_of(TESTCD_field),
                       values_from = all_of(DV_field),
                       values_fn = duplicate_function) %>%
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
