#' Add time-varying covariate
#'
#' Add a column to a [nif::nif] object representing a time-varying covariate. In
#' contrast to observations (see `add_observation()`), covariates are not
#' captured as rows of observation events but are attached as a separate column
#' to the observations in a nif object. The values reflect the status of the
#' covariate at the time of the existing observations and are carried forward as
#' needed.
#'
#' Covariate data may come from any domain, and like for observations, their
#' source is defined by the `domain` and `testcd` arguments. Covariate
#' observations can be further specified with the `cat` and `scat` arguments
#' that refer to the 'xxCAT' and 'xxSCAT' fields of the source domain, and the
#' `observation_filter` argument. This may be necessary, when observations
#' defined by the `testcd` alone are ambiguous.
#'
#' In general, the covariate value and the respective observation time stamp are
#' taken from the 'xxSTRESN' and 'xxDTC' fields of the source (where xx refers
#' to the domain code). Other fields can be specified by the `dv_field` and
#' `dtc_field` arguments.
#'
#' The name of the covariate columns can be specified by the `covariate`
#' argument. By default, it is set to the 'testcd' (without any prefix).
#'
#' @param nif A nif object.
#' @param sdtm The corresponding sdtm object.
#' @param domain The domain as character.
#' @param testcd The xxTESTCD with xx the domain name, as character.
#' @param dv_field The name of the DV field as character.
#' @param testcd_field The name of the TESTCD field. defaults to xxTESTCD (with
#'   xx the domain code), as character.
#' @param observation_filter A filter term for the `domain`, as character.
#' @param duplicate_function The function to apply if multiple covariate values
#'   are found by day.
#' @param covariate The name of the covariate, defaults to the testcd if NULL.
#' @param dtc_field The field to use as the date-time code for the observation.
#'   Defaults to the two-character domain name followed by 'DTC', if NULL.
#' @param silent Suppress messages, defaults to nif_option setting if NULL.
#' @param cat xxCAT filter to apply, as character.
#' @param scat xxSCAT filter to apply, as character.
#'
#' @return A nif object with a new column added that contains the time-varying
#'   covariate values. The name of this column is determined by the `covariate`
#'   parameter (or defaults to the value of `testcd` if not specified). The
#'   covariate values are matched to the nif object by USUBJID and date. For
#'   each subject, missing covariate values are filled using the last observed
#'   value (carrying forward).
#'
#' @seealso [nif::add_baseline()]
#' @export
#'
#' @examples
#' add_covariate(examplinib_poc_nif, examplinib_poc, "vs", "WEIGHT",
#'   covariate = "wt"
#' )
add_covariate <- function(
  nif,
  sdtm,
  domain,
  testcd,
  covariate = NULL,
  dtc_field = NULL,
  dv_field = NULL,
  testcd_field = NULL,
  observation_filter = "TRUE",
  cat = NULL,
  scat = NULL,
  duplicate_function = mean,
  silent = NULL
) {
  # input validation
  validate_nif(nif)
  validate_sdtm(sdtm)

  # validate_char_param(domain, "domain")
  # if (!has_domain(sdtm, domain))
  #   stop(paste0("Domain not found in sdtm object: ", domain))

  # # Get domain data
  # domain_data <- domain(sdtm, str_to_lower(domain))
  #
  # validate_char_param(testcd_field, "testcd_field", allow_null = TRUE)
  # if (is.null(testcd_field))
  #
  # if (is.null(testcd_field))
  #   validate_testcd(sdtm, testcd, domain)


  if (is.null(testcd_field)) {
    validate_testcd(sdtm, testcd, domain)
  } else {
    validate_char_param(domain, "domain")
    if (!testcd_field %in% names(domain(sdtm, domain))) {
      stop(paste0(
        "Testcode field ", testcd_field, " not found in domain ", domain, "!"
      ))
    }
    if (!testcd %in% unique(domain(sdtm, domain)[[testcd_field]])) {
      stop(paste0(
        "Testcd ", testcd, " not found in ",
        toupper(domain), "$", testcd_field, "!"
      ))
    }
  }



  validate_char_param(covariate, "covariate", allow_null = TRUE)
  validate_char_param(dtc_field, "dtc_field", allow_null = TRUE)
  validate_char_param(dv_field, "dv_field", allow_null = TRUE)
  validate_char_param(testcd_field, "testcd_field", allow_null = TRUE)
  validate_char_param(observation_filter, "observation_filter")
  validate_logical_param(silent, "silent", allow_null = TRUE)
  validate_char_param(cat, "cat", allow_null = TRUE)
  validate_char_param(scat, "scat", allow_null = TRUE)

  # Set up field names
  if (is.null(dtc_field)) dtc_field <- paste0(str_to_upper(domain), "DTC")
  if (is.null(dv_field)) dv_field <- paste0(str_to_upper(domain), "STRESN")
  if (is.null(testcd_field)) {
    testcd_field <- paste0(str_to_upper(domain), "TESTCD")
  }
  if (is.null(covariate)) covariate <- str_to_upper(testcd)
  cat_field <- paste0(toupper(domain), "CAT")
  scat_field <- paste0(toupper(domain), "SCAT")

  # check whether covariate name is already taken
  if (covariate %in% names(nif)) {
    stop(paste0(
      "Covariate ", covariate, " is already in nif, please provide a ",
      "unique name in the 'covariate' parameter!"
    ))
  }

  cov_field <- covariate

  # Get domain data
  domain_data <- domain(sdtm, str_to_lower(domain))

  # Validate required fields exist in domain data
  required_fields <- c("USUBJID", dtc_field, dv_field, testcd_field)
  missing_fields <- required_fields[!required_fields %in% names(domain_data)]
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Required fields missing in domain data: ",
      nice_enumeration(missing_fields)
    ))
  }

  # Validate testcd exists in the domain
  if (!testcd %in% domain_data[[testcd_field]]) {
    stop(paste0("Test code '", testcd, "' not found in domain '", domain, "'"))
  }

  # Validate subjects exist in both datasets
  if (!any(domain_data$USUBJID %in% nif$USUBJID)) {
    stop("No matching subjects found between SDTM domain and NIF object")
  }

  filtered_cov <- domain_data

  # Filter by CAT and SCAT
  if (!is.null(cat) && cat_field %in% names(filtered_cov)) {
    filtered_cov <- filter(filtered_cov, .data[[cat_field]] == cat)
  }

  if (!is.null(scat) && scat_field %in% names(filtered_cov)) {
    filtered_cov <- filter(filtered_cov, .data[[scat_field]] == scat)
  }

  # apply other filtering
  filtered_cov <- filtered_cov |>
    filter(.data$USUBJID %in% unique(nif$USUBJID)) |>
    lubrify_dates() |>
    filter(eval(parse(text = observation_filter))) |>
    filter(.data[[testcd_field]] == testcd)

  # Check if any data remains after filtering
  if (nrow(filtered_cov) == 0) {
    stop(paste0(
      "No data found for test code '", testcd,
      "' after applying observation filter"
    ))
  }

  cov <- filtered_cov |>
    tidyr::pivot_wider(
      names_from = all_of(testcd_field),
      values_from = all_of(dv_field),
      values_fn = duplicate_function
    ) |>
    rename("DTC" = all_of(dtc_field)) |>
    rename_with(~cov_field, all_of(testcd)) |>
    select(all_of(c("USUBJID", "DTC", cov_field))) |>
    distinct() |>
    mutate(original = 0)

  nif |>
    mutate(original = 1) |>
    bind_rows(cov) |>
    arrange(.data$USUBJID, .data$DTC, .data$original) |>
    tidyr::fill(!!cov_field) |>
    filter(.data$original == 1) |>
    select(!any_of(c("original"))) |>
    nif()
}
