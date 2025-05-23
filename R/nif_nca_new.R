#' Perform Non-Compartmental Analysis (NCA) for each subject and analyte
#'
#' This function performs NCA analysis using the pknca package for each subject and analyte
#' in a NIF object. It identifies administrations using the EVID field and calculates
#' PK parameters for each dosing interval.
#'
#' @param obj A NIF object containing concentration-time data
#' @param analytes Optional vector of analytes to analyze. If NULL, all analytes will be analyzed.
#' @param parameters Optional vector of PK parameters to calculate. If NULL, default parameters will be used.
#' @param keep Optional vector of additional columns to keep in the output.
#'
#' @return A long data frame containing NCA results with at least the fields USUBJID, PPTESTCD, ANALYTE, PPSTRESN
#' @import dplyr
#' @importFrom PKNCA PKNCAdata PKNCAconc PKNCAdose PKNCAdata PKNCAresults
#' @export
#'
#' @examples
#' # nca_results <- nif_nca_new(nif_object)
nif_nca_new <- function(obj, analytes = NULL, parameters = NULL, keep = NULL) {
  # Validate input
  if (!inherits(obj, "nif")) {
    stop("Input must be a NIF object")
  }

  # Get all analytes if not specified
  if (is.null(analytes)) {
    analytes <- analytes(obj)
  }

  # Default parameters if not specified
  if (is.null(parameters)) {
    parameters <- c(
      "auclast", "aucinf", "cmax", "tmax", "half.life",
      "cl", "vz", "mrt", "lambda.z"
    )
  }

  # Initialize empty result data frame
  results <- data.frame()

  # Process each analyte separately
  for (current_analyte in analytes) {
    # Filter data for current analyte
    analyte_data <- obj %>%
      as.data.frame() %>%
      filter(.data$ANALYTE == current_analyte)

    # Get unique subjects
    subjects <- unique(analyte_data$USUBJID)

    # Process each subject
    for (subject in subjects) {
      # Filter data for current subject
      subject_data <- analyte_data %>%
        filter(.data$USUBJID == subject)

      # Get dosing information
      doses <- subject_data %>%
        filter(.data$EVID == 1) %>%
        select("USUBJID", "TIME", "AMT")

      # Get concentration data
      conc <- subject_data %>%
        filter(.data$EVID == 0) %>%
        select("USUBJID", "TIME", "DV")

      # Skip if no observations
      if (nrow(conc) == 0) next

      # Create PKNCA objects
      conc_obj <- PKNCA::PKNCAconc(conc, DV ~ TIME | USUBJID)
      dose_obj <- PKNCA::PKNCAdose(doses, AMT ~ TIME | USUBJID)

      # Create intervals for each dose
      intervals <- data.frame(
        start = doses$TIME,
        end = c(doses$TIME[-1], max(conc$TIME)),
        aucinf.obs = TRUE
      ) %>%
        filter(.data$start < .data$end)

      # Create PKNCA data object
      data_obj <- PKNCAdata(conc_obj, dose_obj, intervals = intervals)

      nca_results <- PKNCA::pk.nca(data_obj)

      # Extract results
      # subject_results <- summary(nca_results) %>%
      #   mutate(
      #     USUBJID = subject,
      #     ANALYTE = current_analyte,
      #     PPTESTCD = parameter
      #   ) %>%
      #   rename(PPSTRESN = result) %>%
      #   select("USUBJID", "ANALYTE", "PPTESTCD", "PPSTRESN")

      subject_results <- nca_results$result %>%
        mutate(ANALYTE = current_analyte) %>%
        select("USUBJID", "ANALYTE", "PPTESTCD", "PPORRES")

      # Add any additional columns to keep
      if (!is.null(keep)) {
        keep_data <- subject_data %>%
          select(any_of(keep)) %>%
          distinct() %>%
          slice(1)
        subject_results <- subject_results %>%
          bind_cols(keep_data)
      }

      # Add to results
      results <- bind_rows(results, subject_results)
    }
  }

  return(results)
}
