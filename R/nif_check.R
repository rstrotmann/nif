#' Check nif or sdtm object
#'
#' @param obj The nif or sdtm object
#' @param ... Further arguments.
#'
#' @returns The input with the CHECK field added.
#' @export
check <- function(obj, ...) {
  UseMethod("check")
}


#' Check nif object for potential issues
#'
#' @param obj A nif object.
#' @param ntime_threshold The allowed fractional difference between TAD and NTIME.
#' @param silent Suppress messages.
#' @param ref_time Time field
#' @param analyte The analyte to apply the nif check to. Defaults to all if NULL.
#' @param ... Further arguments.
#'
#' @returns The nif object with the CHECK field added.
#' @export
#' @noRd
check.nif <- function(
    obj,
    analyte = NULL,
    ntime_threshold = 0.2,
    ref_time = "TAD",
    silent = NULL,
    ...
  ) {
  validate_nif(obj)
  validate_argument(ntime_threshold, "numeric")

  if (!is.finite(ntime_threshold))
    stop("ntime_threshold must be finite")
  if (ntime_threshold < 0)
    stop("ntime_threshold must not be negative")

  validate_argument(ref_time, "character")
  validate_fields(obj, unique(c("NTIME", "ANALYTE", ref_time)))
  validate_argument(silent, type = "logical", allow_null = TRUE)
  validate_argument(analyte, "character", allow_null = TRUE, allow_multiple = TRUE)

  missing_analytes <- setdiff(analyte, analytes(obj))
  if (length(missing_analytes) > 0)
    stop(paste0(
      "Missing ", plural("analyte", length(missing_analytes) > 1),
      ": ", nice_enumeration(missing_analytes)))

  # setup
  if (!"CHECK" %in% names(obj))
    obj <- mutate(obj, CHECK = "")

  if (is.null(analyte))
    analyte <- analytes(obj)

  # check for conspicuous time deviations
  obj <- obj |>
    mutate(
      .time_deviation_flag = (.data[[ref_time]] - .data$NTIME) >
        .data$NTIME * ntime_threshold &
        .data$ANALYTE %in% analyte
    ) |>
    mutate(CHECK = case_when(
      .data$.time_deviation_flag == TRUE ~
        paste0(ref_time, " inconsistent with NTIME"),
      .default = .data$CHECK))

  n_time_dev_rows <- nrow(filter(obj, .data$.time_deviation_flag == TRUE))

  if (n_time_dev_rows > 0) {
    conditional_cli(
      cli_alert_info(paste0(
        plural("analyte", length(analyte) > 1), " ",
        nice_enumeration(analyte, conjunction = "or"), ": ",
        n_time_dev_rows, plural(" row", n_time_dev_rows > 1),
        " with ", ref_time, " deviating from NTIME by >",
        ntime_threshold * 100, "%"
      )),
      silent = silent
    )
  }

  obj |>
    select(-c(".time_deviation_flag"))
}
