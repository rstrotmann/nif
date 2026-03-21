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
  # input validation
  validate_nif(obj)

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

  conditional_cli(
    cli_alert_info(paste0(
      nrow(filter(obj, .data$.time_deviation_flag == TRUE)),
      " rows with time deviations"
    )),
    silent = silent
  )

  obj |>
    select(-c(".time_deviation_flag"))
}
