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
  # input validation
  validate_nif(obj, fields = unique(c("NTIME", "ANALYTE", ref_time)))
  validate_argument(ntime_threshold, "numeric")

  if (!is.finite(ntime_threshold))
    stop("ntime_threshold must be finite")
  if (ntime_threshold < 0)
    stop("ntime_threshold must not be negative")

  validate_argument(ref_time, "character")
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

  # business logic
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


#' Plot time metrics against each other for observations
#'
#' @param obj A nif object.
#' @param max_time The maximum x axis time as numeric.
#' @param ... Further graphical parameters.
#' @param xtime The x axis time metric as character.
#' @param ytime The y axis time metric as character.
#' @param analyte The analyte as character. Defaults to all, if NULL.
#' @param color The field to color by, as character. Defaults to CHECK.
#'
#' @return A ggplot object.
#' @export
time_plot <- function(
    obj,
    xtime = "TIME",
    ytime = "TAD",
    analyte = NULL,
    max_time = NULL,
    color = "CHECK",
    ...
) {
  # input validation
  validate_argument(xtime, "character", values = c("TIME", "TAFD", "TAD", "NTIME"))
  validate_argument(ytime, "character", values = c("TIME", "TAFD", "TAD", "NTIME"))
  validate_argument(analyte, "character", allow_null = TRUE)
  validate_nif(obj, fields = c(xtime, ytime))
  if (is.null(analyte)) {
    analyte <- analytes(obj)
  }
  validate_analyte(obj, analyte)
  validate_argument(max_time, "numeric", allow_null = TRUE)
  if (is.null(max_time)) {
    max_time <- max(filter(obj, .data$EVID == 0)[, xtime], is.na = TRUE)
  }

  # business logic
  temp <- obj |>
    ensure_analyte() |>
    filter(.data$ANALYTE %in% analyte) |>
    filter(!is.na(.data[[xtime]]), !is.na(.data[[ytime]])) |>
    filter(.data[[xtime]] <= max_time) |>
    filter(.data$EVID == 0)

  p <- temp |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[[xtime]],
        y = .data[[ytime]],
        group = .data$ID
      )
    )

  if (color %in% names(obj)) {
    p <- temp |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data[[xtime]],
          y = .data[[ytime]],
          color = .data[[color]],
          group = .data$ID
        )
      ) +
      labs(color = NULL)
  } else {
    p <- temp |>
      ggplot2::ggplot(
        ggplot2::aes(
          x = .data[[xtime]],
          y = .data[[ytime]],
          group = .data$ID
        )
      )
  }

  p +
    ggplot2::geom_point(...) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    watermark()
}
