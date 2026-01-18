#' Make BINTIME field
#'
#'#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param method Univariate class intervals method, can be one of jenks, kmeans,
#' pretty, quantile, hclust, sd, bclust or fisher. See classInt::classInterval()
#' for details.
#' @param silent Suppress messages.
#' @param obj A nif object.
#'
#' @returns A nif object with the BINTIME, BIN_LEFT and BIN_RIGHT fields added.
#' @importFrom classInt classIntervals
#' @export
add_bintime <- function(
    obj,
    method = "fisher",
    silent = NULL) {
  # input validation
  validate_nif(obj)
  validate_char_param(method, "method")
  if(!method %in%c("jenks", "kmeans", "pretty", "quantile", "hclust", "sd",
                 "bclust", "fisher")) {
    stop(paste0("Method ", method, " not implemented!"))
  }

  # calculate bins
  obj <- ensure_tafd(obj)
  bins <- classInt::classIntervals(obj$TAFD, style = method)
  bin_par <- attributes(bins)$parameters

  breaks <- sort(bins$brks)
  labels <- bin_par[,3]

  bin_par <- as.data.frame(bin_par[order(bin_par[,1]),]) |>
    arrange(min) |>
    mutate(left = breaks[1:length(breaks) - 1]) |>
    mutate(right = breaks[-1]) |>
    mutate(label = round(.data[["class mean"]]))

  obj |>
    mutate(.BINTIME_INDEX = as.numeric(cut(.data$TAFD, breaks, labels))) |>
    mutate(BIN_LEFT = bin_par[.data$.BINTIME_INDEX, "left"]) |>
    mutate(BIN_RIGHT = bin_par[.data$.BINTIME_INDEX, "right"]) |>
    mutate(BINTIME = round(bin_par[.data$.BINTIME_INDEX, "label"])) |>
    select(-c(".BINTIME_INDEX"))
}


#' BINTIME plot
#'
#'#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param analyte The analyte as character.
#' @param method Univariate class intervals method, can be one of jenks, kmeans,
#' pretty, quantile, hclust, sd, bclust or fisher. See classInt::classInterval()
#' for details.
#' @param points Plot original data points as logical.
#' @param caption Show caption as logical.
#' @param size The point size.
#' @param alpha The alpha parameter for the data points.
#'
#' @returns A ggplot2 object.
#' @export
bintime_plot <- function(
    obj,
    analyte,
    method = "fisher",
    points = FALSE,
    caption = TRUE,
    size = 1.5,
    alpha = 1) {
  # input validation
  validate_nif(obj)
  validate_char_param(analyte, "analyte")
  validate_analyte(obj, analyte)
  validate_nif(obj)
  validate_char_param(method, "method")
  if(!method %in%c("jenks", "kmeans", "pretty", "quantile", "hclust", "sd",
                   "bclust", "fisher")) {
    stop(paste0("Method ", method, " not implemented!"))
  }
  # validate_numeric_param(breaks, allow_multiple = TRUE)
  validate_logical_param(points)
  validate_logical_param(caption, allow_null = TRUE)
  validate_numeric_param(alpha)

  if (is.null(caption))
    caption <- FALSE

  temp <- obj |>
    filter(.data$ANALYTE == analyte) |>
    filter(.data$EVID == 0) |>
    add_bintime(method = method) |>
    filter(!is.na(.data$BINTIME)) |>
    filter(!is.na(.data$DV)) |>
    as.data.frame()

  out <- temp |>
    reframe(
      .data$BINTIME,
      .data$ANALYTE,
      .data$BIN_LEFT,
      .data$BIN_RIGHT,
      .individual_mean = mean(.data$DV),
      .by = c("ID", "ANALYTE", "BINTIME")
    ) |>
    distinct() |>
    reframe(
      .data$BIN_LEFT,
      .data$BIN_RIGHT,
      mean = mean(.data$.individual_mean),
      sd = sd(.data$.individual_mean),
      n = n(),
      se = sd/sqrt(.data$n),
      df = .data$n - 1,
      t = stats::qt(p = 0.05/2, df = .data$df, lower.tail = FALSE),
      margin_error = .data$t * .data$se,
      lower_ci = .data$mean - .data$margin_error,
      upper_ci = .data$mean + .data$margin_error,
      .by = c("BINTIME", "ANALYTE")) |>
    distinct() |>

    ggplot(aes(x = .data$BINTIME, y = .data$mean)) +
    labs(y = analyte, x = "TAFD")

  if (points == TRUE) {
    out <- out +
      geom_point(aes(x = .data$TAFD, y = .data$DV), data = temp, alpha = alpha,
                 size = size)
  }

  out <- out +
    geom_segment(aes(x = .data$BIN_LEFT, xend = .data$BIN_RIGHT,
                     y = .data$mean), color = "red") +
    geom_rect(aes(xmin = .data$BIN_LEFT, xmax = .data$BIN_RIGHT,
                  ymin = .data$lower_ci, ymax = .data$upper_ci),
              fill = "red", alpha = 0.3) +
    theme_bw()

  if (caption == TRUE) {
    out <- out +
      labs(caption = "Mean with 90% CI")
  }

  out
}



#' make QTIME field
#'
#' @param obj A nif object.
#' @param breaks The breaks of the QTIME.
#'
#' @returns A data frame.
#' @noRd
add_qtime <- function(obj, breaks) {
  validate_nif(obj)

  n_obs <- obj |>
    as.data.frame() |>
    filter(.data$EVID == 0) |>
    reframe(N = n(), .by = c("ID", "ANALYTE")) |>
    pivot_wider(names_from = "ANALYTE", values_from = "N")

  b <- c(0, 2, 5, 10, 15, 25)
  b <- breaks
  l <- b[-1]
  bw <- b[-1] - b[1:length(b)-1]
  bm <- b[1:length(b)-1] + bw/2

  rect <- data.frame(
    QTIME = l,
    .RIGHT = b[-1],
    .LEFT = b[1:length(b)-1]
  )

  obj |>
    as.data.frame() |>
    mutate(QTIME = as.numeric(as.character(
      cut(.data$TAFD, breaks = b, labels = l))
    )) |>
    left_join(rect, by = "QTIME")
}






#' Plot analyte over time by discretized time after first dose (QTIME)
#'
#' First, individual DV values are summarized by analyte over the QTIME periods,
#' then those mean values are summarized over all subjects. The mean and its
#' 90% CI shown in the figure refers to the latter summary.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param analyte The analyte as character.
#' @param breaks The breaks for TAFD, as numeric.
#' @param points Plot original data points as logical.
#' @param caption Show caption as logical.
#' @param size The point size.
#' @param alpha The alpha parameter for the data points.
#'
#' @importFrom stats qt
#' @returns A ggplot object
#' @export
qtime_plot <- function(
    obj,
    analyte,
    breaks,
    points = FALSE,
    caption = TRUE,
    size = 1.5,
    alpha = 1) {
  validate_nif(obj)
  validate_char_param(analyte, "analyte")
  validate_analyte(obj, analyte)
  validate_numeric_param(breaks, allow_multiple = TRUE)
  validate_logical_param(points)
  validate_logical_param(caption, allow_null = TRUE)
  validate_numeric_param(alpha)

  if (is.null(caption))
    caption <- FALSE

  temp <- obj |>
    add_qtime(breaks) |>
    filter(.data$ANALYTE == analyte) |>
    filter(.data$EVID == 0) |>
    filter(!is.na(.data$QTIME)) |>
    filter(!is.na(.data$DV)) |>
    as.data.frame()

  out <- temp |>
    reframe(
      .data$QTIME,
      .data$ANALYTE,
      .data$.LEFT,
      .data$.RIGHT,
      .individual_mean = mean(.data$DV),
      .by = c("ID", "ANALYTE", "QTIME")
    ) |>
    distinct() |>
    reframe(
      .data$.LEFT,
      .data$.RIGHT,
      mean = mean(.data$.individual_mean),
      sd = sd(.data$.individual_mean),
      n = n(),
      se = sd/sqrt(.data$n),
      df = .data$n - 1,
      t = stats::qt(p = 0.05/2, df = .data$df, lower.tail = FALSE),
      margin_error = .data$t * .data$se,
      lower_ci = .data$mean - .data$margin_error,
      upper_ci = .data$mean + .data$margin_error,
      .by = c("QTIME", "ANALYTE")) |>
    distinct() |>

    ggplot(aes(x = .data$QTIME, y = .data$mean)) +
    labs(y = analyte, x = "TAFD")

  if (points == TRUE) {
    out <- out +
      geom_point(aes(x = .data$TAFD, y = .data$DV), data = temp, alpha = alpha,
                 size = size)
  }

  out <- out +
    geom_segment(aes(x = .data$.LEFT, xend = .data$.RIGHT,
                     y = .data$mean), color = "red") +
    geom_rect(aes(xmin = .data$.LEFT, xmax = .data$.RIGHT,
                  ymin = .data$lower_ci, ymax = .data$upper_ci),
              fill = "red", alpha = 0.3) +
    theme_bw()

  if (caption == TRUE) {
    out <- out +
      labs(caption = "Mean with 90% CI")
  }

  out
}
