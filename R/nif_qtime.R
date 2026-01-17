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
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param analyte The analyte as character.
#' @param breaks The breaks for TAFD, as numeric.
#' @param points Plot original data points as logical.
#' @param caption Show caption as logical.
#' @param alpha The alpha parameter for the data points.
#'
#' @returns A ggplot object
#' @export
qtime_plot <- function(
    obj,
    analyte,
    breaks,
    points = FALSE,
    caption = TRUE,
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
    filter(ANALYTE == analyte) |>
    filter(EVID == 0) |>
    filter(!is.na(QTIME)) |>
    filter(!is.na(DV)) |>
    as.data.frame()

  out <- temp |>
    reframe(
      .LEFT, .RIGHT,
      mean = mean(DV),
      sd = sd(DV),
      n = n(),
      se = sd/sqrt(n),
      df = n - 1,
      t = qt(p = 0.05/2, df = df, lower.tail = F),
      margin_error = t * se,
      lower_ci = mean - margin_error,
      upper_ci = mean + margin_error,
      .by = c("QTIME", "ANALYTE")) |>
    distinct() |>
    ggplot(aes(x = QTIME, y = mean))

  if (points == TRUE) {
    out <- out +
      geom_point(aes(x = TAFD, y = DV), data = temp, alpha = alpha)
  }

  out <- out +
    geom_segment(aes(x = .LEFT, xend = .RIGHT, y = mean), color = "red") +
    geom_rect(aes(xmin = .LEFT, xmax = .RIGHT, ymin = lower_ci, ymax = upper_ci),
              fill = "red", alpha = 0.3) +
    theme_bw()

  if (caption == TRUE) {
    out <- out +
      labs(caption = "Mean with 90% CI")
  }

  out
}
