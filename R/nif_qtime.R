#' Make BINTIME field
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param method Univariate class intervals method, can be one of jenks, kmeans,
#' pretty, quantile, hclust, sd, bclust or fisher. See classInt::classInterval()
#' for details. Default is fisher.
#' @param obj A nif object.
#' @param time The time field to use.
#' @param group The grouping variable(s).
#'
#' @returns A nif object with the BINTIME, BIN_LEFT and BIN_RIGHT fields added.
#' @importFrom classInt classIntervals
#' @export
# add_bintime <- function(
#     obj,
#     method = "fisher",
#     time = "TAFD"
#     ) {
#   # input validation
#   validate_nif(obj)
#   validate_char_param(method, "method")
#   if(!method %in%c("jenks", "kmeans", "pretty", "quantile", "hclust", "sd",
#                  "bclust", "fisher")) {
#     stop(paste0("Method ", method, " not implemented!"))
#   }
#
#   ## to do:
#   ## validate time argument
#
#   # calculate bins
#   obj <- ensure_tafd(obj) |>
#     mutate(active_time = .data[[time]])
#
#   # bins <- classInt::classIntervals(obj$TAFD, style = method)
#   bins <- classInt::classIntervals(obj$active_time, style = method)
#
#   breaks <- sort(bins$brks)
#
#   # Cut and assign bin indices
#   obj <- obj |>
#     mutate(.BINTIME_INDEX = as.numeric(cut(.data$TAFD, breaks = breaks,
#                                            include.lowest = TRUE)))
#
#   # Calculate median TAFD for each bin from actual assignments
#   bin_medians <- obj |>
#     filter(!is.na(.data$.BINTIME_INDEX)) |>
#     reframe(
#       # label = round(median(.data$TAFD, na.rm = TRUE)),
#       label = round(median(.data$active_time, na.rm = TRUE)),
#       .by = .data$.BINTIME_INDEX
#     ) |>
#     arrange(.data$.BINTIME_INDEX)
#
#   # Create bin_par data frame with boundaries and calculated medians
#   bin_par <- data.frame(
#     .BINTIME_INDEX = seq_along(breaks[-1]),
#     left = breaks[-length(breaks)],
#     right = breaks[-1]
#   ) |>
#     left_join(bin_medians, by = ".BINTIME_INDEX") |>
#     mutate(label = ifelse(is.na(.data$label), round(.data$left), .data$label))
#
#   # Assign bin information to obj
#   obj |>
#     mutate(BIN_LEFT = bin_par[.data$.BINTIME_INDEX, "left"]) |>
#     mutate(BIN_RIGHT = bin_par[.data$.BINTIME_INDEX, "right"]) |>
#     mutate(BINTIME = bin_par[.data$.BINTIME_INDEX, "label"]) |>
#     select(-c(".BINTIME_INDEX"))
# }

add_bintime <- function(
    obj,
    method = "fisher",
    time = "TAFD",
    group = NULL
) {
  # input validation
  validate_nif(obj)
  validate_char_param(method, "method")
  if(!method %in%c("jenks", "kmeans", "pretty", "quantile", "hclust", "sd",
                   "bclust", "fisher")) {
    stop(paste0("Method ", method, " not implemented!"))
  }
  validate_char_param(group, "group", allow_null = TRUE, allow_multiple = TRUE)

  # Grouped binning: split by group, apply ungrouped binning to each, combine
  if (!is.null(group)) {
    missing <- setdiff(group, names(obj))
    if (length(missing) > 0)
      stop(paste0("Group variable(s) ", paste(missing, collapse = ", "),
                  " not found in data set!"))

    return(
      obj |>
        tidyr::nest(.by = all_of(group)) |>
        mutate(data = purrr::map(data, function(x) {
          as_nif(x) |> add_bintime(method = method, time = time)
        })) |>
        tidyr::unnest(cols = "data") |>
        as_nif()
    )
  }

  obj <- ensure_tafd(obj) |>
    mutate(active_time = .data[[time]])

  bins <- classInt::classIntervals(obj$active_time, style = method)
  breaks <- sort(bins$brks)

  obj <- obj |>
    mutate(.BINTIME_INDEX = as.numeric(cut(.data$TAFD, breaks = breaks,
                                           include.lowest = TRUE)))

  bin_medians <- obj |>
    filter(!is.na(.data$.BINTIME_INDEX)) |>
    reframe(
      label = round(median(.data$active_time, na.rm = TRUE)),
      .by = .data$.BINTIME_INDEX
    ) |>
    arrange(.data$.BINTIME_INDEX)

  bin_par <- data.frame(
    .BINTIME_INDEX = seq_along(breaks[-1]),
    left = breaks[-length(breaks)],
    right = breaks[-1]
  ) |>
    left_join(bin_medians, by = ".BINTIME_INDEX") |>
    mutate(label = ifelse(is.na(.data$label), round(.data$left), .data$label))

  obj |>
    mutate(BIN_LEFT = bin_par[.data$.BINTIME_INDEX, "left"]) |>
    mutate(BIN_RIGHT = bin_par[.data$.BINTIME_INDEX, "right"]) |>
    mutate(BINTIME = bin_par[.data$.BINTIME_INDEX, "label"]) |>
    select(-c(".BINTIME_INDEX"))
}


#' BINTIME plot
#'
#' @description
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
#' @param min_time The minimal time in units of TAFD, as numeric.
#' @param max_time The minimal time in units of TAFD, as numeric.
#' @param cfb Plot change from baseline, as logical.
#' @param title The plot title, as character. If none is provided, a generic
#'   title based on the analyte will be chosen. Override this by setting
#'.  title = "", if needed.
#' @param time The time field.
#' @param refline Plot horizontal dashed reference lines at these y axis values,
#' defaults to NULL (no lines).
#' @param color The coloring field.
#' @param facet The faceting field, defaults to DOSE.
#' @param scales The scales parameter to facet_wrap.
#' @param legend Show legend.
#'
#' @returns A ggplot2 object.
#' @export
bintime_plot <- function(
    obj,
    analyte,
    method = "kmeans",
    time = "TAFD",
    color = NULL,
    facet = "DOSE",
    min_time = NULL,
    max_time = NULL,
    points = FALSE,
    cfb = FALSE,
    caption = TRUE,
    title = NULL,
    size = 1.5,
    alpha = 1,
    scales = "fixed",
    refline = NULL,
    legend = TRUE) {
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
  validate_logical_param(points, "points")
  validate_logical_param(cfb, "cfb")
  validate_logical_param(caption, "caption", allow_null = TRUE)
  validate_numeric_param(alpha, "alpha")
  validate_char_param(time, "time")
  validate_numeric_param(min_time, "min_time", allow_null = TRUE)
  validate_numeric_param(max_time, "max_time", allow_null = TRUE)

  if (is.null(caption))
    caption <- FALSE

  if (!time %in% names(obj))
    stop(paste0("Time field ", time, " not found in data set!"))

  # time limits
  if (is.null(max_time)) {
    max_time <- max(obj$TAFD, na.rm = TRUE)
  }
  if (is.null(min_time)) {
    min_time <- min(obj$TAFD, na.rm = TRUE)
  }

  # time field
  # obj |>
  #   mutate(active_time = .data[[time]])

  # change from baseline
  if (cfb == TRUE) {
    obj <- mutate(obj, DV = .data$DVCFB)
  }

  # Make title
  if (is.null(title)) {
    title <- analyte
    if (cfb == TRUE)
      title <- paste0(title, " change from baseline")
  }

  temp <- obj |>
    filter(.data$ANALYTE == analyte) |>
    filter(.data$EVID == 0) |>
    ensure_dose() |>
    filter(!is.na(.data$DV)) |>
    as.data.frame()

    #
    # add_bintime(method = method, time = time) |>
    # filter(!is.na(.data$BINTIME)) |>
    # filter(!is.na(.data$DV)) |>
    # as.data.frame()

  # Create COLOR column
  if (length(color) != 0) {
    temp <- tidyr::unite(temp, "COLOR", all_of(!!color), sep = "-",
                         remove = FALSE)
  } else {
    temp <- mutate(temp, COLOR = "all")
  }

  # Create FACET column
  if (!is.null(facet)) {
    if (length(facet) == 1) {
      temp <- mutate(temp, FACET = .data[[facet]])
    } else {
      temp <- tidyr::unite(temp, "FACET", all_of(facet), sep = "-",
                           remove = FALSE)
    }
  }

  # create bin groups
  bin_group_vars <- c()
  if (length(color) != 0)
    bin_group_vars <- c(bin_group_vars, "COLOR")
  if (!is.null(facet))
    bin_group_vars <- c(bin_group_vars, "FACET")
  if (length(bin_group_vars) == 0)
    bin_group_vars <- NULL

  temp <- temp |>
    as_nif() |>
    add_bintime(method = method, time = time, group = bin_group_vars) |>
    filter(!is.na(.data$BINTIME)) |>
    as.data.frame()


  # out <- temp |>
  #   reframe(
  #     .data$BINTIME,
  #     .data$ANALYTE,
  #     .data$BIN_LEFT,
  #     .data$BIN_RIGHT,
  #     .individual_mean = mean(.data$DV),
  #     .by = c("ID", "ANALYTE", "BINTIME")
  #   ) |>
  #   distinct() |>
  #   reframe(
  #     .data$BIN_LEFT,
  #     .data$BIN_RIGHT,
  #     mean = mean(.data$.individual_mean),
  #     sd = sd(.data$.individual_mean),
  #     n = n(),
  #     se = sd/sqrt(.data$n),
  #     df = .data$n - 1,
  #     t = stats::qt(p = 0.05/2, df = .data$df, lower.tail = FALSE),
  #     margin_error = .data$t * .data$se,
  #     lower_ci = lower_ci(.data$mean, .data$sd, .data$n, 0.9),
  #     upper_ci = upper_ci(.data$mean, .data$sd, .data$n, 0.9),
  #     .by = c("BINTIME", "ANALYTE")) |>
  #   distinct() |>
  #   ggplot(aes(x = .data$BINTIME, y = .data$mean)) +
  #   labs(y = analyte, x = time, title = title)
  #
  # if (points == TRUE) {
  #   out <- out +
  #     geom_point(aes(
  #       # x = .data$TAFD,
  #       x = .data$active_time,
  #       y = .data$DV
  #       ),
  #       data = temp,
  #       alpha = alpha,
  #       size = size)
  # }
  #
  # out <- out +
  #   geom_segment(aes(x = .data$BIN_LEFT, xend = .data$BIN_RIGHT,
  #                    y = .data$mean), color = "red") +
  #   geom_rect(aes(xmin = .data$BIN_LEFT, xmax = .data$BIN_RIGHT,
  #                 ymin = .data$lower_ci, ymax = .data$upper_ci),
  #             fill = "red", alpha = 0.3) +
  #   xlim(min_time, max_time) +
  #   theme_bw()
  #
  # if (!is.null(refline)) {
  #   out <- out +
  #     geom_hline(yintercept = refline, color = "blue", linetype = "dashed")
  # }
  #
  # if (caption == TRUE) {
  #   out <- out +
  #     labs(caption = paste0(
  #       "Red: Mean with 90% CI (binning: ", method, ")"))
  # }
  #
  # out

  group_vars_individual <- c("ID", "ANALYTE", "BINTIME", "COLOR")
  group_vars_summary <- c("BINTIME", "ANALYTE", "COLOR")
  if (!is.null(facet)) {
    group_vars_individual <- c(group_vars_individual, "FACET")
    group_vars_summary <- c(group_vars_summary, "FACET")
  }

  summary_data <- temp |>
    reframe(
      .data$BINTIME,
      .data$ANALYTE,
      .data$BIN_LEFT,
      .data$BIN_RIGHT,
      .data$COLOR,
      .individual_mean = mean(.data$DV),
      .by = all_of(group_vars_individual)
    ) |>
    distinct() |>
    reframe(
      .data$BIN_LEFT,
      .data$BIN_RIGHT,
      mean = mean(.data$.individual_mean),
      sd = sd(.data$.individual_mean),
      n = n(),
      se = sd / sqrt(.data$n),
      df = .data$n - 1,
      t = stats::qt(p = 0.05/2, df = .data$df, lower.tail = FALSE),
      margin_error = .data$t * .data$se,
      lower_ci = lower_ci(.data$mean, .data$sd, .data$n, 0.9),
      upper_ci = upper_ci(.data$mean, .data$sd, .data$n, 0.9),
      .by = all_of(group_vars_summary)
    ) |>
    distinct()

  out <- summary_data |>
    ggplot(aes(x = .data$BINTIME, y = .data$mean, color = .data$COLOR)) #+
    # labs(y = analyte, x = time, title = title, color = nice_enumeration(color))

  if (points == TRUE) {
    if (length(color) > 0) {
      out <- out +
        geom_point(
          aes(x = .data$active_time, y = .data$DV, color = .data$COLOR),
          data = temp, alpha = alpha, size = size
        )
    } else {
      out <- out +
        geom_point(
          aes(x = .data$active_time, y = .data$DV),
          data = temp,
          alpha = alpha,
          size = size,
          color = "black"
        )
    }
  }

  out <- out +
    geom_segment(aes(
      x = .data$BIN_LEFT,
      xend = .data$BIN_RIGHT,
      y = .data$mean,
      color = .data$COLOR)) +
    geom_rect(aes(
      xmin = .data$BIN_LEFT,
      xmax = .data$BIN_RIGHT,
      ymin = .data$lower_ci,
      ymax = .data$upper_ci,
      fill = .data$COLOR),
      alpha = 0.3,
      color = NA,
      show.legend = FALSE) +
    xlim(min_time, max_time) +
    theme_bw()

  # Faceting
  if (!is.null(facet)) {
    out <- out + facet_wrap(~FACET, scales = scales)
    title <- paste0(title, " by ", facet)
  }

  # Reference lines
  if (!is.null(refline)) {
    out <- out +
      geom_hline(yintercept = refline, color = "blue", linetype = "dashed")
  }

  # labels
  out <- out +
   labs(y = analyte, x = time, title = title, color = nice_enumeration(color))

  # Caption
  if (caption == TRUE) {
    out <- out +
      labs(caption = paste0("Mean with 90% CI (binning: ", method, ")"))
  }

  # Legend
  out <- out +
    theme(legend.position = ifelse(
      isTRUE(legend) & length(color) > 0, "bottom", "none"
    ))

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
      # lower_ci = .data$mean - .data$margin_error,
      # upper_ci = .data$mean + .data$margin_error,
      lower_ci = lower_ci(.data$mean, .data$sd, .data$n, 0.9),
      upper_ci = upper_ci(.data$mean, .data$sd, .data$n, 0.9),
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
