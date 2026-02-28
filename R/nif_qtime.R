#' Make BINTIME field
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param method Univariate class intervals method, can be one of jenks, kmeans,
#' pretty, quantile, hclust, sd, bclust or fisher. See classInt::classInterval()
#' for details. Default is fisher.
#' @param time The time field to use.
#' @param group The grouping variable(s).
#'
#' @returns A nif object with the BINTIME, BIN_LEFT and BIN_RIGHT fields added.
#' @importFrom classInt classIntervals
#' @noRd
add_bintime <- function(
    obj,
    method = "fisher",
    time = "TAFD",
    group = NULL
) {
  # input validation
  validate_nif(obj)
  validate_char_param(method, "method")
  if(!method %in% c("jenks", "kmeans", "pretty", "quantile", "hclust", "sd",
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
    mutate(
      .BINTIME_INDEX = as.numeric(
        cut(.data$active_time, breaks = breaks, include.lowest = TRUE)))

  bin_medians <- obj |>
    filter(!is.na(.data$.BINTIME_INDEX)) |>
    reframe(
      label = round(median(.data$active_time, na.rm = TRUE)),
      .by = ".BINTIME_INDEX"
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
    select(-c(".BINTIME_INDEX")) |>
    as_nif()
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
#' @param min_time The minimal time, as numeric.
#' @param max_time The maximal time, as numeric.
#' @param cfb Plot change from baseline, as logical.
#' @param title The plot title, as character. If none is provided, a generic
#'   title based on the analyte will be chosen. Override this by setting
#'   title = "", if needed.
#' @param time The time field.
#' @param refline Plot horizontal dashed reference lines at these y axis values,
#' defaults to NULL (no lines).
#' @param color The coloring field.
#' @param facet The faceting field, defaults to DOSE.
#' @param scales The scales parameter to facet_wrap.
#' @param legend Show legend.
#'
#' @returns A ggplot2 object.
#' @importFrom stats qt
#' @export
#' @examples
#' examplinib_poc_nif |>
#'   filter(TAFD < 48) |>
#'   bintime_plot("RS2023", points = T, alpha = 0.2)
#'
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
  validate_argument(analyte, "character")
  validate_analyte(obj, analyte)

  validate_argument(method, "character")
  if(!method %in% c("jenks", "kmeans", "pretty", "quantile", "hclust", "sd",
                   "bclust", "fisher")) {
    stop(paste0("Method ", method, " not implemented!"))
  }

  validate_argument(points, "logical")
  validate_argument(cfb, "logical")
  validate_argument(caption, "logical", allow_null = TRUE)
  validate_argument(alpha, "numeric")
  validate_argument(time, "character")
  validate_argument(min_time, "numeric", allow_null = TRUE)
  validate_argument(max_time, "numeric", allow_null = TRUE)

  if (isTRUE(cfb) && !"DVCFB" %in% names(obj))
    stop("Missing DVCFB column!")

  if (is.null(caption))
    caption <- FALSE

  if (!time %in% names(obj))
    stop(paste0("Time field ", time, " not found in data set!"))

  # time limits
  if (is.null(max_time)) {
    max_time <- max(obj[[time]], na.rm = TRUE)
  }
  if (is.null(min_time)) {
    min_time <- min(obj[[time]], na.rm = TRUE)
  }

  # change from baseline
  if (isTRUE(cfb)) {
    obj <- mutate(obj, DV = .data$DVCFB)
  }

  # Make title
  if (is.null(title)) {
    title <- analyte
    if (isTRUE(cfb))
      title <- paste0(title, " change from baseline")
  }

  temp <- obj |>
    filter(.data$ANALYTE == analyte) |>
    filter(.data$EVID == 0) |>
    ensure_dose() |>
    filter(!is.na(.data$DV)) |>
    as.data.frame()

  # Create COLOR column
  if (length(color) != 0) {
    temp <- tidyr::unite(
      temp, "COLOR", all_of(!!color), sep = "-", remove = FALSE)
  } else {
    temp <- mutate(temp, COLOR = "all")
  }

  # Create FACET column
  if (!is.null(facet)) {
    if (length(facet) == 1) {
      temp <- mutate(temp, FACET = .data[[facet]])
    } else {
      temp <- tidyr::unite(
        temp, "FACET", all_of(facet), sep = "-", remove = FALSE)
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
      n = n(),
      sd = ifelse(.data$n > 1, sd(.data$.individual_mean), NA_real_),
      se = .data$sd / sqrt(.data$n),
      df = .data$n - 1,
      t = ifelse(
        .data$df > 0, stats::qt(p = 0.05/2, df = .data$df, lower.tail = FALSE),
        NA_real_),
      margin_error = .data$t * .data$se,
      lower_ci = ifelse(
        .data$n > 1, lower_ci(.data$mean, .data$sd, .data$n, 0.9), NA_real_),
      upper_ci = ifelse(
        .data$n > 1, upper_ci(.data$mean, .data$sd, .data$n, 0.9), NA_real_),
      .by = all_of(group_vars_summary)
    ) |>
      distinct()

  out <- summary_data |>
    ggplot(aes(x = .data$BINTIME, y = .data$mean, color = .data$COLOR))

  if (isTRUE(points)) {
    if (length(color) > 0) {
      out <- out +
        geom_point(
          aes(x = .data[[time]], y = .data$DV, color = .data$COLOR),
          data = temp, alpha = alpha, size = size
        )
    } else {
      out <- out +
        geom_point(
          aes(x = .data[[time]], y = .data$DV),
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
      yend = .data$mean,
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
    title <- paste0(title, " by ", nice_enumeration(facet))
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
  if (isTRUE(caption)) {
    out <- out +
      labs(caption = paste0("Mean with 90% CI (binning: ", method, ")"))
  }

  # Legend
  out <- out +
    theme(legend.position = ifelse(
      isTRUE(legend) & length(color) > 0, "bottom", "none"
    ))

  out +
    watermark(cex = 1.5)
}



