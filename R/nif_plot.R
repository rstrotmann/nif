#' Prepare data set for plotting from NIF object
#'
#' @inheritParams plot.nif
#'
#' @return A list with:
#'   * 'data', the data table for plotting
#'   * 'group', the grouping variable
#'   * 'color', the coloring variable
#'   * 'facet', the faceting variable
#' @export
#' @keywords internal
#'
#' @examples
#' make_plot_data_set(examplinib_sad_nif)
#' make_plot_data_set(examplinib_sad_nif, color = "RACE")
#' make_plot_data_set(examplinib_sad_nif, facet = "RACE")
#' make_plot_data_set(examplinib_poc_nif, color = c("DI", "RACE"))
make_plot_data_set <- function(
  nif,
  analyte = NULL,
  dose = NULL,
  time = "TAFD",
  color = NULL,
  min_time = NULL,
  max_time = NULL,
  cfb = FALSE,
  dose_norm = FALSE,
  facet = "DOSE",
  na_value = NA
) {
  # assert time parameter
  if (!time %in% c("TIME", "NTIME", "TAFD", "TAD")) {
    stop("time must be either 'TIME', 'NTIME', 'TAFD' or 'TAD'!")
  }

  nif <- nif |>
    ensure_analyte() |>
    ensure_parent() |>
    ensure_dose() |>
    ensure_tafd() |>
    ensure_tad()

  # assert facet parameter
  if (!is.null(facet)) {
    if (!facet %in% names(nif)) {
      stop(paste0("Facetting variable ", facet, " not found in data set!"))
    }
  }

  if (is.null(analyte)) {
    analyte <- analytes(nif)
  }

  parent <- parents(nif)

  out <- nif |>
    filter((.data$ANALYTE %in% analyte & .data$EVID == 0) |
             (.data$ANALYTE %in% parent & .data$EVID == 1)) |>
    mutate(DV = case_when(is.na(.data$DV) ~ na_value, .default = .data$DV))

  if (is.null(dose)) {
    dose <- unique(filter(out, .data$EVID == 0)$DOSE)
  }

  out <- out |>
    filter(.data$DOSE %in% dose) |>
    mutate(active_time = .data[[time]])

  if (is.null(max_time)) {
    max_time <- max(filter(out, .data$EVID == 0)$active_time, na.rm = TRUE)
  }

  if (is.null(min_time)) {
    min_time <- min(out$active_time, na.rm = TRUE)
  }

  out <- out |>
    index_dosing_interval() |>
    mutate(DI = case_match(.data$EVID, 1 ~ NA, .default = .data$DI))

  if (cfb == TRUE)
    out <- mutate(out, DV = .data$DVCFB)

  if (dose_norm == TRUE)
    out <- mutate(out, DV = .data$DV / .data$DOSE)

  out <- out |>
    filter(.data$active_time >= min_time) |>
    filter(.data$active_time <= max_time) |>
    group_by("ID", "ANALYTE") |>
    mutate(n_obs = sum(.data$EVID == 0)) |>
    ungroup() |>
    as.data.frame()

  if (length(analyte) > 1) {
    color <- unique(c("ANALYTE", color))
  }

  out <- out |>
    arrange("ID", "DOSE")

  if (length(color) != 0) {
    out <- tidyr::unite(out, "COLOR", all_of(!!color), sep = "-",
                        remove = FALSE)
  } else {
    out <- mutate(out, COLOR = TRUE)
  }

  if (length(facet) > 0) {
    if (length(facet) == 1) {
      out <- mutate(out, FACET = .data[[facet]])
    } else {
      out <- tidyr::unite(out, "FACET",
        all_of(facet),
        sep = "-", remove = FALSE
      )
    }
  }

  out <- out |>
    arrange("ID", "COLOR", "DOSE", "FACET")

  list(data = out, group = "ID", color = color, facet = facet)
}


#' Create mean plot data set from plot data set
#'
#' @param data_set A list.
#'
#' @return A list.
#' @noRd
#' @seealso [nif::make_plot_data_set()]
make_mean_plot_data_set <- function(data_set) {
  out <- data_set$data |>
    mutate(active_time = .data$NTIME) |>
    select(-c("NTIME")) |>
    reframe(
      ID = 1, n = n(), mean = safe_mean(.data$DV), sd = safe_sd(.data$DV),
      .by = any_of(c(
        "active_time", data_set$color, data_set$facet, "EVID",
        "COLOR", "FACET", "ANALYTE"
      ))
    ) |>
    rename(DV = "mean")

  list(
    data = out, group = "ID", color = data_set$color, facet = data_set$facet
  )
}


#' Plot NIF object.
#'
#' @param x The NIF object to be plotted.
#' @param analyte The analyte(s) to be plotted, as character.
#' @param dose The dose(s) to be filtered for.
#' @param time The time field to be plotted on the x-axis. Can be any of 'TIME',
#'   'NTIME', 'TAFD' or 'TAD'.
#' @param color The column(s) to be used for coloring.
#' @param min_time The minimal time in units of the selected time field, as
#'   numeric.
#' @param max_time The minimal time in units of the selected time field, as
#'   numeric.
#' @param cfb Plot change from baseline, as logical.
#' @param dose_norm Dose-normalized values, as logical.
#' @param facet The column(s) to be used for faceting.
#' @param admin The analyte to be plotted as administration markers, as
#'   character.
#' @param points Plot points, as logical.
#' @param lines Plot lines, as logical.
#' @param log Logarithmic y axis, as logical.
#' @param mean Plot means, as logical.
#' @param title The plot title, as character.
#' @param legend Show the plot legend, as logical.
#' @param size The `size` parameter to [ggplot2::geom_point()] as numeric.
#' @param scales The `scales` parameter to [ggplot2::facet_wrap()], can be
#'   "fixed" (default), "free", "free_x" or "free_y".
#' @param alpha The `alpha` parameter to [ggplot2::geom_point()], as numeric.
#' @param group `r lifecycle::badge("deprecated")` Grouping variable. Use the
#'   `color` or `facet` parameters instead.
#' @param caption The caption line as per [ggplot2::labs()].
#' @param ... Further parameters.
#' @param na_value Value to replace NA values with, as numeric.
#' @param ribbon Plot ribbon in mean plot, as logical.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' plot(examplinib_fe_nif)
#' plot(examplinib_fe_nif, facet = "FASTED", time = "TAD")
#' plot(examplinib_fe_nif, color = "FASTED", time = "TAD", facet = "SEX")
#' plot(examplinib_sad_nif,
#'   mean = FALSE, points = TRUE, dose_norm = FALSE,
#'   facet = "RACE", log = TRUE, max_time = 72
#' )
#' plot(examplinib_sad_nif,
#'   dose_norm = TRUE, facet = NULL, log = TRUE,
#'   max_time = 48, points = TRUE
#' )
#' plot(examplinib_fe_nif,
#'   points = TRUE, color = c("FASTED"), mean = TRUE,
#'   max_time = 12, admin = "RS2023"
#' )
#' plot(examplinib_poc_nif,
#'   points = TRUE, dose_norm = TRUE, facet = NULL,
#'   color = "SEX", max_time = 25, time = "TAD", lines = FALSE, size = 3,
#'   alpha = 0.5, title = "POC study: all analytes and doses"
#' )
#' plot(examplinib_fe_nif,
#'   points = TRUE, color = c("FASTED"), mean = TRUE,
#'   max_time = 12, admin = "RS2023"
#' )
plot.nif <- function(
  x,
  analyte = NULL,
  dose = NULL,
  time = "TAFD",
  color = NULL,
  facet = "DOSE",
  min_time = NULL,
  max_time = NULL,
  cfb = FALSE,
  dose_norm = FALSE,
  admin = NULL,
  points = FALSE,
  lines = TRUE,
  log = FALSE,
  mean = FALSE,
  title = NULL,
  legend = TRUE,
  size = 1.5,
  scales = "fixed",
  alpha = 1,
  caption = NULL,
  ribbon = TRUE,
  group = deprecated(),
  na_value = NA,
  ...
) {
  if (lifecycle::is_present(group)) {
    lifecycle::deprecate_warn("0.50.1", "plot(group)", "plot(color)")
    color <- group
  }

  # input validation
  validate_char_param(analyte, "analyte", allow_null = TRUE,
                      allow_multiple = TRUE)
  validate_numeric_param(dose, "dose", allow_null = TRUE,
                         allow_multiple = TRUE)

  temp <- make_plot_data_set(
    x, analyte, dose, time, color, min_time, max_time, cfb, dose_norm, facet,
    na_value = na_value
  )

  if (isTRUE(mean)) {
    temp <- make_mean_plot_data_set(temp)
    if (is.null(caption)) caption <- "Mean and SD"
  }

  plot_data <- temp$data

  if (isTRUE(log)) {
    plot_data <- mutate(plot_data, DV = case_match(.data$DV, 0 ~ NA,
                                                   .default = .data$DV))
  }

  plot_data <- plot_data |>
    tidyr::unite("GROUP", any_of(c((temp$group), (temp$color), (temp$facet))),
      sep = "-", remove = FALSE
    )

  analytes <- unique(plot_data$ANALYTE)
  y_label <- ifelse(length(analytes) == 1, analytes, "DV")
  if (isTRUE(dose_norm)) y_label <- paste0(y_label, " / DOSE")

  admin_data <- filter(plot_data, .data$EVID == 1)

  if (is.null(caption) && nif_option_value("show_hash") == TRUE) {
    caption <- paste0("dataset hash: ", hash(x))
  }


  p <- plot_data |>
    filter(.data$EVID == 0) |>
    filter(!is.na(.data$DV))

  if (!is.null(admin))
    p <- dplyr::bind_rows(p, dplyr::mutate(admin_data, DV = NA))

  p <- p |>
    arrange("GROUP", "active_time", -.data$EVID) |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$active_time,
      y = .data$DV,
      group = .data$GROUP,
      color = .data$COLOR
    ))

  if (!is.null(admin)) {
    p <- p +
      ggplot2::geom_vline(
        data = admin_data,
        ggplot2::aes(xintercept = .data$active_time), color = "gray"
      )
  }

  if (isTRUE(lines))
    p <- p + ggplot2::geom_line(na.rm = TRUE)

  if (isTRUE(points))
    p <- p + ggplot2::geom_point(size = size, alpha = alpha, na.rm = TRUE)

  if (isTRUE(mean) && isTRUE(ribbon)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pos_diff(.data$DV, .data$sd),
                     ymax = .data$DV + .data$sd,
                     fill = .data$COLOR),
        alpha = 0.3, color = NA, show.legend = FALSE
      )
  }

  if (!is.null(temp$facet)) {
    if (length(unique(plot_data[[temp$facet]])) > 1) {
      p <- p +
        ggplot2::facet_wrap(~FACET, scales = scales)
    }
  }

  if (isTRUE(log)) {
    p <- p +
      ggplot2::scale_y_log10()
  }

  p <- p + ggplot2::labs(color = nice_enumeration(temp$color))

  if (!is.null(caption))
    p <- p + ggplot2::labs(caption = caption)

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = ifelse(
        legend == TRUE & length(temp$color) > 0, "bottom", "none"
      )
    ) +
    ggplot2::ggtitle(title) +
    watermark(cex = 1.5) +
    ggplot2::labs(x = time, y = y_label, color = nice_enumeration(temp$color))

  suppressWarnings(return(p))
}
