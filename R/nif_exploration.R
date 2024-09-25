#' Plot DV time course data from individual subject
#'
#' This function plots DV over TIME for an individual subject, id. Id can be
#' either the ID or the USUBJID. Administration time points are indicated with
#' vertical lines.
#'
#' @param obj The NIF object
#' @param id The subject ID to be plotted
#' @param analyte The analytes to be displayes. Defaults to NULL (all).
#' @param imp The IMP for which administrations are to be indicated by vertical
#'   lines. Defaults to NULL.
#' @param max_time The right limit of the time scale
#' @param tad Logical value to select whether time after dose (TAD) rather than
#'   TIME should be plotted.
#' @param lines Plot lines as logical.
#' @param cmt The compartment to plot as numeric.
#' @param log Logarithmic y scale.
#' @param time_field The field to use as the time metric, as character.
#' @param ... Further graphical parameters.
#' @param point_size Point size as numeric.
#'
#' @return A ggplot2 object.
#' @import dplyr
#' @export
#' @keywords internal
#' @examples
#' nif_plot_id(examplinib_poc_nif, 1)
#' nif_plot_id(examplinib_poc_min_nif, 1, log = TRUE)
#' nif_plot_id(examplinib_poc_nif, 1, log = TRUE)
#' nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023", tad = TRUE)
#' nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte="RS2023", imp="RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte=c("RS2023", "RS2023487A"))
#' nif_plot_id(examplinib_poc_min_nif, 1, analyte="CMT3")
#' nif_plot_id(examplinib_poc_min_nif, 1, tad=TRUE)
nif_plot_id <- function(obj, id, analyte = NULL, cmt = NULL,
                        time_field = "TIME", max_time = NA, lines = TRUE,
                        point_size = 2, log = FALSE,
                        # tad = FALSE,
                        imp = NULL,
                        ...) {
  x <- obj %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    index_dosing_interval() %>%
    as.data.frame() %>%
    assertr::verify(assertr::has_all_names(
      "ID", "TIME", "AMT", "DV", "EVID")) %>%
    {if(!is.null(cmt)) filter(., .$CMT == cmt) else .} %>%
    {if(!is.null(analyte)) filter(., .$ANALYTE %in% analyte) else .}

  id_label <- ""
  plot_label <- ""

  # filter for subject of interest
  if (id %in% x$ID) {
    plot_label <- "ID"
    x <- x %>%
      filter(ID == id)
  } else {
    if("USUBJID" %in% names(x)) {
      if (id %in% x$USUBJID) {
        x <- x %>%
          filter(.data$USUBJID == id)
        id_label <- paste0(" (ID ", x %>% distinct(ID) %>% pull(ID), ")")
        plot_label <- "USUBJID"
      } else {
        stop(paste(id, "is not an ID or USUBJID contained in the NIF object"))
      }
    }
  }

  obs <- x %>%
    mutate(active_time = .data[[time_field]]) %>%
    filter(EVID == 0, !is.na(DV)) %>%
    {if(time_field == "TAD")
      mutate(., group = interaction(ID, as.factor(ANALYTE), DI),
             color = interaction(as.factor(ANALYTE), DI))
      else mutate(., group = interaction(ID, as.factor(ANALYTE)),
                  color = as.factor(ANALYTE))}

  admin <- x %>%
    mutate(active_time = .data[[time_field]]) %>%
    dplyr::filter(EVID == 1) %>%
    {if(!is.null(imp)) filter(., PARENT == imp) else NULL}

  p <- obs %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$active_time,
      y = .data$DV,
      group = .data$group,
      color = .data$color
    )) +
    {if(!is.null(admin) > 0)
      ggplot2::geom_vline(data = admin,
                 ggplot2::aes(xintercept = .data$active_time),
                 color = "gray")} +
    {if (lines == TRUE) ggplot2::geom_line() } +
    ggplot2::geom_point(size = point_size) +
    ggplot2::xlim(0, max_time) +
    ggplot2::labs(
      x = time_field,
      title = paste0(plot_label, ": ", id, id_label),
      color = "analyte"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if(!is.null(imp)) {
    p <- p +
      ggplot2::labs(caption = paste("vertical lines indicate",
                                    imp, "administrations"))
  }

  if (log == TRUE) {
    p <- p + ggplot2::scale_y_log10()
  } else {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, NA))
  }
  return(p)
}


#' Plot dose time course data from individual subject
#'
#' This function plots AMT over TIME for an individual subject, id. Id can be
#' either the ID or the USUBJID. Administration time points are indicated with
#' vertical lines.
#'
#' @param obj The NIF object.
#' @param id The subject ID to be plotted.
#' @param y_scale Y-scale. Use 'scale="log"' for a logarithmic y scale. Default
#'   is "lin".
#' @param max_dose The upper limit of the dose scale.
#' @param analyte The analyte of interest.
#' @param max_time The right limit of the time scale.
#' @param time_field The field to use as the time metric, as character.
#' @param ... Further graphical parameters.
#' @param point_size The point size as numeric.
#'
#' @return A ggplot object.
#' @import dplyr
#' @export
#' @keywords internal
#' @examples
#' dose_plot_id(examplinib_poc_nif, 18)
#' dose_plot_id(examplinib_poc_nif, dose_red_sbs(examplinib_poc_nif)[[1]])
#' dose_plot_id(examplinib_poc_min_nif, 18)
dose_plot_id <- function(obj, id, y_scale = "lin", max_dose = NA,
                         time_field = "TIME", point_size = 2, max_time = NA,
                         analyte = NULL, ...) {

  x <- obj %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    index_dosing_interval() %>%
    assertr::verify(assertr::has_all_names(
      "ID", "TIME", "AMT", "DV", "EVID")) %>%
    {if(!is.null(analyte)) filter(., .$ANALYTE %in% analyte) else .}

  id_label <- ""
  plot_label <- ""

  # filter for subject of interest
  if (id %in% x$ID) {
    plot_label <- "ID"
    x <- x %>%
      filter(ID == id)
  } else {
    if("USUBJID" %in% names(x)) {
      if (id %in% x$USUBJID) {
        x <- x %>%
          filter(.data$USUBJID == id)
        id_label <- paste0(
          " (ID ",
          x %>% distinct(.data$ID) %>% pull(.data$ID),
          ")")
        plot_label <- "USUBJID"
      } else {
        stop(paste(id, "is not an ID or USUBJID contained in the NIF object"))
      }
    }
  }

  admin <- x %>%
    dplyr::filter(EVID == 1) %>%
    mutate(active_time = .data[[time_field]])

  p <- admin %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$active_time,
      y = .data$AMT,
      group = interaction(.data$ID, .data$ANALYTE),
      color = .data$ANALYTE
    )) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = point_size) +
    ggplot2::ylim(0, max_dose) +
    ggplot2::xlim(0, max_time) +
    ggplot2::labs(title = paste0(plot_label, ": ", id), color = "treatment") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if (y_scale == "log") {
    p <- p + ggplot2::scale_y_log10()
  }
  return(p)
}


#' Mean plot over a selected covariate
#'
#' The averaging over subjects expects the `NTIME` field be in the data set.
#'
#' @param obj The NIF object.
#' @param points Boolean to indicate whether points should be drawn.
#' @param lines Boolean to indicate whether lines should be drawn.
#' @param group The grouping covariate, defaults to 'ANALYTE'.
#' @param tad Time after dose. Not used, only for compatibility.
#' @param analyte The analyte as character. If NULL (default), all analytes will
#'   be plotted.
#' @param dose The dose(s) to filter for as numeric.
#' @param min_x The minimal value for the x scale as numeric.
#' @param max_x The maximal value for the x scale as numeric.
#' @param log Boolean to define whether y axis is plotted on the log scale.
#' @param cmt The compartment to filter for as numeric.
#' @param cfb Y value is change from baseline, defaults to FALSE.
#' @param point_size the point size as numeric.
#' @param summary_function The summary function used for the definition of the
#'   baseline value, in case cfb = TRUE, see [add_cfb()].
#' @param alpha The alpha value for the points as numeric.
#' @param title The plot title.
#' @param nominal_time Nominal time. Always used, this switch only for
#'   compatibility.
#' @param admin Plot administrations.
#'
#' @return A ggplot2 plot object.
#' @export
#' @keywords internal
#' @examples
#' nif_mean_plot(examplinib_sad_nif)
#' nif_mean_plot(examplinib_sad_nif, log = TRUE)
#' nif_mean_plot(examplinib_fe_nif, group = "FASTED", max_x = 12, points = TRUE)
#' nif_mean_plot(examplinib_poc_nif, dose = 500, max_x = 24, log = TRUE)
#' nif_mean_plot(examplinib_poc_nif, max_x = 12, group = "SEX")
#' nif_mean_plot(examplinib_poc_nif, max_x = 12, group = "SEX",
#'   analyte = "RS2023")
nif_mean_plot <- function(obj,
                          analyte = NULL, cmt = NULL, dose = NULL,
                          group = "ANALYTE", tad = FALSE, cfb = FALSE,
                          summary_function = median, points = FALSE,
                          point_size = 2, alpha = 1, lines = TRUE,
                          log = FALSE, min_x = NULL, max_x = NULL,
                          nominal_time = FALSE, title = "", admin = FALSE){


  temp <- obj %>%
    assertr::verify(assertr::has_all_names("NTIME", "DOSE", "DV")) %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    ensure_dose() %>%
    add_cfb(summary_function = summary_function) %>%
    as.data.frame() %>%
    {if(!is.null(dose)) filter(., .$DOSE %in% dose) else .} %>%
    {if(!is.null(analyte)) filter(., .$ANALYTE %in% analyte) else .} %>%
    {if(!is.null(cmt)) filter(., .$CMT %in% cmt | .$EVID==1) else .} %>%
    {if(cfb == TRUE) mutate(., DV = DVCFB) else .} %>%
    filter(!is.na(DV)) %>%
    as.data.frame() %>%
    filter(NTIME > 0) %>%
    mutate(TIME = NTIME) %>%
    filter(EVID == 0) %>%
    dplyr::filter(!is.na(DOSE))


  if(is.null(max_x)) {
    max_x <- max_observation_time(temp)}
  if(is.null(min_x)) {
    min_x = 0}

  n_analytes <- length(unique(temp$ANALYTE))

  if(n_analytes == 1) {
    y_label <- unique(temp$ANALYTE)
  } else {
    y_label <- "mean"
    group <- unique(c(group, "ANALYTE"))
  }

  temp %>%
    tidyr::unite(GROUP, group, sep = " | ", remove = FALSE) %>%
    reframe(mean = mean(DV, na.rm = TRUE),
            sd = sd(DV, na.rm = TRUE),
            n = n(),
            DOSE = DOSE,
            ANALYTE = ANALYTE,
            GROUP = GROUP,
            .by = c(TIME, GROUP)) %>%
    mutate(max_y = mean + sd) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = TIME,
      y = mean,
      group = GROUP,
      color = GROUP
    )) +
    {if (lines) ggplot2::geom_line(na.rm = TRUE)} +
    {if (points) ggplot2::geom_point(na.rm = TRUE, size = point_size,
                                     alpha = alpha)} +
    {if (log == TRUE) ggplot2::scale_y_log10()} +
    ggplot2::xlim(min_x, max_x) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = mean - sd, ymax = mean + sd, fill = as.factor(GROUP)),
      alpha = 0.3, color = NA, show.legend = FALSE) +
    {if (length(unique(temp$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
    ggplot2::labs(x = "NTIME",
         y = y_label,
         color = group,
         title = title,
         caption = "Data shown are mean and SD") +
    ggplot2::theme_bw() +
    {if(all(group == "ANALYTE") & n_analytes == 1)
      ggplot2::theme(legend.position = "none") else
        ggplot2::theme(legend.position = "bottom")}
}


# ' Spaghetti plot over a selected covariate
# '
# ' @param obj The NIF object.
# ' @param points Boolean to indicate whether points should be drawn.
# ' @param lines Boolean to indicate whether lines should be drawn.
# ' @param group The grouping covariate, defaults to 'ANALYTE'.
# ' @param tad Boolean to select whether time after dose (TAD) rather than TIME
# '   should be plotted.
# ' @param analyte The analyte as character. If NULL (default), all analytes will
# '   be plotted.
# ' @param dose The dose(s) to filter for as numeric.
# ' @param min_x The minimal value for the x scale as numeric.
# ' @param max_x The maximal value for the x scale as numeric.
# ' @param log Boolean to define whether y axis is plotted on the log scale.
# ' @param cmt The compartment to filter for as numeric.
# ' @param cfb Y value is change from baseline, defaults to FALSE.
# ' @param point_size the point size as numeric.
# ' @param summary_function The summary function used for the definition of the
# '   baseline value, in case cfb = TRUE, see [add_cfb()].
# ' @param alpha The alpha value for the points as numeric.
# ' @param title The plot title
# ' @param admin Plot administrations.
# ' @param nominal_time Plot NTIME rather than TIME. Defaults to FALSE.
# '
# ' @return A ggplot2 plot object.
# ' @import dplyr
# ' @export
# ' @keywords internal
# ' @examples
# ' nif_spaghetti_plot(examplinib_fe_nif)
# ' nif_spaghetti_plot(examplinib_fe_nif, nominal_time = TRUE, group = "FASTED")
# ' nif_spaghetti_plot(examplinib_sad_nif)
# ' nif_spaghetti_plot(examplinib_sad_nif, cfb = TRUE)
# ' nif_spaghetti_plot(examplinib_poc_nif, analyte="RS2023", admin = TRUE)
# ' nif_spaghetti_plot(examplinib_poc_nif)
# ' nif_spaghetti_plot(examplinib_poc_nif, analyte="RS2023", nominal_time = TRUE,
# '   group = "SEX", points = TRUE, lines = FALSE)
# ' nif_spaghetti_plot(examplinib_poc_nif, analyte="RS2023", tad = TRUE,
# '   dose = 500, log = FALSE, points = TRUE, lines = FALSE)
# ' nif_spaghetti_plot(examplinib_sad_min_nif)
# ' nif_spaghetti_plot(examplinib_poc_min_nif, dose = 500, cmt = 2)
# ' nif_spaghetti_plot(examplinib_poc_min_nif, dose = 500, cmt = 2, tad = TRUE,
# '   points = TRUE, lines = FALSE)
nif_spaghetti_plot <- function(obj,
                               analyte = NULL, cmt = NULL, dose = NULL,
                               group = "ANALYTE", tad = FALSE, cfb = FALSE,
                               summary_function = median, points = FALSE,
                               point_size = 2, alpha = 1, lines = TRUE,
                               log = FALSE, min_x = NULL, max_x = NULL,
                               nominal_time = FALSE, title = "",
                               admin = FALSE) {
  x <- obj %>%
    assertr::verify(assertr::has_all_names(
      "ID", "TIME", "AMT", "DV", "EVID")) %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    ensure_dose() %>%
    ensure_tad() %>%
    add_cfb(summary_function = summary_function) %>%
    index_dosing_interval() %>%
    as.data.frame() %>%
    {if(!is.null(dose)) filter(., .$DOSE %in% dose) else .} %>%
    {if(!is.null(analyte)) filter(., .$ANALYTE %in% analyte) else .} %>%
    {if(!is.null(cmt)) filter(., .$CMT %in% cmt | .$EVID==1) else .} %>%
    {if(cfb == TRUE) mutate(., DV = DVCFB) else .} %>%
    group_by(across(c(all_of(group)))) %>%
    filter(sum(!is.na(DV)) > 0) %>%
    ungroup()

  n_analytes <- length(unique(x$ANALYTE))

  if(n_analytes == 1) {
    y_label <- unique(x$ANALYTE)
    if(cfb == TRUE) {
      y_label <- paste(y_label, "change from baseline")
    }
    group <- unique(group)
  } else {
    if(cfb == TRUE) {
      y_label <- "change from baseline"
    } else {
      y_label <- "DV"
    }
    group <- unique(c(group, "ANALYTE"))
  }

  if (tad == TRUE) {
    x <- x %>%
      mutate(TIME = TAD) %>%
      tidyr::unite(GROUP, c(group, "DI", "ID"), sep = " | ", remove = FALSE) %>%
      tidyr::unite(COLOR, c(group, "DI"), sep = " | ", remove = FALSE)
    x_label <- "TAD"
  } else {
    # create mock administrations for all analytes, including metabolites
    # to avoid connecting plot lines across administrations
    mock_admin_for_metabolites <- x %>%
      as.data.frame() %>%
      filter(EVID == 1) %>%
      tidyr::crossing(ANALYTE1 = analytes(x)) %>%
      mutate(ANALYTE = ANALYTE1) %>%
      select(-ANALYTE1)

    x <- x %>%
      rbind(mock_admin_for_metabolites) %>%
      tidyr::unite(GROUP, all_of(c(group, "ID")), sep = " | ", remove = FALSE) %>%
      tidyr::unite(COLOR, all_of(group), sep = " | ", remove = FALSE)
    x_label <- "TIME"
  }

  if(isTRUE(nominal_time)){
    if(!"NTIME" %in% names(x)) {
      stop("NTIME not in NIF object!")
    } else {
      x <- x %>%
        mutate(TIME = NTIME)
      x_label <- "NTIME"
    }
  }

  if(is.null(min_x)) {
    min_x = 0}

  if(is.null(max_x)) {
    max_x <- max_observation_time(x)}

  x %>%
    filter(TIME <= max_x) %>%
    group_by(DOSE, ID) %>%
    filter(sum(EVID==0 & !is.na(DV)) > 1) %>%
    ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(
      x = TIME,
      y = DV,
      group = GROUP,
      color = COLOR,
      admin = EVID
    )) +
    {if (lines) ggplot2::geom_line(na.rm = TRUE)} +
    {if (points) ggplot2::geom_point(na.rm = TRUE, size = point_size,
                                     alpha = alpha)} +
    {if (length(unique(x$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
    {if (log == TRUE) ggplot2::scale_y_log10()} +
    {if(admin == TRUE) geom_admin()} +
    ggplot2::xlim(min_x, max_x) +
    ggplot2::labs(x = x_label, y = y_label, title = title, color = group) +
    ggplot2::theme_bw() +
    {if(all(group == "ANALYTE") & n_analytes == 1)
      ggplot2::theme(legend.position = "none") else
        ggplot2::theme(legend.position = "bottom")}
}


#' Plot nif object.
#'
#' @details
#' A watermark can be added by setting the global option 'watermark', e.g.,
#' `nif_option(watermark = "text")`.
#'
#' @param x A nif object.
#' @param analyte The analyte as character.
#' @param dose The dose as numeric.
#' @param log Logarithmic y scale
#' @param time The time field to use, as character. Can be 'TIME', 'NTIME',
#'   'TAFD' or 'TAD'.
#' @param group The grouping variable as character.
#' @param min_time The minimal time as numeric.
#' @param max_time The maximal time as numeric.
#' @param points Show data points as logical.
#' @param lines Show lines as logical.
#' @param admin Show vertical lines for administrations, as logical.
#' @param cfb Show change from baseline, as logical.
#' @param silent `r lifecycle::badge("deprecated")` Dummy option for
#'   compatibility, set the global option [nif_option()] with `silent = TRUE` to
#'   suppress messages.
#' @param mean Show a mean plot, as logical.
#' @param title The plot title as character.
#' @param caption The plot caption line as character.
#' @param ... Further graphical parameters for `geom_point()`.
#' @param integrate_predose Complete 'DOSE' field for predose values.
#' @param summary_function The summarizing function to apply to multiple
#'   baseline values.
#' @param legend Show legend, as logical.
#' @param show_n Show sample size in mean plot, as logical. Does currently not
#'   implement grouping! `r lifecycle::badge("experimental")`
#' @param shading Show ribbons when mean = TRUE, as logical.
#' @param dose_norm Dose-normalized exposure, as logical.
#'
#' @return A ggplot object.
#' @export
#'
#' @seealso [nif_option()]
#'
#' @examples
#' plot(examplinib_fe_nif, points = TRUE)
#' plot(examplinib_fe_nif, nominal_time = TRUE, group = "FASTED")
#' plot(examplinib_sad_nif, mean = TRUE, max_time = 24, show_n = TRUE)
#' plot(examplinib_poc_nif, analyte="RS2023", admin = TRUE)
#' plot(examplinib_poc_nif, analyte="RS2023", time = "NTIME",
#'   group = "SEX", points = TRUE, lines = FALSE)
#' plot(examplinib_poc_nif, analyte="RS2023", time = "TAD",
#'   dose = 500, log = FALSE, points = TRUE, lines = FALSE)
#' plot(examplinib_poc_min_nif, dose = 500, cmt = 2)
#' plot(examplinib_fe_nif, mean = TRUE, group = "FASTED", max_time = 24)
plot.nif <- function(x, analyte = NULL, dose = NULL, log = FALSE, time = "TAFD",
                     group = NULL, min_time = NULL, max_time = NULL,
                     points = FALSE, lines = TRUE, admin = NULL, cfb = FALSE,
                     summary_function = median, mean = FALSE,
                     title = "", caption = "", integrate_predose = TRUE,
                     legend = TRUE, show_n = FALSE, shading = TRUE,
                     silent = deprecated(), dose_norm = FALSE,
                     ...) {
  # Assert time field
  if(!time %in% c("TIME", "NTIME", "TAFD", "TAD")) {
    stop("time must be either 'TIME', 'NTIME', 'TAFD' or 'TAD'!")
  }

  # watermark
  # if(exists("watermark", envir = .nif_env)) {
  #   watermark = get("watermark", envir = .nif_env)
  # } else {
  #   watermark = ""
  # }

  #################################

  # Assert fields
  temp <- x %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    ensure_dose() %>%
    ensure_tad() %>%
    ensure_tafd() %>%
    ensure_cfb() %>%
    index_dosing_interval() %>%
    mutate(DI = case_match(EVID, 1 ~ NA, .default = DI)) %>%
    assertr::verify(rlang::exec(
      assertr::has_all_names,
      !!!c("ID", time, "ANALYTE", "PARENT", "DOSE", "DV", "EVID"))) #%>%
    # as.data.frame()

  # fill dose to predose values
  if (integrate_predose == TRUE) {
    temp <- temp %>%
      arrange(ID, ANALYTE, TIME) %>%
      group_by(ID, ANALYTE) %>%
      tidyr::fill(DOSE, .direction = "up") %>%
      ungroup()
  }

  # implement change from baseline
  temp <- temp %>%
    {if(cfb == TRUE) mutate(., DV = DVCFB) else .}

  # make active time field
  temp <- mutate(temp, active_time = .data[[time]])
  x_label = paste0(time, " (h)")

  # filter for dose
  if (is.null(dose)) {dose <- unique(temp$DOSE[temp$EVID == 0])}
  temp <- filter(temp, (DOSE %in% dose))

  # filter for analyte
  if(is.null(analyte)) {analyte <- analytes(temp)}
  temp <- filter(temp,
    (ANALYTE %in% analyte) | (EVID == 1 & ANALYTE %in% admin))

  # set y axis label
  n_analyte <- temp %>%
    filter(EVID == 0) %>%
    distinct(ANALYTE) %>%
    nrow()
  y_label <- ifelse(n_analyte == 1,
    as.character(temp %>% filter(EVID == 0) %>% distinct(ANALYTE)), "DV")

  if(dose_norm == TRUE) {
    temp <- mutate(temp, DV = DV/DOSE, DOSE = NA)
    y_label <- paste0(y_label, " / DOSE")
  }

  # implement max_time, min_time
  if(is.null(max_time)) {max_time <- max_time(filter(temp, EVID == 0),
                                              time_field = time)}
  if(is.null(min_time)) {min_time <- min(temp$active_time, na.rm = TRUE)}
  temp <- filter(temp, .data$active_time <= max_time &
                   .data$active_time >= min_time)

  # remove subjects without observation
  temp <- temp %>%
    group_by(ID, ANALYTE) %>%
    mutate(n_obs = sum(EVID == 0)) %>%
    ungroup()

  n_no_obs <- temp %>%
    filter(n_obs == 0, EVID == 0) %>%
    distinct(ID, ANALYTE) %>%
    nrow()

  if(n_no_obs != 0) {
    conditional_message(
      paste0(n_no_obs, " subjects had no observation for the analyte(s) ",
             nice_enumeration(analyte, "or"),
             " and were excluded from plotting:\n",
             df_to_string(filter(temp, n_obs == 0, EVID == 0) %>%
                            distinct(ID, ANALYTE, DOSE),
                          indent = "  ")))}
  temp <- filter(temp, n_obs > 0 | EVID == 1)

  # make plotting group and color
  plot_group <- group
  if(mean == FALSE) {
    plot_group <- unique(c(group, "ID"))}
  if(time == "TAD") {
    plot_group <- unique(c(plot_group, "DI"))}
  if(n_analyte > 1) {plot_group <- unique(c(plot_group, "ANALYTE"))}

  temp <- temp %>%
    {if(length(plot_group) > 0)
      tidyr::unite(., GROUP, all_of(plot_group), sep = "_", remove = FALSE)
      else mutate(., GROUP = TRUE)}

  temp <- temp %>%
    {if(length(group) > 0)
      tidyr::unite(., COLOR, all_of(group), sep = "_", remove = FALSE)
      else mutate(., COLOR = TRUE)}

  color_label <- nice_enumeration(unique(group))
  show_color <- length(unique(group)) > 0

  # make observations and administrations
  obs_data <- filter(temp, ANALYTE %in% analyte) %>%
    mutate(DV = case_when(EVID == 1 ~ NA, .default = DV))
  admin_data <- filter(temp, EVID == 1, ANALYTE %in% admin)

  # plotting
  if(mean == TRUE) {
    # mean plot
    temp %>%
      filter(EVID == 0) %>%
      {if(length(group[!group == "ID"]) > 0)
        tidyr::unite(., GROUP, group[!group == "ID"], sep = "_", remove = FALSE) else
          mutate(., GROUP = TRUE)} %>%
      group_by(NTIME, DOSE, GROUP, COLOR) %>%
      summarize(n = n(), mean = safe_mean(DV),
                sd = safe_sd(DV), .groups = "drop") %>%
      ungroup() %>%
      arrange(DOSE, NTIME, GROUP) %>%

    ggplot2::ggplot(ggplot2::aes(
      x = .data$NTIME,
      y = .data$mean,
      group = .data$GROUP,
      color = .data$COLOR)) +
      {if (lines) ggplot2::geom_line(na.rm = TRUE)} +
      {if (points) ggplot2::geom_point(na.rm = TRUE)} +
      {if (log == TRUE) ggplot2::scale_y_log10()} +
      {if (shading == TRUE) ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pos_diff(mean, sd),
                     ymax = mean + sd,
                     fill = as.factor(GROUP)),
        alpha = 0.3,
        color = NA,
        show.legend = FALSE)} +
      {if (length(unique(temp$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
      ggplot2::xlim(c(min_time, max_time)) +
      ggplot2::labs(x = "nominal time (h)", y = y_label, color = color_label,
           caption = "Mean and SD") +
      {if(show_n == TRUE) ggplot2::geom_text(
        ggplot2::aes(label = paste0 ("N = ", n)),
        x = -Inf,
        y = Inf, hjust = -0.2, vjust = 1.5, color = "darkgrey", size = 3.5)} +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = ifelse(
        show_color == TRUE & legend == TRUE, "bottom", "none")) +
      ggplot2::ggtitle(title) +
      watermark(cex = 1.5)

  } else {
    # spaghetti plot
    obs_data %>%
      {if(log == TRUE) filter(., DV != 0 | is.na(DV)) else .} %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$active_time,
        y = .data$DV,
        group = .data$GROUP,
        color = .data$COLOR)) +
      {if(!is.null(admin)) ggplot2::geom_vline(
        data = admin_data,
        ggplot2::aes(xintercept = .data$active_time), color = "gray")} +
      {if(points == TRUE) ggplot2::geom_point(na.rm = TRUE, ...)} +
      {if (lines) ggplot2::geom_line()} +
      {if (log == TRUE) ggplot2::scale_y_log10()} +
      {if(length(unique(temp$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
      ggplot2::labs(x = x_label, y = y_label, color = color_label,
                    caption = caption) +
      ggplot2::xlim(c(min_time, max_time)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position =
              ifelse(show_color == TRUE & legend == TRUE, "bottom", "none")) +
      ggplot2::ggtitle(title) +
      watermark(cex = 1.5)
  }
}


#' NIF object overview
#'
#' @param object The NIF object.
#' @param ... Further arguments.
# @param egfr_function The function to be used for estimation of the renal
#   function classes, see [add_bl_crcl()] for reference.
#' @return A summary_nif object.
#' @export
#' @noRd
#' @examples
#' summary(examplinib_poc_nif)
#' summary(examplinib_poc_min_nif)
#' summary(new_nif())
summary.nif <- function(object, ...) {
  subjects <- subjects(object)
  analytes <- analytes(object)
  parents <- parents(object)

  dose_red_sbs <- lapply(
    parents,
    function(x) {
      dose_red_sbs(object, analyte = x)
    }
  )
  names(dose_red_sbs) <- parents

  observations <- object %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    group_by(across(any_of(c("CMT", "ANALYTE")))) %>%
    summarize(N = n(), .groups = "drop") %>%
    as.data.frame()

  n_studies <- object %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    group_by(across(any_of(c("STUDYID")))) %>%
    summarize(N = n_distinct(ID), .groups = "drop")

  n_sex <- object %>%
    as.data.frame() %>%
    {if(!"SEX" %in% names(.)) mutate(., SEX = NA) else .} %>%
    mutate(SEX = factor(SEX, levels=c(0, 1))) %>%
    distinct(ID, SEX) %>%
    reframe(n = n(), .by = factor("SEX")) %>%
    tidyr::spread(SEX, n, fill = 0, drop = FALSE)
  n_males <- n_sex[1, "0"]
  n_females <- n_sex[1, "1"]

  if ("BL_CRCL" %in% colnames(object)) {
    renal_function <- object %>%
      as.data.frame() %>%
      mutate(CLASS = as.character(
        cut(BL_CRCL,
          breaks = c(0, 30, 60, 90, Inf),
          labels = c("severe", "moderate", "mild", "normal")
        )
      )) %>%
      distinct(ID, CLASS) %>%
      group_by(CLASS) %>%
      summarize(N = n()) %>%
      arrange(ordered("CLASS", c("normal", "mild", "moderate", "severe")))
  } else {
    renal_function <- NULL
  }

  if("BL_ODWG" %in% colnames(object)) {
    odwg <- object %>%
      as.data.frame() %>%
      mutate(CLASS = .data$BL_ODWG) %>%
      # mutate(BL_ODWG = factor(BL_ODWG,
      #   levels = c("normal", "mild", "moderate", "severe"))) %>%
      distinct(ID, CLASS) %>%
      group_by(CLASS) %>%
      summarize(N = n(), .groups = "drop") %>%
      arrange(factor(CLASS, levels = c("normal", "mild", "moderate", "severe")))
  } else {
    odwg = NULL
  }

  out <- list(
    nif = object,
    studies = studies(object),
    subjects = subjects,
    dose_red_sbs = dose_red_sbs,
    n_studies = n_studies,
    n_subj = nrow(subjects),
    n_males = n_males,
    n_females = n_females,
    n_obs = observations,
    analytes = analytes,
    n_analytes = length(analytes),
    drugs = parents,
    dose_levels = dose_levels(object,
      group = any_of(c("PART", "COHORT", "GROUP"))
    ),
    renal_function = renal_function,
    odwg = odwg,
    administration_duration = administration_summary(object)
  )
  class(out) <- "summary_nif"
  return(out)
}


#' Print NIF summary
#'
#' @param x A summary_nif object.
#' @param color Colorful output.
#' @param ... Further parameters.
#'
#' @noRd
#' @return Nothing.
#' @export
print.summary_nif <- function(x, color = FALSE, ...) {
  indent = " "
  # hline <- paste0(rep("\U2500", 8), collapse="")
  hline <- "-----"
  cat(paste0(hline, " NONMEM input file (NIF) object summary ", hline, "\n"))

  cat(paste(
    "Data from", sum(x$n_studies$N), "subjects across "))
  if(length(x$studies) == 1) {
    cat("one study:\n")
  } else {
    cat(paste0(length(x$studies), " studies:\n"))
  }

  cat(paste0(df_to_string(x$n_studies, color=color, indent = indent), "\n\n"))

  cat(paste0(
    "Males: ", x$n_males, ", females: ", x$n_females, " (",
    round(x$n_females / (x$n_males + x$n_females) * 100, 1), "%)\n\n"
  ))

  if (!is.null(x$renal_function)) {
    cat(paste0("Renal impairment class:\n", df_to_string(
      x$renal_function, color=color, indent = indent), "\n\n"))
  }

  if(!is.null(x$odwg)) {
    cat(paste0("NCI ODWG hepatic impairment class:\n", df_to_string(
      x$odwg, color=color, indent = indent), "\n\n"))
  }

  cat(paste0("Administered drugs:\n",
             paste0(indent, paste(x$drugs,collapse = ", ")), "\n\n"))

  cat(paste0("Analytes:\n",
             paste0(indent, paste0(x$analytes, collapse = ", ")), "\n\n"))

  cat("Subjects per dose levels:\n")
  cat(df_to_string(x$dose_levels, color=color, indent = indent))
  cat("\n\n")

  cat(paste(sum(x$n_obs$N), "observations:\n"))
  cat(paste0(df_to_string(x$n_obs, color=color, indent = indent), "\n\n"))

  dr_summary <- lapply(x$dose_red_sbs, length) %>%
    data.frame()
  cat("Subjects with dose reductions\n")
  cat(df_to_string(dr_summary, color=color, indent = indent))
  cat("\n\n")

  cat("Treatment duration overview:\n")
  cat(df_to_string(x$administration_duration, color=color, indent = indent))
  invisible(x)
}


#' Get covariate plot parameters
#'
#' @param field The field of the NIF object
#' @keywords internal
#' @return A data frame.
get_cov_plot_params <- function(field) {
  params <- tribble(
    ~field, ~binwidth, ~xlabel, ~title, ~limits,
    "AGE", 5, "age (years)", "Age", NULL,
    "WEIGHT", 5, "body weight (kg)", "Body weight", NULL,
    "HEIGHT", 5, "body height (cm)", "Height", NULL,
    "BMI", 1, "BMI (kg/m^2)", "Body mass index", c(18.5, 24.9, 30),
    "BL_CRCL", NA, "baseline creatinine clearance (ml/min)",
    "Baseline creatinine clearance", c(30, 60, 90)
  )

  if (field %in% params$field) {
    return(params[which(params$field == field), ])
  } else {
    return(data.frame(
      field = field, binwidth = NA, xlabel = field,
      title = field
    ))
  }
}


#' Plot NIF summary
#'
#' @param x A NIF object.
#' @param baseline Plot baseline data, as logical.
#' @param analytes Plot analyte overview as logical.
#' @param ... Further arguments.
#'
#' @return A list of ggplot objects.
#' @export
#' @noRd
#' @examples
#' plot(summary(examplinib_poc_nif))
plot.summary_nif <- function(x, baseline = TRUE, analytes = TRUE, ...) {
  nif <- x$nif
  out <- list()

  if(baseline == TRUE){
    for (i in c("AGE", "WEIGHT", "HEIGHT", "BMI",
                str_subset(names(nif), "BL_.*"))) {
      if (i %in% colnames(nif)) {
        if(is.numeric(nif[[i]])) {
          out[[i]] <- covariate_hist(nif, i)
        }
        if(is.factor(nif[[i]])) {
          out[[i]] <- covariate_barplot(nif, i)
        }
      }
    }
    if("WEIGTH" %in% names(nif) & "SEX" %in% names(nif))
      out[["WT_SEX"]] <- wt_by_sex(nif)
    if("WEIGTH" %in% names(nif) & "RACE" %in% names(nif))
      out[["WT_RACE"]] <- wt_by_race(nif)
  }

  if(analytes == TRUE){
    # put analytes for parents first:
    analyte_list <- x$nif %>%
      as.data.frame() %>%
      distinct(PARENT, ANALYTE) %>%
      mutate(score = PARENT == ANALYTE) %>%
      arrange(-.data$score, .data$ANALYTE) %>%
      pull(ANALYTE)

    for (i in analyte_list) {
      out[[i]] <- plot(nif,
        analyte = i, log = TRUE, points = TRUE, lines = FALSE, time = "TIME",
        alpha = 0.3, title = paste(i, "by dose"),
        max_time = max_observation_time(x$nif, i)
      ) +
        ggplot2::labs(y = "")
    }
  }
  return(out)
}


#' Generic covariate distribution histogram
#'
#' @param obj The NIF object.
#' @param nbins The number of bins to be used if no binwidth is specified.
#' Defaults to 11.
#' @param group The field(s) to group by, as character.
#' @param alpha The alpha as numeric.
#' @param cov The covariate field of the NIF object to analyze, as character.
#' @param title The title as character.
#' @param density Plot density instead of N on the y axis, as logical.
#'
#' @return A plot object.
#' @export
#' @examples
#' covariate_hist(examplinib_sad_nif, "AGE")
#' covariate_hist(examplinib_sad_nif, "BL_CRCL")
covariate_hist <- function(obj, cov, nbins = 11, group = NULL, alpha = 0.5,
                           density = TRUE, title = NULL) {
  cov_params <- get_cov_plot_params(cov)
  xlabel <- cov_params$xlabel
  if (is.na(xlabel)) {
    xlabel <- cov_params$field
  }
  if(is.null(title)){
    title <- cov_params$title
    if (is.na(title)) {
      title <- paste(cov_params$field, "distribution")
    }
    if(!is.null(group)) {
      title <- paste0(title, " grouped by ", nice_enumeration(group))
    }
  }
  limits <- unlist(cov_params$limits)

  binwidth = NULL
  if(is.na(cov_params$binwidth)) {
    binwidth = NULL
    bins = nbins
  } else {
    binwidth = cov_params$binwidth
    bins = NULL
  }

  if(!is.null(group)) {
    p <- obj %>%
      as.data.frame() %>%
      mutate_at(group, factor) %>%
      tidyr::unite(GROUP, all_of(group), remove = FALSE) %>%
      distinct_at(c("ID", cov_params$field, "GROUP")) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data[[cov_params$field]], group = GROUP,
                 fill = GROUP)) +
      {if(density == FALSE) ggplot2::geom_histogram(
        bins = bins, binwidth = binwidth, position = "identity",
        alpha = alpha) else
          ggplot2::geom_histogram(ggplot2::aes(
            y = ggplot2::after_stat(density)),
            bins = bins, binwidth = binwidth,
            position = "identity", alpha = alpha)}
  } else {
    p <- obj %>%
      as.data.frame() %>%
      distinct_at(c("ID", cov_params$field)) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data[[cov_params$field]])) +
      {if(density == FALSE) ggplot2::geom_histogram(
        bins = bins, binwidth = binwidth,
        position = "identity", fill = "grey") else
          ggplot2::geom_histogram(ggplot2::aes(
            y = ggplot2::after_stat(density)),
            bins = bins,
            binwidth = binwidth, position = "identity", fill = "grey")}
  }
  p +
    ggplot2::geom_vline(xintercept = limits, color = "red") +
    ggplot2::theme_bw() +
    {if(density == FALSE) ggplot2::labs(
      x = xlabel, y = "number of subjects") else
        ggplot2::labs(x = xlabel, y = "density")} +
    ggplot2::ggtitle(title) +
    watermark(cex = 1.5)
}


#' Generic covariate barplot
#'
#' @param obj A nif object.
#' @param cov The (categorical) baseline covariate as character.
#' @param title The plot title as character. Defaults to cov, if NULL.
#' @param group A grouping variable, as character.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' covariate_barplot(examplinib_poc_nif, "SEX")
covariate_barplot <- function(obj, cov, group = NULL, title = NULL) {
  if(is.null(title)) {
    title = cov
    if(!is.null(group)) {
      title <- paste0(title, " grouped by ", nice_enumeration(group))
    }
  }

  if(!is.null(group)) {
    out <- obj %>%
      as.data.frame() %>%
      mutate_at(group, factor) %>%
      tidyr::unite(GROUP, all_of(group), remove = FALSE) %>%
      mutate(CLASS = as.factor(.data[[cov]])) %>%
      distinct_at(c("ID", "CLASS", "GROUP")) %>%
      group_by(CLASS, GROUP) %>%
      summarize(n = n(), .groups = "drop") %>%
      ggplot2::ggplot(ggplot2::aes(
        x = CLASS, y = n, group = GROUP, fill = GROUP)) +
      ggplot2::scale_x_discrete(drop = FALSE, name = cov) +
      ggplot2::geom_bar(stat = "identity",
               position = ggplot2::position_dodge(preserve = "single"),
               width = 0.5, alpha = 0.8) +
      ggplot2::labs(fill = group)
  } else {
    out <- obj %>%
      as.data.frame() %>%
      mutate(CLASS = as.factor(.data[[cov]])) %>%
      distinct(ID, CLASS) %>%
      group_by(CLASS) %>%
      summarize(n = n()) %>%
      ggplot2::ggplot(ggplot2::aes(x = CLASS, y = n)) +
      ggplot2::scale_x_discrete(drop = FALSE, name = cov) +
      ggplot2::geom_bar(
        stat = "identity", fill = "white", color = "black", width = 0.5)
  }

  out +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "N") +
    ggplot2::ggtitle(title) +
    watermark()
}


#' Weight by sex diagram
#'
#' @param obj The NIF file object.
#'
#' @return A plot object.
#' @export
#' @keywords internal
#' @examples
#' wt_by_sex(examplinib_poc_nif)
wt_by_sex <- function(obj) {
  obj %>%
    as.data.frame() %>%
    group_by(ID) %>%
    mutate(bl_wt = mean(WEIGHT[TIME == 0])) %>%
    ungroup() %>%
    distinct(ID, SEX, bl_wt) %>%
    group_by(SEX) %>%
    mutate(count = n(), maxwt = max(bl_wt)) %>%
    ggplot2::ggplot(ggplot2::aes(x = SEX, y = bl_wt, group = SEX)) +
    ggplot2::scale_x_continuous(breaks = c(0, 1)) +
    ggplot2::geom_boxplot(width = 0.5) +
    ggplot2::geom_label(ggplot2::aes(
      label = paste0("N=", count), y = maxwt + 5),
      label.size = 0, position = ggplot2::position_dodge(width = 0.75)
    ) +
    ggplot2::labs(x = "sex", y = "baseline weight (kg)") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Body weight by sex") +
    watermark()
}


#' Weight by race diagram
#'
#' @param obj The NIF file object.
#' @return A plot object.
#' @export
#' @keywords internal
#' @examples
#' wt_by_race(examplinib_poc_nif)
wt_by_race <- function(obj) {
  obj %>%
    as.data.frame() %>%
    group_by(ID) %>%
    mutate(bl_wt = mean(WEIGHT[TIME == 0])) %>%
    ungroup() %>%
    mutate(rc = as.factor(
      case_match(as.character(RACE),
        "WHITE" ~ "White",
        "BLACK OR AFRICAN AMERICAN" ~ "Black",
        "ASIAN" ~ "Asian",
        "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "Pacific",
        "AMERICAN INDIAN OR ALASKA NATIVE" ~ "Native",
        "OTHER" ~ "Other",
        .default = RACE
      )
    )) %>%
    distinct(ID, bl_wt, rc) %>%
    mutate(maxwt = max(bl_wt, na.rm = TRUE)) %>%
    group_by(rc) %>%
    mutate(count = n()) %>%
    ggplot2::ggplot(ggplot2::aes(x = rc, y = bl_wt, group = rc)) +
    ggplot2::geom_boxplot(width = 0.5) +
    ggplot2::geom_label(ggplot2::aes(
      label = paste0("N=", count), y = maxwt + 5), label.size = 0
    ) +
    ggplot2::labs(x = "", y = "baseline weight (kg)") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("Body weight by race") +
    watermark()
}


#' Weight by height scatterplot
#'
#' @param obj The NIF object.
#' @param alpha The alpha for the point rendering.
#' @return A plot object.
#' @export
#' @examples
#' wt_by_ht(examplinib_poc_nif)
wt_by_ht <- function(obj, alpha = 0.7) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, HEIGHT, WEIGHT) %>%
    ggplot2::ggplot(ggplot2::aes(x = HEIGHT, y = WEIGHT)) +
    ggplot2::geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
    ggplot2::geom_point(size = 3, alpha = alpha) +
    ggplot2::labs(x = "height (cm)", y = "baseline weight (kg)") +
    ggplot2::ggtitle("Body weight by height") +
    ggplot2::theme_bw()
}


#' Weight by height scatterplot
#'
#' @param obj The NIF object.
#' @param alpha The alpha for the point rendering.
#' @return A plot object.
#' @export
#' @examples
#' ht_by_wt(examplinib_poc_nif)
ht_by_wt <- function(obj, alpha = 0.7) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, HEIGHT, WEIGHT) %>%
    ggplot2::ggplot(ggplot2::aes(x = WEIGHT, y = HEIGHT)) +
    ggplot2::geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
    ggplot2::geom_point(size = 3, alpha = alpha) +
    ggplot2::labs(y = "height (cm)", x = "baseline weight (kg)") +
    ggplot2::ggtitle("Body height by weight") +
    ggplot2::theme_bw() +
    watermark()
}


#' BMI by age scatterplot
#'
#' @param obj The NIF object.
#' @param alpha The alpha for the point rendering.
#' @return A plot object.
#' @export
#' @examples
#' bmi_by_age(examplinib_poc_nif)
bmi_by_age <- function(obj, alpha = 0.7) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, AGE, BMI) %>%
    ggplot2::ggplot(ggplot2::aes(x = AGE, y = BMI)) +
    ggplot2::geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
    ggplot2::geom_point(size = 3, alpha = alpha) +
    ggplot2::labs(y = "BMI (kg/m^2)", x = "age (y)") +
    ggplot2::ggtitle("Body mass index by age") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    watermark()
}


#' Plot TIME vs NTIME
#'
#' @param obj A nif object.
#' @param max_time The maximum time as numeric.
#' @param ... Further graphical parameters.
#'
#' @return A ggplot object.
#' @export
time_by_ntime <- function(obj, max_time = NULL, ...) {
  if(is.null(max_time)) {
    max_time <- max_time(obj, only_observations = TRUE)
  }

  obj %>%
    ensure_analyte() %>%
    assertr::verify(assertr::has_all_names(
      "ID", "TIME", "NTIME", "EVID", "ANALYTE")) %>%
    filter(TIME <= max_time) %>%
    filter(EVID == 0) %>%
    filter(!is.na(TIME)) %>%
    ggplot2::ggplot(ggplot2::aes(x = NTIME, y = TIME, group = ID)) +
    ggplot2::geom_point(...) +
    ggplot2::theme_bw() +
    watermark()
}


#' Overview on the number of administrations in the subjects by parent
#'
#' @param obj A NIF object.
#' @return A data frame.
#' @importFrom stats median
#' @export
#' @examples
#' administration_summary(examplinib_poc_nif)
#' administration_summary(new_nif())
administration_summary <- function(obj) {
  temp <- obj %>%
    ensure_parent() %>%
    n_administrations()

  if(nrow(temp) == 0) {
    data.frame(PARENT = character(), min = numeric(), max = numeric(),
               mean = numeric(), median = numeric())
  } else {
    temp %>%
      filter(PARENT != "") %>%
      group_by(across(any_of(c("PARENT")))) %>%
      summarize(
        min = min(N, na.rm = TRUE), max = max(N, na.rm = TRUE),
        mean = round(mean(N, na.rm = TRUE), 1),
        median = stats::median(N, na.rm = TRUE)
      ) %>%
      as.data.frame()
  }
}


#' Mean dose plot
#'
#' This function plots the mean dose per day over time
#'
#' @param obj A NIF object.
#' @param analyte The compound as character (i.e., the ANALYTE within the data
#'   set).
#' @param title The plot title as character.
#' @return A ggplot object.
#' @export
#' @keywords internal
#' @examples
#' mean_dose_plot(examplinib_poc_nif)
mean_dose_plot <- function(obj, analyte = NULL, title = NULL) {
  if (is.null(analyte)) {
    analyte <- guess_analyte(obj)
  }

  if(is.null(title)) {
    title <- paste0("Mean ", analyte, " dose over time")
  }

  obj %>%
    as.data.frame() %>%
    mutate(DAY = floor(TIME / 24) + 1) %>%
    filter(EVID == 1, ANALYTE == analyte) %>%
    group_by(DAY) %>%
    summarize(
      "mean dose (mg)" = mean(DOSE, na.rm = TRUE), "N" = n(),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(cols = -DAY, names_to = "PARAM", values_to = "VAL") %>%
    ggplot2::ggplot(ggplot2::aes(x = DAY, y = VAL)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(PARAM ~ ., scales = "free_y") +
    ggplot2::labs(x = "time (days)", y = "") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(title) +
    watermark()
}


#' Subjects per dose level
#'
#' The number of subjects that have observations, per dose level and analyte.
#'
#' @param obj A NIF object.
#' @param group An (optional) grouping variable as character. Defaults to
#' @param analyte The analyte as character. If `NULL` (default), all analytes
#'   are selected.
#' @return A data frame.
#' @export
#' @examples
#' subs_per_dose_level(examplinib_poc_nif)
#' subs_per_dose_level(examplinib_sad_nif)
#' subs_per_dose_level(examplinib_poc_nif, group = "SEX", analyte = "RS2023")
subs_per_dose_level <- function(obj, analyte = NULL, group = NULL) {
  if(is.null(analyte)) {
    analyte <- analytes(obj)
  }
  obj %>%
    ensure_analyte() %>%
    add_dose_level() %>%
    as.data.frame() %>%
    filter(ANALYTE %in% analyte) %>%
    filter(EVID == 0) %>%
    distinct(across(any_of(c("ID", "DL", "ANALYTE", group)))) %>%
    reframe(N = n(), .by=any_of(c("DL", "ANALYTE", "SEX"))) %>%
    arrange(DL, ANALYTE)
}


#' Observations per dose level
#'
#' The total number of observations, per dose level and analyte.
#' @param group An (optional) grouping variable as character. Defaults to
#'   'NULL'.
#' @param analyte The analyte as character. If `NULL` (default), all analytes
#'   are selected.
#' @param obj A NIF object.
#' @return A data frame.
#' @export
#' @examples
#' obs_per_dose_level(examplinib_poc_nif)
#' obs_per_dose_level(examplinib_sad_nif)
#' obs_per_dose_level(examplinib_poc_nif, group = "SEX", analyte = "RS2023")
obs_per_dose_level <- function(obj, analyte = NULL, group = NULL) {
  if(is.null(analyte)) {
    analyte <- analytes(obj)
  }
  obj %>%
    ensure_analyte() %>%
    add_dose_level() %>%
    as.data.frame() %>%
    filter(ANALYTE %in% analyte) %>%
    filter(EVID == 0) %>%
    reframe(N = n(), .by=any_of(c("DL", "ANALYTE", group))) %>%
    arrange(DL, ANALYTE)
}


#' Drug-induced serious hepatotoxicity (eDISH) plot
#'
#' @description
#'
#' Refer to the [FDA guidance on Drug-induced liver injury](https://www.fda.gov/media/116737/download)
#' and [Watkins 2011](https://doi.org/10.2165/11586600-000000000-00000).
#'
#' @param nif A nif object.
#' @param sdtm A sdtm object.
#' @param enzyme The transaminase enzyme to be plotted on the x axis as
#'   character, can be 'AST' or 'ALT' (default).
#' @param title The plot title as character.
#' @param size The point size as numeric.
#' @param alpha The alpha value as numeric.
#' @param ... Further graphical parameters.
#' @param show_labels Show ID labels per point.
#' @param nominal_time Use NTIME as logical.
#' @param time time/nominal time filter as numeric.
#' @param parent The parent compound as character.
#' @param shading Highlight Hy's law area, as logical.
#' @param observation_filter A filter term as character.
#' @param autoscale Use automatic axis scaling, as logical. Defaults to
#'   0.01-1000 for ALT/AST and 0.01-100 for bili.
#'
#' @return A ggplot object.
#' @importFrom ggrepel geom_text_repel
#' @export
edish_plot <- function(nif, sdtm, enzyme = "ALT",
                       observation_filter = "LBSPEC != 'URINE'",
                       show_labels = FALSE, autoscale = TRUE,
                       shading = TRUE, nominal_time = TRUE, time = NULL,
                       parent = NULL, title = "eDISH plot: All time points",
                       size = 3, alpha = 0.5, ...) {
  lb <- sdtm %>%
    domain("lb") %>%
    filter(eval(parse(text = observation_filter))) %>%
    assertr::verify(assertr::has_all_names(
      "USUBJID", "LBTESTCD", "LBSTRESN", "LBSTNRHI"))

  if(!all(c("BILI", enzyme) %in% lb$LBTESTCD))
    stop("Liver markers values not found in LB")

  lb1 <- lb %>%
    mutate(LB1DTC = .data$LBDTC) %>%
    filter(.data$LBTESTCD %in% c(enzyme, "BILI")) %>%
    mutate(LB1TESTCD = case_match(.data$LBTESTCD, "BILI" ~ "BILI",
                                 .default = "ENZ")) %>%
    mutate(LB1TESTCD = paste0(.data$LB1TESTCD, "_X_ULN"),
           LB1STRESN = .data$LBSTRESN / .data$LBSTNRHI)

  sdtm$domains[["lb1"]] <- lb1

  if(nominal_time == TRUE) {
    nif <- nif %>%
      mutate(TIME = .data$NTIME)
  }
  if(!is.null(time)) {
    nif <- nif %>%
      filter(.data$TIME %in% time)
  }
  if(is.null(parent)) {
    parent <- guess_parent(nif)
  }

  # suppressWarnings({
    p <- nif %>%
      add_observation(sdtm, "lb1", "ENZ_X_ULN", parent=parent) %>%
      add_observation(sdtm, "lb1", "BILI_X_ULN", parent=parent) %>%
      as.data.frame() %>%
      filter(!is.na(DV)) %>%
      filter(.data$ANALYTE %in% c("ENZ_X_ULN", "BILI_X_ULN")) %>%
      select(.data$ID, .data$TIME, .data$ANALYTE, .data$DV) %>%
      group_by(.data$ID, .data$TIME, .data$ANALYTE) %>%
      summarize(DV = mean(.data$DV, na.rm = TRUE), .groups = "drop") %>%
      group_by(.data$ID, .data$TIME) %>%
      tidyr::pivot_wider(names_from = "ANALYTE", values_from = "DV") %>%
      ungroup() %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$ENZ_X_ULN, y = .data$BILI_X_ULN,
                                   color = (TIME > 0), label = .data$ID)) +
      ggplot2::geom_point(size = size, alpha = alpha) +
      {if(show_labels == TRUE) ggrepel::geom_text_repel()} +
      {if(!autoscale == TRUE) ggplot2::scale_x_log10(
        limits = c(0.01, 1000)) else ggplot2::scale_x_log10()} +
      {if(!autoscale == TRUE) ggplot2::scale_y_log10(limits = c(0.01, 100)) else
        ggplot2::scale_y_log10()} +

      {if(shading == TRUE) ggplot2::annotate('rect', xmin = 3, xmax = Inf,
                                             ymin = 2, ymax = Inf, alpha=.15,
                                             fill='grey')} +
      ggplot2::geom_hline(yintercept = 2, linetype="dashed") +
      ggplot2::geom_vline(xintercept = 3, linetype="dashed") +
      ggplot2::labs(x = paste0(enzyme, "/ULN"), y = "BILI/ULN") +#,
           # caption = paste0(length(unique(nif$ID)),
           #   " subjects", "red: predose, grey area: Hy's law.")) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle(title)

    caption <- paste0(length(unique(nif$ID)), " subjects, red: predose")
    caption <- ifelse(shading == TRUE,
                      paste0(caption, ", grey area: Hy's law."), caption)
    p <- p +
      ggplot2::labs(caption = caption) +
      watermark(cex = 1.5)
  # })
  return(p)
}









