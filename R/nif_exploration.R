#' Plot DV time course data from individual subject
#'
#' This function plots DV over TIME for an individual subject, id. Id can be
#' either the ID or the USUBJID. Administration time points are indicated with
#' vertical lines.
#'
#' @param obj The NIF object
#' @param id The subject ID to be plotted
#' @param y_scale Y-scale. Use 'scale="log"' for a logarithmic y scale. Default
#'   is "lin".
#' @param analyte The analytes to be displayes. Defaults to NULL (all).
#' @param imp The IMP for which administrations are to be indicated by vertical
#'   lines. Defaults to NULL.
#' @param max_time The right limit of the time scale
#' @param tad Logivcal value to select whether time after dose (TAD) rather than
#'   TIME should be plotted.
#' @param lines Plot lines as logical.
#' @param cmt The compartment to plot as numeric.
#' @param point_size Point size as numeric.
#' @return A ggplot2 object.
#' @import dplyr
#' @import ggplot2
#' @export
#' @keywords internal
#' @examples
#' nif_plot_id(examplinib_poc_nif, 1)
#' nif_plot_id(examplinib_poc_min_nif, 1)
#' #' nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023", tad = TRUE)
#' nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte="RS2023", imp="RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte=c("RS2023", "RS2023487A"))
#' nif_plot_id(examplinib_poc_min_nif, 1, analyte="CMT3")
#' nif_plot_id(examplinib_poc_min_nif, 1, tad=TRUE)
nif_plot_id <- function(obj, id, analyte = NULL, cmt = NULL, y_scale = "lin",
                        max_time = NA, lines = TRUE, point_size = 2,
                        tad = FALSE, imp = NULL) {
  x <- obj %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    index_dosing_interval() %>%
    as.data.frame() %>%
    verify(has_all_names(
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
    # id_label <- ""
  } else {
    if("USUBJID" %in% names(x)) {
      if (id %in% x$USUBJID) {
        x <- x %>%
          filter(USUBJID == id)
        id_label <- paste0(" (ID ", x %>% distinct(ID) %>% pull(ID), ")")
        plot_label <- "USUBJID"
      } else {
        stop(paste(id, "is not an ID or USUBJID contained in the NIF object"))
      }
    }
  }

  if (tad == TRUE && "PARENT" %in% names(x)) {
    if (!"TAD" %in% colnames(x)) {
      x <- x %>% add_tad()
    }
    x <- x %>%
      mutate(TIME = TAD)
  }

  obs <- x %>%
    filter(EVID == 0)

  admin <- x %>%
    # as.data.frame() %>%
    dplyr::filter(EVID == 1) %>%
    # dplyr::filter(PARENT == imp)
    {if(!is.null(imp)) filter(., PARENT == imp) else NULL}

  if (tad == TRUE) {
    p <- obs %>%
      ggplot2::ggplot(ggplot2::aes(
        x = TIME, y = DV,
        group = interaction(ID, as.factor(ANALYTE), DI),
        color = interaction(as.factor(ANALYTE), DI)
      ))
  } else {
    p <- obs %>%
      ggplot2::ggplot(ggplot2::aes(
        x = TIME, y = DV,
        group = interaction(ID, as.factor(ANALYTE)),
        color = as.factor(ANALYTE)
      ))
  }

  p <- p +
    { if(!is.null(admin) > 0)
      geom_vline(data = admin,
                 aes(xintercept = TIME),
                 color = "gray")} +
    { if (lines == TRUE) ggplot2::geom_line() } +
    ggplot2::geom_point(size = point_size) +
    ggplot2::xlim(0, max_time) +
    ggplot2::labs(
      title = paste0(plot_label, ": ", id, id_label),
      color = "analyte"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if(!is.null(imp)) {
    p <- p +
      labs(caption = paste("vertical lines indicate", imp, "administrations"))
  }

  if (y_scale == "log") {
    p <- p + scale_y_log10()
  } else {
    p <- p + scale_y_continuous(limits = c(0, NA))
  }
  return(p)
}


#' Plot dose time course data from individual subject
#'
#' This function plots AMT over TIME for an individual subject, id. Id can be
#' either the ID or the USUBJID. Administration time points are indicated with
#' vertical lines.
#' @param obj The NIF object.
#' @param id The subject ID to be plotted.
#' @param y_scale Y-scale. Use 'scale="log"' for a logarithmic y scale. Default
#'   is "lin".
#' @param max_dose The upper limit of the dose scale.
#' @param analyte The analyte of interest.
#' @param max_time The right limit of the time scale.
#' @param point_size The point size as numeric.
#' @return A ggplot object.
#' @import dplyr
#' @import ggplot2
#' @export
#' @keywords internal
#' @examples
#' dose_plot_id(examplinib_poc_nif, 18)
#' dose_plot_id(examplinib_poc_nif, dose_red_sbs(examplinib_poc_nif)[[1]])
#' dose_plot_id(examplinib_poc_min_nif, 18)
dose_plot_id <- function(obj, id, y_scale = "lin", max_dose = NA,
                         point_size = 2, max_time = NA, analyte = NULL) {

  x <- obj %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    index_dosing_interval() %>%
    as.data.frame() %>%
    verify(has_all_names(
      "ID", "TIME", "AMT", "DV", "EVID")) %>%
    # {if(!is.null(cmt)) filter(., .$CMT == cmt) else .} %>%
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
          filter(USUBJID == id)
        id_label <- paste0(" (ID ", x %>% distinct(ID) %>% pull(ID), ")")
        plot_label <- "USUBJID"
      } else {
        stop(paste(id, "is not an ID or USUBJID contained in the NIF object"))
      }
    }
  }

  # if (id %in% obj$ID) {
  #   plot_label <- "ID"
  #   obj <- obj %>% filter(ID == id)
  # } else if (id %in% obj$USUBJID) {
  #   obj <- obj %>% filter(USUBJID == id)
  #   plot_label <- "USUBJID"
  # } else {
  #   stop(paste(id, "is not an ID or USUBJID contained in the NIF object"))
  # }
  #
  # if (!is.null(analyte)) {
  #   obj <- obj %>%
  #     filter(ANALYTE %in% analyte)
  # }

  admin <- x %>%
    dplyr::filter(EVID == 1)

  p <- admin %>%
    ggplot2::ggplot(ggplot2::aes(
      x = TIME, y = AMT,
      group = interaction(ID, ANALYTE),
      color = ANALYTE
    )) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = point_size) +
    ggplot2::ylim(0, max_dose) +
    ggplot2::xlim(0, max_time) +
    ggplot2::labs(title = paste0(plot_label, ": ", id), color = "treatment") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if (y_scale == "log") {
    p <- p + scale_y_log10()
  }
  return(p)
}


#' Mean plot over a selected covariate
#'
#' The averaging over subjects expects the `NTIME` field be in the data set.
#'
#' @param x The NIF object.
#' @param points Boolean to indicate whether points should be drawn.
#' @param lines Boolean to indicate whether lines should be drawn.
#' @param dose The dose(s) to filter for as numeric.
#' @param analyte The analyte as character. If NULL (default), all analytes will
#'   be plotted.
#' @param min_x The minimal value for the x scale as numeric.
#' @param max_x The maximal value for the x scale as numeric.
#' @param log Logarithmic y scale as logical.
#' @param point_size The point size as numeric, defaults to 2.
#' @param group The grouping covariate, defaults to 'ANALYTE'.
#' @return A ggplot2 plot object.
#' @export
#' @keywords internal
#' @examples
#' nif_mean_plot(examplinib_sad_nif)
#' nif_mean_plot(examplinib_fe_nif, group = "FASTED")
#' nif_mean_plot(examplinib_poc_nif, dose = 500, max_x = 24)
#' nif_mean_plot(examplinib_poc_nif, dose = 500, max_x = 12, group = "SEX")
nif_mean_plot <- function(x,
                          points = FALSE, lines = TRUE, group = "ANALYTE",
                          dose = NULL, analyte = NULL, min_x = NULL,
                          max_x = NULL, log = FALSE, point_size = 2) {
  if(is.null(max_x)) {
    max_x <- max_observation_time(x)}
  if(is.null(min_x)) {
    min_x = 0}

  temp <- x %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    ensure_dose() %>%
    as.data.frame() %>%
    verify(has_all_names("NTIME", "DOSE", "DV")) %>%
    {if(!is.null(dose)) filter(., .$DOSE %in% dose) else .} %>%
    {if(!is.null(analyte)) filter(., .$ANALYTE %in% analyte) else .} %>%

    filter(NTIME > 0) %>%
    filter(EVID == 0) %>%
    dplyr::filter(!is.na(DOSE)) %>%
    dplyr::group_by(NTIME, .data[[group]], DOSE) %>%
    dplyr::summarize(
      mean = mean(DV, na.rm = TRUE), sd = sd(DV, na.rm = TRUE),
      n = n(), .groups = "drop"
    ) %>%
    mutate(max_y = mean + sd)

  temp %>%
    ggplot2::ggplot(ggplot2::aes(
      x = NTIME, y = mean,
      group = as.factor(.data[[group]]),
      color = as.factor(.data[[group]])
    )) +
    {if (lines) geom_line(na.rm = TRUE)} +
    {if (points) geom_point(na.rm = TRUE, size = point_size)} +
    xlim(min_x, max_x) +
    ggplot2::geom_ribbon(
      aes(
        ymin = mean - sd, ymax = mean + sd,
        fill = as.factor(.data[[group]])
      ),
      alpha = 0.3, color = NA, show.legend = FALSE
    ) +
    {if (length(unique(temp$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
    ggplot2::theme_bw() +
    # ggplot2::labs(caption = "Data shown are mean \u00B1 SD") +
    ggplot2::labs(caption = "Data shown are mean and SD") +
    labs(color = group) +
    ylim(0, max(temp$max_y, na.rm = TRUE)) +
    ggplot2::theme(legend.position = "bottom")
}


#' Spaghetti plot over a selected covariate
#'
#' @param obj The NIF object.
#' @param points Boolean to indicate whether points should be drawn.
#' @param lines Boolean to indicate whether lines should be drawn.
#' @param group The grouping covariate, defaults to 'ANALYTE'.
#' @param nominal_time Boolean to indicate whether the x-axis should be NTIME
#'   rather than TIME.
#' @param tad Boolean to select whether time after dose (TAD) rather than TIME
#'   should be plotted.
#' @param analyte The analyte as character. If NULL (default), all analytes will
#'   be plotted.
#' @param dose The dose(s) to filter for as numeric.
#' @param min_x The minimal value for the x scale as numeric.
#' @param max_x The maximal value for the x scale as numeric.
#' @param log Boolean to define whether y axis is plotted on the log scale.
#' @param point_size the point size as numeric.
#' @return A ggplot2 plot object.
#' @import assertr
#' @export
#' @keywords internal
#' @examples
#' nif_spaghetti_plot(examplinib_fe_nif)
#' nif_spaghetti_plot(examplinib_sad_nif)
#' nif_spaghetti_plot(examplinib_poc_nif, analyte="RS2023")
#' nif_spaghetti_plot(examplinib_poc_nif, analyte="RS2023", tad = TRUE,
#'   dose = 500, log = TRUE, points = TRUE, lines = FALSE)
#' nif_spaghetti_plot(examplinib_sad_min_nif)
#' nif_spaghetti_plot(examplinib_poc_min_nif)
nif_spaghetti_plot <- function(obj,
                               points = FALSE, lines = TRUE, group = "ANALYTE",
                               nominal_time = FALSE, tad = FALSE, dose = NULL,
                               analyte = NULL, min_x = NULL, max_x = NULL,
                               log = FALSE, point_size = 2) {

  # if (!is.null(dose)) {
  #   obj <- obj %>%
  #     dplyr::filter(DOSE %in% dose)}
  #
  if(is.null(max_x)) {
    max_x <- max_observation_time(obj)}

  if(is.null(min_x)) {
    min_x = 0
  }
  #
  # x <- obj %>%
  #   index_dosing_interval() %>%
  #   as.data.frame() %>%
  #   verify(has_all_names("ID", "TIME", "NTIME", "AMT", "DV", "ANALYTE")) %>%
  #   dplyr::filter(!is.na(DOSE)) %>%
  #   filter(EVID==1 | !is.na(DV))
  #
  # if (!is.null(analyte)) {
  #   x <- x %>% dplyr::filter(ANALYTE == analyte)
  # }

  x <- obj %>%
    ensure_parent() %>%
    ensure_analyte() %>%
    ensure_dose() %>%
    index_dosing_interval() %>%
    as.data.frame() %>%
    verify(has_all_names(
      "ID", "TIME", "AMT", "DV", "EVID")) %>%
    {if(!is.null(dose)) filter(., .$DOSE %in% dose) else .} %>%
    # {if(!is.null(cmt)) filter(., .$CMT == cmt) else .} %>%
    {if(!is.null(analyte)) filter(., .$ANALYTE %in% analyte) else .}






  if (tad == TRUE) {
    x <- x %>%
      verify(has_all_names("TAD")) %>%
      filter(EVID == 0) %>%
      mutate(TIME = TAD) %>%
      mutate(GROUP = interaction(
        as.factor(ID), as.factor(ANALYTE), as.factor(DI))) %>%
      mutate(COLOR = interaction(as.factor(ANALYTE), as.factor(DI)))
    x_label <- "TAD (h)"
  } else {

    # create mock administrations for all analytes, including metabolites
    # to avoid connecting plot lines across administrations
    mock_admin_for_metabolites <- x %>%
      as.data.frame() %>%
      filter(EVID==1) %>%
      crossing(ANALYTE1=analytes(x)) %>%
      mutate(ANALYTE=ANALYTE1) %>%
      select(-ANALYTE1) %>%
      filter(TIME <= max_x)

    x <- x %>%
      filter(TIME <= max_x) %>%
      filter(EVID == 0) %>%
      filter(!is.na(DOSE)) %>%
      rbind(mock_admin_for_metabolites) %>%
      mutate(GROUP = interaction(
        as.factor(ID), as.factor(.data[[group]]))) %>%
      mutate(COLOR = as.factor(.data[[group]]))
    x_label <- "TIME (h)"
  }

  if (nominal_time) {
    if(!"NTIME" %in% names(x)) {
      stop("NTIME not included in NIF data set.")
    } else {
      p <- x %>%
        ggplot2::ggplot(ggplot2::aes(
          x = NTIME,
          y = DV,
          group = interaction(USUBJID, ANALYTE, as.factor(.data[[group]])),
          color = as.factor(.data[[group]])
        ))
    }
  } else {
    p <- x %>%
      ggplot2::ggplot(ggplot2::aes(
        x = TIME,
        y = DV,
        group = GROUP,
        color = COLOR
      ))
  }

  p +
    {if (lines) geom_line(na.rm = TRUE)} +
    {if (points) geom_point(na.rm = TRUE, size = point_size)} +
    {if (length(unique(x$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
    {if (log == TRUE) scale_y_log10()} +
    xlim(min_x, max_x) +
    labs(color = group) +
    ggplot2::theme_bw() +
    labs(x = x_label) +
    ggplot2::theme(legend.position = "bottom")
}


#' Spaghetti plot over a selected covariate
#'
#' @param obj The NIF object.
#' @param points Boolean to indicate whether points should be drawn.
#' @param lines Boolean to indicate whether lines should be drawn.
#' @param group The grouping covariate, defaults to 'ANALYTE'.
#' @param nominal_time Boolean to indicate whether the x-axis should be NTIME
#'   rather than TIME.
#' @param tad Boolean to select whether time after dose (TAD) rather than TIME
#'   should be plotted.
#' @param analyte The analyte as character. If NULL (default), all analytes will
#'   be plotted.
#' @param dose The dose(s) to filter for as numeric.
#' @param min_x The minimal value for the x scale as numeric.
#' @param max_x The maximal value for the x scale as numeric.
#' @param log Boolean to define whether y axis is plotted on the log scale.
#' @param point_size the point size as numeric.
#' @return A ggplot2 plot object.
#' @import assertr
#' @export
#' @keywords internal
#' @examples
#' nif_spaghetti_plot(examplinib_fe_nif)
#' nif_spaghetti_plot(examplinib_sad_nif)
#' nif_spaghetti_plot(examplinib_poc_nif, analyte="RS2023")
#' nif_spaghetti_plot(examplinib_poc_nif, analyte="RS2023", tad = TRUE,
#'   dose = 500, log = TRUE, points = TRUE, lines = FALSE)
nif_spaghetti_plot1 <- function(obj,
                               points = FALSE, lines = TRUE, group = "ANALYTE",
                               nominal_time = FALSE, tad = FALSE, dose = NULL,
                               analyte = NULL, min_x = NULL, max_x = NULL,
                               log = FALSE, point_size = 2) {

  # if (!is.null(dose)) {
  #   obj <- obj %>%
  #     dplyr::filter(DOSE %in% dose)}

  # if(is.null(max_x)) {
  #   max_x <- max_observation_time(obj)}

  # if(is.null(min_x)) {
  #   min_x = 0
  # }

  x <- obj %>%
    # index_dosing_interval() %>%
    as.data.frame() %>%
    # verify(has_all_names("ID", "TIME", "NTIME", "AMT", "DV", "ANALYTE")) %>%
    # dplyr::filter(!is.na(DOSE)) %>%
    filter(EVID==1 | !is.na(DV))

  if (!is.null(analyte)) {
    x <- x %>% dplyr::filter(ANALYTE == analyte)
  }

  if (tad == TRUE) {
    x <- x %>%
      verify(has_all_names("TAD")) %>%
      filter(EVID == 0) %>%
      mutate(TIME = TAD) %>%
      mutate(GROUP = interaction(
        as.factor(ID), as.factor(ANALYTE), as.factor(DI))) %>%
      mutate(COLOR = interaction(as.factor(ANALYTE), as.factor(DI)))
    x_label <- "TAD (h)"
  } else {

    # create mock administrations for all analytes, including metabolites
    # to avoid connecting plot lines across administrations
    mock_admin_for_metabolites <- x %>%
      as.data.frame() %>%
      filter(EVID==1) %>%
      crossing(ANALYTE1=analytes(x)) %>%
      mutate(ANALYTE=ANALYTE1) %>%
      select(-ANALYTE1) %>%
      filter(TIME <= max_x)

    x <- x %>%
      filter(TIME <= max_x) %>%
      filter(EVID == 0) %>%
      rbind(mock_admin_for_metabolites) %>%
      mutate(GROUP = interaction(
        as.factor(ID), as.factor(.data[[group]]))) %>%
      mutate(COLOR = as.factor(.data[[group]]))
    x_label <- "TIME (h)"
  }

  if (nominal_time) {
    p <- x %>%
      ggplot2::ggplot(ggplot2::aes(
        x = NTIME,
        y = DV,
        group = interaction(USUBJID, ANALYTE, as.factor(.data[[group]])),
        color = as.factor(.data[[group]])
      ))
  } else {
    p <- x %>%
      ggplot2::ggplot(ggplot2::aes(
        x = TIME,
        y = DV,
        group = GROUP,
        color = COLOR
      ))
  }

  p +
    {if (lines) geom_line(na.rm = TRUE)} +
    {if (points) geom_point(na.rm = TRUE, size = point_size)} +
    {if (length(unique(x$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
    {if (log == TRUE) scale_y_log10()} +
    xlim(min_x, max_x) +
    labs(color = group) +
    ggplot2::theme_bw() +
    labs(x = x_label) +
    ggplot2::theme(legend.position = "bottom")
}



#' Plot NIF object
#'
#' This function plots a NIF object, grouped by the variable `group`. If no
#' grouping variable is provided, `DOSE` will be used.
#'
#' @param x The NIF object to be plotted.
#' @param y_scale Type of y-axis scale. Can be 'log' or 'lin'. Default is "lin".
#' @param max_x Maximal x (time) scale value.
#' @param analyte The analyte to be plotted If this argument is not supplied
#'   (or set to NULL), all analytes are shown.
#' @param mean Boolean value to indicate whether the mean value by dose and
#'   analyte is to be plotted. In that case, the nominal time (NTIME) is shown
#'   on the x axis. 'mean=T' will cast an error if multiple analytes are in the
#'   data set of have been defined with 'analyte'.
#' @param dose The doses to be plotted. Can be a scalar value or a numeric
#'   vector or NULL to plot all doses.
#' @param points Boolean value to define whether points should be plotted.
#' @param id Numerical scalar or vector of IDs to be plotted.
#' @param usubjid Character scalar or vector of USUBJIDs to be plotted.
#' @param group Character scalar to define a grouping variable. If specified,
#'   this will cast an error if multiple analytes are in the data set or have
#'   been defined by 'analyte'.
#' @param administrations Boolean value to indicate whether vertical lines
#'   marking the administration times should be plotted. Has no function if
#'   mean=F.
#' @param nominal_time Boolean to indicate whether NTIME rather than TIME should
#'   be plotted on the x-axis.
#' @param title The plot title as string.
#' @param ... Further arguments.
#' @param min_x Minimum x (time) scale value.
#' @param lines Boolean to define whether lines are to be drawn.
#' @param alpha Numerical value between 0 and 1, passed on as alpha to ggplot.
#' @param tad Boolean to select wheter time after dose (TAD) rather than TIME
#'   should be plotted.
#' @param log Boolean to select whether the y-scale is logarithmic. Alternative
#' @param cmt The compartment to be plotted as numeric.
#' `y_scale="log"`.
#' @return A ggplot2 object.
#' @seealso [nif_viewer()]
#' @examples
#' plot(examplinib_fe_nif)
#' plot(examplinib_fe_nif, analyte = "RS2023", points = TRUE)
#' plot(examplinib_fe_nif, analyte = "RS2023", group = "FASTED")
#' plot(examplinib_fe_nif, max_x = 24, point = TRUE)
#' plot(examplinib_fe_nif,
#'   analyte = "RS2023", group = "FASTED",
#'   nominal_time = TRUE, points = TRUE
#' )
#' plot(examplinib_fe_nif, analyte = "RS2023", group = "FASTED", mean = TRUE)
#' plot(examplinib_fe_nif, analyte = "RS2023", group = "FASTED", mean = TRUE,
#'   max_x = 24)
#' plot(examplinib_poc_nif, dose = 500, analyte = "RS2023", points = TRUE,
#'   lines = FALSE, tad = TRUE, log =TRUE)
#' plot(examplinib_poc_min_nif, dose=500, tad = TRUE, cmt = 2, max_x = 24,
#'   points = TRUE, lines = FALSE)
#' @export
plot.nif <- function(x, y_scale = "lin", log = FALSE, min_x = NULL,
                     max_x = NULL, analyte = NULL, cmt = NULL, mean = FALSE,
                     dose = NULL, points = FALSE, id = NULL, usubjid = NULL,
                     group = NULL, administrations = FALSE,
                     nominal_time = FALSE, tad = FALSE, lines = TRUE,
                     alpha = 1, title = NULL, ...) {

x <- x %>%
  ensure_parent() %>%
  ensure_analyte() %>%
  ensure_dose() %>%
  add_tad() %>%
  # index_dosing_interval() %>%
  as.data.frame() %>%
  verify(has_all_names(
    "ID", "TIME", "AMT", "DV", "EVID")) %>%
  # {if(!is.null(dose)) filter(., .$DOSE %in% dose) else .} %>%
  # {if(!is.null(analyte)) filter(., .$ANALYTE %in% analyte) else .} %>%
  {if(!is.null(id)) filter(., .$ID %in% id) else .} %>%
  {if(!is.null(usubjid)) filter(., .$USUBJID %in% usubjid) else .} %>%
  {if(!is.null(cmt)) filter(., .$CMT %in% cmt | .$EVID==1) else .}

  # if (!is.null(id)) {
  #   x <- x %>%
  #     dplyr::filter(ID %in% id)
  # }

  # if (!is.null(usubjid)) {
  #   x <- x %>%
  #     dplyr::filter(USUBJID %in% usubjid)
  # }

  # if (!is.null(analyte)) {
  #   x <- x %>%
  #     dplyr::filter(ANALYTE == analyte)
  # }

  # if (!is.null(doses)) {
  #   x <- x %>%
  #     dplyr::filter(DOSE %in% doses)
  # }

  if (!is.null(group)) {
    cov <- group
    if (is.null(analyte) || length(analyte) > 1) {
      stop(paste0(
        "Plotting multiple analytes in the same graph does not make ",
        "sense. Consider selecting a (single) analyte!"
      ))
    }
  } else {
    cov <- "ANALYTE"
  }

  # x <- x %>%
  #   ensure_analyte() %>%
  #   ensure_dose()

  if (mean == TRUE) {
    if (!is.null(group) && is.null(analyte) && length(analytes(x) > 1)) {
      stop(paste0(
        "Plotting means over multiple analytes does not make sense! ",
        "Consider selecting a specific analyte"
      ))
    }
    p <- x %>%
      # assert_rows("NTIME") %>%
      nif_mean_plot(points = points, lines = lines, group = cov, min_x = min_x,
                    max_x = max_x, log = log, dose = dose, analyte = analyte)
  } else {
    p <- x %>%
      nif_spaghetti_plot(
        points = points, lines = lines, group = cov, tad = tad,
        nominal_time = nominal_time, dose = dose, analyte = analyte,
        min_x = min_x, max_x = max_x, log = log)
  }

  p +
    # xlim(min_x, max_x) +
    {if (!is.null(title)) ggtitle(title)} +
    # {if (y_scale == "log" || log == TRUE) scale_y_log10()} +
    {if (length(analyte) == 1) labs(y = analyte)} +
    # {if (tad == FALSE) labs(color = cov)} +
    labs(color = cov) +
    {if (tad == FALSE &&
        x %>%
          pull(cov) %>%
          unique() %>%
          length() < 2) {
        theme(legend.position = "none")}}
}


#' NIF or SDTM object overview
#'
#' @param object The NIF or SDTM object.
#' @param ... Further arguments.
#' @return A summary_nif or summary_sdtm object.
#' @export
#' @examples
#' summary(examplinib_poc)
#' summary(examplinib_poc_nif)
#' summary(examplinib_poc_min_nif)
summary <- function(object, ...) {
  UseMethod("summary")
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
    summarize(N = n_distinct(ID))

  n_sex <- object %>%
    as.data.frame() %>%
    {if(!"SEX" %in% names(.)) mutate(., SEX = NA) else .} %>%
    mutate(SEX = factor(SEX, levels=c(0, 1))) %>%
    distinct(ID, SEX) %>%
    reframe(n = n(), .by = factor("SEX")) %>%
    spread(SEX, n, fill = NA, drop = FALSE)
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
    administration_duration = administration_summary(object)
  )
  class(out) <- "summary_nif"
  return(out)
}


#' Print NIF summary
#'
#' @param x A summary_nif object.
#' @param ... Further parameters.
#' @noRd
#' @return Nothing.
#' @export
print.summary_nif <- function(x, ...) {
  color = TRUE
  indent = " "
  # hline <- paste0(rep("\U2500", 8), collapse="")
  hline <- paste0(rep("-", 8), collapse="")
  cat(paste0(hline, " NONMEM input file (NIF) object summary ", hline, "\n"))

  cat(paste(
    "Data from", sum(x$n_studies$N), "subjects across "))
  if(length(x$studies == 1)) {
    cat("one study:\n")
  } else {
    cat(paste0(length(x$studies), "studies:\n"))
  }

  cat(paste0(df_to_string(x$n_studies, color=color, indent = indent), "\n\n"))

  cat(paste0(
    "Males: ", x$n_males, ", females: ", x$n_females, " (",
    round(x$n_females / (x$n_males + x$n_females) * 100, 1), "%)\n\n"
  ))

  if (!is.null(x$renal_function)) {
    cat(paste0("Renal function:\n", df_to_string(
      x$renal_function, color=color, indent = indent), "\n\n"))
  }

  cat(paste0("Analytes:\n", paste0(indent, paste0(x$analytes, collapse = ", ")), "\n\n"))

  cat(paste(sum(x$n_obs$N), "observations:\n"))
  cat(paste0(df_to_string(x$n_obs, color=color, indent = indent), "\n\n"))

  cat(paste0("Administered drugs:\n", paste0(indent, paste(x$drugs, collapse = ", ")), "\n\n"))

  cat("Dose levels:\n")
  cat(df_to_string(x$dose_levels, color=color, indent = indent))
  cat("\n\n")

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
#' @param ... Further arguments.
#' @return A list of ggplot objects.
#' @export
#' @noRd
#' @examples
#' plot(summary(examplinib_poc_nif))
plot.summary_nif <- function(x, ...) {
  nif <- x$nif
  out <- list()

  for (i in c("AGE", "WEIGHT", "HEIGHT", "BMI", "BL_CRCL")) {
    if (i %in% colnames(nif)) {
      out[[i]] <- covariate_hist(nif, i)
    }
  }

  out[["WT_SEX"]] <- wt_by_sex(nif)
  out[["WT_RACE"]] <- wt_by_race(nif)

  for (i in x$analytes) {
    out[[i]] <- plot(nif,
      analyte = i, y_scale = "log", points = TRUE, line = FALSE,
      alpha = 0.3, title = paste(i, "overview by dose"),
      max_x = max_observation_time(x$nif, i)
    )
  }
  return(out)
}


#' Generic covariate distribution histogram
#'
#' @param obj The NIF object.
#' @param nbins The number of bins to be used if no binwidth is specified.
#' Defaults to 11.
#' @param field The field of the NIF object as character.
#' @return A plot object.
#' @export
#' @examples
#' covariate_hist(examplinib_sad_nif, "AGE")
#' covariate_hist(examplinib_sad_nif, "BL_CRCL")
covariate_hist <- function(obj, field, nbins = 11) {
  cov_params <- get_cov_plot_params(field)
  xlabel <- cov_params$xlabel
  if (is.na(xlabel)) {
    xlabel <- cov_params$field
  }
  title <- cov_params$title
  if (is.na(title)) {
    title <- paste(cov_params$field, "distribution")
  }
  limits <- unlist(cov_params$limits)

  temp <- obj %>%
    as.data.frame() %>%
    distinct(ID, .data[[cov_params$field]])

  if (is.na(cov_params$binwidth)) {
    p <- temp %>%
      ggplot(aes(x = .data[[cov_params$field]])) +
      geom_histogram(bins = nbins, fill = "grey")
  } else {
    p <- temp %>%
      ggplot(aes(x = .data[[cov_params$field]])) +
      geom_histogram(binwidth = cov_params$binwidth, fill = "grey")
  }
  p +
    geom_vline(xintercept = limits, color = "red") +
    theme_bw() +
    labs(x = xlabel, y = "number of subjects") +
    ggtitle(title)
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
    ggplot(aes(x = SEX, y = bl_wt, group = SEX)) +
    scale_x_continuous(breaks = c(0, 1)) +
    geom_boxplot(width = 0.5) +
    geom_label(aes(label = paste0("N=", count), y = maxwt + 5),
      label.size = 0, position = position_dodge(width = 0.75)
    ) +
    labs(x = "sex", y = "baseline weight (kg)") +
    theme_bw() +
    ggtitle("Body weight by sex")
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
    ggplot(aes(x = rc, y = bl_wt, group = rc)) +
    geom_boxplot(width = 0.5) +
    geom_label(aes(label = paste0("N=", count), y = maxwt + 5),
      label.size = 0
    ) +
    labs(x = "", y = "baseline weight (kg)") +
    theme_bw() +
    ggtitle("Body weight by race")
}


#' Weight by height scatterplot
#'
#' @param obj The NIF object.
#'
#' @return A plot object.
#' @export
#' @examples
#' wt_by_ht(examplinib_poc_nif)
wt_by_ht <- function(obj) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, HEIGHT, WEIGHT) %>%
    ggplot(aes(x = HEIGHT, y = WEIGHT)) +
    geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
    geom_point(size = 3) +
    labs(x = "height (cm)", y = "baseline weight (kg)") +
    ggtitle("Body weight by height") +
    theme_bw()
}


#' Weight by height scatterplot
#'
#' @param obj The NIF object.
#' @return A plot object.
#' @export
#' @examples
#' ht_by_wt(examplinib_poc_nif)
ht_by_wt <- function(obj) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, HEIGHT, WEIGHT) %>%
    ggplot(aes(x = WEIGHT, y = HEIGHT)) +
    geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
    geom_point(size = 3) +
    labs(y = "height (cm)", x = "baseline weight (kg)") +
    ggtitle("Body height by weight") +
    theme_bw()
}


#' BMI by age scatterplot
#'
#' @param obj The NIF object.
#'
#' @return A plot object.
#' @export
#' @examples
#' bmi_by_age(examplinib_poc_nif)
bmi_by_age <- function(obj) {
  obj %>%
    as.data.frame() %>%
    distinct(ID, AGE, BMI) %>%
    ggplot(aes(x = AGE, y = BMI)) +
    geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
    geom_point(size = 3) +
    labs(y = "BMI (kg/m^2)", x = "age (y)") +
    ggtitle("Body mass index by age") +
    theme_bw() +
    theme(legend.position = "none")
}


#' Overview on the number of administrations in the subjects by parent
#'
#' @param obj A NIF object.
#' @return A data frame.
#' @importFrom stats median
#' @export
#' @examples
#' administration_summary(examplinib_poc_nif)
administration_summary <- function(obj) {
  obj %>%
    n_administrations() %>%
    filter(PARENT != "") %>%
    group_by(across(any_of(c("PARENT")))) %>%
    summarize(
      min = min(N, na.rm = TRUE), max = max(N, na.rm = TRUE),
      mean = round(mean(N, na.rm = TRUE), 1),
      median = stats::median(N, na.rm = TRUE)
    ) %>%
    as.data.frame()
}


#' Mean dose plot
#'
#' This function plots the mean dose per day over time
#' @param obj A NIF object.
#' @param analyte The compound as character (i.e., the ANALYTE within the data
#'   set).
#' @return A ggplot object.
#' @export
#' @keywords internal
#' @examples
#' mean_dose_plot(examplinib_poc_nif)
mean_dose_plot <- function(obj, analyte = NULL) {
  if (is.null(analyte)) {
    analyte <- guess_analyte(obj)
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
    pivot_longer(cols = -DAY, names_to = "PARAM", values_to = "VAL") %>%
    ggplot(aes(x = DAY, y = VAL)) +
    geom_line() +
    facet_grid(PARAM ~ ., scales = "free_y") +
    labs(x = "time (days)", y = "") +
    theme_bw() +
    ggtitle(paste0("Mean ", analyte, " dose over time"))
}
