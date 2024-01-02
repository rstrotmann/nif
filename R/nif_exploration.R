#' Plot DV time course data from individual subject
#'
#' This function plots DV over TIME for an individual subject, id. Id can be
#'   either the ID or the USUBJID. Administration time points are indicated with
#'   vertical lines.
#'
#' @param nif The NIF data set
#' @param id The subject ID to be plotted
#' @param y_scale Y-scale. Use 'scale="log"' for a logarithmic y scale. Default
#'   is "lin".
#' @param analyte The analytes to be displayes. Defaults to NULL (all).
#' @param imp The IMP for which administrations are to be indicated by vertical
#'   lines. Defaults to 'none'.
#' @param max_time The right limit of the time scale
#' @param tad Boolean to select whether time after dose (TAD) rather than TIME
#'   should be plotted.
#' @param lines Boolean to define whether lines should be plotted.
#' @param point_size Point size as numeric.
#'
#' @return the plot object
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' nif_plot_id(examplinib_poc_nif, 1)
#' nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte="RS2023", imp="RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte=c("RS2023", "RS2023487A"))
nif_plot_id <- function(nif, id, analyte = NULL, y_scale = "lin",
                        max_time = NA, lines = TRUE, point_size = 2,
                        tad = FALSE, imp = "none") {
  x <- nif %>%
    as.data.frame() %>%
    verify(has_all_names(
      "ID", "USUBJID", "TIME", "NTIME", "AMT", "DV", "ANALYTE",
      "EVID"
    )) #%>%
    # filter(EVID == 0)

  # if not specified, show all analytes
  if (is.null(analyte)) {
    analyte <- x %>%
      distinct(ANALYTE) %>%
      pull(ANALYTE)
  }

  # filter for subject of interest
  if (id %in% x$ID) {
    plot_label <- "ID"
    x <- x %>%
      filter(ID == id)
    id_label <- ""
  } else {
    if (id %in% x$USUBJID) {
      x <- x %>%
        filter(USUBJID == id)
      id_label <- paste0(" (ID ", x %>% distinct(ID) %>% pull(ID), ")")
      plot_label <- "USUBJID"
    } else {
      stop(paste(id, "is not an ID or USUBJID contained in the NIF data set"))
    }
  }

  if (tad == TRUE) {
    if (!"TAD" %in% colnames(x)) {
      x <- x %>% add_tad()
    }
    x <- x %>%
      # add_trtdy() %>%
      mutate(TIME = TAD)
  }

  obs <- x %>%
    as.data.frame() %>%
    filter(ANALYTE %in% analyte) %>%
    filter(EVID == 0)

  admin <- x %>%
    # as.data.frame() %>%
    dplyr::filter(EVID == 1) %>%
    dplyr::filter(PARENT == imp)

  if (tad == TRUE) {
    p <- obs %>%
      ggplot2::ggplot(ggplot2::aes(
        x = TIME, y = DV,
        group = interaction(ID, as.factor(ANALYTE), TRTDY),
        color = interaction(as.factor(ANALYTE), TRTDY)
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
    ggplot2::geom_vline(
      data = admin,
      ggplot2::aes(xintercept = TIME),
      color = "gray"
    ) +
    { if (lines == TRUE) ggplot2::geom_line() } +
    ggplot2::geom_point(size = point_size) +
    ggplot2::xlim(0, max_time) +
    ggplot2::labs(
      title = paste0(plot_label, ": ", id, id_label),
      color = "analyte"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if (!(imp %in% c("NA", "", "none"))) {
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
#'   either the ID or the USUBJID. Administration time points are indicated with
#'   vertical lines.
#'
#' @param nif The NIF data set.
#' @param id The subject ID to be plotted.
#' @param y_scale Y-scale. Use 'scale="log"' for a logarithmic y scale. Default
#'   is "lin".
#' @param max_dose The upper limit of the dose scale.
#' @param analyte The analyte of interest.
#' @param max_time The right limit of the time scale.
#' @param point_size The point size as numeric.
#'
#' @return A ggplot object.
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' dose_plot_id(examplinib_poc_nif, 18)
#' dose_plot_id(examplinib_poc_nif, dose_red_sbs(examplinib_poc_nif)[[4]])
dose_plot_id <- function(nif, id, y_scale = "lin", max_dose = NA,
                         point_size = 2, max_time = NA, analyte = NULL) {
  if (id %in% nif$ID) {
    plot_label <- "ID"
    nif <- nif %>% filter(ID == id)
  } else if (id %in% nif$USUBJID) {
    nif <- nif %>% filter(USUBJID == id)
    plot_label <- "USUBJID"
  } else {
    stop(paste(id, "is not an ID or USUBJID contained in the NIF data set"))
  }

  if (!is.null(analyte)) {
    nif <- nif %>%
      filter(ANALYTE %in% analyte)
  }

  admin <- nif %>%
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
#' @param x The NIF data set.
#' @param points Boolean to indicate whether points should be drawn.
#' @param lines Boolean to indicate whether lines should be drawn.
#' @param group The grouping covariate, defaults to 'ANALYTE'.
#'
#' @return A ggplot2 plot object.
#' @export
#'
#' @examples
#' nif_mean_plot(examplinib_sad_nif)
#' nif_mean_plot(examplinib_fe_nif, group = "FASTED")
nif_mean_plot <- function(x, points = FALSE, lines = TRUE, group = "ANALYTE") {
  temp <- x %>%
    as.data.frame() %>%
    verify(has_all_names("NTIME", "DOSE", "DV")) %>%
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
    {if (lines) geom_line()} +
    {if (points) geom_point()} +
    ggplot2::geom_ribbon(
      aes(
        ymin = mean - sd, ymax = mean + sd,
        fill = as.factor(.data[[group]])
      ),
      alpha = 0.3, color = NA, show.legend = FALSE
    ) +
    {if (length(unique(temp$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
    ggplot2::theme_bw() +
    ggplot2::labs(caption = "Data shown are mean \u00B1 SD") +
    labs(color = group) +
    ylim(0, max(temp$max_y, na.rm = TRUE)) +
    ggplot2::theme(legend.position = "bottom")
}


#' Spaghetti plot over a selected covariate
#'
#' @param obj The NIF data set.
#' @param points Boolean to indicate whether points should be drawn.
#' @param lines Boolean to indicate whether lines should be drawn.
#' @param group The grouping covariate, defaults to 'ANALYTE'.
#' @param nominal_time Boolean to indicate whether the x-axis should be NTIME
#'   rather than TIME.
#' @param tad Boolean to select whether time after dose (TAD) rather than TIME
#'   should be plotted.
#' @param analyte The analyte as character. If NULL (default), all analytes will
#'   be plotted.
#'
#' @return A ggplot2 plot object.
#' @import assertr
#' @export
#'
#' @examples
#' nif_spaghetti_plot(examplinib_fe_nif)
#' nif_spaghetti_plot(examplinib_sad_nif)
nif_spaghetti_plot <- function(obj,
                               points = FALSE, lines = TRUE, group = "ANALYTE",
                               nominal_time = FALSE, tad = FALSE,
                               analyte = NULL) {

  x <- obj %>%
    as.data.frame() %>%
    verify(has_all_names("ID", "TIME", "NTIME", "AMT", "DV", "ANALYTE")) %>%
    dplyr::filter(!is.na(DOSE)) %>%
    filter(EVID == 0) %>%
    filter(!is.na(DV))

  if (!is.null(analyte)) {
    x <- x %>% dplyr::filter(ANALYTE == analyte)
  }

  if (tad == TRUE) {
    x <- x %>%
      verify(has_all_names("TAD")) %>%
      filter(EVID == 0) %>%
      mutate(TIME = TAD) %>%
      mutate(GROUP = interaction(as.factor(ID),
                                 as.factor(ANALYTE),
                                 as.factor(TRTDY))) %>%

      mutate(COLOR = interaction(as.factor(ANALYTE),
                                 as.factor(TRTDY)))
    x_label <- "TAD (h)"
  } else {
    x <- x %>%
      filter(EVID == 0) %>%
      # mutate(TIME = TAD) %>%
      # mutate(GROUP = interaction(as.factor(ID),
      #                            # as.factor(ANALYTE),
      #                            as.factor(.data[[group]]),
      #                            TRTDY)) %>%
      mutate(GROUP = interaction(as.factor(ID),
                                 # as.factor(ANALYTE),
                                 as.factor(.data[[group]]))) %>%
      mutate(COLOR = as.factor(.data[[group]]))
    x_label <- "TIME (h)"
  }

  # x <- x

  if (nominal_time) {
    p <- x %>%
      ggplot2::ggplot(ggplot2::aes(
        x = NTIME, y = DV,
        group = interaction(USUBJID, ANALYTE, as.factor(.data[[group]])),
        color = as.factor(.data[[group]])
      ))
  } else {
    p <- x %>%
      ggplot2::ggplot(ggplot2::aes(
        x = TIME, y = DV,
        group = GROUP,
        color = COLOR
      ))
  }

  p +
    {if (lines) geom_line()} +
    {if (points) geom_point()} +
    {if (length(unique(x$DOSE)) > 1) ggplot2::facet_wrap(~DOSE)} +
    labs(color = group) +
    ggplot2::theme_bw() +
    labs(x = x_label) +
    # xlim(0, 24) +
    # scale_y_log10() +
    ggplot2::theme(legend.position = "bottom")
}


#' Plot NIF data set
#'
#' This function plots a NIF data set, grouped by the variable `group`. If no
#'   grouping variable is provided, `DOSE` will be used.
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
#' @param doses The doses to be plotted. Can be a scalar value or a numeric
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
#'
#' @return The plot object
#' @seealso [nif_viewer()]
#' @examples
#' plot(examplinib_fe_nif)
#' plot(examplinib_fe_nif, analyte = "RS2023", points = TRUE)
#' plot(examplinib_fe_nif, analyte = "RS2023", group = "FASTED")
#' plot(examplinib_fe_nif, max_x = 24, point = TRUE)
#' plot(examplinib_fe_nif,
#'   analyte = "RS2023", group = "FASTED",
#'   nominal_time = TRUE
#' )
#' plot(examplinib_fe_nif, analyte = "RS2023", group = "FASTED", mean = TRUE)
#' plot(examplinib_fe_nif,
#'   analyte = "RS2023", group = "FASTED", mean = TRUE,
#'   max_x = 24
#' )
#'
#' @export
plot.nif <- function(x, y_scale = "lin", min_x = 0, max_x = NA, analyte = NULL,
                     mean = FALSE, doses = NULL, points = FALSE, id = NULL,
                     usubjid = NULL, group = NULL, administrations = FALSE,
                     nominal_time = FALSE, tad = FALSE, lines = TRUE, alpha = 1,
                     title = NULL, ...) {
  if (!is.null(id)) {
    x <- x %>%
      dplyr::filter(ID %in% id)
  }

  if (!is.null(usubjid)) {
    x <- x %>%
      dplyr::filter(USUBJID %in% usubjid)
  }

  if (!is.null(analyte)) {
    x <- x %>%
      dplyr::filter(ANALYTE == analyte)
  }

  if (!is.null(doses)) {
    x <- x %>%
      dplyr::filter(DOSE %in% doses)
  }

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

  if (mean == TRUE) {
    if (!is.null(group) && is.null(analyte) && length(analytes(x) > 1)) {
      stop(paste0(
        "Plotting means over multiple analytes does not make sense! ",
        "Consider selecting a specific analyte"
      ))
    }
    p <- x %>%
      nif_mean_plot(points = points, lines = lines, group = cov)
  } else {
    p <- x %>%
      nif_spaghetti_plot(
        points = points, lines = lines, group = cov, tad = tad,
        nominal_time = nominal_time
      )
  }

  p +
    xlim(min_x, max_x) +
    {if (!is.null(title)) ggtitle(title)} +
    {if (y_scale == "log") scale_y_log10()} +
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


#' NIF data set overview
#'
#' @param object The NIF data set.
#' @param ... Further arguments.
#' @param egfr_function The function to be used for estimation of the renal
#' function classes, see [add_bl_crcl()] for reference.
#'
#' @return A summary_nif object.
#' @export
#' @examples
#' summary(examplinib_poc_nif)
#'
summary.nif <- function(object, egfr_function = egfr_cg, ...) {
  subjects <- subjects(object)
  analytes <- analytes(object)
  parents <- object %>%
    as.data.frame() %>%
    distinct(PARENT) %>%
    filter(PARENT != "") %>%
    pull(PARENT)
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
    group_by(ANALYTE) %>%
    summarize(N = n(), .groups = "drop") %>%
    as.data.frame()

  n_studies <- object %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    group_by(STUDYID) %>%
    summarize(N = n_distinct(USUBJID))

  n_sex <- object %>%
    dplyr::distinct(USUBJID, SEX) %>%
    dplyr::group_by(SEX) %>%
    dplyr::summarize(n = n())

  n_males <- n_sex %>%
    dplyr::filter(SEX == 0) %>%
    dplyr::pull(n)
  if (length(n_males) == 0) {
    n_males <- 0
  }

  n_females <- n_sex %>%
    dplyr::filter(SEX == 1) %>%
    dplyr::pull(n)
  if (length(n_females) == 0) {
    n_females <- 0
  }

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
      grouping = any_of(c("PART", "COHORT", "GROUP"))
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
#'
#' @return Nothing.
#' @export
print.summary_nif <- function(x, ...) {
  cat(paste("NONMEM input file (NIF) data set summary\n\n"))

  cat(paste(
    "Data from", sum(x$n_studies$N), "subjects across",
    length(x$studies)
  ), "studies:\n")
  cat(paste0(df_to_string(x$n_studies), "\n\n"))

  cat(paste0(
    "Males: ", x$n_males, ", females: ", x$n_females, " (",
    round(x$n_females / (x$n_males + x$n_females) * 100, 1), "%)\n\n"
  ))

  if (!is.null(x$renal_function)) {
    cat(paste0("Renal function:\n", df_to_string(x$renal_function), "\n\n"))
  }

  cat(paste0("Analytes:\n", paste(x$analytes, collapse = ", "), "\n\n"))

  cat(paste(sum(x$n_obs$N), "observations:\n"))
  cat(paste0(df_to_string(x$n_obs), "\n\n"))

  cat(paste0("Administered drugs:\n", paste(x$drugs, collapse = ", "), "\n\n"))

  cat("Dose levels:\n")
  cat(df_to_string(x$dose_levels))
  cat("\n\n")

  dr_summary <- lapply(x$dose_red_sbs, length) %>%
    data.frame()
  cat("Subjects with dose reductions\n")
  cat(df_to_string((dr_summary)))
  cat("\n\n")

  cat("Treatment duration overview:\n")
  cat(df_to_string(x$administration_duration))
  invisible(x)
}


#' Get covariate plot parameters
#'
#' @param field The field of the NIF data set
#'
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
#' @param x A NIF data set.
#' @param ... Further arguments.
#' @return A list of ggplot objects.
#' @export
#' @examples
#' plot(summary(examplinib_poc_nif))
#'
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
#' @param obj The NIF data set.
#' @param nbins The number of bins to be used if no binwidth is specified.
#' Defaults to 11.
#' @param field The field of the NIF object as character.
#'
#' @return A plot object.
#' @export
#' @examples
#' covariate_hist(examplinib_sad_nif, "AGE")
#' covariate_hist(examplinib_sad_nif, "BL_CRCL")
#'
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
#' @examples
#' wt_by_sex(examplinib_poc_nif)
#'
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
#'
#' @return A plot object.
#' @export
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
#'
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
#' @param obj A NIF data set.
#'
#' @return A data frame.
#' @importFrom stats median
#' @export
#' @examples
#' administration_summary(examplinib_poc_nif)
#'
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
#'
#' @param obj A NIF data set.
#' @param analyte The compound as character (i.e., the ANALYTE within the data
#'   set).
#'
#' @return A ggplot object.
#' @export
#' @examples
#' mean_dose_plot(examplinib_poc_nif)
#'
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
