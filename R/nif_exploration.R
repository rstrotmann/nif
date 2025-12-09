#' Plot DV time course data from individual subject
#'
#' This function plots DV over TIME for an individual subject, id. Id can be
#' either the ID or the USUBJID. Administration time points are indicated with
#' vertical lines.
#'
#' @param obj The NIF object
#' @param id The subject ID to be plotted
#' @param analyte The analytes to be displayed. Defaults to NULL (all).
#' @param imp The IMP for which administrations are to be indicated by vertical
#'   lines. Defaults to NULL.
#' @param max_time The right limit of the time scale
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
nif_plot_id <- function(
    obj,
    id,
    analyte = NULL,
    cmt = NULL,
    time_field = "TIME",
    max_time = NA,
    lines = TRUE,
    point_size = 2,
    log = FALSE,
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

  # remove zeros or negatives for log plotting
  if(log == TRUE){
    obs <- filter(obs, DV > 0)
  }

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
#' dose_plot_id(examplinib_poc_nif, dose_red_sbs(examplinib_poc_nif)[1, 1])
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
      filter(ID == as.numeric(id))
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


#' NIF object overview
#'
#' @param object The NIF object.
#' @param sampling Include sampling summary.
#' @param ... Further arguments.
#'
#' @return A summary_nif object.
#' @export
#' @import tidyr
#' @noRd
#' @examples
#' summary(examplinib_poc_nif)
#' summary(examplinib_poc_min_nif)
#' summary(new_nif())
summary.nif <- function(
    object,
    sampling = TRUE,
    ...) {
  # input validation
  validate_min_nif(object)
  validate_logical_param(sampling, "sampling")

  # Validate data is not empty
  if (nrow(object) == 0) {
    out <- list(
      nif = object,
      studies = character(0),
      subjects = NULL,
      dose_red_sbs = NULL,
      n_studies = data.frame(STUDYID = character(0), N = integer(0)),
      n_subj = 0,
      sex = NULL,
      n_males = 0,
      n_females = 0,
      n_obs = NULL,
      analytes = NULL,
      n_analytes = 0,
      drugs = NULL,
      dose_levels = NULL,
      renal_function = NULL,
      odwg = NULL,
      administration_duration = NULL,
      sampling = NULL,
      hash = NULL,
      last = NULL
    )
    class(out) <- "summary_nif"
    return(out)
  }

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

  # Handle sex distribution with safety checks
  sex <- object %>%
    as.data.frame() %>%
    {if(!"SEX" %in% names(.)) mutate(., SEX = NA) else .} %>%
    distinct(ID, SEX) %>%
    reframe(N = n(), .by = "SEX") %>%
    complete(SEX = c(0, 1), fill = list(N = 0))

  n_males = as.numeric(sex[which(sex$SEX == 0), "N"])
  n_females = as.numeric(sex[which(sex$SEX == 1), "N"])

  # dose levels
  dl_groups <- intersect(c("PART", "COHORT", "GROUP"), names(object))
  dose_levels <- dose_levels(object, group = dl_groups)

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
      mutate(CLASS = factor(CLASS, levels = c("normal", "mild", "moderate", "severe"))) %>%
      reframe(
        N = n(),
        .by = CLASS) %>%
      tidyr::complete(CLASS, fill = list(N = 0)) %>%
      mutate(CLASS = as.character(CLASS))

  } else {
    renal_function <- NULL
  }

  if("BL_ODWG" %in% colnames(object)) {
    odwg <- object %>%
      as.data.frame() %>%
      mutate(CLASS = .data$BL_ODWG) %>%
      distinct(ID, CLASS) %>%
      mutate(CLASS = factor(CLASS, levels = c("normal", "mild", "moderate", "severe"))) %>%
      reframe(
        N = n(),
        .by = CLASS) %>%
      tidyr::complete(CLASS, fill = list(N = 0)) %>%
      mutate(CLASS = as.character(CLASS))
  } else {
    odwg = NULL
  }

  # sampling overview
  sampling_table <- NULL
  if("NTIME" %in% names(object) & sampling == TRUE)
    sampling_table <- sampling_summary(object)

  out <- list(
    nif = object,
    studies = studies(object),
    subjects = subjects,
    dose_red_sbs = dose_red_sbs,
    n_studies = n_studies,
    n_subj = nrow(subjects),
    sex = sex,
    n_males = n_males,
    n_females = n_females,
    n_obs = observations,
    analytes = analytes,
    n_analytes = length(analytes),
    drugs = parents,
    dose_levels = dose_levels,
    renal_function = renal_function,
    odwg = odwg,
    administration_duration = administration_summary(object),
    sampling = sampling_table,
    hash = hash(object),
    last = last_dtc(object)
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
print.summary_nif <- function(
    x,
    color = FALSE,
    ...) {
  # input validation
  validate_logical_param(color, "color")

  indent = 2
  spacer = paste(replicate(indent, " "), collapse = "")
  hline <- "-----"
  cat(paste0(hline, " NONMEM Input Format (NIF) data summary ", hline, "\n"))

  cat(paste(
    "Data from", sum(x$n_studies$N), "subjects across "))
  if(length(x$studies) == 1) {
    cat("one study:\n")
  } else {
    cat(paste0(length(x$studies), " studies:\n"))
  }

  cat(paste0(df_to_string(x$n_studies, color=color, indent = indent), "\n\n"))

  if (!is.null(x$sex)){
    cat(paste0(
      "Sex distribution:\n",
      df_to_string(
        x$sex %>%
          mutate(SEX = case_match(
            SEX,
            0 ~ "male",
            1 ~ "female")) %>%
          mutate(percent = round(N / sum(N) * 100, 1)),
        indent = indent),
      "\n\n"
      ))
  }

  if (!is.null(x$renal_function)) {
    cat(paste0(
      "Renal impairment class:\n",
      df_to_string(
        x$renal_function %>%
          mutate(percent = round(N / sum(N) * 100, 1)),
        indent = indent),
      "\n\n"))
  }

  if(!is.null(x$odwg)) {
    cat(paste0(
      "NCI ODWG hepatic impairment class:\n",
      df_to_string(
      x$odwg %>%
        mutate(percent = round(N / sum(N) * 100, 1)),
      color=color, indent = indent), "\n\n"))
  }

  cat(paste0("Treatments:\n",
             paste0(spacer, paste(x$drugs,collapse = ", ")), "\n\n"))

  cat(paste0("Analytes:\n",
             paste0(spacer, paste0(x$analytes, collapse = ", ")), "\n\n"))

  cat("Subjects per dose level:\n")
  cat(df_to_string(x$dose_levels, color=color, indent = indent))
  cat("\n\n")

  cat(paste(sum(x$n_obs$N), "observations:\n"))
  cat(paste0(df_to_string(x$n_obs, color=color, indent = indent), "\n\n"))

  # sampling overview
  if(!is.null(x$sampling)) {
    sampling_schedule <- x$sampling
    # footer <- ""
    # if(nrow(sampling_schedule) > 10) {
    #   sampling_schedule <- head(sampling_schedule, 5)
    #   footer <- paste0("\n", nrow(x$sampling) - 5, " more rows")
    # }

    cat("Sampling schedule:\n")
    # cat(df_to_string(sampling_schedule, indent = indent))
    cat(df_to_string(
      sampling_schedule, indent = indent, abbr_lines = 5, abbr_threshold=10))
    # cat(footer)
    cat("\n\n")

  }

  dr_summary <- lapply(x$dose_red_sbs, nrow) %>%
    data.frame()
  cat("Subjects with dose reductions\n")
  cat(df_to_string(dr_summary, color=color, indent = indent))
  cat("\n\n")

  cat("Treatment duration overview:\n")
  cat(df_to_string(x$administration_duration, color=color, indent = indent))

  cat(paste0("\n\nHash: ", x$hash))
  if(!is.null(x$last))
    cat(paste0("\nLast DTC: ", x$last))
  invisible(x)
}


#' Get covariate plot parameters
#'
#' @param field The field of the NIF object
#' @keywords internal
#' @return A data frame.
#' @noRd
get_cov_plot_params <- function(field) {
  # input validation
  validate_char_param(field, "field")

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
    if("WEIGHT" %in% names(nif) & "SEX" %in% names(nif))
      out[["WT_SEX"]] <- wt_by_sex(nif)
    if("WEIGHT" %in% names(nif) & "RACE" %in% names(nif))
      out[["WT_RACE"]] <- wt_by_race(nif)
  }

  if(analytes == TRUE){
    # put analytes for parents first:
    analyte_list <- nif %>%
      # as.data.frame() %>%
      distinct(PARENT, ANALYTE) %>%
      mutate(score = PARENT == ANALYTE) %>%
      arrange(-.data$score, .data$ANALYTE) %>%
      pull(ANALYTE)

    for (i in analyte_list) {
      out[[i]] <- plot(nif,
        analyte = i, log = TRUE, points = TRUE, lines = FALSE, time = "TIME",
        alpha = 0.3, title = paste(i, "by dose"),
        max_time = max_observation_time(nif, i)
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
#' @return A ggplot object.
#' @export
#' @examples
#' covariate_hist(examplinib_sad_nif, "AGE")
#' covariate_hist(examplinib_sad_nif, "BL_CRCL")
covariate_hist <- function(
    obj, cov, nbins = 11, group = NULL, alpha = 0.5,
    density = TRUE, title = NULL) {
  # input validation
  validate_min_nif(obj)
  validate_char_param(cov, "cov")
  validate_numeric_param(nbins, "nbins")
  validate_char_param(group, "group", allow_null = TRUE)
  validate_numeric_param(alpha, "alpha")
  validate_logical_param(density, "density")
  validate_char_param(title, "title", allow_null = TRUE)

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
covariate_barplot <- function(
    obj, cov, group = NULL, title = NULL) {
  # input validation
  validate_min_nif(obj)
  validate_char_param(cov, "cov")
  validate_char_param(group, "group", allow_null = TRUE)
  validate_char_param(title, "title", allow_null = TRUE)

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
#' @return A ggplot object.
#' @export
#' @keywords internal
#' @examples
#' wt_by_sex(examplinib_poc_nif)
wt_by_sex <- function(obj) {
  # input validation
  validate_min_nif(obj, c("SEX", "WEIGHT"))

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
    # ggplot2::geom_label(ggplot2::aes(
    #   label = paste0("N=", count), y = maxwt + 5),
    #   label.size = 0,
    #   position = ggplot2::position_dodge(width = 0.75)
    # ) +
    ggplot2::geom_text(ggplot2::aes(
      label = paste0("N=", count), y = maxwt + 5),
      # label.size = 0,
      position = ggplot2::position_dodge(width = 0.75)
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
  # input validation
  validate_min_nif(obj, c("RACE", "WEIGHT"))

  obj %>%
    as.data.frame() %>%
    group_by(ID) %>%
    mutate(bl_wt = mean(WEIGHT[TIME == 0])) %>%
    ungroup() %>%
    left_join(race_coding, by = "RACE") %>%

    distinct(across(any_of(c("ID", "bl_wt", "LABEL")))) %>%
    mutate(maxwt = max(bl_wt, na.rm = TRUE)) %>%
    group_by(.data$LABEL) %>%
    mutate(count = n()) %>%
    ggplot2::ggplot(ggplot2::aes(x = LABEL, y = bl_wt, group = LABEL)) +
    ggplot2::geom_boxplot(width = 0.5) +
    # ggplot2::geom_label(ggplot2::aes(
    #   label = paste0("N=", count), y = maxwt + 5), label.size = 0
    # ) +
    ggplot2::geom_text(ggplot2::aes(
      label = paste0("N=", count), y = maxwt + 5)
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
#' @keywords internal
#' @examples
#' wt_by_ht(examplinib_poc_nif)
wt_by_ht <- function(obj, alpha = 0.7) {
  # input validation
  validate_min_nif(obj, c("HEIGHT", "WEIGHT"))
  validate_numeric_param(alpha, "alpha")

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
#' @keywords internal
#' @examples
#' ht_by_wt(examplinib_poc_nif)
ht_by_wt <- function(obj, alpha = 0.7) {
  # input validation
  validate_min_nif(obj, c("HEIGHT", "WEIGHT"))
  validate_numeric_param(alpha, "alpha")

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
#' @keywords internal
#' @examples
#' bmi_by_age(examplinib_poc_nif)
bmi_by_age <- function(obj, alpha = 0.7) {
  # input validation
  validate_min_nif(obj, c("AGE", "BMI"))
  validate_numeric_param(alpha, "alpha")

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
#' @keywords internal
#' @export
time_by_ntime <- function(obj, max_time = NULL, ...) {
  # input validation
  validate_min_nif(obj, c("NTIME", "ANALYTE"))
  validate_numeric_param(max_time, "max_time", allow_null = TRUE)

  if(is.null(max_time)) {
    max_time <- max_time(obj, only_observations = TRUE)
  }

  obj %>%
    ensure_analyte() %>%
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
administration_summary <- function(obj) {
  # input validation
  validate_min_nif(obj)

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


#' Sampling scheme overview
#'
#' @param obj A nif object.
#'
#' @returns A data frame.
#' @export
#'
#' @examples
#' sampling_summary(examplinib_poc_nif)
sampling_summary <- function(obj) {
  # validate input
  validate_nif(obj)

  if(!"NTIME" %in% names(obj))
    stop("NTIME not found in input!")

  out <- obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    distinct(ANALYTE, NTIME) %>%
    mutate(FLAG = "X") %>%
    pivot_wider(names_from = ANALYTE, values_from = FLAG, values_fill = "") %>%
    arrange(NTIME) %>%
    as.data.frame()

  return(out)
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
mean_dose_plot <- function(
    obj, analyte = NULL, title = NULL) {
  # input validation
  validate_min_nif(obj, c("ANALYTE"))
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(title, "title", allow_null = TRUE)

  if (is.null(analyte)) {
    analyte <- guess_analyte(obj)
  }

  # make plot title
  if(is.null(title)) {
    if("STUDYID" %in% names(obj)) {
      title <- paste0(nice_enumeration(unique(obj$STUDYID)), ": ")
    } else {
      title <- ""
    }
    title <- paste0(title, "Mean ", analyte, " dose over time")
  }

  obj %>%
    ensure_tafd() %>%
    as.data.frame() %>%
    mutate(DAY = floor(.data$TAFD / 24) + 1) %>%
    filter(EVID == 1, ANALYTE == analyte) %>%
    group_by(DAY) %>%
    summarize(
      "mean dose (mg)" = mean(.data$DOSE, na.rm = TRUE), "N" = n(),
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
subs_per_dose_level <- function(
    obj, analyte = NULL, group = NULL) {
  # input validation
  validate_min_nif(obj)
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(group, "group", allow_null = TRUE)

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
obs_per_dose_level <- function(
    obj, analyte = NULL, group = NULL) {
  # input validation
  validate_min_nif(obj)
  validate_char_param(analyte, "analyte", allow_null = TRUE)
  validate_char_param(group, "group", allow_null = TRUE)

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
#' @param ntime_method the field to derive the nominal time from. Allowed values
#'   are "TPT" and "ELTM".Defaults to xxTPT where xx is the domain name, if NULL.
#'
#' @return A ggplot object.
#' @importFrom ggrepel geom_text_repel
#' @export
edish_plot <- function(
    nif,
    sdtm,
    enzyme = "ALT",
    observation_filter = "LBSPEC != 'URINE'",
    show_labels = FALSE,
    autoscale = TRUE,
    shading = TRUE,
    nominal_time = TRUE,
    ntime_method = NULL,
    time = NULL,
    parent = NULL,
    title = "eDISH plot: All time points",
    size = 3,
    alpha = 0.5, ...) {
  # Input validation
  if (!inherits(sdtm, "sdtm")) {
    stop("sdtm must be an sdtm object")
  }

  if (!inherits(nif, "nif")) {
    stop("nif must be a nif object")
  }

  if (!enzyme %in% c("ALT", "AST")) {
    stop("enzyme must be either 'ALT' or 'AST'")
  }

  lb <- tryCatch({
    sdtm %>%
      domain("lb") %>%
      filter(!is.na(.data$LBSTRESN)) %>%
      filter(!is.na(.data$LBSTNRHI)) %>%
      filter(eval(parse(text = observation_filter))) %>%
      # Verify required columns exist
      assertr::verify(assertr::has_all_names(
        "USUBJID", "LBTESTCD", "LBSTRESN", "LBSTNRHI")) %>%
      # Verify numeric columns are actually numeric
      assertr::verify(is.numeric(.data$LBSTRESN)) %>%
      assertr::verify(is.numeric(.data$LBSTNRHI)) %>%
      # Verify values are valid
      assertr::verify(.data$LBSTRESN >= 0) %>%  # No negative lab values
      assertr::verify(.data$LBSTNRHI > 0)  # No zero or negative ULN values
  }, error = function(e) {
    stop("Data validation failed: ") #, e$message)
  })

  expected_tests <- c("BILI", enzyme)
  missing_tests <- setdiff(expected_tests, unique(lb$LBTESTCD))
  if(length(missing_tests) > 0)
    stop(paste0(
      "missing lab tests for ", nice_enumeration(missing_tests)))

  temp <- lb %>%
    mutate(L_DTC = .data$LBDTC) %>%
    filter(.data$LBTESTCD %in% c(enzyme, "BILI")) %>%
    mutate(L_TESTCD = case_match(.data$LBTESTCD, "BILI" ~ "BILI",
                                  .default = "ENZ")) %>%
    mutate(L_TESTCD = paste0(.data$L_TESTCD, "_X_ULN"),
           L_STRESN = .data$LBSTRESN / .data$LBSTNRHI) %>%
    mutate(DOMAIN = "L_")

  sdtm$domains[["l_"]] <- temp

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

  p <- nif %>%
    add_observation(
      sdtm, "l_", "ENZ_X_ULN", parent=parent, ntime_method = ntime_method,
      duplicates = "ignore",
      silent = TRUE) %>%
    add_observation(
      sdtm, "l_", "BILI_X_ULN", parent=parent, ntime_method = ntime_method,
      duplicates = "ignore",
      silent = TRUE) %>%
    as.data.frame() %>%
    filter(!is.na(DV)) %>%
    filter(.data$ANALYTE %in% c("ENZ_X_ULN", "BILI_X_ULN")) %>%
    select(c("ID", "TIME", "ANALYTE", "DV")) %>%
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
    ggplot2::labs(x = paste0(enzyme, "/ULN"), y = "BILI/ULN") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)

  caption <- paste0(length(unique(nif$ID)), " subjects, red: predose")
  caption <- ifelse(shading == TRUE,
                    paste0(caption, ", grey area: Hy's law."), caption)
  p <- p +
    ggplot2::labs(caption = caption) +
    watermark(cex = 1.5)

  return(p)
}





