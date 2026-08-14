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
#' nif_plot_id(examplinib_poc_nif, 1, analyte = "RS2023")
#' nif_plot_id(examplinib_poc_nif, 1, analyte = "RS2023", time_field = "TAD")
#' nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte = "RS2023")
#' nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte = "RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte = "RS2023", imp = "RS2023")
#' nif_plot_id(examplinib_poc_nif, 8, analyte = c("RS2023", "RS2023487A"))
#' nif_plot_id(examplinib_poc_min_nif, 1, analyte = "CMT3")
#' nif_plot_id(examplinib_poc_min_nif, 1)
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
  ...
) {
  validate_nif(obj)

  x <- obj |>
    ensure_parent() |>
    ensure_analyte() |>
    index_dosing_interval() |>
    as.data.frame()

  if (!is.null(cmt))
    x <- filter(x, .data$CMT == cmt)

  if (!is.null(analyte))
    x <- filter(x, .data$ANALYTE %in% analyte)


  id_label <- ""
  plot_label <- ""

  # filter for subject of interest
  if (id %in% x$ID) {
    plot_label <- "ID"
    x <- x |>
      filter(.data$ID == id)
  } else {
    if ("USUBJID" %in% names(x)) {
      if (id %in% x$USUBJID) {
        x <- x |>
          filter(.data$USUBJID == id)
        id_label <- paste0(
          " (ID ",
          x |>
            distinct(.data$ID) |>
            pull(.data$ID),
          ")"
        )
        plot_label <- "USUBJID"
      } else {
        stop(paste(id, "is not an ID or USUBJID contained in the NIF object"))
      }
    }
  }

  obs <- x |>
    mutate(active_time = .data[[time_field]]) |>
    filter(.data$EVID == 0, !is.na(.data$DV))

  if (time_field == "TAD") {
    obs <- mutate(
      obs,
      group = interaction(.data$ID, as.factor(.data$ANALYTE), .data$DI),
      color = interaction(as.factor(.data$ANALYTE), .data$DI)
    )
  } else {
    obs <- mutate(
      obs,
      group = interaction(.data$ID, as.factor(.data$ANALYTE)),
      color = as.factor(.data$ANALYTE)
    )
  }

  # remove zeros or negatives for log plotting
  if (log == TRUE) {
    obs <- filter(obs, .data$DV > 0)
  }

  admin <- x |>
    mutate(active_time = .data[[time_field]]) |>
    dplyr::filter(.data$EVID == 1)

  if (!is.null(imp))
    admin <- filter(admin, .data$ANALYTE == imp)

  p <- obs |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$active_time,
      y = .data$DV,
      group = .data$group,
      color = .data$color
    ))

  if (!is.null(imp) > 0) {
    p <- p +
      ggplot2::geom_vline(
        data = admin,
        ggplot2::aes(xintercept = .data$active_time),
        color = "gray"
      )
  }

  if (isTRUE(lines)) {
    line_obs <- obs |>
      dplyr::group_by(.data$group) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::ungroup()
    if (nrow(line_obs) > 0) {
      p <- p + ggplot2::geom_line(data = line_obs)
    }
  }

  p <- p +
    ggplot2::geom_point(size = point_size) +
    ggplot2::xlim(0, max_time) +
    ggplot2::labs(
      x = time_field,
      title = paste0(plot_label, ": ", id, id_label),
      color = "analyte"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(imp)) {
    p <- p +
      ggplot2::labs(caption = paste(
        "vertical lines indicate",
        imp, "administrations"
      ))
  }

  if (log == TRUE) {
    p <- p + ggplot2::scale_y_log10()
  } else {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, NA))
  }
  p
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
  validate_nif(obj)

  x <- obj |>
    ensure_parent() |>
    ensure_analyte() |>
    index_dosing_interval()

  if (!is.null(analyte))
    x <- filter(x, .data$ANALYTE %in% analyte)

  plot_label <- ""

  # filter for subject of interest
  if (id %in% x$ID) {
    plot_label <- "ID"
    x <- x |>
      filter(.data$ID == as.numeric(id))
  } else {
    if ("USUBJID" %in% names(x)) {
      if (id %in% x$USUBJID) {
        x <- x |>
          filter(.data$USUBJID == id)
        plot_label <- "USUBJID"
      } else {
        stop(paste(id, "is not an ID or USUBJID contained in the NIF object"))
      }
    }
  }

  admin <- x |>
    dplyr::filter(.data$EVID == 1) |>
    mutate(
      active_time = .data[[time_field]],
      group = interaction(.data$ID, .data$ANALYTE)
    )

  p <- admin |>
    ggplot2::ggplot(ggplot2::aes(
      x = .data$active_time,
      y = .data$AMT,
      group = .data$group,
      color = .data$ANALYTE
    ))

  line_admin <- admin |>
    dplyr::group_by(.data$group) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()
  if (nrow(line_admin) > 0) {
    p <- p + ggplot2::geom_line(data = line_admin)
  }

  p <- p +
    ggplot2::geom_point(size = point_size) +
    ggplot2::ylim(0, max_dose) +
    ggplot2::xlim(0, max_time) +
    ggplot2::labs(title = paste0(plot_label, ": ", id), color = "treatment") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")

  if (y_scale == "log") {
    p <- p + ggplot2::scale_y_log10()
  }
  p
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
#' summary(nif())
summary.nif <- function(
  object,
  sampling = TRUE,
  ...
) {
  # input validation
  validate_nif(object)
  validate_argument(sampling, "logical")

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

  nif_check_id_integrity(object)

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

  observations <- object |>
    as.data.frame() |>
    filter(.data$EVID == 0) |>
    group_by(across(any_of(c("CMT", "ANALYTE")))) |>
    summarize(n = n(), .groups = "drop") |>
    as.data.frame()

  n_studies <- object |>
    as.data.frame() |>
    filter(.data$EVID == 1) |>
    group_by(across(any_of(c("STUDYID")))) |>
    summarize(N = n_distinct(.data$ID), .groups = "drop")

  # Handle sex distribution with safety checks
  sex <- object |>
    as.data.frame()

  if (!"SEX" %in% names(sex))
    sex <- mutate(sex, SEX = NA)

  sex <- sex |>
    distinct(.data$ID, .data$SEX) |>
    reframe(N = n(), .by = "SEX") |>
    complete(SEX = c(0, 1), fill = list(N = 0))

  n_males <- as.numeric(sex[which(sex$SEX == 0), "N"])
  n_females <- as.numeric(sex[which(sex$SEX == 1), "N"])

  # dose levels
  dose_levels <- NULL

  tryCatch({
    dose_levels <- object |>
      add_dose_level(silent = TRUE) |>
      reframe(n = n_distinct(.data$ID), .by = "DL")
  }, error = function(msg) {
    invisible(NULL)
  })

  if ("BL_CRCL" %in% colnames(object)) {
    renal_function <- object |>
      as.data.frame() |>
      mutate(CLASS = as.character(
        cut(.data$BL_CRCL,
          breaks = c(0, 30, 60, 90, Inf),
          labels = c("severe", "moderate", "mild", "normal")
        )
      )) |>
      distinct(.data$ID, .data$CLASS) |>
      mutate(CLASS = factor(
        .data$CLASS,
        levels = c("normal", "mild", "moderate", "severe")
      )) |>
      reframe(
        N = n(),
        .by = "CLASS"
      ) |>
      tidyr::complete(.data$CLASS, fill = list(N = 0)) |>
      mutate(CLASS = as.character(.data$CLASS))
  } else {
    renal_function <- NULL
  }

  if ("BL_ODWG" %in% colnames(object)) {
    odwg <- object |>
      as.data.frame() |>
      mutate(CLASS = .data$BL_ODWG) |>
      distinct(.data$ID, .data$CLASS) |>
      mutate(
        CLASS = factor(
          .data$CLASS,
          levels = c("normal", "mild", "moderate", "severe")
        )
      ) |>
      reframe(
        N = n(),
        .by = "CLASS"
      ) |>
      tidyr::complete(.data$CLASS, fill = list(N = 0)) |>
      mutate(CLASS = as.character(.data$CLASS))
  } else {
    odwg <- NULL
  }

  # sampling overview
  sampling_table <- NULL
  if ("NTIME" %in% names(object) && sampling == TRUE) {
    sampling_table <- object |>
      as.data.frame() |>
      filter(.data$EVID == 0) |>
      reframe(n = n(), .by = c("NTIME", "ANALYTE")) |>
      pivot_wider(names_from = "ANALYTE", values_from = "n") |>
      arrange(.data$NTIME) |>
      as.data.frame()
  }

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
    hash = hash.nif(object),
    last = last_dtc(object)
  )
  class(out) <- "summary_nif"
  out
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
  ...
) {
  # input validation
  validate_argument(color, "logical")

  # business logic
  indent <- 2
  spacer <- paste(replicate(indent, " "), collapse = "")
  hline <- "-----"
  cat(paste0(hline, " NONMEM Input Format (NIF) data summary ", hline, "\n"))

  compose_message <- function(..., condition = TRUE) {
    args <- list(...)
    if (length(args) > 0) {
      trimws(paste(lapply(
        args,
        function(x) {
          ifelse(
            inherits(x, "data.frame"),
            df_to_string(x, color = color, indent = indent),
            as.character(x)
          )
        }
      ), collapse = "\n"))
    }
  }

  out <- list(
    # study disposition
    compose_message(
      paste(
        "Data from", sum(x$n_studies$N), "subjects across",
        ifelse(
          length(x$studies) == 1, "one study:",
          paste0(length(x$studies), "studies:")
        )
      ),
      x$n_studies
    ),

    # sex disposition
    ifelse(is.null(x$sex), "",
      compose_message(
        "Sex distribution:",
        mutate(x$sex, SEX = case_when(
          .data$SEX == 0 ~ "male",
          .data$SEX == 1 ~ "female"
        )) |>
        mutate(percent = round(.data$N / sum(.data$N) * 100, 1))
      )
    ),

    # renal function disposition
    ifelse(is.null(x$renal_function), "",
      compose_message(
        "Renal impairment class:",
        mutate(x$renal_function,
               percent = round(.data$N / sum(.data$N) * 100, 1))
      )
    ),

    # hepatic function class
    ifelse(is.null(x$odwg), "",
      compose_message(
        "NCI ODWG hepatic impairment class:",
        mutate(x$odwg, percent = round(.data$N / sum(.data$N) * 100, 1))
      )
    ),

    # treatments and analytes
    trimws(paste0("Treatments: ", nice_enumeration(x$drugs))),
    trimws(paste0("Analytes: ", nice_enumeration(x$analytes))),

    # dose levels
    ifelse(is.null(x$dose_levels), "",
      compose_message(
        "Subjects per dose level:",
        x$dose_levels
      )
    ),

    # observations
    compose_message(
      paste(sum(x$n_obs$n), "observations:"),
      x$n_obs
    ),

    # sampling
    ifelse(is.null(x$sampling), "",
      compose_message(
        "Observations by NTIME:",
        round(x$sampling, 3) |>
          mutate(across(-1, function(x) ifelse(is.na(x), "-", as.character(x))))
      )
    ),

    # dose reductions
    compose_message(
      "Subjects with dose reductions:",
      sapply(x$dose_red_sbs, nrow) |>
        tibble::enframe(name = "treatment", value = "n")
    ),

    # treatment duration
    compose_message(
      "Treatment duration overview:",
      x$administration_duration
    ),

    # hash and time stamp
    trimws(
      paste0(
        "Hash: ", x$hash,
        ifelse(is.null(x$last), "", paste0("\nLast DTC: ", x$last))
      )
    )
  )

  dummy <- lapply(
    out,
    function(x) {
      cat(ifelse(x == "", "", paste0(x, "\n\n")))
    })

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
  validate_argument(field, "character")

  # business logic
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
    params[which(params$field == field), ]
  } else {
    data.frame(
      field = field, binwidth = NA, xlabel = field,
      title = field
    )
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
plot.summary_nif <- function(
    x,
    baseline = TRUE,
    analytes = TRUE,
    ...
) {
  # input validation
  validate_argument(baseline, "logical")
  validate_argument(analytes, "logical")

  # business logic
  nif <- x$nif
  out <- list()

  if (baseline == TRUE) {
    for (i in c(
      "AGE", "WEIGHT", "HEIGHT", "BMI",
      str_subset(names(nif), "BL_.*")
    )) {
      if (i %in% colnames(nif)) {
        if (is.numeric(nif[[i]])) {
          out[[i]] <- covariate_hist(nif, i)
        }
        if (is.factor(nif[[i]])) {
          out[[i]] <- covariate_barplot(nif, i)
        }
      }
    }
    if ("WEIGHT" %in% names(nif) && "SEX" %in% names(nif)) {
      out[["WT_SEX"]] <- wt_by_sex(nif)
    }
    if ("WEIGHT" %in% names(nif) && "RACE" %in% names(nif)) {
      out[["WT_RACE"]] <- wt_by_race(nif)
    }
  }

  if (analytes == TRUE) {
    # put analytes for parents first:
    analyte_list <- nif |>
      as.data.frame() |>
      filter(.data$EVID == 0) |>
      distinct(.data$PARENT, .data$ANALYTE) |>
      mutate(score = .data$PARENT == .data$ANALYTE) |>
      arrange(-.data$score, .data$ANALYTE) |>
      pull(.data$ANALYTE) |>
      unique()

    for (i in analyte_list) {
      out[[i]] <- plot(nif,
        analyte = i, log = TRUE, points = TRUE, lines = FALSE, time = "TIME",
        alpha = 0.3, title = paste(i, "by dose"),
        max_time = max_observation_time(nif, i)
      ) +
        ggplot2::labs(y = "")
    }
  }
  out
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
  obj,
  cov,
  nbins = 11,
  group = NULL,
  alpha = 0.5,
  density = TRUE,
  title = NULL
) {
  # input validation
  validate_argument(cov, "char")
  validate_argument(group, "character", allow_multiple = TRUE, allow_null = TRUE)
  validate_nif(obj, fields = unique(c(cov, group)))
  validate_argument(nbins, "numeric")
  validate_argument(alpha, "numeric")
  validate_argument(density, "logical")
  validate_argument(title, "character", allow_null = TRUE)

  cov_params <- get_cov_plot_params(cov)
  xlabel <- cov_params$xlabel
  if (is.na(xlabel)) {
    xlabel <- cov_params$field
  }
  if (is.null(title)) {
    title <- cov_params$title
    if (is.na(title)) {
      title <- paste(cov_params$field, "distribution")
    }
    if (!is.null(group)) {
      title <- paste0(title, " grouped by ", nice_enumeration(group))
    }
  }
  limits <- unlist(cov_params$limits)

  binwidth <- NULL
  if (is.na(cov_params$binwidth)) {
    binwidth <- NULL
    bins <- nbins
  } else {
    binwidth <- cov_params$binwidth
    bins <- NULL
  }

  if (!is.null(group)) {
    p <- obj |>
      as.data.frame() |>
      mutate_at(group, factor) |>
      tidyr::unite("GROUP", all_of(group), remove = FALSE) |>
      distinct_at(c("ID", cov_params$field, "GROUP")) |>
      ggplot2::ggplot(ggplot2::aes(
        x = .data[[cov_params$field]], group = .data$GROUP,
        fill = .data$GROUP
      ))

    if (density == FALSE) {
      p <- p +
        ggplot2::geom_histogram(
          bins = bins, binwidth = binwidth, position = "identity",
          alpha = alpha
        )
    } else {
      p <- p +
        ggplot2::geom_histogram(
          ggplot2::aes(y = ggplot2::after_stat(density)),
          bins = bins,
          binwidth = binwidth,
          position = "identity",
          alpha = alpha
        )
    }
  } else {
    p <- obj |>
      as.data.frame() |>
      distinct_at(c("ID", cov_params$field)) |>
      filter(!is.na(.data[[cov_params$field]])) |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[cov_params$field]]))

    if (density == FALSE) {
      p <- p +
        ggplot2::geom_histogram(
          bins = bins, binwidth = binwidth,
          position = "identity", fill = "grey"
        )
    } else {
      p <- p +
        ggplot2::geom_histogram(
          ggplot2::aes(
            y = ggplot2::after_stat(density)
          ),
          bins = bins,
          binwidth = binwidth, position = "identity", fill = "grey"
        )
    }
  }

  p <- p +
    ggplot2::geom_vline(xintercept = limits, color = "red") +
    ggplot2::theme_bw()

  if (density == FALSE) {
    p <- p + ggplot2::labs(x = xlabel, y = "number of subjects")
  } else {
    p <- p + ggplot2::labs(x = xlabel, y = "density")
  }

  p +
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
  obj,
  cov,
  group = NULL,
  title = NULL
) {
  # input validation
  validate_argument(cov, "char")
  validate_argument(group, "character", allow_multiple = TRUE, allow_null = TRUE)
  validate_nif(obj, fields = unique(c(cov, group)))
  validate_argument(title, "character", allow_null = TRUE)

  if (is.null(title)) {
    title <- cov
    if (!is.null(group)) {
      title <- paste0(title, " grouped by ", nice_enumeration(group))
    }
  }

  if (!is.null(group)) {
    out <- obj |>
      as.data.frame() |>
      mutate_at(group, factor) |>
      tidyr::unite("GROUP", all_of(group), remove = FALSE) |>
      mutate(CLASS = as.factor(.data[[cov]])) |>
      distinct_at(c("ID", "CLASS", "GROUP")) |>
      group_by(.data$CLASS, .data$GROUP) |>
      summarize(n = n(), .groups = "drop") |>
      ggplot2::ggplot(ggplot2::aes(
        x = .data$CLASS, y = .data$n, group = .data$GROUP, fill = .data$GROUP
      )) +
      ggplot2::scale_x_discrete(drop = FALSE, name = cov) +
      ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_dodge(preserve = "single"),
        width = 0.5, alpha = 0.8
      ) +
      ggplot2::labs(fill = group)
  } else {
    out <- obj |>
      as.data.frame() |>
      mutate(CLASS = as.factor(.data[[cov]])) |>
      distinct(.data$ID, .data$CLASS) |>
      group_by(.data$CLASS) |>
      summarize(n = n()) |>
      ggplot2::ggplot(ggplot2::aes(x = .data$CLASS, y = .data$n)) +
      ggplot2::scale_x_discrete(drop = FALSE, name = cov) +
      ggplot2::geom_bar(
        stat = "identity", fill = "white", color = "black", width = 0.5
      )
  }

  out +
    ggplot2::theme_bw() +
    ggplot2::labs(y = "N") +
    ggplot2::ggtitle(title) +
    watermark()
}


#' boxplot over categories
#'
#' @param obj The nif object.
#' @param cat_field The category field.
#' @param val_field The value field.
#' @param title The figure title.
#'
#' @returns A ggplot2 object.
#' @noRd
cat_boxplot <- function(
    obj,
    cat_field,
    val_field,
    title = NULL
) {
  # input validation
  validate_argument(cat_field, "character")
  validate_argument(val_field, "character")
  validate_nif(obj, fields = unique(c(cat_field, val_field)))
  validate_argument(title, "character", allow_null = TRUE)

  # business logic
  temp <- obj |>
    as.data.frame() |>
    mutate(.cat = as.factor(.data[[cat_field]])) |>
    filter(!is.na(.data[[val_field]])) |>
    group_by(.data$ID) |>
    mutate(.bl = mean(.data[[val_field]][.data$TIME == 0])) |>
    ungroup() |>
    distinct(.data$ID, .data$.cat, .data$.bl)

  n <- temp |>
    reframe(n = n(), max_bl = max(.data$.bl), .by = ".cat")

  out <- ggplot(temp, aes(x = .data$.cat, y = .data$.bl, group = .data$.cat)) +
    ggplot2::geom_boxplot(width = 0.5) +
    ggplot2::geom_text(
      data = n,
      aes(label = paste0("N=", n), y = .data$max_bl + 5),
      position = ggplot2::position_dodge(width = 0.75)
    ) +
    ggplot2::labs(x = cat_field, y = paste0("baseline ", val_field)) +
    ggplot2::theme_bw() +
    watermark()

  if (!is.null(title))
    out <- out + ggtitle(title)

  out
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
  validate_nif(obj, fields = c("SEX", "WEIGHT"))
  cat_boxplot(obj, "SEX", "WEIGHT", "Body weight by sex")
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
  validate_nif(obj, fields = c("RACE", "WEIGHT"))
  cat_boxplot(obj, "RACE", "WEIGHT", "Body weight by race")
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
  validate_nif(obj, fields = c("HEIGHT", "WEIGHT"))
  validate_argument(alpha, "numeric")

  obj |>
    as.data.frame() |>
    distinct(.data$ID, .data$HEIGHT, .data$WEIGHT) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$HEIGHT, y = .data$WEIGHT)) +
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
  validate_nif(obj, fields = c("HEIGHT", "WEIGHT"))
  validate_argument(alpha, "numeric")

  obj |>
    as.data.frame() |>
    distinct(.data$ID, .data$HEIGHT, .data$WEIGHT) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$WEIGHT, y = .data$HEIGHT)) +
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
  validate_nif(obj, fields = c("AGE", "BMI"))
  validate_argument(alpha, "numeric")

  obj |>
    as.data.frame() |>
    distinct(.data$ID, .data$AGE, .data$BMI) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$AGE, y = .data$BMI)) +
    ggplot2::geom_smooth(method = "lm", formula = "y ~ x", alpha = 0.3) +
    ggplot2::geom_point(size = 3, alpha = alpha) +
    ggplot2::labs(y = "BMI (kg/m^2)", x = "age (y)") +
    ggplot2::ggtitle("Body mass index by age") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    watermark()
}


#' Administration summary
#'
#' Summary statistics on the number of administrations per subject, by parent.
#' This may useful to characterized the population exposure to a treatment.
#'
#' @param obj A NIF object.
#' @return A data frame.
#' @importFrom stats median
#' @export
#' @examples
#' administration_summary(examplinib_poc_nif)
administration_summary <- function(obj) {
  # input validation
  validate_nif(obj)

  temp <- obj |>
    ensure_parent() |>
    n_administrations()

  if (nrow(temp) == 0) {
    data.frame(
      PARENT = character(), min = numeric(), max = numeric(),
      mean = numeric(), median = numeric()
    )
  } else {
    temp |>
      filter(.data$PARENT != "") |>
      group_by(across(any_of(c("PARENT")))) |>
      summarize(
        min = min(.data$N, na.rm = TRUE), max = max(.data$N, na.rm = TRUE),
        mean = round(mean(.data$N, na.rm = TRUE), 1),
        median = stats::median(.data$N, na.rm = TRUE)
      ) |>
      as.data.frame()
  }
}


#' Sampling scheme overview
#'
#' @param obj A nif object.
#'
#' @returns A data frame.
#' @noRd
sampling_summary <- function(obj) {
  # input validation
  validate_nif(obj, fields = "NTIME")

  # business_logic
  obj |>
    as.data.frame() |>
    filter(.data$EVID == 0) |>
    distinct(.data$ANALYTE, .data$NTIME) |>
    mutate(FLAG = "X") |>
    pivot_wider(names_from = "ANALYTE", values_from = "FLAG",
                values_fill = "") |>
    arrange(.data$NTIME) |>
    as.data.frame()
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
  obj,
  analyte = NULL,
  title = NULL
) {
  # input validation
  validate_argument(analyte, "char", allow_null = TRUE)
  validate_argument(title, "character", allow_null = TRUE)
  validate_nif(obj, fields = c("ANALYTE"))

  if (is.null(analyte)) {
    analyte <- guess_analyte(obj)
  }
  validate_analyte(obj, analyte)

  # make plot title
  if (is.null(title)) {
    if ("STUDYID" %in% names(obj)) {
      title <- paste0(nice_enumeration(unique(obj$STUDYID)), ": ")
    } else {
      title <- ""
    }
    title <- paste0(title, "Mean ", analyte, " dose over time")
  }

  # business logic
  obj |>
    ensure_tafd() |>
    as.data.frame() |>
    mutate(DAY = floor(.data$TAFD / 24) + 1) |>
    filter(.data$EVID == 1, .data$ANALYTE == analyte) |>
    group_by(.data$DAY) |>
    summarize(
      "mean dose (mg)" = mean(.data$DOSE, na.rm = TRUE), "N" = n(),
      .groups = "drop"
    ) |>
    tidyr::pivot_longer(cols = -"DAY", names_to = "PARAM", values_to = "VAL") |>
    ggplot2::ggplot(ggplot2::aes(x = .data$DAY, y = .data$VAL)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(PARAM ~ ., scales = "free_y") +
    ggplot2::labs(x = "time (days)", y = "") +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(title) +
    watermark()
}


#' extract the dose of the first component from a dose level string
#'
#' @param dl Dose level as character.
#'
#' @returns Dose of the first dose level component as numeric.
#' @noRd
sorting_dose_level <- function(dl) {
  as.numeric(sub("-.*$", "", dl))
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
  obj,
  analyte = NULL,
  group = NULL
) {
  # input validation
  validate_argument(analyte, "character", allow_null = TRUE)
  validate_argument(group, "character", allow_null = TRUE)
  validate_nif(obj, fields = unique(c("ANALYTE", group)))

  if (is.null(analyte)) {
    analyte <- analytes(obj)
  }
  validate_analyte(obj, analyte)

  # business logic
  obj |>
    ensure_analyte() |>
    add_dose_level() |>
    as.data.frame() |>
    filter(.data$ANALYTE %in% analyte) |>
    filter(.data$EVID == 0) |>
    distinct(across(any_of(c("ID", "DL", "ANALYTE", group)))) |>
    reframe(N = n(), .by = any_of(c("DL", "ANALYTE", "SEX"))) |>
    mutate(.sorting = sorting_dose_level(.data$DL)) |>
    arrange(.data$.sorting, .data$ANALYTE) |>
    select(-".sorting")
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
  obj,
  analyte = NULL,
  group = NULL
) {
  # input validation
  validate_argument(analyte, "character", allow_null = TRUE)
  validate_argument(group, "character", allow_null = TRUE)
  validate_nif(obj, fields = unique(c("ANALYTE", group)))

  if (is.null(analyte)) {
    analyte <- analytes(obj)
  }
  validate_analyte(obj, analyte)

  # business logic
  obj |>
    ensure_analyte() |>
    add_dose_level() |>
    as.data.frame() |>
    filter(.data$ANALYTE %in% analyte) |>
    filter(.data$EVID == 0) |>
    reframe(N = n(), .by = any_of(c("DL", "ANALYTE", group))) |>
    mutate(.sorting = sorting_dose_level(.data$DL)) |>
    arrange(.data$.sorting, .data$ANALYTE) |>
    select(-".sorting")
}


#' Drug-induced serious hepatotoxicity (eDISH) plot
#'
#' @description
#'
#' Refer to the FDA guidance on DILI, https://www.fda.gov/media/116737/download
#' and Watkins 2011, \doi{10.2165/11586600-000000000-00000}.
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
#' @param shading Highlight Hy's law area, as logical.
#' @param observation_filter A filter term as character.
#' @param silent Suppress messages.
#' @param baseline_filter A filter term to identify baseline conditions.
#'
#' @return A ggplot object.
#' @importFrom ggrepel geom_text_repel
#' @export
edish_plot <- function(
  nif,
  sdtm,
  enzyme = "ALT",
  observation_filter = NULL,
  baseline_filter = "LBBLFL == 'Y'",
  show_labels = FALSE,
  shading = TRUE,
  title = "eDISH plot",
  size = 3,
  alpha = 0.5,
  silent = NULL,
  ...
) {
  # input validation
  validate_sdtm(sdtm, "lb")
  validate_nif(nif)
  validate_argument(enzyme, "character", values = c("ALT", "AST"))
  validate_argument(observation_filter, "character", allow_null = TRUE)
  validate_argument(baseline_filter, "character", allow_null = TRUE)
  validate_argument(show_labels, "logical")
  validate_argument(shading, "logical")
  validate_argument(title, "character", allow_null = TRUE)
  validate_argument(size, "numeric")
  validate_argument(alpha, "numeric")
  validate_argument(silent, "logical")

  # if (!inherits(sdtm, "sdtm")) {
  #   stop("sdtm must be an sdtm object")
  # }
  #
  # if (!inherits(nif, "nif")) {
  #   stop("nif must be a nif object")
  # }

  # if (!enzyme %in% c("ALT", "AST")) {
  #   stop("enzyme must be either 'ALT' or 'AST'")
  # }

  lb <- domain(sdtm, "lb")

  if (is.null(observation_filter)) {
    if ("LBSPEC" %in% names(lb)) {
      observation_filter = "LBSPEC != 'URINE'"
      conditional_cli(
        cli_alert_info(
          paste0("Observation filter set to ", observation_filter)
        ),
        silent = silent
      )
    } else {
      observation_filter = "TRUE"
    }
  }

  if (nrow(lb) > 0) {
    obs_expr <- validate_filter(observation_filter, data = lb)
  } else {
    obs_expr <- validate_filter(observation_filter)
  }

  expected_tests <- c("BILI", enzyme)
  missing_tests <- setdiff(expected_tests, unique(lb$LBTESTCD))
  if (length(missing_tests) > 0) {
    stop(paste0(
      "missing lab tests for ", nice_enumeration(missing_tests)
    ))
  }

  bl_expr <- tryCatch(
    validate_filter(baseline_filter, data = lb),
    error = function(e) {
      conditional_cli(
        cli_alert_warning(
          paste0(
            "Invalid baseline filter (",
            baseline_filter,
            ") ignored!"
          )
        ),
        silent = silent
      )
      rlang::parse_expr("FALSE")
    }
  )

  xuln <- lb |>
    filter(!is.na(.data$LBSTRESN)) |>
    filter(!is.na(.data$LBSTNRHI)) |>
    filter(rlang::eval_tidy(obs_expr, data = pick(everything()))) |>
    filter(.data$LBTESTCD %in% expected_tests) |>
    mutate(XULN = .data$LBSTRESN / .data$LBSTNRHI) |>
    inner_join(subjects(nif), by = "USUBJID") |>
    mutate(
      LBTESTCD = case_when(
        .data$LBTESTCD == enzyme ~ "ENZ",
        .default = .data$LBTESTCD
      )
    )

  xuln <- mutate(
    xuln, BL = case_when(
      rlang::eval_tidy(bl_expr, data = pick(everything())) ~ TRUE,
      .default = FALSE
    )
  )

  p <- xuln |>
    pivot_wider(
      id_cols = any_of(c("LBDY", "BL", "ID", "USUBJID", "LBDTC")),
      names_from = "LBTESTCD",
      values_from = "XULN"
    ) |>
    filter(!is.na(.data$ENZ), !is.na(.data$BILI)) |>
    ggplot(
      aes(
        x = .data$ENZ,
        y = .data$BILI,
        color = !.data$BL,
        label = .data$ID
      )
    ) +
    ggplot2::geom_point(size = size, alpha = alpha) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::geom_hline(yintercept = 2, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 3, linetype = "dashed") +
    ggplot2::labs(x = paste0(enzyme, "/ULN"), y = "BILI/ULN") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ggtitle(title)

  if (show_labels == TRUE)
    p <- p + ggrepel::geom_text_repel()

  if (shading == TRUE) {
    p <- p +
      ggplot2::annotate(
        "rect",
        xmin = 3,
        xmax = Inf,
        ymin = 2,
        ymax = Inf,
        alpha = .15,
        fill = "grey"
      )
  }

  caption <- paste0(length(unique(nif$ID)), " subjects, red: baseline")
  caption <- ifelse(
    shading == TRUE,
    paste0(caption, ", grey area: Hy's law."),
    caption
  )

  p +
    ggplot2::labs(caption = caption) +
    watermark(cex = 1.5)
}
