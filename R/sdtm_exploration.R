#' Filter out any rows containing DTC rows with incomplete data format as per
#' ISO 8601
#'
#' @param obj The SDTM domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#' @param silent `r lifecycle::badge("deprecated")` Dummy option for
#' compatibility, set the global option [nif_option()] with `silent = TRUE` to
#' suppress messages.
#'
#' @return The filtered SDTM domain as data frame.
#' @keywords internal
#' @export
filter_correct_date_format <- function(obj, verbose = TRUE,
                                       silent = deprecated()) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~ !(is_iso_date(.) | . == "")))

  if (nrow(temp) > 0) {
    out <- paste0(
      domain, ": ", nrow(temp),
      " rows containing DTC fields with incomplete date format ignored!"
    )
    if (verbose && !get("silent", .nif_env)) {
      out <- paste0(out, "\n")
    }
    message(out)
    obj <- obj %>%
      filter(if_all(ends_with("DTC"), ~ (is_iso_date(.) | . == "")))
  }
  return(obj)
}


#' Check for incomplete date format as per ISO 8601 in columns ending with 'DTC'
#'
#' @param obj The SDTM domain as data frame.
#' @param verbose Boolean to indicate whether to issue message output.
#' @return The (unchanged) SDTM domain.
#' @export
#' @keywords internal
#' @examples
#' ex <- check_date_format(examplinib_poc$ex)
check_date_format <- function(obj, verbose = TRUE) {
  domain <- obj %>% distinct(.data$DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~ !(is_iso_date(.) | . == ""))) %>%
    select(c("USUBJID", "DOMAIN", ends_with("DTC")))

  if (nrow(temp) > 0) {
    out <- paste0(domain, ": Incomplete date format in ", nrow(temp), " rows")
    if (verbose) {
      out <- paste0(out, ":\n", df_to_string(temp), "\n")
    }
    message(out)
  }
  return(obj)
}


#' Check for incomplete date-time format in columns ending with "DTC"as per
#' ISO 8601
#'
#' @param obj The SDTM domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#' @return The unchanged SDTM domain.
#' @export
#' @keywords internal
check_date_time_format <- function(obj, verbose = TRUE) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~ !(is_iso_date_time(.) | . == ""))) %>%
    select(c("USUBJID", "DOMAIN", ends_with("DTC")))

  if (nrow(temp) > 0) {
    out <- paste0(
      domain, ": Incomplete date-time format in ", nrow(temp),
      " rows"
    )
    if (verbose) {
      out <- paste0(out, ":\n", df_to_string(temp), "\n")
    }
    message(out)
  }
  return(obj)
}


#' Check for missing time in columns ending with "DTC"
#'
#' @param obj The SDTM domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#' @return The unchanged SDTM domain.
#' @export
#' @keywords internal
check_missing_time <- function(obj, verbose = TRUE) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(
      ends_with("DTC"),
      ~ is_iso_date(.) & !(is_iso_date_time(.))
    )) %>%
    select(c("USUBJID", "DOMAIN", ends_with("DTC")))

  if (nrow(temp) > 0) {
    out <- paste0(domain, ": Missing time in ", nrow(temp), " rows")
    if (verbose) {
      out <- paste0(out, ":\n", df_to_string(temp), "\n")
    }
    message(out)
  }
  return(obj)
}


#' Check EXENDTC for the last administration.
#'
#' Absent EXENDTC in the last administration by subject and treatment may
#' indicate that at the time of the data cut-off, this subject was still on
#' treatment.
#' @param ex The SDTM EX domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#' @return The unchanged EX domain.
#' @export
#' @keywords internal
check_last_exendtc <- function(ex, verbose = TRUE) {
  domain <- ex %>% distinct(DOMAIN)

  temp <- ex %>%
    assertr::verify(assertr::has_all_names(
      "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"
    )) %>%
    lubrify_dates() %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    filter(LAST_ADMIN == TRUE, is.na(.data$EXENDTC)) %>%
    select("USUBJID", "DOMAIN", "EXTRT", "EXSTDTC", "EXENDTC")

  if (nrow(temp) > 0) {
    out <- paste0(
      domain, ": Missing EXENDTC for the last administration in ",
      nrow(temp), " rows"
    )
    if (verbose) {
      out <- paste0(out, ":\n", df_to_string(temp), "\n")
    }
    message(out)
  }
  return(ex)
}


#' Check SDTM domains DM, EX and PC for missing date and time information.
#'
#' @param sdtm The SDTM as SDTM object.
#' @param verbose Boolean to indicate whether to include details.
#'
#' @return Nothing.
#' @export
#' @keywords internal
#' @examples
#' check_sdtm(examplinib_poc)
check_sdtm <- function(sdtm, verbose = TRUE) {
  ## Date-times in DM
  sdtm %>%
    domain("dm") %>%
    filter(.data$ACTARMCD != "SCRNFAIL") %>%
    select("USUBJID", "DOMAIN", "RFENDTC") %>%
    check_date_format(verbose = verbose) %>%
    check_missing_time(verbose = verbose)

  ## Date-times in EX
  sdtm %>%
    domain("ex") %>%
    check_date_format(verbose = verbose) %>%
    # filter_correct_date_format(verbose = verbose) %>%
    check_missing_time(verbose = verbose) %>%
    check_last_exendtc(verbose = verbose)

  ## Date-times in PC
  sdtm %>%
    domain("pc") %>%
    select("USUBJID", "DOMAIN", "PCDTC") %>%
    check_date_format(verbose = verbose) %>%
    # filter_correct_date_format(verbose = verbose) %>%
    check_missing_time(verbose = verbose)
  return(invisible(NULL))
}


#' Plot SDTM object
#'
#' @param x The sdtm object.
#' @param domain The domain to be plotted, defaults to 'dm'.
#' @param usubjid The USUBJID to filter for. All subjects if `NULL` (default).
#' @param ... Further printing parameters.
#' @param lines Boolean whether to plot lines.
#' @param points Boolean whether to plot points.
#' @param analyte The analyte to be plotted as character.
#' @param legend Show legend, as logical.
#' @param log Boolean whether to use a logarithmic y axis.
#' @param subject_filter Filter term as character.
#'
#' @return Nothing.
#' @export
#' @examples
#' plot(examplinib_poc)
#' plot(examplinib_poc, "dm")
#' plot(examplinib_poc, domain = "ex")
#' plot(examplinib_poc, domain = "pc")
#' plot(examplinib_poc, domain = "vs", lines = FALSE, points = TRUE)
plot.sdtm <- function(x, domain = "dm", usubjid = NULL, lines = TRUE,
                      points = FALSE, analyte = NULL, log = FALSE,
                      legend = FALSE, subject_filter = TRUE, ...) {
  obj <- x %>%
    domain(domain) %>%
    filter(if (!is.null(usubjid)) .data$USUBJID %in% usubjid else TRUE) %>%
    lubrify_dates() %>%
    filter(eval(parse(text = subject_filter))) %>%
    as.data.frame()

  if (domain == "pc") {
    return(
      obj %>%
        filter(if (!is.null(analyte)) .data$PCTESTCD %in% analyte else TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$PCDTC,
          y = .data$PCSTRESN,
          group = interaction(.data$USUBJID, .data$PCTESTCD),
          color = .data$PCTESTCD
        )) +
        {
          if (lines == TRUE) ggplot2::geom_line()
        } +
        {
          if (points == TRUE) ggplot2::geom_point()
        } +
        {
          if (log == TRUE) ggplot2::scale_y_log10()
        } +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::ggtitle(
          paste0("Study ", distinct(obj, .data$STUDYID), ", PC")
        ) +
        watermark()
    )
  }

  if (domain == "ex") {
    return(
      obj %>%
        arrange(.data$EXSTDTC) %>%
        mutate(ID = as.numeric(
          factor(.data$USUBJID, unique(.data$USUBJID))
        )) %>%
        ggplot2::ggplot() +
        ggplot2::geom_segment(ggplot2::aes(
          x = .data$EXSTDTC,
          xend = .data$EXENDTC,
          y = .data$ID,
          yend = .data$ID
        )) +
        {
          if (points == TRUE) {
            ggplot2::geom_point(ggplot2::aes(
              x = .data$EXSTDTC, y = .data$ID
            ))
          }
        } +
        {
          if (points == TRUE) {
            ggplot2::geom_point(ggplot2::aes(
              x = .data$EXENDTC, y = .data$ID
            ))
          }
        } +
        ggplot2::scale_y_discrete(name = "USUBJID", labels = NULL) +
        ggplot2::scale_x_datetime(
          name = "EXSTDTC - EXENDTC", date_labels = "%Y-%m-%d"
        ) +
        ggplot2::theme_bw() +
        {
          if (legend == TRUE) {
            ggplot2::theme(legend.position = "bottom")
          } else {
            ggplot2::theme(legend.position = "none")
          }
        } +
        ggplot2::ggtitle(paste0(
          "Study ", distinct(obj, .data$STUDYID), ", EX"
        )) +
        watermark()
    )
  }

  if (domain == "dm") {
    return(
      obj %>%
        filter(!is.na(.data$RFSTDTC)) %>%
        arrange(.data$RFSTDTC) %>%
        mutate(ID = as.numeric(factor(.data$USUBJID, unique(.data$USUBJID)))) %>%
        ggplot2::ggplot() +
        ggplot2::geom_segment(ggplot2::aes(
          x = .data$RFSTDTC,
          xend = .data$RFENDTC,
          y = .data$ID,
          yend = .data$ID
        )) +
        {
          if (points == TRUE) {
            ggplot2::geom_point(ggplot2::aes(
              x = .data$RFSTDTC, y = .data$ID
            ))
          }
        } +
        {
          if (points == TRUE) {
            ggplot2::geom_point(ggplot2::aes(
              x = .data$RFENDTC, y = .data$ID
            ))
          }
        } +
        ggplot2::scale_y_discrete(
          labels = NULL, breaks = NULL, name = "USUBJID"
        ) +
        ggplot2::scale_x_datetime(
          name = "RFSTDTC - RFENDTC", date_labels = "%Y-%m-%d"
        ) +
        ggplot2::theme_bw() +
        {
          if (legend == TRUE) {
            ggplot2::theme(legend.position = "bottom")
          } else {
            ggplot2::theme(legend.position = "none")
          }
        } +
        ggplot2::ggtitle(
          paste0("Study ", distinct(obj, .data$STUDYID), ", DM")
        ) +
        watermark()
    )
  }

  if (domain == "lb") {
    return(
      obj %>%
        filter(if (!is.null(analyte)) .data$LBTESTCD %in% analyte else TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$LBDTC,
          y = .data$LBSTRESN,
          group = interaction(.data$USUBJID, .data$LBTESTCD),
          color = .data$LBTESTCD
        )) +
        {
          if (lines == TRUE) ggplot2::geom_line()
        } +
        {
          if (points == TRUE) ggplot2::geom_point()
        } +
        {
          if (log == TRUE) ggplot2::scale_y_log10()
        } +
        ggplot2::scale_x_datetime(name = "LBDY", date_labels = "%Y-%m-%d") +
        ggplot2::theme_bw() +
        {
          if (legend == TRUE) {
            ggplot2::theme(legend.position = "bottom")
          } else {
            ggplot2::theme(legend.position = "none")
          }
        } +
        ggplot2::ggtitle(
          paste0("Study ", distinct(obj, .data$STUDYID), ", LB")
        ) +
        watermark()
    )
  }

  if (domain == "vs") {
    return(
      obj %>%
        filter(if (!is.null(analyte)) VSTESTCD %in% analyte else TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data$VSDTC,
          y = .data$VSSTRESN,
          group = interaction(.data$USUBJID, .data$VSTESTCD),
          color = .data$VSTESTCD
        )) +
        {
          if (lines == TRUE) ggplot2::geom_line()
        } +
        {
          if (points == TRUE) ggplot2::geom_point()
        } +
        {
          if (log == TRUE) ggplot2::scale_y_log10()
        } +
        ggplot2::theme_bw() +
        {
          if (legend == TRUE) {
            ggplot2::theme(legend.position = "bottom")
          } else {
            ggplot2::theme(legend.position = "none")
          }
        } +
        ggplot2::ggtitle
        (paste0("Study ", distinct(obj, .data$STUDYID), ", PC")) +
        watermark()
    )
  }

  if (!domain %in% c("pc", "dm", "lb", "vs", "ex")) {
    dtc_variable <- paste0(toupper(domain), "DTC")
    return(
      obj %>%
        group_by(.data$USUBJID) %>%
        filter(sum(!is.na({{ dtc_variable }})) > 0) %>%
        mutate(ID = cur_group_id()) %>%
        mutate(
          min_time = min(.data[[dtc_variable]], na.rm = TRUE),
          max_time = max(.data[[dtc_variable]], na.rm = TRUE)
        ) %>%
        arrange(.data$ID) %>%
        ggplot2::ggplot(ggplot2::aes(
          x = .data[[dtc_variable]], y = .data$ID
        )) +
        {
          if (points == TRUE) ggplot2::geom_point()
        } +
        ggplot2::geom_segment(ggplot2::aes(
          x = .data$min_time, xend = .data$max_time,
          y = .data$ID, yend = .data$ID
        )) +
        ggplot2::scale_y_discrete(labels = NULL, name = "USUBJID") +
        ggplot2::scale_x_datetime(
          name = dtc_variable, date_labels = "%Y-%m-%d"
        ) +
        {
          if (legend == TRUE) {
            ggplot2::theme(legend.position = "bottom")
          } else {
            ggplot2::theme(legend.position = "none")
          }
        } +
        ggplot2::theme_bw() +
        ggplot2::ggtitle(paste0(
          "Study ", distinct(obj, .data$STUDYID), ", ",
          toupper(domain)
        )) +
        watermark()
    )
  }
}


#' AE summary
#'
#' @param sdtm_data An sdtm object.
#' @param level The level to summarize by, as character. Can be one or multiple
#' of:
#'
#' * 'AETERM':   Reported term
#' * 'AELLT':    Lowest level term
#' * 'AEDECOD':  Dictionary-derived term
#' * 'AEHLT':    High level term
#' * 'AEBODSYS': Body system or organ class
#' * 'AESOC':    System organ class
#'
#' @param show_cd Show AE term code, as logical.
#' @param group Additional grouping variable, as character.
#' @param order_by_subj Order by number of subject, instead of by number of
#' event, as logical.
#' @param ae_filter A filter term as character.
#'
#' @return A data frame.
#' @export
ae_summary <- function(sdtm_data, level = "AESOC", show_cd = FALSE,
                       group = NULL, order_by_subj = F, ae_filter = "TRUE") {
  if (!all(level %in% c("AETERM", "AELLT", "AEDECOD", "AEHLT", "AEBODSYS",
                        "AESOC"))) {
    stop("unknown level!")
  }

  # level <- paste0("AE", level)
  level = toupper(level)
  ae <- sdtm_data %>% domain("ae")

  grouping <- c(level, group)
  if (show_cd == TRUE) {
    grouping <- c(grouping, paste0(level, "CD"))
  }

  ae %>%
    filter(eval(parse(text = ae_filter))) %>%
    group_by(across(all_of(grouping))) %>%
    summarize(n = n(), n_subj = n_distinct(.data$USUBJID), .groups = "drop") %>%
    {
      if (order_by_subj == T) {
        arrange(., desc(.data$n_subj))
      } else {
        arrange(., desc(.data$n))
      }
    } %>%
    as.data.frame()
}


#' Subject disposition overview
#'
#' @param sdtm_data SDTM data object.
#'
#' @return A data frame
#' @export
#' @examples
#' disposition_summary(examplinib_sad)
disposition_summary <- function(sdtm_data) {
  dm <- domain(sdtm_data, "dm")
  if (is.null(dm)) stop("DM not found in SDTM data!")

  dm %>%
    assertr::verify(assertr::has_all_names(
      "USUBJID", "ACTARMCD", "RFSTDTC", "RFENDTC"
    )) %>%
    distinct(.data$USUBJID, .data$ACTARMCD, .data$RFSTDTC, .data$RFENDTC) %>%
    mutate(ONGOING = case_when(
      .data$RFSTDTC != "" & .data$RFENDTC == "" ~ TRUE,
      .data$RFSTDTC != "" & .data$RFENDTC != "" ~ FALSE
    )) %>%
    select(-all_of(c("RFSTDTC", "RFENDTC"))) %>%
    reframe(N = n(), .by = c("ACTARMCD", "ONGOING")) %>%
    arrange(.data$ACTARMCD, -.data$ONGOING)
}
