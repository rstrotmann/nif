#' Test whether string represents ISO 8601-formatted date-time
#'
#' The expected format is "dddd-dd-ddTdd:dd" with "d" a digit. This function
#' tests whether the above is part of the input, i.e., dat-time formats that
#' also include seconds information are also recognized.
#' @param x The input as character.
#' @return Boolean.
#' @export
#' @keywords internal
#' @examples
#' is_iso_date_time("2023-09-27T15:04")
#' is_iso_date_time("2023-09-27T15:04:00")
#' is_iso_date_time(c("2023-03-21T11:55", "2023-07-18"))
is_iso_date_time <- function(x) {
  str_detect(x, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}")
}


#' Test whether string represents ISO 8601-formatted date
#'
#' The expected format is "dddd-dd-dd" with "d" a digit. This function tests
#' whether the above is part of the input, i.e., ISO 8601-formatted date-time
#' objects like "dddd-dd-ddTdd:dd" are also recognized.
#' @param x The input as character.
#' @return Boolean.
#' @export
#' @keywords internal
#' @examples
#' is_iso_date("2023-09-27")
#' is_iso_date(c("2023-03-21T11:55", "2023-07-18"))
is_iso_date <- function(x) {
  str_detect(x, "\\d{4}-\\d{2}-\\d{2}")
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
    select(c(.data$USUBJID, .data$DOMAIN, ends_with("DTC")))

  if (nrow(temp) > 0) {
    out <- paste0(domain, ": Incomplete date format in ", nrow(temp), " rows")
    if (verbose) {
      out <- paste0(out, ":\n", df_to_string(temp), "\n")
    }
    message(out)
  }
  return(obj)
}


#' Filter out any rows containing DTC rows with incomplete data format as per
#' ISO 8601
#'
#' @param obj The SDTM domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#' @param silent Boolean to indicate whether message output is produced.
#' @return The filtered SDTM domain as data frame.
#' @keywords internal
#' @export
filter_correct_date_format <- function(obj, verbose = TRUE, silent = FALSE) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~ !(is_iso_date(.) | . == "")))

  if (nrow(temp) > 0) {
    out <- paste0(
      domain, ": ", nrow(temp),
      " rows containing DTC fields with incomplete date format ignored!"
    )
    if (verbose && !silent) {
      out <- paste0(out, "\n")
    }
    message(out)
    obj <- obj %>%
      filter(if_all(ends_with("DTC"), ~ (is_iso_date(.) | . == "")))
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
    select(c(.data$USUBJID, .data$DOMAIN, ends_with("DTC")))

  if (nrow(temp) > 0) {
    out <- paste0(domain, ": Incomplete date-time format in ", nrow(temp),
                  " rows")
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
    filter(if_any(ends_with("DTC"),
                  ~ is_iso_date(.) & !(is_iso_date_time(.)))) %>%
    select(c(.data$USUBJID, .data$DOMAIN, ends_with("DTC")))

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
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    lubrify_dates() %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    filter(LAST_ADMIN == TRUE, is.na(.data$EXENDTC)) %>%
    select(.data$USUBJID, .data$DOMAIN, .data$EXTRT, .data$EXSTDTC,
           .data$EXENDTC)

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
    select(.data$USUBJID, .data$DOMAIN, .data$RFENDTC) %>%
    check_date_format(verbose = verbose) %>%
    check_missing_time(verbose = verbose)

  ## Date-times in EX
  sdtm %>%
    domain("ex") %>%
    check_date_format(verbose = verbose) %>%
    filter_correct_date_format(verbose = verbose) %>%
    check_missing_time(verbose = verbose) %>%
    check_last_exendtc(verbose = verbose)

  ## Date-times in PC
  sdtm %>%
    domain("pc") %>%
    select(.data$USUBJID, .data$DOMAIN, .data$PCDTC) %>%
    check_date_format(verbose = verbose) %>%
    filter_correct_date_format(verbose = verbose) %>%
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
#' @param log Boolean whether to use a logarithmic y axis.
#' @return Nothing.
#' @export
#' @examples
#' plot(examplinib_poc)
#' plot(examplinib_poc, "dm")
#' plot(examplinib_poc, domain="ex")
#' plot(examplinib_poc, domain="pc")
#' plot(examplinib_poc, domain="vs", lines = FALSE, points = TRUE)
plot.sdtm <- function(x, domain = "dm", usubjid = NULL, lines = TRUE,
                      points = FALSE, analyte = NULL, log = FALSE, ...) {
  obj <- x$domains[[domain]] %>%
    filter(if (!is.null(usubjid)) .data$USUBJID %in% usubjid else TRUE) %>%
    lubrify_dates() %>%
    as.data.frame()

  if (domain == "pc") {
    return(
      obj %>%
        filter (if(!is.null(analyte)) .data$PCTESTCD %in% analyte else TRUE) %>%
        ggplot(aes(
          x = .data$PCDTC,
          y = .data$PCSTRESN,
          group = interaction(.data$USUBJID, .data$PCTESTCD),
          color = .data$PCTESTCD)) +
      {if (lines == TRUE) geom_line()} +
      {if (points == TRUE) geom_point()} +
      {if (log == TRUE) scale_y_log10()} +
      theme_bw() +
      theme(legend.position = "bottom") +
        ggtitle(paste0("Study ", distinct(obj, .data$STUDYID), ", PC"))
    )
  }

  if (domain == "ex") {
    return(
      obj %>%
        arrange(desc(.data$EXSTDTC)) %>%
        group_by(.data$USUBJID) %>%
        mutate(ID = cur_group_id()) %>%
        arrange(.data$ID) %>%
        ggplot() +
        geom_segment(aes(x = .data$EXSTDTC,
                         xend = .data$EXENDTC,
                         y = .data$ID,
                         yend = .data$ID)) +
        {if (points == TRUE) geom_point(aes(x = .data$EXSTDTC, y = .data$ID))} +
        {if (points == TRUE) geom_point(aes(x = .data$EXENDTC, y = .data$ID))} +
        scale_y_discrete(name="USUBJID", labels=NULL) +
        scale_x_datetime(name="EXSTDTC - EXENDTC", date_labels = "%Y-%m-%d") +
        theme_bw() +
        theme(legend.position = "bottom") +
        ggtitle(paste0("Study ", distinct(obj, .data$STUDYID), ", EX"))
    )
  }

  if (domain == "dm") {
    return(
      obj %>%
        group_by(.data$USUBJID) %>%
        mutate(ID = cur_group_id()) %>%
        arrange(.data$ID) %>%
        ggplot() +
        geom_segment(aes(x = .data$RFSTDTC,
                         xend = .data$RFENDTC,
                         y = .data$ID,
                         yend = .data$ID)) +
        scale_y_discrete(labels = NULL, name = "USUBJID") +
        scale_x_datetime(name = "RFSTDTC - RFENDTC", date_labels = "%Y-%m-%d") +
        theme_bw() +
        ggtitle(paste0("Study ", distinct(obj, .data$STUDYID), ", DM"))
    )
  }

  if (domain == "lb") {
    return(
      obj %>%
        filter(if(!is.null(analyte)) .data$LBTESTCD %in% analyte else TRUE) %>%
        ggplot(aes(
          x = .data$LBDTC,
          y = .data$LBSTRESN,
          group = interaction(.data$USUBJID, .data$LBTESTCD),
          color = .data$LBTESTCD)) +
        {if (lines == TRUE) geom_line()} +
        {if (points == TRUE) geom_point()} +
        {if (log == TRUE) scale_y_log10()} +
        scale_x_datetime(name="LBDY", date_labels = "%Y-%m-%d") +
        theme_bw() +
        theme(legend.position = "bottom") +
        ggtitle(paste0("Study ", distinct(obj, .data$STUDYID), ", LB"))
    )
  }

  if (domain == "vs") {
    return(
      obj %>%
        filter (if(!is.null(analyte)) VSTESTCD %in% analyte else TRUE) %>%
        ggplot(aes(
          x = .data$VSDTC,
          y = .data$VSSTRESN,
          group = interaction(.data$USUBJID, .data$VSTESTCD),
          color = .data$VSTESTCD)) +
        {if (lines == TRUE) geom_line()} +
        {if (points == TRUE) geom_point()} +
        {if (log == TRUE) scale_y_log10()} +
        theme_bw() +
        theme(legend.position = "bottom") +
        ggtitle(paste0("Study ", distinct(obj, .data$STUDYID), ", PC"))
    )
  }

  if(!domain %in% c("pc", "dm", "lb", "vs", "ex")) {
    dtc_variable <- paste0(toupper(domain), "DTC")
    return(
      obj %>%
        group_by(.data$USUBJID) %>%
        filter(sum(!is.na({{dtc_variable}})) > 0) %>%
        mutate(ID = cur_group_id()) %>%
        mutate(min_time = min(.data[[dtc_variable]], na.rm = TRUE),
               max_time = max(.data[[dtc_variable]], na.rm = TRUE)) %>%
        arrange(.data$ID) %>%
        ggplot(aes(x = .data[[dtc_variable]], y = .data$ID)) +
        {if (points == TRUE) geom_point()} +
        geom_segment(aes(x = .data$min_time, xend = .data$max_time,
                         y = .data$ID, yend = .data$ID)) +
        scale_y_discrete(labels = NULL, name = "USUBJID") +
        scale_x_datetime(name = dtc_variable, date_labels = "%Y-%m-%d") +
        theme_bw() +
        ggtitle(paste0("Study ", distinct(obj, .data$STUDYID), ", ",
                       toupper(domain)))
    )
  }
}
