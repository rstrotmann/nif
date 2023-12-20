#' Test whether string represents ISO 8601-formatted date-time
#'
#' @param x The input as character.
#'
#' The expected format is "dddd-dd-ddTdd:dd" with "d" a digit. This function
#' tests whether the above is part of the input, i.e., dat-time formats that
#' also include seconds information are also recognized.
#'
#' @return Boolean.
#' @export
#'
#' @examples
#' is.iso_date_time("2023-09-27T15:04")
#' is.iso_date_time("2023-09-27T15:04:00")
#' is.iso_date_time(c("2023-03-21T11:55", "2023-07-18"))
is.iso_date_time <- function(x) {
  str_detect(x, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}")
}


#' Test whether string represents ISO 8601-formatted date
#'
#' The expected format is "dddd-dd-dd" with "d" a digit. This function tests
#' whether the above is part of the input, i.e., ISO 8601-formatted date-time
#' objects like "dddd-dd-ddTdd:dd" are also recognized.
#'
#' @param x The input as character.
#'
#' @return Boolean.
#' @export
#'
#' @examples
#' is.iso_date("2023-09-27")
#' is.iso_date(c("2023-03-21T11:55", "2023-07-18"))
is.iso_date <- function(x) {
  str_detect(x, "\\d{4}-\\d{2}-\\d{2}")
}


#' Check for incomplete date format in columns ending with "DTC"
#'
#' @param obj The SDTM domain as data frame
#' @param verbose Boolean to indicate whether to include details.
#'
#' @return The unchanged SDTM domain.
#' @export
check_date_format <- function(obj, verbose=T) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~!(is.iso_date(.) | .==""))) %>%
    select(c(USUBJID, DOMAIN, ends_with("DTC")))

  if(nrow(temp)>0) {
    out <- paste0(domain, ": Incomplete date format in ", nrow(temp), " rows")
    if(verbose) {
      out <- paste0(out, ":\n", df.to.string(temp), "\n")}
    message(out)
  }
  return(obj)
}


#' Filter out any rows containing DTC rows with incomplete data format
#'
#' @param obj The SDTM domain as data frame
#' @param verbose Boolean to indicate whether to include details.
#' @param silent Boolean to indicate whether message output is produced.
#'
#' @return The filtered SDTM domain as data frame.
#' @export
filter_correct_date_format <- function(obj, verbose=T, silent=F) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~!(is.iso_date(.) | .=="")))

  if(nrow(temp)>0) {
    out <- paste0(domain, ": ", nrow(temp),
      " rows containing DTC fields with incomplete date format ignored!")
    if(verbose & !silent) {
      out <- paste0(out, "\n")}
    message(out)
    obj <- obj %>%
      filter(if_all(ends_with("DTC"), ~(is.iso_date(.) | .=="")))
  }
  return(obj)
}


#' Check for incomplete date-time format in columns ending with "DTC"
#'
#' @param obj The SDTM domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#'
#' @return The unchanged SDTM domain.
#' @export
check_date_time_format <- function(obj, verbose=T) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~!(is.iso_date_time(.) | .==""))) %>%
    select(c(USUBJID, DOMAIN, ends_with("DTC")))

  if(nrow(temp)>0) {
    out <- paste0(domain, ": Incomplete date-time format in ", nrow(temp), " rows")
    if(verbose) {
      out <- paste0(out, ":\n", df.to.string(temp), "\n")}
    message(out)
  }
  return(obj)
}


#' Check for missing time in columns ending with "DTC"
#'
#' @param obj The SDTM domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#'
#' @return The unchanged SDTM domain.
#' @export
check_missing_time <- function(obj, verbose=T) {
  domain <- obj %>% distinct(DOMAIN)
  temp <- obj %>%
    filter(if_any(ends_with("DTC"), ~is.iso_date(.) & !(is.iso_date_time(.)))) %>%
    select(c(USUBJID, DOMAIN, ends_with("DTC")))

  if(nrow(temp)>0) {
    out <- paste0(domain, ": Missing time in ", nrow(temp), " rows")
    if(verbose) {
      out <- paste0(out, ":\n", df.to.string(temp), "\n")}
    message(out)
  }
  return(obj)
}


# sdtm_table_missing_EXENDTC_time <- function(ex) {
#   ex %>%
#     verify(has_all_names("STUDYID", "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC",
#                          "EXENDTC", "EXDOSE", "EPOCH")) %>%
#     lubrify_dates() %>%
#     mutate(EXSTDTC_has_time=has_time(EXSTDTC),
#            EXENDTC_has_time=has_time(EXENDTC)) %>%
#
#     # extract start and end dates and times
#     mutate(start.date=extract_date(EXSTDTC)) %>%
#     dplyr::mutate(start.time=case_when(
#       EXSTDTC_has_time==T ~ extract_time(EXSTDTC),
#       .default=NA)) %>%
#
#     mutate(end.date=extract_date(EXENDTC)) %>%
#     dplyr::mutate(end.time=case_when(
#       EXENDTC_has_time==T ~ extract_time(EXENDTC),
#       .default=NA)) %>%
#
#     mutate(impute_EXENDTC_time = !is.na(EXENDTC) & EXENDTC_has_time==FALSE &
#                                     !is.na(start.time))
# }


#' Check EXENDTC for the last administration.
#'
#' Absent EXENDTC in the last administration by subject and treatment may
#' indicate that at the time of the data cut-off, this subject was still on
#' treatment.
#'
#' @param ex The SDTM EX domain as data frame.
#' @param verbose Boolean to indicate whether to include details.
#'
#' @return The unchanged EX domain.
#' @export
check_last_EXENDTC <- function(ex, verbose=T) {
  domain <- ex %>% distinct(DOMAIN)

  temp <- ex %>%
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    lubrify_dates() %>%
    group_by(USUBJID, EXTRT) %>%
    arrange(USUBJID, EXTRT, EXSTDTC) %>%
    mutate(LAST_ADMIN=row_number()==max(row_number())) %>%
    filter(LAST_ADMIN==T, is.na(EXENDTC)) %>%
    select(USUBJID, DOMAIN, EXTRT, EXSTDTC, EXENDTC)

  if(nrow(temp)>0) {
    out <- paste0(domain, ": Missing EXENDTC for the last administration in ",
                  nrow(temp), " rows")
    if(verbose) {
      out <- paste0(out, ":\n", df.to.string(temp), "\n")}
    message(out)
  }
  return(ex)
}


#' Check SDTM domains for missing date time information.
#'
#' @param sdtm The SDTM as SDTM object.
#' @param verbose Boolean to indicate whether to include details.
#'
#' @return Nothing.
#' @export
check_sdtm <- function(sdtm, verbose=T) {
  ## Date-times in DM
  dummy <- sdtm %>%
    domain("dm") %>%
    filter(ACTARMCD!="SCRNFAIL") %>%
    select(USUBJID, DOMAIN, RFENDTC) %>%
    check_date_format(verbose=verbose) %>%
    check_missing_time(verbose=verbose)

  ## Date-times in EX
  dummy <- sdtm %>%
    domain("ex") %>%
    check_date_format(verbose=verbose) %>%
    filter_correct_date_format(verbose=verbose) %>%
    check_missing_time(verbose=verbose) %>%
    check_last_EXENDTC(verbose=verbose)

  ## Date-times in PC
  dummy <- sdtm %>%
    domain("pc") %>%
    select(USUBJID, DOMAIN, PCDTC) %>%
    check_date_format(verbose=verbose) %>%
    filter_correct_date_format(verbose=verbose) %>%
    check_missing_time(verbose=verbose)
}









