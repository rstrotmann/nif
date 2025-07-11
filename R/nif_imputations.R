#' Impute very last EXENDTC for a subject and EXTRT to RFENDTC, if absent
#'
#' In EX for multiple-dose studies, the EXENDTC field for the very last
#' administration epoch may be missing. This is occasionally found when SDTM
#' data are generated before full cleaning of the clinical data, e.g., in the
#' case of interim analyses of clinical study data. In some of these cases, the
#' DM domain is however already completed with the RFENDTC field. This is the
#' reference end date-time field that specifies the date-time of the last
#' treatment administration. This function completes EXENDTC for the very last
#' administration time based on the RFENDTC, if provided in DM.
#'
#' @param ex The EX domain as data frame.
#' @param silent Suppress messages, defaults to nif_options setting if NULL.
#' @param dm The DM domain as data frame.
#'
#' @return The updated EX domain as data frame.
#' @keywords internal
#' @export
impute_exendtc_to_rfendtc <- function(ex, dm, silent = NULL) {
  # Validate input
  expected_dm_cols <- c("USUBJID", "RFSTDTC", "RFENDTC")
  missing_dm_cols <- setdiff(expected_dm_cols, names(dm))
  n = length(missing_dm_cols)
  if(n > 0)
    stop(paste0("Missing ", plural("colum", n > 1), " in domain DM: ",
                nice_enumeration(missing_dm_cols)))

  expected_ex_columns <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
  missing_ex_columns <- setdiff(expected_ex_columns, names(ex))
  n = length(missing_ex_columns)
  if(n > 0)
    stop(paste0("Missing ", plural("colum", n > 1), " in domain EX: ",
                nice_enumeration(missing_ex_columns)))

  if(!"IMPUTATION" %in% names(ex)) {
    ex <- mutate(ex, IMPUTATION = "")
  }

  temp <- ex %>%
    arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC) %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    left_join(dm %>% select(c("USUBJID", "RFSTDTC", "RFENDTC")),
              by = "USUBJID") %>%
    ungroup()

  replace_n <- temp %>%
    filter(is.na(.data$EXENDTC) & !is.na(.data$RFENDTC) & LAST_ADMIN == TRUE) %>%
    nrow()

  if (replace_n > 0) {
    conditional_message(
      replace_n, " ",
      plural("subject", replace_n > 1),
      " had a missing EXENDTC in their final administration episode.\n",
      "In these cases, EXENDTC was imputed to RFENDTC:\n",
      df_to_string(
        temp %>%
          filter(is.na(.data$EXENDTC) & !is.na(.data$RFENDTC) &
                   .data$LAST_ADMIN == TRUE) %>%
          select(any_of(c("USUBJID", "EXTRT", "EXSEQ", "EXSTDTC", "EXENDTC", "RFENDTC"))),
        indent = 2
      ), "\n",
      silent = silent
    )

    temp %>%
      mutate(IMPUTATION = case_when(
        (.data$LAST_ADMIN == TRUE & is.na(.data$EXENDTC) &
           !is.na(.data$RFENDTC)) ~ "missing EXENDTC set to RFENDTC",
        .default = .data$IMPUTATION
      )) %>%
      mutate(EXENDTC = case_when(
        (.data$LAST_ADMIN == TRUE & is.na(.data$EXENDTC) &
           !is.na(.data$RFENDTC)) ~ .data$RFENDTC,
        .default = .data$EXENDTC
      )) %>%
      select(-c("LAST_ADMIN", "RFENDTC", "RFSTDTC"))
  } else {
    ex
  }
}


#' Impute missing EXENDTC to the day before the next EXSTDTC.
#'
#' In some cases, EX does not contain EXENDTC for administration episodes that
#' are not the very last administration episode. This should only occur when
#' non-clean clinical data is analyzed, e.g., in the context of an interim
#' analysis. In most cases, such instances must be manually resolved. There
#' could be AE information with consequences of "drug withdrawn" available
#' that may be helpful, or other information from clinical context can be used
#' to determine how many IMP administrations were done in the respective
#' interval. This function should only be used if no other information is
#' available as it takes a worst-case approach, i.e., assumes that IMP was
#' administered up to the day before the subsequent administration interval.
#' Note that this imputation does not apply to the last administration per
#' subject and EXTRT. For these cases, missing EXENDTC can be imputed to the
#' global cut off date using [impute_exendtc_to_cutoff()].
#'
#' As this function conducts rather aggressive imputations, the message output
#' is not optional, i.e., cannot be suppressed using the global 'silent' option,
#' but is issued in all cases.
#'
#' @param ex The updated EX domain as data frame.
#' @return The updated EX domain as data frame.
#' @keywords internal
#' @export
impute_missing_exendtc <- function(ex) {
  # Input validation
  expected_columns <- c("USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC")
  missing_columns <- setdiff(expected_columns, names(ex))
  n = length(missing_columns)
  if(n > 0)
    stop(paste0("Missing ", plural("colum", n > 1), " in domain EX: ",
                nice_enumeration(missing_columns)))

  if(!"IMPUTATION" %in% names(ex)) {
    ex <- mutate(ex, IMPUTATION = "")
  }

  temp <- ex %>%
    lubrify_dates() %>%
    arrange(.data$USUBJID, .data$EXSTDTC) %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    mutate(next_start = lead(.data$EXSTDTC)) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    ungroup()

  to_replace <- temp %>%
    filter(is.na(.data$EXENDTC) & .data$LAST_ADMIN == FALSE)

  if (nrow(to_replace) > 0) {
    message(
      nrow(to_replace),
      " rows in EX had no EXENDTC. These values are imputed as the day ",
      "before\nthe next EXSTDTC. The following entries are affected:\n",
      df_to_string(select(to_replace, c("USUBJID", "EXSEQ", "EXTRT",
                                        "EXSTDTC", "EXENDTC")), indent = 2),
      "\n"
    )

    temp <- temp %>%
      mutate(imputation_flag = (is.na(.data$EXENDTC) &
                                  .data$LAST_ADMIN == FALSE)) %>%
      mutate(EXENDTC = case_match(.data$imputation_flag,
                                  TRUE ~ .data$next_start - days(1),
                                  FALSE ~ .data$EXENDTC)) %>%
      mutate(IMPUTATION = case_match(.data$imputation_flag,
                                     TRUE ~ "EXENDTC imputed as the day before the next EXSTDTC",
                                     FALSE ~ .data$IMPUTATION)) %>%
      select(-"imputation_flag")
  }

  temp %>%
    select(-c("next_start", "LAST_ADMIN"))
}


#' Impute last EXENDTC per subject and treatment to cutoff date when absent.
#'
#' In some instances, particularly when analyzing non-cleaned SDTM data from
#' ongoing clinical studies with multiple-dose administrations, the last
#' administration epoch in EX may have an empty EXENDTC field. Often, the
#' underlying reason is that the respective subjects are still on treatment.
#' This function replaces the missing EXENDTC with the global data cut-off
#' date, `cut_off_date`.
#'
#' @param ex The EX domain as data frame.
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#' @param cut_off_date The cut-off date.
#'
#' @return The updated EX domain as data frame.
#' @keywords internal
#' @export
impute_exendtc_to_cutoff <- function(ex, cut_off_date = NA, silent = NULL) {
  # input validation
  expected_columns <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
  missing_columns <- setdiff(expected_columns, names(ex))
  n = length(missing_columns)
  if(n > 0)
    stop(paste0(
      "Missing ", plural("colum", n > 1), " in domain EX: ",
      nice_enumeration(missing_columns)))

  temp <- ex %>%
    lubrify_dates() %>%
    assertr::verify(is.POSIXct(.data$EXSTDTC)) %>%
    assertr::verify(is.POSIXct(.data$EXENDTC)) %>%

    # identify last administration per subject and EXTRT
    arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC) %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    mutate(flag = .data$LAST_ADMIN == TRUE & is.na(.data$EXENDTC)) %>%
    ungroup()

  to_replace <- temp %>%
    filter(.data$flag == TRUE)

  if (nrow(to_replace) > 0) {
    conditional_message(
      "In ", nrow(to_replace), " subjects, EXENDTC for the ",
      "last row is absent and is replaced by the cut off date, ",
      format(cut_off_date, format = "%Y-%m-%d %H:%M"), ":\n",
      df_to_string(
        to_replace %>%
          select(all_of(c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")))), "\n",
      silent = silent)

    temp <- temp %>%
      mutate(EXENDTC = case_when(
        .data$flag == TRUE ~ cut_off_date,
        .default = .data$EXENDTC)) %>%
      mutate(IMPUTATION = case_when(
        .data$flag == TRUE ~ "missing EXENDTC set to data cutoff",
        .default = .data$IMPUTATION))
  }

  return(temp %>% select(-c("LAST_ADMIN", "flag")))
}


#' Derive administration time from PCRFTDTC
#'
#' @param obj A data frame.
#' @param pc The corresponding PC domain as data frame.
#' @param analyte The analyte as character.
#' @param pctestcd The PCTESTCD corresponding to the analyte as character.
#' @param silent Suppress messages, defaults to nif_option setting, if NULL.
#'
#' @return A data frame.
#' @keywords internal
#' @export
impute_admin_times_from_pcrftdtc <- function(
    obj, pc, analyte, pctestcd, silent = NULL) {

  pc_ref <- pc %>%
    lubrify_dates() %>%
    filter(PCTESTCD == pctestcd) %>%
    mutate(ANALYTE = analyte) %>%
    decompose_dtc("PCRFTDTC") %>%
    select(c("USUBJID", "ANALYTE", "PCRFTDTC_date",
             "PCRFTDTC_time")) %>%
    distinct()

  temp <- obj %>%
    lubrify_dates() %>%
    decompose_dtc("DTC") %>%
    left_join(pc_ref,
              by = c("USUBJID", "ANALYTE", "DTC_date" = "PCRFTDTC_date"))

  if(!"IMPUTATION" %in% names(temp)) {
    temp <- mutate(temp, IMPUTATION = "")
  }

  temp <- temp %>%
    mutate(IMPUTATION = case_when(
      !is.na(.data$PCRFTDTC_time) & is.na(.data$DTC_time) ~
        "admin time copied from PCRFTDTC",
      .default = .data$IMPUTATION)) %>%

    mutate(DTC_time = case_when(
      !is.na(.data$PCRFTDTC_time) & is.na(.data$DTC_time) ~ .data$PCRFTDTC_time,
      .default = .data$DTC_time))

  # Rows with conflicting DTC and PCRFTDTC
  conflicting_rows <- which(
    !is.na(temp$PCRFTDTC_time) &
      !is.na(temp$DTC_time) &
      temp$DTC_time != temp$PCRFTDTC_time)

  if(length(conflicting_rows) != 0) {
    conditional_message(paste0(
      "Analyte ", analyte, ": ",
      "Conflicting administration time and PCRFTDTC. PCRFTDTC. was prioritized!\n",
      df_to_string(
        temp[conflicting_rows, c("USUBJID", "ANALYTE", "DTC",
                                 "PCRFTDTC_time", "IMPUTATION")], indent = 2)),
      silent = silent)

    temp <- temp %>%
      mutate(DTC_time = case_when(
        row_number() %in% conflicting_rows ~ .data$PCRFTDTC_time,
        .default = .data$DTC_time)) %>%
      mutate(IMPUTATION = case_when(
        row_number() %in% conflicting_rows ~
          "admin time from PCRFTDTC prioritized",
        .default = .data$IMPUTATION))
  }

  temp <- temp %>%
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    select(-c("PCRFTDTC_time", "DTC_date", "DTC_time"))
}


#' Remove administrations with EXSTDTC after EXENDTC
#'
#' @param ex The ex domain as data frame.
#' @param dm The dm domain as data frame.
#' @param silent Suppress messages, defaults to nif_option setting, if NULL.
#'
#' @return A data frame.
#' @keywords internal
#' @export
filter_EXSTDTC_after_EXENDTC <- function(ex, dm, silent = NULL) {
  temp <- ex %>%
    filter(.data$EXSTDTC > .data$EXENDTC) %>%
    left_join(
      dm %>%
        select("USUBJID", "RFENDTC"),
      by = "USUBJID"
    )

  if(nrow(temp) > 0) {
    conditional_message(paste0(
      nrow(temp),
      " administration episodes had an EXENDTC before the EXSTDTC and were\n",
      "removed from the data set:\n",
      df_to_string(select(
        temp, "USUBJID", "EXTRT", "EXSEQ", "EXSTDTC", "EXENDTC", "RFENDTC"),
        indent = 2),
      "\n"), silent = silent)
  }
  ex %>%
    filter(.data$EXSTDTC <= .data$EXENDTC)
}
