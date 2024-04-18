#' Impute time of EXENDTC to time of EXSTDTC, when missing.
#'
#' Within the EX domain, some entries in EXENDTC may only have date but not time
#' information included in the EXENDTC field. This is particularly often found
#' in multiple-dose studies, and in cases where the affected administration
#' interval is not associated with PK sampling, the exact time of these IMP
#' administrations does not matter a lot. However, to convert the EXSTDTC to
#' EXENDTC interval into a series of relative administration times, the time of
#' the day in EXENDTC must be set to a plausible value. This is what this
#' function does. For entries with missing time-of-the-day information in
#' EXENDTC, it is assumed to be the same as for the EXSTDTC field.
#' @param ex The EX domain as data frame.
#' @param silent Switch to disable message output.
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_missing_exendtc_time <- function(ex, silent = FALSE) {
  temp <- ex %>%
    verify(has_all_names(
      "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    lubrify_dates() %>%
    mutate(
      EXSTDTC_has_time = has_time(EXSTDTC),
      EXENDTC_has_time = has_time(EXENDTC)
    ) %>%
    # extract start and end dates and times from EXSTDTC and EXENDTC
    mutate(start.date = extract_date(EXSTDTC)) %>%
    mutate(start.time = case_when(
      EXSTDTC_has_time == TRUE ~ extract_time(EXSTDTC),
      .default = NA
    )) %>%
    mutate(end.date = extract_date(EXENDTC)) %>%
    mutate(end.time = case_when(
      EXENDTC_has_time == TRUE ~ extract_time(EXENDTC),
      .default = NA
    )) %>%
    # flag entries for EXENDTC time imputation
    mutate(impute_exendtc_time = (!is.na(EXENDTC) &
                                    EXENDTC_has_time == FALSE &
                                    !is.na(start.time)))

  if (sum(temp$impute_exendtc_time) > 0) {
    temp <- temp %>%
      mutate(EXENDTC1 = case_when(
        impute_exendtc_time == TRUE ~ paste(
          as.character(end.date),
          as.character(start.time)
        ), .default = NA) %>%
        str_trim() %>%
        as_datetime(format = c("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"))) %>%

      mutate(EXENDTC = case_when(
        impute_exendtc_time == TRUE ~ EXENDTC1,
        .default = EXENDTC
      )) %>%
      select(-c("EXENDTC1")) %>%
      mutate(IMPUTATION = case_when(
        impute_exendtc_time == TRUE ~ "time imputed from EXSTDTC",
        .default = .data$IMPUTATION
      ))

    conditional_message(
      "In ", sum(temp$impute_exendtc_time),
      " administrations, a missing time in EXENDTC is imputed from EXSTDTC:\n",
      temp %>%
        filter(impute_exendtc_time == TRUE) %>%
        select(c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
        df_to_string(), "\n",
      silent = silent
    )
  }

  temp %>%
    select(-c("EXENDTC_has_time", "EXSTDTC_has_time", "start.date",
              "start.time", "end.date", "end.time", "impute_exendtc_time"))
}


#' Filter out EX events after the last dose as specified in RFENDTC
#'
#' @param ex The EX domain as data.frame.
#' @param dm The DM domain as data.frame
#' @param silent Switch to disable message output.
#' @return The modified EX domain as data.frame.
#' @keywords internal
exclude_exstdtc_after_rfendtc <- function(ex, dm, silent = FALSE) {
  ex %>%
    left_join(dm %>% select(c("USUBJID", "RFENDTC")),
              by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    filter(floor_date(.data$EXSTDTC, "day") <=
             floor_date(.data$RFENDTC, "day") |
             is.na(.data$RFENDTC)) %>%
    select(-"RFENDTC")
}


#' Impute very last EXENDTC for a subject and EXTRT to RFENDTC if absent
#'
#' In EX for multiple-dose studies, the EXENDTC field for the very last
#' administration epoch may be missing. This is occasionally found when SDTM
#' data are generated before full cleaning of the clinical data, i.e., in the
#' case of interim analyses of clinical study data. In some of these cases, the
#' DM domain is however already completed with the RFENDTC field. This is the
#' reference end date-time field that specifies the date-time of the last
#' treatment administration. This function completes EXENDTC for the very last
#' administration time based on the RFENDTC, if provided in DM.
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @param silent Switch to disable message output.
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_exendtc_to_rfendtc <- function(ex, dm, silent = FALSE) {
  dm %>%
    verify(has_all_names("USUBJID", "RFSTDTC", "RFENDTC"))

  temp <- ex %>%
    verify(has_all_names("USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
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
      replace_n,
      " subjects had a missing EXENDTC in their final administration episode.\n",
      "In these cases, EXENDTC was imputed to RFENDTC:\n",
      df_to_string(
        temp %>%
          filter(is.na(.data$EXENDTC) & !is.na(.data$RFENDTC) &
                   .data$LAST_ADMIN == TRUE) %>%
          select(c("USUBJID", "EXTRT", "EXSEQ", "EXSTDTC", "EXENDTC", "RFENDTC")),
        indent = "  "
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
#' In some cases, EX does not contain EXENDTC for administration epochs that
#' are not the very last administration epoch This should only occur when
#' non-clean clinical data is analyzed, e.g., in the context of an interim
#' analysis. In most cases, such instances must be manually resolved. There
#' could be AE information with consequences of "drug withdrawn" available
#' that may be helpful, or other information from clinical context can be used
#' to determine how many IMP administrations were done in the respective
#' interval. This function should only be used if no other information is
#' available as it takes a worst-case approach, i.e., assumes that IMP was
#' administered up to the day before the subsequent administration interval.
#' Note that this imputation does not apply to the last administration per
#' subject and EXTRT. For these cases, missing EXENDT can be imputed to the
#' global cut off date using `impute_exendtc_to_cutoff`.
#'
#' As this function conducts rather aggressive imputations, the message output
#' is not optional, i.e., cannot be suppressed using the `silent` flag, but is
#' issued in all cases.
#'
#' @param ex The updated EX domain as data frame.
#' @param silent Switch to disable message output.
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_missing_exendtc <- function(ex, silent = FALSE) {
  temp <- ex %>%
    assertr::verify(has_all_names("USUBJID", "EXSEQ", "EXTRT", "EXSTDTC",
                                  "EXENDTC")) %>%
    lubrify_dates() %>%
    arrange(.data$USUBJID, .data$EXSTDTC) %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    mutate(next_start = lead(.data$EXSTDTC)) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    ungroup()

  to_replace <- temp %>%
    filter(is.na(.data$EXENDTC) & .data$LAST_ADMIN == FALSE)

  if (nrow(to_replace > 0)) {
    message(
      nrow(to_replace),
      " rows in EX had no EXENDTC. These values are imputed as the day ",
      "before\nthe next EXSTDTC. The following entries are affected:\n",
      df_to_string(select(to_replace, c("USUBJID", "EXSEQ", "EXTRT",
                            "EXSTDTC", "EXENDTC")), indent = "  ")
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
#' date, `cut.off.date`.
#'
#' @param ex The EX domain as data frame.
#' @param cut.off.date The cut-off date.
#' @param silent Switch to disable message output.
#' @return The updated EX domain as data frame.
#' @import assertr
#' @keywords internal
impute_exendtc_to_cutoff <- function(ex, cut.off.date = NA, silent = FALSE) {
  temp <- ex %>%
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    lubrify_dates() %>%
    assertr::verify(is.POSIXct(.data$EXSTDTC)) %>%
    assertr::verify(is.POSIXct(.data$EXENDTC)) %>%
    # identify last administration per subject and EXTRT
    arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC) %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number()))

  to_replace <- temp %>%
    filter(.data$LAST_ADMIN == TRUE, is.na(.data$EXENDTC))

  if (nrow(to_replace) > 0) {
    conditional_message("In ", nrow(to_replace), " subjects, EXENDTC is ",
      "absent and is replaced by the cut off date, ",
      format(cut.off.date, format = "%Y-%m-%d %H:%M"), ":\n",
      df_to_string(to_replace %>% select(all_of(c("USUBJID", "EXTRT",
                                         "EXSTDTC", "EXENDTC")))),
      "\n",
      silent = silent
    )
    temp <- temp %>%
      mutate(EXENDTC = case_when(
        (LAST_ADMIN == TRUE & is.na(.data$EXENDTC)) ~ cut.off.date,
        .default = .data$EXENDTC
      ))
  }
  temp %>%
    select(-"LAST_ADMIN")
}


#' Remove administrations with EXSTDTC after RFENDTC
#'
#' @param ex The EX domain as data frame.
#' @param dm the DM domain as data frame.
#' @param silent Suppress messages as logical.
#'
#' @return A data frame.
#' @export
filter_EXSTDTC <- function(ex, dm, silent = FALSE) {
  out <- ex %>%
    left_join(
      dm %>%
        decompose_dtc("RFENDTC") %>%
        select(.data$USUBJID, .data$RFENDTC_date),
      by = "USUBJID"
    )

  temp <- out %>%
    filter(!(.data$EXSTDTC <= .data$EXENDTC) &
             (.data$EXSTDTC_date > .data$RFENDTC_date))

  if(nrow(temp) > 0) {
    conditional_message(paste0(
      nrow(temp),
      " subjects had an administration episode with the EXSTDTC date after ",
      "their RFENDTC date.\nThese administrations were ",
      "removed from the data set:\n",
      df_to_string(select(temp, .data$USUBJID, .data$EXTRT, .data$EXSEQ,
                          .data$EXSTDTC, .data$EXENDTC,
                          .data$RFENDTC_date), indent = "  ")),
      silent = silent)
  }

  out %>%
    filter((.data$EXSTDTC_date < .data$EXENDTC_date) |
             (.data$EXSTDTC_date <= .data$RFENDTC_date))
}


#' Remove administrations with EXSTDTC after EXENDTC
#'
#' @param ex The ex domain as data frame.
#' @param silent Suppress messages as logical.
#' @param dm The dm domain as data frame.
#'
#' @return A data frame.
#' @export
filter_EXSTDTC_after_EXENDTC <- function(ex, dm, silent = FALSE) {
  temp <- ex %>%
    filter(.data$EXSTDTC > .data$EXENDTC) %>%
    left_join(
      dm %>%
        select(.data$USUBJID, .data$RFENDTC),
      by = "USUBJID"
    )

  if(nrow(temp) > 0) {
    conditional_message(paste0(
      nrow(temp),
      " administration episodes had an EXENDTC before the EXSTDTC and were\n",
      "removed from the data set:\n",
      df_to_string(select(temp, .data$USUBJID, .data$EXTRT, .data$EXSEQ,
                          .data$EXSTDTC, .data$EXENDTC, .data$RFENDTC),
                   indent = "  "),
      "\n"),
      silent = silent)
  }
  ex %>%
    filter(.data$EXSTDTC <= .data$EXENDTC)
}


#' Make EXSTDY and EXENDY by USUBJID, EXTRT
#'
#' Treatment days are calculated relative to RFSTDTC from DM.
#'
#' @details
#' Caution: Currently, the function works only with treatment days after Day 1.
#'  If this be ever used for days before Day 1, a term must be
#' implemented to correct for the missing Day 0 in the time nomenclature.
#'
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @return The enhanced EX domain as data frame.
#' @import assertr
#' @keywords internal
make_exstdy_exendy <- function(ex, dm) {
  ex %>%
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    assertr::verify(is.POSIXct(c(EXSTDTC, EXENDTC))) %>%
    left_join(
      dm %>%
        distinct(.data$USUBJID, .data$RFSTDTC),
      by = "USUBJID"
    ) %>%
    mutate(EXSTDY = floor(as.numeric(difftime(.data$EXSTDTC, .data$RFSTDTC),
                                     units = "days")) + 1) %>%
    mutate(EXENDY = floor(as.numeric(difftime(.data$EXENDTC, .data$RFSTDTC),
                                     units = "days")) + 1) %>%
    select(-RFSTDTC)
}


#' Replace administration time with time found in PCRFTDTC
#'
#' @param admin The administration data frame.
#' @param obs The observation data frame.
#' @param silent Switch to disable message output.
#' @return The updated administration data frame.
#' @keywords internal
#' @import assertr
impute_admin_dtc_to_pcrftdtc <- function(admin, obs, silent = FALSE) {
  temp <- admin %>%
    assertr::verify(has_all_names("USUBJID", "PARENT", "date", "time")) %>%
    select(-any_of(c("ref.time", "ref.date", "PCRFTDTC"))) %>%
    left_join(
      (obs %>%
         assertr::verify(has_all_names("USUBJID", "PARENT", "PCRFTDTC")) %>%
         mutate(ref.date = as.Date(extract_date(.data$PCRFTDTC))) %>%
         mutate(ref.time = extract_time(.data$PCRFTDTC)) %>%
         distinct(.data$USUBJID, .data$PARENT, .data$PCRFTDTC, .data$ref.date,
                   .data$ref.time)),
      by = c("USUBJID", "PARENT", "date" = "ref.date")) %>%
    group_by(.data$USUBJID, .data$PARENT) %>%
    fill(ref.time, .direction = "down") %>%
    ungroup()

  n_cases <- temp %>%
    filter(.data$time != .data$ref.time) %>%
    nrow()

  conditional_message(
    paste0("Time found in PCRFTDTC used in ", n_cases, " cases."),
    silent = silent || (n_cases == 0)
  )

  temp %>%
    mutate(imputation_flag = case_when(
      (is.na(.data$time) | .data$time != .data$ref.time) ~ TRUE,
      .default = FALSE)) %>%
    mutate(DTC = case_match(.data$imputation_flag,
      TRUE ~ compose_dtc(.data$date, .data$ref.time),
      FALSE ~ .data$DTC)) %>%
    mutate(IMPUTATION = case_match(.data$imputation_flag,
      TRUE ~ "time imputed from PCRFTDTC",
      FALSE ~ IMPUTATION)) %>%
    select(-"imputation_flag")
}


#' Make administration data set from EX domain
#'
#' This function expands the administration ranges specified by EXSTDTC and
#' EXENDTC in each record of EX to that the result has individual records for
#' all administrations, with the time point in the TIME column.
#'
#' @section Specific imputations:
#' If the end date (EXENDTC) is missing, i.e., the administration is ongoing
#' At the time of the data cutoff of the SDTM data set, EXENDTC is replaced by
#' cut_off_date.
#'
#' @param ex EX domain as data frame.
#' @param dm DM domain as data frame.
#' @param cut_off_date The cut-off date to be used where no EXENDTC is recorded,
#' in POSIX format.
#' @param drug_mapping A data frame with the columns of EXTRT and PCTESTCD
#' that associate both.
#' @param silent Switch to disable message output.
#' @return A tibble with individual administrations
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import assertr
#' @keywords internal
make_admin <- function(ex,
                       dm,
                       drug_mapping,
                       cut_off_date,
                       silent = FALSE) {
  admin <- ex %>%
    verify(has_all_names(
      "STUDYID", "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC",
      "EXENDTC", "EXDOSE", "EPOCH"
    )) %>%
    lubrify_dates() %>%
    mutate(
      EXSTDTC_has_time = has_time(EXSTDTC),
      EXENDTC_has_time = has_time(EXENDTC)
    ) %>%

    ### The following should be probably done after expansion!
    # filter for entries with start before cut-off
    dplyr::filter(.data$EXSTDTC <= cut_off_date) %>%

    # isolate start and end dates and times
    mutate(start.date = extract_date(.data$EXSTDTC)) %>%
    dplyr::mutate(start.time = case_when(
      EXSTDTC_has_time == TRUE ~ extract_time(.data$EXSTDTC),
      .default = NA
    )) %>%
    mutate(end.date = extract_date(.data$EXENDTC)) %>%
    dplyr::mutate(end.time = case_when(
      EXENDTC_has_time == TRUE ~ extract_time(.data$EXENDTC),
      .default = NA
    )) %>%
    make_exstdy_exendy(dm)

  ret <- admin %>%
    rowwise() %>%
    mutate(date = list(seq(as.Date(.data$start.date),
                           as.Date(.data$end.date),
                           by = "days"))) %>%
    unnest(c(date)) %>%
    group_by(.data$USUBJID, .data$EXTRT, .data$end.date) %>%
    dplyr::mutate(time = case_when(
      row_number() == n() ~ .data$end.time,
      .default = .data$start.time
    )) %>%
    ungroup() %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    dplyr::mutate(EXDY = .data$EXSTDY + (row_number() - 1)) %>%
    dplyr::ungroup() %>%
    mutate(DTC = compose_dtc(.data$date, .data$time)) %>%

    # set treatment, standard fields
    dplyr::mutate(
      NTIME = 0, DV = NA, LNDV = NA, DOSE = .data$EXDOSE, AMT = .data$EXDOSE,
      EVID = 1
    ) %>%
    dplyr::mutate(TYPE = NA, PCTPTNUM = 0, MDV = 1, RATE = 0) %>%
    dplyr::left_join(drug_mapping, by = "EXTRT") %>%

    ## CAUTION: there may be administrations of multiple drugs. They should
    ## go to different administration compartments! For now, all go into CMT 1.
    ## This needs to be resolved in the future!!

    mutate(CMT = 1)

  return(as.data.frame(ret))
}


#' Guess the most likely PCSPEC
#'
#' The PC specimen is selected based on the likelihood in the order of 'plasma'
#' < 'serum' < 'blood'.
#' @param pc A data frame.
#' @param silent Switch to disable message output.
#' @return The imputed spec as character.
#' @export
#' @keywords internal
#' @examples
#' guess_pcspec(examplinib_poc$pc)
guess_pcspec <- function(pc, silent = TRUE) {
  pcspecs <- unique(pc$PCSPEC)
  standard_specs <- c(
    "PLASMA", "Plasma", "plasma", "SERUM", "Serum", "serum",
    "BLOOD", "Blood", "blood"
  )

  spec <- standard_specs[standard_specs %in% pcspecs][1]
  conditional_message("No specimen specified. Set to ", spec,
                      " as the most likely.", "\n", silent = silent)
  return(spec)
}


#' Guess the most likely LBSPEC
#'
#' @param lb The LB SDTM domain as data frame.
#' @param silent Switch to disable message output.
#' @return the imputed LBSPEC as character.
#' @export
#' @keywords internal
#' @examples
#' guess_lbspec(domain(examplinib_poc, "lb"))
guess_lbspec <- function(lb, silent = TRUE) {
  lbspecs <- unique(lb$LBSPEC)
  standard_specs <- c("SERUM", "serum", "URINE", "urine")
  spec <- standard_specs[standard_specs %in% lbspecs][1]
  conditional_message("No specimen specified. Set to '", spec,
                      "' as the most likely.", "\n", silent = silent)
  return(spec)
}


#' Make observation data set from PC
#'
#' This function creates an observation data frame from PC SDTM data.
#'
#' @details Nominal time is either derived from `PCTPTNUM` (if
#'   `use_pctptnum=TRUE`), or from `PCELTM` (the relative nominal time). Both
#'   are permissible fields per the CDISC specification and may be absent from
#'   the clinical data. In contrast to `PCTPOTNUM`, `PCELTM` follows a defined
#'   format, i.e., the [ISO 8601](https://w.wiki/8Bzr) specification for time
#'   durations. Note that the DV is converted into mg/l assuming that PCSTRESN
#'   is provided in mg/ml!
#'
#' @param pc The SDTM PC domain as a data.frame.
#' @param time_mapping The time mapping.
#' @param spec The specimen to be represented in the NIF object as string (e.g.,
#'   "BLOOD", "PLASMA", "URINE", "FECES"). When spec is an empty string (""),
#'   which is the default setting, the most likely specimen, i.e., "BLOOD" or
#'   "PLASMA" is selected, depending what is found in the PC data.
#' @param use_pctptnum Use PCTPTNUM as nominal time.
#' @param silent Switch to disable message output.
#' @param drug_mapping The drug mapping as data frame.
#' @return A data frame with individual observations with certain NONMEM input
#'   variables set
#' @import dplyr
#' @import lubridate
#' @import assertr
#' @seealso [add_time_mapping()]
#' @keywords internal
make_obs <- function(pc,
                     drug_mapping,
                     time_mapping = NULL,
                     spec = NULL,
                     silent = FALSE,
                     use_pctptnum = FALSE) {
  # Assertions
  # pc %>% verify(has_all_names("PCSPEC", "PCDTC", "PCSTRESN", "PCTESTCD"))

  if (length(spec) == 0) {spec <- guess_pcspec(pc)}

  obs <- pc %>%
    verify(has_all_names("PCSPEC", "PCDTC", "PCSTRESN", "PCTESTCD")) %>%
    dplyr::filter(.data$PCSPEC %in% spec)

  # filter for PC data marked as 'not done'
  if ("PCSTAT" %in% colnames(obs)) {
    n <- sum(obs$PCSTAT == "NOT DONE")
    if (n > 0) {
      conditional_message(n, " samples are marked as 'not done' and",
        " were removed fromthe data set.", "\n",
        silent = silent
      )
    }
    obs <- obs %>%
      dplyr::filter(.data$PCSTAT != "NOT DONE")
  }

  # identify observation date and time
  obs <- obs %>%
    # extract date and time of observation
    mutate(DTC = .data$PCDTC) %>%
    mutate(start.date = extract_date(.data$DTC)) %>%
    mutate(start.time = case_when(has_time(.data$PCDTC) ~
                                    extract_time(.data$DTC),
      .default = NA
    ))

  # identify nominal time
  if (use_pctptnum) {
    obs <- obs %>%
      dplyr::mutate(NTIME = as.numeric(.data$PCTPTNUM))
  } else {
    if ("PCELTM" %in% names(pc)) {
      obs <- obs %>%
        dplyr::mutate(NTIME = as.numeric(
          stringr::str_extract(.data$PCELTM, "PT([.0-9]+)H", group = 1)
        ))
    } else {
      if (is.null(time_mapping) || nrow(time_mapping) == 0) {
        stop(paste(
          "No PCELM in PC. Please add time mapping to SDTM data set",
          "(see '?add_time_mapping()' for details)."
        ))
      }
      obs <- obs %>%
        dplyr::left_join(time_mapping, by = "PCTPT")
    }
  }

  obs <- obs %>%
    mutate(
      EVID = 0,
      AMT = 0,
      DV = .data$PCSTRESN / 1000,
      LNDV = log(.data$DV),
      MDV = case_when(is.na(.data$DV) ~ 1, .default = 0),
      RATE = 0) %>%
    mutate(IMPUTATION = "") %>%
    left_join(drug_mapping, by = "PCTESTCD") %>%
    filter(!is.na(.data$CMT))

  # return(obs %>% as.data.frame())
  return(obs)
}


#' Extract last observation time from observation tibble
#'
#' @param obs tibble as created with make_obs
#' @return A datetime object representing the last recorded observation time
#' @import dplyr
#' @keywords internal
last_obs_dtc <- function(obs) {
  return(max(obs$DTC, na.rm = TRUE))
}


#' Last administration DTC
#'
#' @param ex The EX domain as data table.
#' @return The last administration in DTC format.
#' @keywords internal
last_ex_dtc <- function(ex) {
  return(max(ex$EXENDTC, na.rm = TRUE))
}


#' Extract baseline vital sign covariates from VS
#'
#' @param vs The VS domain as data frame.
#' @param baseline_filter R filter expression to select the baseline time point.
#' @param silent Switch to disable message output.
#'
#' @return Baseline VS data as wide data frame.
#' @export
#' @keywords internal
#' @examples
#' baseline_covariates(examplinib_sad$vs)
baseline_covariates <- function(vs, baseline_filter = {TRUE}, silent = FALSE) {
  temp <- vs %>%
    filter(VSTESTCD %in% c("HEIGHT", "WEIGHT"))

  bl_cov <- NULL

  if ("VISIT" %in% names(temp)) {
    if ("SCREENING" %in% str_to_upper(unique(temp$VISIT))) {
      bl_cov <- temp %>%
        filter(str_to_upper(VISIT) == "SCREENING")
    } else {
      if ("VSBLFL" %in% names(temp)) {
        bl_cov <- temp %>%
          filter(VSBLFL == "Y")
      }
    }
  } else {
    if ("VSBLFL" %in% names(temp)) {
      bl_cov <- temp %>%
        filter(VSBLFL == "Y")
    } else {
      conditional_message("Baseline VS data could not be identified!", "\n",
        silent = silent
      )
    }
  }

  bl_cov <- bl_cov %>%
    group_by(.data$USUBJID, .data$VSTESTCD) %>%
    summarize(mean = mean(VSSTRESN), .groups = "drop") %>%
    pivot_wider(names_from = "VSTESTCD", values_from = "mean")

  # Calculate BMI if height and weight are available
  if ("HEIGHT" %in% colnames(bl_cov) && "WEIGHT" %in% colnames(bl_cov)) {
    bl_cov <- bl_cov %>%
      mutate(BMI = .data$WEIGHT / (.data$HEIGHT / 100)^2)
  }
  return(bl_cov)
}


#' Issue message based on silent flag
#'
#' @param msg The message as character.
#' @param silent A Boolean.
#' @param ... Further message components.
#' @return Nothing.
#' @keywords internal
conditional_message <- function(msg, ..., silent = FALSE) {
  parameters <- c(as.list(environment()), list(...))
  parameters <- lapply(parameters, as.character)
  if (silent == FALSE) {
    message(paste(as.character(parameters[names(parameters) != "silent"])))
  }
}


#' Create the drug mapping data frame for PC observations
#'
#' @param sdtm_data The sdtm data as SDTM object.
#' @return A data frame.
#' @keywords internal
make_drug_mapping <- function(sdtm_data) {
  drug_mapping <- sdtm_data$analyte_mapping %>%
    filter(PCTESTCD %in% unique(sdtm_data$pc$PCTESTCD)) %>%
    # mutate(PCTESTCD = TESTCD) %>%
    # select(-TESTCD) %>%
    rbind(
      data.frame(EXTRT = intersect(
        unique(sdtm_data$ex$EXTRT),
        unique(sdtm_data$pc$PCTESTCD)
      )) %>%
        mutate(PCTESTCD = EXTRT)
    ) %>%
    mutate(PARENT = PCTESTCD)

  # add metabolite mapping, if available
  if (nrow(sdtm_data$metabolite_mapping) != 0) {
    drug_mapping <- drug_mapping %>%
      rbind(
        sdtm_data$metabolite_mapping %>%
          rename(PARENT = PCTESTCD_parent, PCTESTCD = PCTESTCD_metab) %>%
          mutate(EXTRT = "")
      )
  }

  drug_mapping <- drug_mapping %>%
    mutate(METABOLITE = (PCTESTCD != PARENT)) %>%
    mutate(ANALYTE = PCTESTCD) %>%
    distinct()
}


#' Add TIME field to table
#'
#' TIME is created as the difference between the DTC field and the first DTC
#' field on the USUBJID level. TIME is in hours, rounded by 3 digits.
#' @param x The table as data frame.
#' @return A data frame with FIRSTDTC and TIME added.
#' @keywords internal
add_time <- function(x) {
  x %>%
    assertr::verify(has_all_names("USUBJID", "DTC")) %>%
    assertr::verify(is.POSIXct(.data$DTC)) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TIME = round(
      as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "h")),
      digits = 3
    ))
}


#' Make a NIF object from SDTM-formatted data
#'
#' This function creates a basic NONMEM input file (NIF) data set from
#' SDTM-formatted clinical study data following the conventions summarized in
#' [Bauer, CPT Pharmacometrics Syst. Pharmacol.
#' (2019)](https://doi.org/10.1002/psp4.12404). For a more in-depth tutorial,
#' see `vignette("nif-vignette")`.
#'
#' @section Imputations: Subjects with administration but no observations for
#'   the respective analyte are deleted from the data set. For further
#'   imputations, see `vignette("nif-imputations")`.
#'
#' @section Output fields:
#' * `ID` Subject identification number
#' * `TIME` Recorded time of administration or observation events in hours
#'   relative to the first individual event.
#' * `AMT` Dose administered for dosing record, or zero for observations.
#' * `DOSE` Dose in mg for administrations and post-dose observations.
#' * `DV` The dependent variable, i.e., observed concentration, or zero for
#'   administration records, in mg/l.
#' * `LNDV` The natural Log of DV.
#' * `RATE` Rate of infusion of drug or zero if drug is given as a bolus.
#' * `MDV` One for missing DV, else zero.
#' * `EVID` Event ID: 0 for observations, 1 for administrations.
#' * `CMT` Pharmacokinetic compartment. Will be set to 1 for administrations
#'   and 2 for observations. Should be changed afterwards, if needed.
#' * `DTC` The date-time of the data record.
#' * `FIRSTDTC` Date and time of first event per subject. This field is used
#'   internally for the calculation of `TIME`. Although it is not needed for
#'   NONMEM analysis, it is provided for subsequent NIF file building steps,
#'   e.g., addition of further time-dependent endpoints.
#' * `FIRSTADMINDTC` The date-time of the first administration of the
#'   respective parent drug for the respective subject.
#' * `FIRSTTRTDTC` The date-time of the first administration of any parent
#'   drug for the respective subject.
#' * `ANALYTE` The analyte or drug in the data record.
#' * `TRTDY` The treatment day, i.e., the relative day after the first
#'   treatment for the respective subject.
#'
#' @param sdtm_data A `sdtm` object, i.e., essentially a list of SDTM domains as
#'   data tables. Typically, the SDTM data are loaded using [read_sdtm_sas()] or
#'   [read_sdtm_xpt()]. As a minimum, the following SDTM domains are needed: DM,
#'   VS, PC and EX.
#' @param spec The sample specimen for the PC data as string (e.g., "BLOOD",
#'   "PLASMA", "URINE", "FECES"). When spec is NULL (default), the most likely
#'   specimen is selected.
#' @param truncate_to_last_observation Boolean to indicate whether the data set
#'   should be truncated to the last observation. In this case, administrations
#'   after the last observation time point will deleted. The default is 'TRUE'.
#' @param silent Switch to disable message output.
#' @param use_pctptnum Boolean to indicate whether to derive nominal time
#'   ('NTIME') from 'PCTPTNUM'.
#' @param truncate_to_last_individual_obs Boolean to indicate whether
#'   observations should be truncted to the last individual observation.
#' @param analyte_cmt_mapping The analyte-compartment association as data frame
#'   with the columns 'ANALYTE' and 'CMT'.
#' @return A NIF object.
#' @seealso [summary()]
#' @seealso [plot.nif()]
#' @seealso [write_nif()]
#' @seealso [write_csv.nif()]
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' make_nif(examplinib_fe)
#' make_nif(examplinib_poc)
make_nif <- function(
    sdtm_data,
    spec = NULL,
    silent = FALSE,
    truncate_to_last_observation = TRUE,
    truncate_to_last_individual_obs = FALSE,
    use_pctptnum = FALSE,
    analyte_cmt_mapping = NULL) {

  vs <- sdtm_data$domains[["vs"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()
  ex <- sdtm_data$domains[["ex"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()
  pc <- sdtm_data$domains[["pc"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()
  dm <- sdtm_data$domains[["dm"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()

  # define sample specimen
  if (length(spec) == 0) {spec <- guess_pcspec(pc, silent = silent)}

  # define compartment mapping
  if(is.null(analyte_cmt_mapping)) {
    temp <- pc %>%
      filter(.data$PCSPEC %in% spec) %>%
      filter(!is.na(.data$PCSTRESN)) %>%
      distinct(.data$PCTESTCD) %>%
      pull(.data$PCTESTCD)
    cmt_mapping <- data.frame(
      ANALYTE = temp,
      CMT = seq(2, length(temp) +1)
    )
  } else {
    cmt_mapping <- data.frame(
      ANALYTE = names(analyte_cmt_mapping),
      CMT = analyte_cmt_mapping)
  }

  # define final drug mapping
  drug_mapping <- make_drug_mapping(sdtm_data) %>%
    left_join(cmt_mapping, by = "ANALYTE")

  # make baseline covariates
  bl_cov <- baseline_covariates(vs, silent = silent)

  # make observations from PC
  obs <- make_obs(pc,
                  drug_mapping,
                  time_mapping = sdtm_data$time_mapping,
                  spec = spec,
                  silent = silent,
                  use_pctptnum = use_pctptnum) %>%
    # left_join(drug_mapping, by = "PCTESTCD") %>%
    # filter(!is.na(CMT)) %>%
    group_by(.data$USUBJID, .data$PARENT) %>%
    mutate(last_obs = max(.data$DTC, na.rm = TRUE)) %>%
    ungroup()

  # EX: basic imputations
  ex <- ex %>%
    mutate(IMPUTATION = "") %>%
    impute_missing_exendtc_time(silent = silent) %>%
    exclude_exstdtc_after_rfendtc(dm, silent = silent) %>%
    impute_exendtc_to_rfendtc(dm, silent = silent) %>%
    impute_missing_exendtc(silent = silent)

  # define cut-off date
  if (truncate_to_last_observation == TRUE) {
    cut_off_date <- last_obs_dtc(obs)
    conditional_message("Data cut-off was set to last observation time, ",
      cut_off_date, "\n",
      silent = silent
    )
  } else {
    cut_off_date <- last_ex_dtc(ex)
  }

  # EX: apply cut-off date
  ex <- ex %>%
    impute_exendtc_to_cutoff(cut_off_date, silent = silent) %>%
    filter(.data$EXENDTC >= .data$EXSTDTC)

  # identify subjects with observations by analyte
  obs_sbs <- obs %>%
    filter(!is.na(.data$PCSTRESN)) %>%
    unite("ut", .data$USUBJID, .data$PCTESTCD, remove = FALSE) %>%
    distinct(.data$USUBJID, .data$PCTESTCD, ut)

  # make administrations based on EX
  admin <- make_admin(ex, dm,
                      drug_mapping,
                      cut_off_date,
                      silent = silent)
  # change administration time to the time included in PCRFTDTC if available
  if ("PCRFTDTC" %in% names(pc)) {
    admin <- admin %>%
      impute_admin_dtc_to_pcrftdtc(obs, silent = silent)
  }

  # truncate to last individual observation
  if (truncate_to_last_individual_obs == TRUE) {
    admin <- admin %>%
      left_join(obs %>% distinct(.data$USUBJID, .data$PARENT, .data$last_obs),
                by = c("USUBJID", "PARENT")) %>%
      filter(.data$DTC <= .data$last_obs)
  }

  # Remove all administrations with PCTESTCD==NA
  #  those rows may come from treatments that have no analyte mapping
  admin <- admin %>%
    dplyr::filter(!is.na(.data$PCTESTCD))

  # filter admin for subjects who actually have observations
  no_obs_sbs <- admin %>%
    unite("ut", .data$USUBJID, .data$PCTESTCD, remove = FALSE) %>%
    filter(!(.data$ut %in% obs_sbs$ut)) %>%
    distinct(.data$USUBJID, .data$PCTESTCD, ut) %>%
    as.data.frame()

  # Issue message about excluded administrations, if applicable
  if (nrow(no_obs_sbs) > 0) {
    out <- no_obs_sbs %>%
      arrange(.data$PCTESTCD, .data$USUBJID) %>%
      select(c("USUBJID", "PCTESTCD")) %>%
      df_to_string()

    conditional_message("The following subjects had no observations for ",
      "the respective analyte and were removed from the data set:\n",
      out, "\n",
      silent = silent
    )
  }
  # and filter out excluded administrations
  admin <- admin %>%
    tidyr::unite("ut", .data$USUBJID, .data$PCTESTCD, remove = FALSE) %>%
    dplyr::filter(.data$ut %in% obs_sbs$ut) %>%
    dplyr::select(-ut)

  if (nrow(admin) == 0) {
    stop(paste0(
      "No subjects in the data set left after filtering out ",
      "subjects without observations.\n",
      "  In most cases, this is because of `PCTESTCD` not matching with a ",
      "corresponding `EXTRT`.\n",
      "  Consider adding analyte mappings to the SDTM data object!\n",
      "  For futher information, please see `?add_analyte_mapping`,",
      "`suggest_sdtm()` and the\n",
      "  vignette 'Creating NIF files from SDTM data'!"
    ))
  }

  # calculate age from birthday and informed consent signature date
  if ("RFICDTC" %in% colnames(dm) && "BRTHDTC" %in% colnames(dm)) {
    dm <- dm %>%
      dplyr::mutate(age1 = floor(as.duration(interval(.data$BRTHDTC,
                                                      .data$RFICDTC)) /
                                   as.duration(years(1)))) %>%
      dplyr::mutate(AGE = case_when(is.na(.data$AGE) ~ .data$age1,
                                    .default = .data$AGE)) %>%
      dplyr::select(-"age1")
  }

  ## assemble NIF object from admin and obs and baseline data.
  nif <- obs %>%
    dplyr::bind_rows(
      admin %>%
        dplyr::filter(.data$USUBJID %in% obs$USUBJID)
    ) %>%
    dplyr::left_join(dm, by = c("USUBJID", "STUDYID")) %>%
    dplyr::left_join(bl_cov, by = "USUBJID") %>%

    # filter out rows without parent information
    dplyr::filter(.data$PARENT != "") %>%

    # filter out observations without administration
    dplyr::group_by(.data$USUBJID, .data$PARENT) %>%
    dplyr::filter(sum(.data$AMT) != 0) %>%
    dplyr::ungroup() %>%

    # identify first administration per PARENT and first treatment overall
    group_by(.data$USUBJID, .data$PARENT) %>%
    mutate(FIRSTADMINDTC = min(.data$DTC[.data$EVID == 1], na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(.data$USUBJID) %>%
    mutate(FIRSTTRTDTC = min(.data$FIRSTADMINDTC, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::arrange(.data$USUBJID, .data$PARENT, .data$DTC, .data$EVID) %>%
    dplyr::group_by(.data$USUBJID, .data$PARENT) %>%
    tidyr::fill(.data$DOSE, .direction = "down") %>%
    tidyr::fill(any_of(c(
      "AGE", "SEX", "RACE", "ETHNIC", "ACTARMCD", "HEIGHT",
      "WEIGHT", "COUNTRY", "ARM", "SUBJID", "EXSEQ"
    )), .direction = "down") %>%
    dplyr::ungroup() %>%
    add_time() %>%
    mutate(TRTDY = interval(date(.data$FIRSTTRTDTC),
                            date(.data$DTC)) / days(1) + 1) %>%
    recode_sex() %>%
    dplyr::arrange(.data$USUBJID, .data$TIME, -.data$EVID) %>%
    dplyr::mutate(ID = as.numeric(as.factor(.data$USUBJID))) %>%
    dplyr::relocate(.data$ID) %>%
    new_nif() %>%
    # add_tad() %>%
    make_time() %>%
    index_nif()

  comment(nif) <- paste0("created with nif ", packageVersion("nif"))
  return(nif)
}


#' This function orders a NIF object and adds a REF field
#'
#' The input data format expected by NONMEM requires all rows ordered by ID and
#' TIME, and indexed sequentially on a subject level with a REF field.
#' Re-indexing may be required if a NIF object is extended, e.g., by merging in
#' further data.
#'
#' @param nif NIF object, e.g., as created by [make_nif()] and manually
#'   modified.
#' @return The updated NIF dataset including an updated REF field.
#' @import dplyr
#' @export
#' @examples
#' index_nif(examplinib_fe_nif)
index_nif <- function(nif) {
  nif %>%
    as.data.frame() %>%
    dplyr::arrange(ID, TIME, -EVID) %>%
    dplyr::mutate(REF = row_number()) %>%
    dplyr::relocate(REF) %>%
    new_nif()
}


#' This function removes columns from a NIF object that are not needed for
#' downstream analysis
#'
#' During creating of a NIF object using [make_nif()], multiple SDTM tables are
#' aggregated without deleting the original fields (columns). Many of these
#' fields may not be required for the final analysis. This function reduces the
#' fields to those typically required in the downstream analysis. Applying
#' [compress_nif()] is typically the last step in the creation of a NIF data
#' set, after creating the basic NIF object  with [make_nif()] and applying
#' custom imputations and manually deriving convariates as needed.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `compress_nif()` has been superseded in favor of [compress()].
#' @param nif A NIF object.
#' @param ... Further optional parameters are fields to be included in the
#'  output. If none are provided, the standard set will be used.
#' @return A NIF dataset with only the specified fields
#' @import dplyr
#' @import lifecycle
#' @export
#' @examples
#' compress_nif(examplinib_fe_nif)
compress_nif <- function(nif, ...) {
  lifecycle::deprecate_warn("0.40.1", "compress_nif()", "compress()")
  temp <- as.character(unlist(c(as.list(environment())[-1], list(...))))

  if (length(temp) == 0) {
    columns <- standard_nif_fields
  } else {
    columns <- temp
  }
  nif %>%
    dplyr::select(any_of(columns)) %>%
    dplyr::select(any_of(c(columns, starts_with("BL_"),
                           starts_with("TV_")))) %>%
    new_nif()
}


#' Compress a NIF object
#'
#' @param nif A NIF object
#' @param fields Fields to be included as character. If 'fields' is NULL
#'   (default), the fields defined in `standard_nif_fields` plus all fields with
#'   a name that starts with 'BL_' (baseline covariates) or 'TV_' (time varying
#'   covariates) are included.
#' @param debug Logic value to indicate whether the debug fields, `PCREFID` and
#' `EXSEQ` should be included in the NIF object.
#' @return A NIF object
#' @export
compress <- function(nif, fields = NULL, debug = FALSE) {
  if (is.null(fields)) {
    fields <- c(
      standard_nif_fields,
      colnames(nif)[grep("BL_", colnames(nif))],
      colnames(nif)[grep("TV_", colnames(nif))])
  }
  if(debug == TRUE) {
    fields <- c(fields, "EXSEQ", "PCREFID", "EXTRT", "IMPUTATION")
  }
  nif %>%
    as.data.frame() %>%
    dplyr::select(any_of(fields)) %>%
    fill_bl_tv() %>%
    new_nif()
}


#' Reduce a NIF object on the subject level by excluding all
#' administrations after the last observation
#'
#' @param nif A NIF dataset.
#' @return A NIF dataset.
#' @import dplyr
#' @export
#' @examples
#' clip_nif(examplinib_poc_nif)
clip_nif <- function(nif) {
  last_obs <- nif %>%
    as.data.frame() %>%
    dplyr::filter(EVID == 0) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(last_obs = max(.data$TIME)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(.data$USUBJID, .data$last_obs)

  ret <- nif %>%
    dplyr::left_join(last_obs, by = "USUBJID") %>%
    dplyr::filter(.data$TIME <= .data$last_obs)
  return(new_nif(ret))
}


#' Add baseline lab covariate
#'
#' @details
#' Lab parameters not found in LB will be reported in a warning message.
#' @param obj A NIF dataset.
#' @param lb SDTM LB domain as data frame.
#' @param lbspec The specimen, e.g., "BLOOD", "SERUM" or "URINE".
#' @param lbtestcd Lab parameter(s) as encoded by LBTESTCD.
#' @param silent Disable messages.
#' @return A NIF dataset.
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom rlang .data
#' @export
add_bl_lab <- function(obj, lb, lbtestcd, lbspec = NULL, silent = FALSE) {
  if(is.null(lbspec)) {
    lbspec = guess_lbspec(lb)
  }
  temp <- lbtestcd %in% (lb %>%
                           dplyr::distinct(.data$LBTESTCD) %>%
                           dplyr::pull(.data$LBTESTCD))
  if (!all(temp)) {
    if (!silent) {
      message(paste0("The following was not found in lb: ", lbtestcd[!temp]))
    }
    lbtestcd <- lbtestcd[temp]
    if (length(lbtestcd) == 0) {
      return(obj)
    }
  }

  temp <- lb %>%
    dplyr::filter(.data$LBSPEC == lbspec) %>%
    dplyr::filter(.data$LBBLFL == "Y") %>%
    dplyr::filter(.data$LBTESTCD %in% lbtestcd) %>%
    dplyr::select(c("USUBJID", "LBTESTCD", "LBSTRESN")) %>%
    tidyr::pivot_wider(names_from = "LBTESTCD", values_from = "LBSTRESN") %>%
    dplyr::rename_with(~ stringr::str_c("BL_", .), .cols = -1)

  obj %>%
    as.data.frame() %>%
    dplyr::left_join(temp, by = "USUBJID") %>%
    new_nif()
}


#' Add time-variant lab covariate
#'
#' Add column named 'TV_xxx' with the time-varying values for the respective
#' lab covariate. Last values are carried forward.
#' @details
#' Lab parameters not found in LB will be reported in a warning message.
#' @inheritParams add_bl_lab
#' @return A NIF object.
#' @export
add_tv_lab <- function(obj, lb, lbtestcd, lbspec = NULL, silent = FALSE) {
  if(is.null(lbspec)) {
    lbspec = guess_lbspec(lb)
  }
  temp <- lbtestcd %in% unique(lb$LBTESTCD)

  if(!all(temp)) {
    conditional_message(
      paste0("The following was not found in lb: ", lbtestcd[!temp]),
      silent=silent)
    if(all(temp == FALSE)) {
      return(obj)
    }
  }

  tv_cov <- lb %>%
    lubrify_dates() %>%
    filter(!.data$LBSTAT %in% c("NOT DONE")) %>%
    filter(!is.na(.data$LBSTRESN)) %>%
    filter(.data$LBSPEC == lbspec, .data$LBTESTCD %in% lbtestcd[temp]) %>%
    select(c("USUBJID", "DTC" = "LBDTC", "LBTESTCD", "LBSTRESN")) %>%
    distinct() %>%
    pivot_wider(names_from = "LBTESTCD", values_from = "LBSTRESN") %>%
    rename_with(~ stringr::str_c("TV_", .),
                .cols = -c(.data$USUBJID, .data$DTC)) %>%
    mutate(FLAG = TRUE)

  out <- bind_rows(obj, tv_cov) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    fill(starts_with("TV_"), .direction = "down") %>%
    filter(is.na(.data$FLAG)) %>%
    select(-c("FLAG"))

  return(out)
}


#' Add time-variant vital sign covariate
#'
#' Add column named 'TV_xxx' with the time-varying values for the respective
#' vital sign covariate. Last values are carried forward.
#' @inheritParams add_bl_lab
#' @param vs SDTM VS domain as data frame.
#' @param vstestcd Vital sign parameter(s) as encoded by VSTESTCD.
#' @param duplicate_function A function to resolve duplicate entries into a
#'   single entry, e.g., `mean`, `min`, or `median`. Defaults to `mean`.
#' @return A NIF object.
#' @export
add_tv_vs <- function(obj, vs, vstestcd, duplicate_function = mean,
                      silent = FALSE) {
  temp <- vstestcd %in% unique(vs$VSTESTCD)

  if(!all(temp)) {
    conditional_message(
      paste0("The following was not found in vs: ", vstestcd[!temp]),
      silent=silent)
    if(all(temp == FALSE)) {
      return(obj)
    }
  }

  vs_cov <- vs %>%
    lubrify_dates() %>%
    filter(!.data$VSSTAT %in% c("NOT DONE")) %>%
    filter(!is.na(.data$VSSTRESN)) %>%
    filter(.data$VSTESTCD %in% vstestcd[temp]) %>%
    select(c("USUBJID", "DTC" = "VSDTC", "VSTESTCD", "VSSTRESN")) %>%
    distinct() %>%
    dplyr::group_by(.data$USUBJID, .data$DTC, .data$VSTESTCD) %>%
    dplyr::summarise(
      VSSTRESN = duplicate_function(.data$VSSTRESN, na.rm = TRUE),
      .groups = "drop") %>%
    pivot_wider(names_from = "VSTESTCD", values_from = "VSSTRESN") %>%
    rename_with(~ stringr::str_c("TV_", .),
                .cols = -c(.data$USUBJID, .data$DTC)) %>%
    mutate(FLAG = TRUE)

  out <- bind_rows(obj, vs_cov) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    fill(starts_with("TV_"), .direction = "down") %>%
    filter(is.na(.data$FLAG)) %>%
    select(-c("FLAG"))

  return(out)
}


#' Add lab covariate
#'
#' This functions adds columns for the lab parameters specified in `lbtestcd`,
#' in a time-varying way, i.e., the actual lab value at the time of the
#' observation or administration. This is in contrast to  [add_bl_lab()]. In
#' rows of missing lab data, the last value is carried forward.
#'
#' Note that for some lab parameters, e.g., leukocytes, bili, etc., there may be
#' observations both in serum and in urine. It is therefore necessary to specify
#' the specimen tested. This corresponds to the `LBSPEC` field used in the LB
#' SDTM domain.
#'
#' @param obj The NIF object.
#' @param lb The LB SDTM domain
#' @param lbspec The specimen, e.g., SERUM.
#' @param lbtestcd Lab parameters to be included as character scalar or vector.
#' @param silent Switch to disable message output.
#' @return A NIF object
#' @import dplyr
#' @export
#' @seealso [add_bl_lab()]
#' @seealso [add_lab_observation()]
add_lab_covariate <- function(obj, lb, lbspec = "SERUM", lbtestcd,
                              silent = FALSE) {
  temp <- lbtestcd %in% (
    (lb %>%
       filter(.data$LBSPEC %in% lbspec) %>%
       dplyr::distinct(.data$LBTESTCD) %>%
       dplyr::pull(.data$LBTESTCD))
  )
  if (!all(temp)) {
    if (!silent) {
      message(paste0(
        "The following was not found in lb: ",
        lbtestcd[!temp], " (", lbspec, ")\n"
      ))
    }
    lbtestcd <- lbtestcd[temp]
    if (length(lbtestcd) == 0) {
      return(obj)
    }
  }

  lb_params <- lb %>%
    mutate(dtc = lubridate::as_datetime(
      .data$LBDTC,
      format = dtc_formats
    )) %>%
    mutate(date = format(.data$dtc, format = "%Y-%m-%d")) %>%
    mutate(labdate = date) %>%
    dplyr::filter(.data$LBSPEC %in% lbspec) %>%
    dplyr::filter(.data$LBTESTCD %in% lbtestcd) %>%
    dplyr::select(c("USUBJID", "date", "labdate", "LBTESTCD", "LBSTRESN")) %>%
    pivot_wider(
      names_from = .data$LBTESTCD, values_from = .data$LBSTRESN,
      values_fn = mean
    )

  temp <- obj %>%
    as.data.frame() %>%
    mutate(date = format(.data$DTC, format = "%Y-%m-%d")) %>%
    bind_rows(lb_params) %>%
    arrange(.data$USUBJID, date) %>%
    group_by(.data$USUBJID) %>%
    fill(all_of(lbtestcd), .data$labdate) %>%
    filter(!is.na(.data$EVID)) %>%
    ungroup()

  return(new_nif(temp))
}


#' Add lab value observation
#'
#' @param obj The NIF object.
#' @param lb The LB SDTM domain.
#' @param lbspec The LBSPECT.
#' @param lbtestcd The LBTESTCD.
#' @param cmt A numerical value to specify the compartment this observation is
#'   assigned to.
#' @param parent The parent as character. If provided, the parent field defines
#'   which parent compound administration is taken as the reference time when
#'   time after dose (TAD) is calculated.
#' @param silent Switch to disable message output.
#'
#' @return The resulting NIF object.
#' @export
add_lab_observation <- function(obj, lb, lbtestcd, cmt = NULL, lbspec = "",
                                parent = NA, silent = FALSE) {
  obj %>%
    verify(has_all_names("ID", "USUBJID", "TIME", "EVID"))

  test <- lbtestcd %in% unique(lb$LBTESTCD)
  if (!all(test)) {
    stop(paste0(
      "The following parameters were not not found in lb: ",
      df_to_string(lbtestcd[!test], header = FALSE)
    ))
  }

  if (is.null(cmt)) {
    cmt <- max(obj$CMT) + 1
    message(paste0(
      "Compartment for ", lbtestcd,
      " was not specified and has been set to ", cmt
    ))
  }

  # if(is.null(parent)) {
  #   parent <- data.frame(EXTRT = NA, TESTCD = NA)
  # }

  lb_params <- lb %>%
    verify(has_all_names("USUBJID", "LBDTC", "LBSPEC", "LBTESTCD", "LBDY")) %>%
    lubrify_dates() %>%
    filter(.data$USUBJID %in% (obj %>%
                                  subjects() %>%
                                  pull(.data$USUBJID))) %>%
    mutate(DTC = .data$LBDTC) %>%
    filter(.data$LBSPEC %in% lbspec) %>%
    filter(.data$LBTESTCD == lbtestcd) %>%
    # left_join(analyte_mapping, by = c("LBTESTCD" = "TESTCD")) %>%
    # mutate(PARENT = EXTRT) %>%

    left_join(obj %>%
                add_time() %>%
                as.data.frame() %>%
                distinct(.data$USUBJID, .data$FIRSTDTC, .data$ID),
              by = "USUBJID") %>%
    dplyr::mutate(ANALYTE = lbtestcd, CMT = cmt, MDV = 0, EVID = 0, AMT = 0,
                  RATE = 0, PARENT = parent) %>%
    dplyr::mutate(
      TIME = round(as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "h")),
      digits = 3
    )) %>%
    mutate(IMPUTATION = "") %>%
    mutate(DV = .data$LBSTRESN) %>%
    mutate(LNDV = log(.data$DV)) %>%
    mutate(NTIME = case_when(.data$LBDY < 0 ~ .data$LBDY * 24,
      .default = (.data$LBDY - 1) * 24
    )) %>%
    # add_tad() %>%
    dplyr::select(all_of(c(
      "ID", "STUDYID", "USUBJID", "DTC", "FIRSTDTC", "ANALYTE", "PARENT", "CMT",
      "EVID", "TIME", "NTIME", "DV", "LNDV", "AMT", "RATE", "IMPUTATION"
    )))

  temp <- obj %>%
    as.data.frame() %>%
    bind_rows(lb_params) %>%
    dplyr::arrange(.data$USUBJID, .data$TIME, -.data$EVID) %>%
    dplyr::mutate(REF = row_number()) %>%
    dplyr::group_by(.data$USUBJID) %>%
    tidyr::fill(starts_with("BL_"), .direction = "downup") %>%
    tidyr::fill(
      any_of(c(
        "ID", "AGE", "SEX", "RACE", "BMI", "ACTARMCD", "HEIGHT", "WEIGHT",
        "DOSE", "PART", "COHORT")),
      .direction = "downup"
    ) %>%
    mutate(MDV = case_when(is.na(.data$DV) ~ 1, .default = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TIME = round(
      as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "h")),
      digits = 3
    )) %>%
    new_nif() %>%
    add_trtdy() %>%
    add_tad() %>%
    index_nif()

  return(temp)
}


#' Fill all baseline and time-varying covariates
#'
#' @param obj A NIF object.
#' @return A NIF object.
fill_bl_tv <- function(obj) {
  obj %>%
    arrange(ID, TIME) %>%
    fill(starts_with("BL_"), .direction = "down") %>%
    fill(starts_with("TV_"), .direction = "down")
}


#' Add an observation from a generic source
#'
#' @param obj A NIF object.
#' @param source The data source as data frame.
#' @param DTC_field The date-time information field as character.
#' @param selector R code to conduct filtering of the data set.
#' @param DV_field The DV field as data character.
#' @param analyte_name The name of the analyte in the output, as character.
#' @param silent Suppress messages.
#' @param cmt The comopartment for the added observation.
#' @param parent The parent as character. If provided, the parent field defines
#'   which parent compound administration is taken as the reference time when
#'   time after dose (TAD) is calculated.
#'
#' @return A NIF object
#' @import assertr
#' @import rlang
#' @export
#' @examples
#' add_generic_observation(examplinib_poc_nif, domain(examplinib_poc, "vs"),
#'   "VSDTC", {VSTESTCD == "WEIGHT"}, "VSSTRESN", "wt")
add_generic_observation <- function(obj, source, DTC_field, selector, DV_field,
                                    analyte_name, cmt = NULL, parent = NA,
                                    silent = FALSE) {
  filter_term <- substitute(selector)
  if (is.null(cmt)) {
    cmt <- max(obj$CMT) + 1
    conditional_message(paste0(
      "Compartment for ", DV_field,
      " was not specified and has been set to ", cmt), silent = silent)
  }
  obs <- source %>%
    assertr::verify(has_all_names("USUBJID", DTC_field, DV_field)) %>%
    filter(.data$USUBJID %in% unique(obj$USUBJID)) %>%
    lubrify_dates() %>%
    filter(eval(filter_term)) %>%
    mutate(DTC = .data[[DTC_field]]) %>%
    mutate(DV = .data[[DV_field]]) %>%
    mutate(ANALYTE = analyte_name) %>%
    mutate(TIME = NA, EVID = 0, AMT = 0, CMT = cmt, RATE = 0) %>%
    mutate(PARENT = parent) %>%
    mutate(METABOLITE = FALSE) %>%
    mutate(LNDV = log(.data$DV)) %>%
    mutate(MDV = is.na(.data$DV)) %>%
    select(c("STUDYID", "USUBJID", "TIME", "DTC", "ANALYTE", "PARENT",
             "METABOLITE", "EVID", "AMT", "RATE", "CMT", "DV", "LNDV",
             "MDV")) %>%
    left_join(subjects(obj), by = "USUBJID")

  obj %>%
    bind_rows(obs) %>%
    add_time() %>%
    arrange(.data$USUBJID, .data$TIME, -.data$EVID) %>%
    tidyr::fill(any_of(fillable_nif_fields),
      .direction = "downup") %>%
    fill(any_of(c(starts_with("TV_"), starts_with("BL_"))),
         .direction = "downup") %>%
    new_nif() %>%
    add_tad() %>%
    add_trtdy() %>%
    index_nif()
}


#' Compile subject information
#'
#' @param dm The DM domain as data table.
#' @param vs The VS domain as data table.
#' @param silent No messages as logical.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param cleanup Keep only required fields, as logical.
#'
#' @return A data table.
#' @import assertr
#' @import tidyselect
#' @export
#' @keywords internal
make_subjects <- function(
    dm, vs, silent = FALSE,
    subject_filter = {!ACTARMCD %in% c("SCRNFAIL", "NOTTRT")},
    cleanup = TRUE
  ) {
  # if AGE is not present in DM, calculate age from birthday and informed
  #   consent signature date
  if ("RFICDTC" %in% colnames(dm) && "BRTHDTC" %in% colnames(dm)) {
    dm <- dm %>%
      lubrify_dates() %>%
      mutate(age_brthdtc = floor(as.duration(
        interval(.data$BRTHDTC, .data$RFICDTC)) / as.duration(years(1)))) %>%
      mutate(AGE = case_when(is.na(.data$AGE) ~ .data$age_brthdtc,
                             .default = .data$AGE)) %>%
      select(-"age_brthdtc")
  }

  out <- dm %>%
    verify(has_all_names("USUBJID", "SEX", "ACTARMCD", "RFXSTDTC")) %>%
    lubrify_dates() %>%
    filter({{subject_filter}}) %>%
    left_join(baseline_covariates(vs, silent = silent),
              by = "USUBJID") %>%
    recode_sex() %>%
    mutate(ID = NA) %>%
    relocate("ID") %>%
    arrange("ID") %>%
    {if(cleanup == TRUE) select(., any_of(c(
      "ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "HEIGHT",
      "WEIGHT", "BMI", "ACTARMCD", "RFXSTDTC"))) else .}
  return(out)
}


#' Compile observation data frame
#'
#' @description
#' Create a data frame of observations from a SDTM domain specified by 'domain'
#' where the dependent variable comes from the 'DV_field' parameter and the
#' timing information from the 'DTC_field' parameter.
#'
#' The 'TIME' in the output is `NA` throughout and needs to be calculated based
#' on administration time point information provided separately.
#'
#' If the 'NTIME_lookup' parameter is provided, 'NTIME' can be derived from a
#' field contained in the input data set, e.g., 'PCELTM' (see the code
#' examples). Otherwise, 'NTIME' will be `NA`.
#'
#' @param sdtm A sdtm object. Needs at least the 'DM' and 'VS' domains, and the
#'   domain the observations come from.
#' @param domain The domain as character.
#' @param DTC_field The field to use as the date-time code for the observation.
#'   Defaults to the two-character domain name followed by 'DTC', if NULL.
#' @param DV_field the field to use as the dependent variable. Defaults to the
#'   two-character domain name followed by 'STRESN', if NULL.
#' @param analyte The name for the analyte. Defaults to the 'testcd', if NULL.
#' @param cmt The compartment for the observation as numeric.
#' @param parent The name of the parent analyte for the observation as
#'   character.
#' @param silent Suppress messages as logical.
#' @param observation_filter The filtering to apply to the observation source
#'   data.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param NTIME_lookup A data frame with two columns, a column that defines the
#'   custom nominal time information in the target domain (e.g., 'PCELTM'), and
#'   'NTIME'. This data frame is left_join()ed into the observation data frame
#'   to provide the NTIME field.
#' @param cleanup Keep only necessary fields, as logical.
#' @param testcd The xxTESTCD entry that corresponds to the analyte of interest,
#'   as character.
#' @param TESTCD_field The xxTESTCD field. Defaults to the two-character domain
#'   name followed by 'TESTCD', if NULL.
#'
#' @return A data frame.
#' @export
#' @import stringr
#'
make_observation <- function(
    sdtm,
    domain,
    testcd,
    analyte = NULL,
    parent = NA,
    DTC_field = NULL,
    DV_field = NULL,
    TESTCD_field = NULL,
    cmt = NA,
    observation_filter = {TRUE},
    # subject_filter = {!ACTARMCD %in% c("SCRNFAIL", "NOTTRT")},
    subject_filter = {TRUE},
    NTIME_lookup = NULL,
    silent = FALSE,
    cleanup = TRUE) {
  if(is.null(DTC_field)) DTC_field <- paste0(str_to_upper(domain), "DTC")
  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(str_to_upper(domain),
                                                   "TESTCD")
  if(is.null(analyte)) analyte <- testcd
  if(is.null(parent)) parent <- analyte

  # imp <- nif %>%
  #   as.data.frame() %>%
  #   filter(EVID == 1) %>%
  #   distinct(ANALYTE) %>%
  #   pull(ANALYTE)
  #
  # if(is.null(parent)) {
  #   if(analyte %in% imp) {
  #     parent <- analyte
  #   } else {
  #     parent <- guess_parent(nif)
  #     conditional_message(paste0("Parent for ", analyte, " was set to ",
  #                                parent, "!"), silent = silent)
  #   }
  # }

  sbs <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"),
                       subject_filter = {{subject_filter}}, cleanup = cleanup)

  obj <- domain(sdtm, str_to_lower(domain)) %>%
    lubrify_dates()

  if(is.null(NTIME_lookup)) {
    if(str_to_lower(domain) == "pc") {
      if("time_mapping" %in% names(sdtm) & nrow(sdtm$time_mapping) > 0 &
          any(names(sdtm$time_mapping) %in% names(obj))){
        NTIME_lookup <- sdtm$time_mapping
      } else {
        if("PCELTM" %in% names(obj)) {
          if("PCDY" %in% names(obj)){
            NTIME_lookup <- obj %>%
              distinct(.data$PCDY, .data$PCELTM) %>%
            mutate(NTIME = as.numeric(stringr::str_extract(
              .data$PCELTM, "PT([.0-9]+)H", group = 1)) #+
                # (.data[["PCDY"]] - 1) * 24)
            )
          } else {
            NTIME_lookup <- obj %>%
              distinct(.data$PCELTM) %>%
              mutate(NTIME = as.numeric(stringr::str_extract(
                .data$PCELTM, "PT([.0-9]+)H", group = 1)))
          }
        }
      }
    } else {
      xxdy <- paste0(str_to_upper(domain), "DY")
      if(xxdy %in% names(obj)) {
        NTIME_lookup <- distinct(obj, .data[[xxdy]]) %>%
          mutate(NTIME = (.data[[xxdy]] - 1) * 24)
      }
    }
  }

  obj %>%
    filter({{observation_filter}}) %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    mutate(
      DTC = .data[[DTC_field]],
      DV = .data[[DV_field]],
      ANALYTE = analyte,
      TIME = NA,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = FALSE,
      EVID = 0,
      MDV = as.numeric(is.na(DV)),
      IMPUTATION = "") %>%
    {if(!is.null(NTIME_lookup)) suppressMessages(
      left_join(., NTIME_lookup)) else
      mutate(., NTIME = NA)} %>%
    {if(cleanup == TRUE) select(., any_of(c("ID", "STUDYID", "USUBJID", "AGE",
      "SEX", "RACE",  "HEIGHT", "WEIGHT", "BMI", "DTC", "TIME", "NTIME", "TAFD",
      "PCELTM", "EVID", "AMT", "ANALYTE", "CMT",  "PARENT", "METABOLITE",
      "DOSE", "DV", "MDV", "ACTARMCD", "IMPUTATION"))) else .} %>%
    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFXSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    filter(!is.na(.data$DTC)) %>%
    new_nif()
}


#' Add a baseline filed to a nif object
#'
#' @param sdtm A sdtm object.
#' @param domain The domain as character.
#' @param DV_field the field to use as the dependent variable. Defaults to the
#'   two-character domain name followed by 'STRESN'.
#' @param observation_filter The filtering to apply to the observation source
#'   data.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param testcd The xxTESTCD to filter for, as character.
#' @param TESTCD_field The xxTESTCD field as character. Defaults to "xxTESTCD"
#'   where xx is the domain name.
#' @param baseline_filter The filter to identify the baseline value. Defaults to
#'   {xxBLFL == 'Y'} where xx is the domain name.
#' @param summary_function The summary funcition to apply if there are multiple
#'   baseline values, defaults to `mean`.
#' @param silent Suppress messages as logical.
#'
#' @return A data frame.
#'
#' @import rlang
#' @export
make_baseline <- function(
    sdtm,
    domain,
    testcd,
    DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = {TRUE},
    subject_filter = {TRUE},
    baseline_filter = NULL,
    summary_function = mean,
    silent = FALSE) {

  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(
    str_to_upper(domain), "TESTCD")
  if(is.null(baseline_filter)) baseline_filter <- paste0(
    ".data[['", str_to_upper(domain), "BLFL']] == 'Y'")

  sbs <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"),
                       subject_filter = {{subject_filter}}, cleanup = FALSE)

  temp <- domain(sdtm, str_to_lower(domain)) %>%
    lubrify_dates() %>%
    filter(eval_tidy(observation_filter)) %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    filter(!!parse_expr(baseline_filter)) %>%
    select("USUBJID", {{DV_field}}) %>%
    group_by(.data$USUBJID) %>%
    summarize(BL = summary_function(.data[[DV_field]], na.rm = TRUE)) %>%
    rename_with(~str_c("BL_", testcd), .cols = .data$BL)

  return(temp)
}


#' Derive administration time from the PCRFTDTC
#'
#' @param obj A data frame.
#' @param pc The corresponding PC domain as data frame.
#' @param analyte The analyte as string.
#' @param pctestcd The PCTESTCD corresponding to the analyte.
#'
#' @return A data frame.
#' @keywords internal
#' @export
impute_admin_times_from_pcrftdtc <- function(obj, pc, analyte, pctestcd) {
  pc_ref <- pc %>%
    filter(PCTESTCD == pctestcd) %>%
    mutate(ANALYTE = analyte) %>%
    decompose_dtc("PCRFTDTC") %>%
    select(c("USUBJID", "ANALYTE", "PCRFTDTC_date",
             "PCRFTDTC_time")) %>%
    distinct()

  # obj %>%
  #   decompose_dtc("DTC") %>%
  #   left_join(pc_ref,
  #             by = c("USUBJID", "ANALYTE", "DTC_date" = "PCRFTDTC_date")) %>%
  #   mutate(IMPUTATION = case_when(
  #     is.na(DTC_time) & !is.na(PCRFTDTC_time) ~
  #       paste(.data$IMPUTATION, "admin time imputed from PCRFTDTC"),
  #     .default = .data$IMPUTATION)) %>%
  #   mutate(DTC_time = case_when(is.na(DTC_time) ~ PCRFTDTC_time,
  #                               .default = .data$DTC_time)) %>%
  #   mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
  #   select(-c("PCRFTDTC_time", "DTC_date", "DTC_time"))

  obj %>%
    decompose_dtc("DTC") %>%
    left_join(pc_ref,
              by = c("USUBJID", "ANALYTE", "DTC_date" = "PCRFTDTC_date")) %>%
    mutate(IMPUTATION = case_when(
      !is.na(PCRFTDTC_time) ~
        paste(.data$IMPUTATION, "admin time imputed from PCRFTDTC"),
      .default = .data$IMPUTATION)) %>%
    mutate(DTC_time = case_when(!is.na(PCRFTDTC_time) ~ PCRFTDTC_time,
                                .default = .data$DTC_time)) %>%
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    select(-c("PCRFTDTC_time", "DTC_date", "DTC_time"))
}




#' Compile administration data frame
#'
#' @param sdtm A sdtm object.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param extrt The EXTRT for the administration, as character.
#' @param analyte The name of the analyte as character.
#' @param cmt The compartment for the administration as numeric.
#' @param cut_off_date The data cut-off date as Posix date-time.
#' @param silent Suppress messages, as logical.
#' @param cleanup Remove unnecessary fields, as logical.
#'
#' @return A data frame.
#' @export
#' @keywords internal
make_administration <- function(
    sdtm,
    extrt,
    analyte = NA,
    cmt = 1,
    cut_off_date = NULL,
    subject_filter = {!ACTARMCD %in% c("SCRNFAIL", "NOTTRT")},
    silent = FALSE,
    cleanup = TRUE) {
  dm <- domain(sdtm, "dm") %>% lubrify_dates()
  ex <- domain(sdtm, "ex") %>% lubrify_dates()
  pc <- domain(sdtm, "pc") %>% lubrify_dates()

  if(is.na(analyte)) {analyte <- extrt}
  if(is.null(cut_off_date)) cut_off_date <- last_ex_dtc(ex)

  sbs <- make_subjects(dm, domain(sdtm, "vs"),
                       subject_filter = {{subject_filter}}, cleanup = cleanup)

  admin <- ex %>%
    mutate(IMPUTATION = "") %>%
    filter(.data$EXTRT == extrt) %>%
    filter(.data$EXSTDTC <= cut_off_date) %>%
    decompose_dtc("EXSTDTC") %>%

    impute_exendtc_to_rfendtc(dm, silent = silent) %>%
    filter_EXSTDTC_after_EXENDTC(dm, silent = silent) %>%

    # time imputations
    # impute_exendtc_to_rfendtc(dm, silent = silent) %>%
    impute_exendtc_to_cutoff(cut.off.date = cut_off_date, silent = silent) %>%
    impute_missing_exendtc(silent = silent) %>%
    decompose_dtc("EXENDTC") %>%

    # make generic fields
    mutate(TIME = NA, NTIME = 0, ANALYTE = analyte, PARENT = analyte,
           METABOLITE = FALSE, DV = NA, CMT = cmt, EVID = 1, MDV = 1,
           DOSE = EXDOSE, AMT = EXDOSE) %>%

    # expand administration intervals to individual entries per administration
    rowwise() %>%
    mutate(DTC_date = list(seq(
      as.Date(.data$EXSTDTC_date),
      as.Date(.data$EXENDTC_date),
      by = "days"))) %>%
    unnest(.data$DTC_date) %>%

    # make time
    group_by(.data$USUBJID, .data$ANALYTE, .data$EXENDTC_date) %>%
    mutate(DTC_time = case_when(
      row_number() == n() ~ .data$EXENDTC_time,
      .default = .data$EXSTDTC_time
    )) %>%
    ungroup() %>%

    select(-c("EXSTDTC_date", "EXSTDTC_time", "EXENDTC_date",
              "EXENDTC_time")) %>%

    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%

    # impute missing administration times from PCRFTDTC
    {if("PCRFTDTC" %in% names(pc))
        impute_admin_times_from_pcrftdtc(., pc, analyte, analyte) else .} %>%

    # carry forward missing administration times
    decompose_dtc("DTC") %>%
    arrange(.data$USUBJID, .data$ANALYTE, .data$DTC) %>%
    mutate(IMPUTATION = case_when(
      # is.na(.data$DTC_time) == TRUE ~ append_statement(IMPUTATION, "time carried forward"),
      is.na(.data$DTC_time) == TRUE ~ "time carried forward",
      .default = .data$IMPUTATION)) %>%
    group_by(.data$USUBJID, .data$ANALYTE) %>%
    fill(.data$DTC_time, .direction = "down") %>%
    ungroup() %>%

    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%

    select(-c("DTC_date", "DTC_time")) %>%

    {if(cleanup == TRUE)
      select(., any_of(c("STUDYID", "USUBJID", "IMPUTATION", "TIME", "NTIME",
         "TAFD", "TAD", "ANALYTE", "PARENT", "METABOLITE", "DV", "CMT", "EVID",
         "MDV", "DOSE", "AMT", "EXDY", "DTC")))
      else .} %>%
    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFXSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    new_nif()

  return(admin)
}


#' Calculate TIME field
#'
#' @description
#' This function generates the following time fields:
#'
#' 'TIME' is the time in hours relative to the subject's first record, be it an
#' administration or observation event.
#'
#' 'TAFD' is the time in hours relative to the subject's first administration of
#' the respective parent. Note that if a subject has received multiple drugs
#' (parents), the 'TAFD' field refers to the respective first administration.
#'
#' @param obj A nif object.
#'
#' @return A nif object.
#' @export
#' @import assertr
#' @keywords internal
make_time <- function(obj) {
  obj %>%
    as.data.frame() %>%
    verify(has_all_names("ID", "DTC", "ANALYTE", "PARENT", "EVID")) %>%
    verify(is.POSIXct(.data$DTC)) %>%
    group_by(.data$ID) %>%
    mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(.data$ID, .data$PARENT) %>%
    mutate(FIRSTADMIN = min(.data$DTC[.data$EVID == 1], na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(TIME = round(
      as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "h")),
      digits = 3)) %>%
    mutate(TAFD = round(
      as.numeric(difftime(.data$DTC, .data$FIRSTADMIN, units = "h")),
      digits = 3)) %>%
    add_tad() %>%
    new_nif()
}


#' Complete DOSE field
#'
#' @param obj A nif object.
#'
#' @return A nif object.
#' @export
#' @keywords internal
carry_forward_dose <- function(obj) {
  obj %>%
    arrange(.data$ID, .data$DTC, -.data$EVID) %>%
    group_by(.data$ID, .data$PARENT) %>%
    fill(DOSE, .direction = "downup") %>%
    ungroup()
}


#' Add an observation to a nif object
#'
#' @param nif A nif object.
#' @inheritParams make_observation
#'
#' @return A nif object.
#' @export
add_observation <- function(nif, sdtm, domain, testcd,
    analyte = NULL, parent = NULL, DTC_field = NULL, DV_field = NULL,
    TESTCD_field = NULL, cmt = NULL, observation_filter = {TRUE},
    subject_filter = {!ACTARMCD %in% c("SCRNFAIL", "NOTTRT")},
    NTIME_lookup = NULL, silent = FALSE,
    cleanup = TRUE) {
  if(is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_message(paste0(
      "Compartment for ", testcd,
      " was not specified and has been set to ", cmt), silent = silent)
  }

  if(is.null(analyte)) analyte <- testcd

  imp <- nif %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    distinct(ANALYTE) %>%
    pull(ANALYTE)

  if(is.null(parent)) {
    if(analyte %in% imp) {
      parent <- analyte
    } else {
      parent <- guess_parent(nif)
      conditional_message(paste0("Parent for ", analyte, " was set to ",
                                 parent, "!"), silent = silent)
    }
  }

  obj <- bind_rows(nif,
            make_observation(
              sdtm = sdtm,
              domain = domain,
              testcd = testcd,
              analyte = analyte,
              parent = parent,
              DTC_field = DTC_field,
              DV_field = DV_field,
              TESTCD_field = TESTCD_field,
              cmt = cmt,
              observation_filter = {{observation_filter}},
              subject_filter = {{subject_filter}},
              NTIME_lookup = NTIME_lookup,
              silent = silent,
              cleanup = cleanup)
            ) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    mutate(ID = as.numeric(as.factor(.data$USUBJID))) %>%
    group_by(.data$USUBJID, .data$PARENT) %>%
    mutate(NO_ADMIN_FLAG = case_when(sum(EVID == 1) == 0 ~ TRUE,
                                     .default = FALSE)) %>%
    ungroup()

  n_no_admin <- sum(obj$NO_ADMIN_FLAG == TRUE)
  if(n_no_admin != 0) {
    conditional_message(paste0("Missing administration information in ",
      n_no_admin, " observations (did you set a\n",
      "parent for these observations?):\n",
      df_to_string(
          obj %>%
            filter(.data$NO_ADMIN_FLAG == TRUE) %>%
            group_by(.data$USUBJID, .data$PARENT, .data$ANALYTE) %>%
            mutate(N = sum(EVID == 0)) %>%
            ungroup() %>%
            distinct(.data$USUBJID, .data$PARENT, .data$ANALYTE, N),
        indent = "  ")),
      silent = silent)

    obj <- obj %>%
      filter(.data$NO_ADMIN_FLAG == 0)
  }

  obj %>%
    select(-c("NO_ADMIN_FLAG")) %>%
    make_time() %>%
    carry_forward_dose() %>%
    new_nif()
}


#' Add an administration to a nif object
#'
#' @param nif A nif object.
#' @inheritParams make_administration
#'
#' @return A nif object.
#' @export
add_administration <- function(
    nif, sdtm, extrt, analyte = NA, cmt = 1, cut_off_date = NULL,
    subject_filter = {!ACTARMCD %in% c("SCRNFAIL", "NOTTRT")},
    silent = FALSE, cleanup = TRUE) {
  bind_rows(nif, make_administration(
    sdtm = sdtm,
    extrt = extrt,
    analyte = analyte,
    cmt = cmt,
    cut_off_date = cut_off_date,
    subject_filter = {{subject_filter}},
    silent = silent,
    cleanup = cleanup
  )) %>%
    arrange(.data$USUBJID, .data$DTC) %>%
    mutate(ID = as.numeric(as.factor(.data$USUBJID))) %>%
    make_time() %>%
    carry_forward_dose() %>%
    new_nif()
}


#' Add baseline field
#'
#' @param nif A nif object.
#' @param sdtm A sdtm object.
#' @param domain The domain as character.
#' @param testcd The xxTESTCD with xx the domain name, as character.
#' @param DV_field The name of the DV field as character.
#' @param TESTCD_field The name of the TESTCD field. defaults to xxTESTCD with
#'   xx the domain name, as character.
#' @param observation_filter A filter term for the `domain`.
#' @param subject_filter A filter term for the DM domain.
#' @param baseline_filter A filter term to identify the baseline condition
#'   within the `domain`. Defaults to `xxBLFL == "Y"` with xx the domain name.
#' @param summary_function The summary function to summarize multiple baseline
#'   values. Defaults to `mean`.
#' @param silent Suppress messages, as logical.
#'
#' @return A nif object.
#' @import assertr
#' @export
add_baseline <- function(
    nif,
    sdtm,
    domain,
    testcd,
    DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = {TRUE},
    subject_filter = {TRUE},
    baseline_filter = NULL,
    summary_function = mean,
    silent = FALSE) {

  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(
    str_to_upper(domain), "TESTCD")
  if(is.null(baseline_filter)) baseline_filter <- paste0(
    ".data[['", str_to_upper(domain), "BLFL']] == 'Y'")

  sbs <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"),
                       subject_filter = {{subject_filter}})

  temp <- domain(sdtm, str_to_lower(domain)) %>%
    # verify(exec(has_all_names(!!!c("USUBJID", DV_field, TESTCD_field)))) %>%
    lubrify_dates() %>%
    filter({{observation_filter}}) %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    filter(!!parse_expr(baseline_filter)) %>%
    select("USUBJID", {{DV_field}}) %>%
    group_by(.data$USUBJID) %>%
    summarize(BL = summary_function(.data[[DV_field]], na.rm = TRUE)) %>%
    rename_with(~str_c("BL_", testcd), .cols = .data$BL)

  nif %>%
    left_join(temp, by = "USUBJID")
}








#' Subset nif to rows with DTC before the last individual or global observation
#'
#' @param obj A nif object.
#' @param individual Apply by ID, as logical.
#' @param keep_no_obs_sbs Retain subjects without observations.
#'
#' @return A nif object.
#' @export
limit <- function(obj, individual = TRUE, keep_no_obs_sbs = FALSE) {
  max_or_inf <- function(x) {
    if(length(x) == 0) return(max(obj$DTC, na.rm = TRUE))
    return(max(x, na.rm = TRUE))
  }

  if(keep_no_obs_sbs == FALSE) {
    obj <- obj %>%
      group_by(.data$ID) %>%
      filter(sum(EVID == 0) > 0) %>%
      ungroup()
  }
  if(individual == TRUE) {
    obj %>%
      # as.data.frame() %>%
      group_by(.data$ID) %>%
      mutate(LAST_OBS_DTC = max_or_inf(.data$DTC[EVID == 0])) %>%
      ungroup() %>%
      filter(.data$DTC <= .data$LAST_OBS_DTC) %>%
      select(-c("LAST_OBS_DTC")) %>%
      new_nif()
  } else {
    last_obs_dtc <- max(obj$DTC[obj$EVID == 0])
    obj %>%
      filter(.data$DTC <= last_obs_dtc) %>%
      new_nif()
  }
}


#' Filter nif object, eliminating subjects without observations or
#' administrations
#'
#' @param obj A nif object.
#' @param keep_no_obs Retain subjects without observations, as logical.
#' @param keep_no_admin Retain subjects without administeration, as logical.
#'
#' @return A nif object.
#' @export
filter_subjects <- function(obj,
                            keep_no_obs = FALSE,
                            keep_no_admin = FALSE) {
  obj %>%
    as.data.frame() %>%
    group_by(.data$ID) %>%
    # mutate(nadmin = sum(EVID==1)) %>%
    {if(keep_no_obs == FALSE) filter(., sum(EVID == 0) > 0) else .} %>%
    {if(keep_no_admin == FALSE) filter(., sum(EVID == 1) > 0) else .} %>%
    # {if(keep_no_admin == FALSE) filter(., nadmin > 0) else .} %>%
    # select(-nadmin) %>%
    new_nif()
}





