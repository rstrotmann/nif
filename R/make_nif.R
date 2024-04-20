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
    rename_with(~str_c("BL_", testcd), .cols = "BL")

  nif %>%
    left_join(temp, by = "USUBJID")
}


#' Add covariate column to nif object
#'
#' A time-varying covariate is added as a new field with daily time granularity
#' and carried forward for missing entries.
#'
#' @param nif A nif object.
#' @param sdtm The corresponding sdtm object.
#' @param domain The domain as character.
#' @param testcd The xxTESTCD with xx the domain name, as character.
#' @param DV_field The name of the DV field as character.
#' @param TESTCD_field The name of the TESTCD field. defaults to xxTESTCD with
#'   xx the domain name, as character.
#' @param observation_filter A filter term for the `domain`.
#' @param covariate The name of the covariate, defaults to the testcd if 'NULL'.
#' @param DTC_field The field to use as the date-time code for the observation.
#'   Defaults to the two-character domain name followed by 'DTC', if NULL.
#'
#' @return A nif object.
#' @export
#'
#' @examples
#' add_covariate(examplinib_poc_nif, examplinib_poc, "vs", "WEIGHT",
#'   covariate = "wt")
add_covariate <- function(nif, sdtm, domain, testcd,
    covariate = NULL,
    DTC_field = NULL, DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = {TRUE}
    ) {

  if(is.null(DTC_field)) DTC_field <- paste0(str_to_upper(domain), "DTC")
  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(str_to_upper(domain),
                                                   "TESTCD")
  if(is.null(covariate)) covariate <- str_to_upper(testcd)
  COV_field <- covariate

  obj <- domain(sdtm, str_to_lower(domain)) %>%
    lubrify_dates()

  cov <- domain(sdtm, str_to_lower(domain)) %>%
    filter(.data$USUBJID %in% unique(nif$USUBJID)) %>%
    lubrify_dates() %>%
    filter({{observation_filter}}) %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    select(.data$USUBJID, DTC = .data[[DTC_field]],
           !!COV_field := .data[[DV_field]]) %>%
    decompose_dtc("DTC") %>%
    select(-c("DTC", "DTC_time")) %>%
    distinct()

  temp <- nif %>%
    mutate(original = TRUE) %>%
    decompose_dtc("DTC") %>%
    full_join(cov, by = c("USUBJID", "DTC_date")) %>%
    arrange(.data$USUBJID, .data$DTC_date) %>%
    group_by(.data$USUBJID) %>%
    fill(!!COV_field) %>%
    ungroup() %>%
    filter(.data$original == TRUE) %>%
    select(-c("original", "DTC_date", "DTC_time")) %>%
    new_nif()

  return(temp)
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


# ' Filter nif object, eliminating subjects without observations or
# ' administrations
# '
# ' @param obj A nif object.
# ' @param keep_no_obs Retain subjects without observations, as logical.
# ' @param keep_no_admin Retain subjects without administeration, as logical.
# '
# ' @return A nif object.
# ' @export
# filter_subjects <- function(obj,
#                             keep_no_obs = FALSE,
#                             keep_no_admin = FALSE) {
#   obj %>%
#     as.data.frame() %>%
#     group_by(.data$ID) %>%
#     # mutate(nadmin = sum(EVID==1)) %>%
#     {if(keep_no_obs == FALSE) filter(., sum(EVID == 0) > 0) else .} %>%
#     {if(keep_no_admin == FALSE) filter(., sum(EVID == 1) > 0) else .} %>%
#     # {if(keep_no_admin == FALSE) filter(., nadmin > 0) else .} %>%
#     # select(-nadmin) %>%
#     new_nif()
# }





