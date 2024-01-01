#' Render data frame object to string
#'
#' This function renders a data.frame into a string similar to its
#' representation when printed without line numbers
#'
#' @param df The data.frame to be rendered
#' @param indent A string that defines the left indentation of the rendered
#'   output.
#' @param header Boolean to indicate whether the header row is to be included.
#' @param n The number of lines to be included, or all if NULL.
#' @return The output as string.
#' @import utils
#' @export
df_to_string <- function(df, indent = "", n = NULL, header = TRUE) {
  df <- as.data.frame(df) %>%
    mutate(across(everything(), as.character))
  max_widths <- as.numeric(lapply(
    rbind(df, names(df)),
    FUN = function(x) max(sapply(as.character(x), nchar), na.rm = TRUE)
  ))

  render_line <- function(line) {
    paste0(
      indent,
      paste0(
        mapply(
          function(element, width) {
            format(element, width = width + 3)
          },
          element = as.character(line), width = max_widths
        ),
        collapse = ""
      )
    )
  }

  if (header == TRUE) {
    out <- render_line(data.frame(as.list(names(df))))
  } else {
    out <- ""
  }

  if (!is.null(n)) {
    df <- utils::head(df, n = n)
  }

  temp <- lapply(as.list(as.data.frame(t(df))), render_line)
  out <- paste(out, paste(temp, collapse = "\n"), sep = "\n")

  return(out)
}


#' Check whether POSIX datetime object includes time information
#'
#' @param datetime The datetime object in POSIX format.
#'
#' @return A boolean value.
has_time <- function(datetime) {
  as.numeric(datetime) %% 86400 != 0
}


#' Recode SEX field in a data frame
#'
#' This function recodes the SEX field in a data frame. All numerical values are
#'   kept while "M" is recoded to 0 and "F" to 1.
#'   If your downstream analysis requires different coding, please manually
#'   re-code.
#'
#' @param obj The data.frame containing a SEX field
#' @return The output data frame
#' @import dplyr
recode_sex <- function(obj) {
  obj %>%
    dplyr::mutate(SEX = as.numeric(
      dplyr::case_match(as.character(SEX),
        "M" ~ 0,
        "F" ~ 1,
        "1" ~ 1,
        "0" ~ 0,
        .default = NA
      )
    ))
}


#' The list of expected date/time formats in the SDTM data
#'
dtc_formats <- c("%Y-%m-%dT%H:%M", "%Y-%m-%d", "%Y-%m-%dT%H:%M:%S", "%Y")


#' Convert date fileds to POSIX format
#'
#' This function converts date time code (DTC) variables from the
#' \href{https://w.wiki/8Bzr}{ISO 8601} format used in SDTM (i.e., something
#' like "2001-01-02T09:59" where date and
#' time are separated by "T") to standard POSIXct format. The names of the
#' variables to be converted need to be provided by `fields`.
#'
#' @param obj A data frame.
#' @param fields Date variable names as character.
#'
#' @return A data frame
#' @export
standardize_date_format <- function(obj, fields = NULL) {
  obj %>%
    dplyr::mutate_at(fields, function(x) {
      lubridate::as_datetime(x, format = dtc_formats)
    })
}


#' Convert date fields to ISO 8601 format
#'
#' @param obj A data frame.
#' @param fields Date variable names as character.
#'
#' @return A data frame.
#' @export
isofy_date_format <- function(obj, fields = NULL) {
  obj %>%
    dplyr::mutate_at(fields, function(x) {
      format(x, "%Y-%m-%dT%H:%M")
    })
}


#' Convert all DTC fields from ISO 8601 into POSIXct
#'
#' @param obj A data frame.
#'
#' @return A data frame.
lubrify_dates <- function(obj) {
  obj %>% dplyr::mutate_at(
    vars(ends_with("DTC")),
    function(x) {
      if (!is.POSIXct(x)) {
        x <- lubridate::as_datetime(x, format = dtc_formats)
      }
      return(x)
    }
  )
}


#' Convert all DTC fields into ISO 8601 string format
#'
#' @param obj A data frame.
#'
#' @return A data frame.
isofy_dates <- function(obj) {
  obj %>%
    dplyr::mutate_at(vars(ends_with("DTC")), ~ format(., "%Y-%m-%dT%H:%M"))
}


#' Compose DTC from date and time components
#'
#' @param date A date in POSIX or character format.
#' @param time A time in character format.
#'
#' @return A POSICct object
compose_dtc <- function(date, time) {
  data.frame(date = as.character(date), time = as.character(time)) %>%
    mutate(time = case_when(is.na(time) ~ "", .default = time)) %>%
    mutate(DTC = str_trim(paste(as.character(date), time))) %>%
    mutate(DTC = lubridate::as_datetime(DTC,
      format = c("%Y-%m-%d %H:%M", "%Y-%m-%d")
    )) %>%
    pull(DTC)
}


#' Extract date component of a POSICct object
#'
#' @param dtc The POSIX-formatted datetime.
#'
#' @return The date as character.
extract_date <- function(dtc) {
  format(dtc, format = "%Y-%m-%d")
}


#' Extract time component of a POSICct object
#'
#' @param dtc The POSIX-formatted datetime.
#'
#' @return The time as character.
extract_time <- function(dtc) {
  format(dtc, format = "%H:%M")
}


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
#'
#' @param ex The EX domain as data frame.
#' @param silent A boolean.
#'
#' @return The updated EX domain as data frame.
impute_missing_exendtc_time <- function(ex, silent = FALSE) {
  temp <- ex %>%
    verify(has_all_names(
      "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC",
      "EXENDTC"
    )) %>%
    lubrify_dates() %>%
    mutate(
      EXSTDTC_has_time = has_time(EXSTDTC),
      EXENDTC_has_time = has_time(EXENDTC)
    ) %>%
    # extract start and end dates and times from EXSTDTC and EXENDTC
    mutate(start.date = extract_date(EXSTDTC)) %>%
    dplyr::mutate(start.time = case_when(
      EXSTDTC_has_time == TRUE ~ extract_time(EXSTDTC),
      .default = NA
    )) %>%
    mutate(end.date = extract_date(EXENDTC)) %>%
    dplyr::mutate(end.time = case_when(
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
        ),
        .default = NA
      ) %>%
        str_trim() %>%
        as_datetime(format = c("%Y-%m-%d %H:%M", "%Y-%m-%d %H:%M:%S"))) %>%
      mutate(EXENDTC_new = case_when(
        impute_exendtc_time == TRUE ~ EXENDTC1,
        .default = EXENDTC
      )) %>%
      select(-EXENDTC1)

    conditional_message(
      "In ", sum(temp$impute_exendtc_time),
      " administrations, a missing time in EXENDTC is imputed from EXSTDTC:\n",
      temp %>%
        filter(impute_exendtc_time == TRUE) %>%
        select(USUBJID, EXTRT, EXSTDTC, EXENDTC, EXENDTC_new) %>%
        df_to_string(), "\n",
      silent = silent
    )

    temp <- temp %>%
      mutate(EXENDTC = EXENDTC_new) %>%
      select(-EXENDTC_new)
  }

  temp %>%
    select(-c(
      "EXENDTC_has_time", "EXSTDTC_has_time", "start.date",
      "start.time", "end.date", "end.time", "impute_exendtc_time"
    ))
}


#' Filter out EX events after the last dose as specified in RFENDTC
#'
#' @param ex The EX domain as data.frame.
#' @param dm The DM domain as data.frame
#' @param silent A boolean.
#'
#' @return The modified EX domain as data.frame.
exclude_exstdtc_after_rfendtc <- function(ex, dm, silent = FALSE) {
  ex %>%
    left_join(dm %>% select(USUBJID, RFENDTC),
              by = "USUBJID") %>%
    group_by(USUBJID) %>%
    filter(floor_date(EXSTDTC, "day") <=
             floor_date(RFENDTC, "day") |
             is.na(RFENDTC)) %>%
    select(-RFENDTC)
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
#'
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @param silent Boolean.
#'
#' @return The updated EX domain as data frame.
impute_exendtc_to_rfendtc <- function(ex, dm, silent = FALSE) {
  dm %>%
    verify(has_all_names("USUBJID", "RFSTDTC", "RFENDTC"))

  temp <- ex %>%
    verify(has_all_names("USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    arrange(USUBJID, EXTRT, EXSTDTC) %>%
    group_by(USUBJID, EXTRT) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    left_join(dm %>%
                select(USUBJID, RFSTDTC, RFENDTC), by = "USUBJID") %>%
    ungroup()

  replace_n <- temp %>%
    filter(is.na(EXENDTC) & !is.na(RFENDTC) & LAST_ADMIN == TRUE) %>%
    nrow()

  if (replace_n > 0) {
    conditional_message(
      replace_n,
      " subjects in which a missing EXENDTC will be set to RFENDTC:\n",
      df_to_string(
        temp %>%
          filter(is.na(EXENDTC) & !is.na(RFENDTC) & LAST_ADMIN == TRUE) %>%
          select(USUBJID, EXTRT, EXSTDTC, EXENDTC, RFENDTC)
      ), "\n",
      silent = silent
    )

    temp %>%
      mutate(EXENDTC = case_when(
        (LAST_ADMIN == TRUE & is.na(EXENDTC) & !is.na(RFENDTC)) ~ RFENDTC,
        .default = EXENDTC
      )) %>%
      select(-c(LAST_ADMIN, RFENDTC, RFSTDTC))
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
#' @param silent A boolean.
#'
#' @return The updated EX domain as data frame.
impute_missing_exendtc <- function(ex, silent = FALSE) {
  temp <- ex %>%
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    lubrify_dates() %>%
    arrange(USUBJID, EXSTDTC) %>%
    group_by(USUBJID, EXTRT) %>%
    mutate(next_start = lead(EXSTDTC)) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number())) %>%
    ungroup()

  to_replace <- temp %>%
    filter(is.na(EXENDTC) & LAST_ADMIN == FALSE)

  if (nrow(to_replace > 0)) {
    message(
      nrow(to_replace),
      " rows in EX had no EXENDTC. These values are imputed as the day ",
      "before\n",
      "the next EXSTDTC. The missing entries should be added to the SDTM data ",
      "set\n",
      "or manually imputed before running `make_nif()`.\n",
      "The following entries are affected:\n",
      df_to_string(to_replace %>%
                     select(USUBJID, EXSEQ, EXTRT, EXSTDTC, EXENDTC))
    )

    temp <- temp %>%
      mutate(EXENDTC1 = case_when(
        (is.na(EXENDTC) & LAST_ADMIN == FALSE) ~ next_start - days(1),
        .default = EXENDTC
      )) %>%
      mutate(EXENDTC = EXENDTC1) %>%
      select(-EXENDTC1)
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
#' @param silent Boolean.
#'
#' @return The updated EX domain as data frame.
#' @import assertr
impute_exendtc_to_cutoff <- function(ex, cut.off.date = NA, silent = FALSE) {
  temp <- ex %>%
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    lubrify_dates() %>%
    assertr::verify(is.POSIXct(EXSTDTC)) %>%
    assertr::verify(is.POSIXct(EXENDTC)) %>%
    # identify last administration per subject and EXTRT
    arrange(USUBJID, EXTRT, EXSTDTC) %>%
    group_by(USUBJID, EXTRT) %>%
    mutate(LAST_ADMIN = row_number() == max(row_number()))

  to_replace <- temp %>%
    filter(LAST_ADMIN == TRUE, is.na(EXENDTC))

  if (nrow(to_replace) > 0) {
    conditional_message("In ", nrow(to_replace), " subjects, EXENDTC is ",
      "absent and is replaced by the cut off date, ",
      format(cut.off.date, format = "%Y-%m-%d %H:%M"), ":\n",
      df_to_string(to_replace %>% select(USUBJID, EXTRT, EXSTDTC, EXENDTC)),
      "\n",
      silent = silent
    )
    temp <- temp %>%
      mutate(EXENDTC = case_when(
        (LAST_ADMIN == TRUE & is.na(EXENDTC)) ~ cut.off.date,
        .default = EXENDTC
      ))
  }
  temp %>%
    select(-LAST_ADMIN)
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
#'
#' @import assertr
#'
#' @return The enhanced EX domain as data frame.
make_exstdy_exendy <- function(ex, dm) {
  ex %>%
    assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
    assertr::verify(is.POSIXct(c(EXSTDTC, EXENDTC))) %>%
    left_join(
      dm %>%
        distinct(USUBJID, RFSTDTC),
      by = "USUBJID"
    ) %>%
    mutate(EXSTDY = floor(as.numeric(difftime(EXSTDTC, RFSTDTC),
                                     units = "days")) + 1) %>%
    mutate(EXENDY = floor(as.numeric(difftime(EXENDTC, RFSTDTC),
                                     units = "days")) + 1) %>%
    select(-RFSTDTC)
}


#' Make administration data set from EX domain
#'
#' This function expands the administration ranges specified by EXSTDTC and
#'  EXENDTC in each record of EX to that the result has individual records for
#'  all administrations, with the time point in the TIME column.
#'
#' @section Specific imputations:
#'  If the end date (EXENDTC) is missing, i.e., the administration is ongoing
#'  At the timeof the data cutoff of the SDTM data set, EXENDTC is replaced by
#'  cut.off.date (which needs to be supplied in the format used by SDTM)
#'
#' @param ex EX domain as data frame.
#' @param dm DM domain as data frame.
#' @param cut_off_date The cut-off date to be used where no EXENDTC is recorded,
#'  in POSIX format.
#' @param drug_mapping A data frame with the columns of EXTRT and PCTESTCD
#'  that associate both.
#'
#' @param silent Boolean value to indicate whether warnings should be printed.
#'
#' @return A tibble with individual administrations
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import assertr
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
    dplyr::filter(EXSTDTC <= cut_off_date) %>%
    # isolate start and end dates and times
    mutate(start.date = extract_date(EXSTDTC)) %>%
    dplyr::mutate(start.time = case_when(
      EXSTDTC_has_time == TRUE ~ extract_time(EXSTDTC),
      .default = NA
    )) %>%
    mutate(end.date = extract_date(EXENDTC)) %>%
    dplyr::mutate(end.time = case_when(
      EXENDTC_has_time == TRUE ~ extract_time(EXENDTC),
      .default = NA
    )) %>%
    make_exstdy_exendy(dm)

  ret <- admin %>%
    rowwise() %>%
    mutate(date = list(seq(as.Date(start.date),
                           as.Date(end.date),
                           by = "days"))) %>%
    unnest(c(date)) %>%
    dplyr::mutate(time = case_when(
      row_number() == n() ~ end.time,
      .default = start.time
    )) %>%
    dplyr::mutate(EXDY = EXSTDY + (row_number() - 1)) %>%
    dplyr::ungroup() %>%
    mutate(DTC = compose_dtc(date, time)) %>%
    # set treatment, standard fields
    dplyr::mutate(
      NTIME = 0, DV = NA, LNDV = NA, DOSE = EXDOSE, AMT = EXDOSE,
      EVID = 1
    ) %>%
    dplyr::mutate(TYPE = NA, CMT = 1, PCTPTNUM = 0, MDV = 1, RATE = 0) %>%
    dplyr::left_join(drug_mapping, by = "EXTRT") %>%
    mutate(ANALYTE = PCTESTCD)

  return(as.data.frame(ret))
}


#' Make observation data set from PC
#'
#' This function creates an observation data frame from PC SDTM data.
#'
#' @details
#' Nominal time is either derived from `PCTPTNUM` (if `use_pctptnum=TRUE`), or
#' from `PCELTM` (the relative nominal time). Both are permissible fields per
#' the CDISC specification and may be absent from the clinical data. In contrast
#' to `PCTPOTNUM`, `PCELTM` follows a defined format, i.e., the
#' \href{https://w.wiki/8Bzr}{ISO 8601} specification for time durations.
#'
#' Note that the DV is converted into mg/l assuming that PCSTRESN is provided
#' in mg/ml!
#'
#' @param pc The SDTM PC domain as a data.frame.
#' @param time_mapping The time mapping.
#' @param spec The specimen to be represented in the NIF data set as string
#'   (e.g., "BLOOD", "PLASMA", "URINE", "FECES"). When spec is an empty string
#'   (""), which is the default setting, the most likely specimen, i.e., "BLOOD"
#'   or "PLASMA" is selected, depending what is found in the PC data.
#' @param use_pctptnum Use PCTPTNUM as nominal time.
#' @param silent Boolean value to indicate whether warnings should be printed.
#'
#' @return A data frame with individual observations with certain NONMEM input
#' variables set
#' @import dplyr
#' @import lubridate
#' @import assertr
#' @seealso [add_time_mapping()]
make_obs <- function(pc,
                     time_mapping = NULL,
                     spec = NULL,
                     silent = FALSE,
                     use_pctptnum = FALSE) {
  # Assertions
  pc %>% verify(has_all_names("PCSPEC", "PCDTC", "PCSTRESN", "PCTESTCD"))

  # Filter for specific specimen, guess specimen if none defined
  pcspecs <- unique(pc$PCSPEC)
  standard_specs <- c(
    "PLASMA", "Plasma", "plasma", "SERUM", "Serum", "serum",
    "BLOOD", "Blood", "blood"
  )
  if (length(spec) == 0) {
    spec <- standard_specs[standard_specs %in% pcspecs][1]
    conditional_message("No specimen specified. Set to ", spec,
      " as the most likely.", "\n",
      silent = silent
    )
  }

  obs <- pc %>%
    dplyr::filter(PCSPEC %in% spec)

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
      dplyr::filter(PCSTAT != "NOT DONE")
  }

  # identify observation date and time
  obs <- obs %>%
    # extract date and time of observation
    mutate(DTC = PCDTC) %>%
    mutate(start.date = extract_date(DTC)) %>%
    mutate(start.time = case_when(has_time(PCDTC) ~ extract_time(DTC),
      .default = NA
    ))

  # identify nominal time
  if (use_pctptnum) {
    obs <- obs %>%
      dplyr::mutate(NTIME = as.numeric(PCTPTNUM))
  } else {
    if ("PCELTM" %in% names(pc)) {
      obs <- obs %>%
        dplyr::mutate(NTIME = as.numeric(
          stringr::str_extract(PCELTM, "PT([.0-9]+)H", group = 1)
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
    dplyr::mutate(
      EVID = 0, CMT = 2, AMT = 0, DV = PCSTRESN / 1000,
      LNDV = log(DV)
    ) %>%
    dplyr::mutate(MDV = case_when(is.na(DV) ~ 1, .default = 0)) %>%
    dplyr::mutate(ANALYTE = PCTESTCD, RATE = 0)

  return(obs %>% as.data.frame())
}


#' Extract last observation time from observation tibble
#'
#' .
#'
#' @param obs tibble as created with make_obs
#' @return A datetime object representing the last recorded observation time
#' @import dplyr
last_obs_dtc <- function(obs) {
  return(max(obs$DTC, na.rm = TRUE))
}


#' Last administration DTC
#'
#' @param ex The EX domain as data table.
#'
#' @return The last administration in DTC format.
#' @export
last_ex_dtc <- function(ex) {
  return(max(ex$EXENDTC, na.rm = TRUE))
}


#' Impute missing administration times
#'
#' This function fills in administration times from the PCREFDTC field when
#'  available, and carries forward last administration dates
#'
#'  PCRFTDTC is the time for nominal_time=0
#'  (ref: https://www.lexjansen.com/phuse/2016/pp/PP20.pdf)
#'
#' @param admin An admin data set as created by 'make_admin()'.
#' @param obs An observation data set as created by 'make_obs()'.
#' @return An admin data set.
#' @import tidyr
#' @import dplyr
impute_administration_time <- function(admin, obs) {
  # Assertions
  admin %>%
    verify(has_all_names("USUBJID", "date", "PCTESTCD"))

  obs %>%
    verify(has_all_names("DTC", "PCRFTDTC", "USUBJID", "PCTESTCD"))

  # 'reference' is the reference time by subject, analyte and observation date.
  # This is the PCRFDTC that is recorded along with observations in PC, i.e.,
  # the date/time of administration of the IMP related to the observation. This
  # is a 'permissible' field as per SDTM, i.e., it cannot be expected that is is
  # present in the data set.
  reference <- obs %>%
    mutate(dtc.date = extract_date(DTC)) %>%
    mutate(ref.date = extract_date(PCRFTDTC)) %>%
    mutate(ref.time = extract_time(PCRFTDTC)) %>%
    dplyr::filter(dtc.date == ref.date) %>%
    dplyr::group_by(USUBJID, dtc.date, PCTESTCD) %>%
    dplyr::distinct(ref.time)

  ret <- admin %>%
    mutate(dtc.date = extract_date(date)) %>%
    dplyr::left_join(reference, by = c(
      "USUBJID" = "USUBJID", "dtc.date" = "dtc.date",
      "EXTRT" = "PCTESTCD"
    )) %>%
    # take time for administration from PCREFDTC where time is not available
    #   from EX
    dplyr::mutate(admin.time = case_when(
      !is.na(ref.time) ~ ref.time,
      .default = time
    )) %>%
    mutate(admin.time = case_when(is.na(admin.time) ~ "",
      .default = admin.time
    )) %>%
    dplyr::group_by(USUBJID, EXTRT) %>%
    dplyr::arrange(date) %>%
    # carry forward/back last administration time
    tidyr::fill(admin.time, .direction = "downup") %>%
    dplyr::ungroup() %>%
    mutate(DTC = compose_dtc(date, admin.time)) %>%
    dplyr::select(-admin.time, -ref.time)
  return(ret)
}



#' Extract baseline vital sign covariates from VS
#'
#' @param vs The VS domain as data frame.
#' @param silent Boolean to indicate whether message output should be provided.
#'
#' @return Baseline VS data as wide data frame.
#' @export
#' @examples
#' baseline_covariates(examplinib_sad$vs)
#'
baseline_covariates <- function(vs, silent = FALSE) {
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
    dplyr::group_by(USUBJID, VSTESTCD) %>%
    dplyr::summarize(mean = mean(VSSTRESN), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = VSTESTCD, values_from = mean)

  # Calculate BMI if height and weight are available
  if ("HEIGHT" %in% colnames(bl_cov) && "WEIGHT" %in% colnames(bl_cov)) {
    bl_cov <- bl_cov %>%
      mutate(BMI = WEIGHT / (HEIGHT / 100)^2)
  }
  return(bl_cov)
}


#' Issue message based on silent flag
#'
#' @param msg The message as character.
#' @param silent A boolean.
#' @param ... Further message components.
#'
#' @return Nothing.
conditional_message <- function(msg, ..., silent = FALSE) {
  parameters <- c(as.list(environment()), list(...))
  parameters <- lapply(parameters, as.character)
  if (!silent) {
    message(paste(as.character(parameters[names(parameters) != "silent"])))
  }
}


#' Create the drug mapping data frame from
#'
#' @param sdtm_data The sdtm data as SDTM object.
#'
#' @return A data frame.
make_drug_mapping <- function(sdtm_data) {
  drug_mapping <- sdtm_data$analyte_mapping %>%
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
    distinct()
}


#' Add TIME field to table
#'
#' TIME is created as the difference between the DTC field and the first DTC
#' field on the USUBJID level. TIME is in hours, rounded by 3 digits.
#'
#' @param x The table as data frame.
#'
#' @return A data frame with FIRSTDTC and TIME added.
add_time <- function(x) {
  x %>%
    assertr::verify(has_all_names("USUBJID", "DTC")) %>%
    assertr::verify(is.POSIXct(DTC)) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(FIRSTDTC = min(DTC, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TIME = round(
      as.numeric(difftime(DTC, FIRSTDTC, units = "h")),
      digits = 3
    ))
}


#' Make a NIF data set from SDTM-formatted data
#'
#' This function makes a basic NONMEM input file (NIF) data set from
#' SDTM-formatted clinical study data following the conventions summarized in
#' [Bauer, CPT Pharmacometrics Syst. Pharmacol. (2019)](https://doi.org/10.1002/psp4.12404).
#' For a more in-depth tutorial, see `vignette("nif-vignette")`.
#'
#' @section Imputations:
#' Subjects with administration but no observations for the respective
#' analyte are deleted from the data set. For further imputations, see
#' `vignette("nif-imputations")`.
#'
#' @section Output fields:
#' * `ID` Subject identification number
#' * `TIME` Recorded time of administration or observation events in hours
#'      relative to the first individual event.
#' * `AMT` Dose administered for dosing record, or zero for observations.
#' * `DOSE` Dose in mg for administrations and post-dose observations.
#' * `DV` The dependent variable, i.e., observed concentration, or zero for
#'      administration records, in mg/l.
#' * `LNDV` The natural Log of DV.
#' * `RATE` Rate of infusion of drug or zero if drug is given as a bolus.
#' * `MDV` One for missing DV, else zero.
#' * `EVID` Event ID: 0 for observations, 1 for administrations.
#' * `CMT` Pharmacokinetic compartment. Will be set to 1 for administrations
#'      and 2 for observations. Should be changed afterwards, if needed.
#' * `DTC` The date-time of the data record.
#' * `FIRSTDTC` Date and time of first event per subject. This field is used
#'      internally for the calculation of `TIME`. Although it is not needed
#'      for NONMEM analysis, it is provided for subsequent NIF file building
#'      steps, e.g., addition of further time-dependent endpoints.
#' * `FIRSTADMINDTC` The date-time of the first administration of the
#'      respective parent drug for the respective subject.
#' * `FIRSTTRTDTC` The date-time of the first administration of any parent
#'      drug for the respective subject.
#' * `ANALYTE` The analyte or drug in the data record.
#' * `TRTDY` The treatment day, i.e., the relative day after the first
#'      treatment for the respective subject.
#'
#' @param sdtm_data A `sdtm` object, i.e., essentially a list of SDTM domains
#' as data tables. Typically, the SDTM data are loaded using [read_sdtm_sas()]
#' or [read_sdtm_xpt()]. As a minimum, the
#' following SDTM domains are needed: DM, VS, PC and EX.
#'
#' @param spec The sample specimen for the PC data as string
#' (e.g., "BLOOD", "PLASMA", "URINE", "FECES"). When spec is NULL (default),
#' the most likely specimen is selected.
#'
#' @param impute_administration_time A boolean value to indicate whether the
#' time of administration is to be imputed from the PCRFDTC field from the
#' PC domain. This field is 'permissible' and may not be present in certain
#' SDTM data. The default is 'TRUE'.
#'
#' @param truncate_to_last_observation Boolean to indicate whether the data set
#' should be truncated to the last observation. In this case, administrations
#' after the last observation time point will deleted. The default is 'TRUE'.
#'
#' @param silent Boolean value to indicate whether warnings should be printed.
#' The default is 'FALSE'.
#'
#' @param use_pctptnum Boolean to indicate whether to derive nominal time
#' ('NTIME') from 'PCTPTNUM'.
#'
#' @return A NIF object.
#' @seealso [summary.nif()]
#' @seealso [plot.nif()]
#' @seealso [write_nif()]
#' @seealso [write_csv.nif()]
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' make_nif(examplinib_fe)
#'
make_nif <- function(
    sdtm_data,
    spec = NULL,
    impute_administration_time = TRUE,
    silent = FALSE,
    truncate_to_last_observation = TRUE,
    use_pctptnum = TRUE) {
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

  ex <- ex %>%
    impute_missing_exendtc_time(silent = silent) %>%
    exclude_exstdtc_after_rfendtc(dm, silent = silent) %>%
    impute_exendtc_to_rfendtc(dm, silent = silent) %>%
    impute_missing_exendtc(silent = silent)

  bl_cov <- baseline_covariates(vs, silent = silent)
  drug_mapping <- make_drug_mapping(sdtm_data)

  # make observations
  obs <- make_obs(pc,
    time_mapping = sdtm_data$time_mapping,
    spec = spec, silent = silent, use_pctptnum = use_pctptnum
  ) %>%
    left_join(drug_mapping, by = "PCTESTCD")

  # define cut-off date
  if (truncate_to_last_observation == TRUE) {
    cut.off.date <- last_obs_dtc(obs)
    conditional_message("Data cut-off was set to last observation time, ",
      cut.off.date, "\n",
      silent = silent
    )
  } else {
    cut.off.date <- last_ex_dtc(ex)
  }

  ex <- ex %>%
    impute_exendtc_to_cutoff(cut.off.date, silent = silent) %>%
    filter(EXENDTC >= EXSTDTC)

  # identify subjects with observations by analyte
  obs_sbs <- obs %>%
    filter(!is.na(PCSTRESN)) %>%
    tidyr::unite("ut", USUBJID, PCTESTCD, remove = FALSE) %>%
    dplyr::distinct(USUBJID, PCTESTCD, ut)

  # make administrations
  admin <- make_admin(ex, dm,
    drug_mapping = drug_mapping, cut.off.date,
    silent = silent
  )

  # Remove all administrations with PCTESTCD==NA
  #  those rows may come from treatments that have no analyte mapping
  admin <- admin %>%
    dplyr::filter(!is.na(PCTESTCD))

  # filter admin for subjects who actually have observations
  no_obs_sbs <- admin %>%
    tidyr::unite("ut", USUBJID, PCTESTCD, remove = FALSE) %>%
    dplyr::filter(!(ut %in% obs_sbs$ut)) %>%
    dplyr::distinct(USUBJID, PCTESTCD, ut) %>%
    as.data.frame()

  # Issue message about excluded administrations, if applicable
  if (nrow(no_obs_sbs) > 0) {
    out <- no_obs_sbs %>%
      dplyr::arrange(PCTESTCD, USUBJID) %>%
      dplyr::select(USUBJID, PCTESTCD) %>%
      df_to_string()

    conditional_message("The following subjects had no observations for ",
      "the respective analyte and were removed from the data set:\n",
      out, "\n",
      silent = silent
    )
  }
  # and filter out excluded administrations
  admin <- admin %>%
    tidyr::unite("ut", USUBJID, PCTESTCD, remove = FALSE) %>%
    dplyr::filter(ut %in% obs_sbs$ut) %>%
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

  # impute administration times
  if (impute_administration_time == TRUE) {
    admin <- impute_administration_time(admin, obs)
  }

  # calculate age from birthday and informed consent signature date
  if ("RFICDTC" %in% colnames(dm) && "BRTHDTC" %in% colnames(dm)) {
    dm <- dm %>%
      dplyr::mutate(age1 = floor(as.duration(interval(BRTHDTC, RFICDTC)) /
                                   as.duration(years(1)))) %>%
      dplyr::mutate(AGE = case_when(is.na(AGE) ~ age1, .default = AGE)) %>%
      dplyr::select(-age1)
  }

  ## assemble NIF data set from administrations and observations and baseline
  #    data.
  nif <- obs %>%
    dplyr::bind_rows(
      admin %>%
        dplyr::filter(USUBJID %in% obs$USUBJID)
    ) %>%
    dplyr::left_join(dm, by = c("USUBJID", "STUDYID")) %>%
    dplyr::left_join(bl_cov, by = "USUBJID") %>%
    # filter out rows without parent information
    dplyr::filter(PARENT != "") %>%
    # filter out observations without administration
    dplyr::group_by(USUBJID, PARENT) %>%
    dplyr::filter(sum(AMT) != 0) %>%
    dplyr::ungroup() %>%
    # identify first administration per PARENT and first treatment overall
    group_by(USUBJID, PARENT) %>%
    dplyr::mutate(FIRSTADMINDTC = min(DTC[EVID == 1], na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(USUBJID) %>%
    mutate(FIRSTTRTDTC = min(FIRSTADMINDTC, na.rm = TRUE)) %>%
    ungroup() %>%
    dplyr::arrange(USUBJID, PARENT, DTC, EVID) %>%
    dplyr::group_by(USUBJID, PARENT) %>%
    tidyr::fill(DOSE, .direction = "down") %>%
    tidyr::fill(any_of(c(
      "AGE", "SEX", "RACE", "ETHNIC", "ACTARMCD", "HEIGHT",
      "WEIGHT", "COUNTRY", "ARM", "SUBJID"
    )), .direction = "down") %>%
    dplyr::ungroup() %>%
    add_time() %>%
    mutate(TRTDY = interval(date(FIRSTTRTDTC), date(DTC)) / days(1) + 1) %>%
    recode_sex() %>%
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(ID = as.numeric(as.factor(USUBJID))) %>%
    dplyr::relocate(ID) %>%
    new_nif() %>%
    add_tad() %>%
    index_nif()

  return(nif)
}


#' This function orders a NIF data set and adds a REF field
#'
#' The input data format expected by NONMEM requires all rows ordered by ID
#' and TIME, and indexed sequentially on a subject level with a REF field.
#' Re-indexing may be required if a NIF data set is extended, e.g., by merging
#' in further data.
#'
#' @param nif NIF data set, e.g., as created by [make_nif()] and manually
#' modified.
#' @return The updated NIF dataset including an updated REF field.
#' @import dplyr
#' @export
#' @examples
#' index_nif(examplinib_fe_nif)
#'
index_nif <- function(nif) {
  nif %>%
    as.data.frame() %>%
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(REF = row_number()) %>%
    dplyr::relocate(REF) %>%
    new_nif()
}


#' This function removes columns from a NIF data set that are not needed for
#' downstream analysis
#'
#' During creating of a NIF data set using [make_nif()], multiple SDTM tables
#' are aggregated without deleting the original fields (columns). Many of these
#' fields may not be required for the final analysis. This function reduces the
#' fields to those typically required in the downstream analysis. Applying
#' [compress_nif()] is typically the last step in the creation of a NIF data
#' set, after creating the basic NIF data set  with [make_nif()] and applying
#' custom imputations and manually deriving convariates as needed.
#'
#' @param nif A NIF data set.
#'
#' @param ... Further optional parameters are fields to be included in the
#'  output. If none are provided, the standard set will be used.
#'
#' @return A NIF dataset with only the specified fields
#' @import dplyr
#' @import lifecycle
#' @export
#' @examples
#' compress_nif(examplinib_fe_nif)
#'
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
    dplyr::select(any_of(c(columns, starts_with("BL_")))) %>%
    new_nif()
}


#' Generic function for compress
#'
#' @param nif A NIF data set object.
#' @param fields Fields to be kept as character.
#'
#' @return A NIF data set object.
#' @export
compress <- function(nif, fields = standard_nif_fields) {
  UseMethod("compress")
}


#' Compress a NIF data set
#'
#' @param nif A NIF data set object.
#' @param fields Fields to be included as character. If 'fields' is NULL
#' (default), the fields defined in `standard_nif_fields` plus all fields with
#' a name that starts with 'BL_' (baseline covariates) are included.
#'
#' @return A NIF data set object.
#' @export
compress.nif <- function(nif, fields = NULL) {
  if (is.null(fields)) {
    fields <- c(
      standard_nif_fields,
      colnames(examplinib_poc_nif)[grep("BL_", colnames(examplinib_poc_nif))])
  }
  nif %>%
    as.data.frame() %>%
    dplyr::select(any_of(fields)) %>%
    new_nif()
}


#' Add time-after-dose (TAD) field
#'
#' @param nif A NIF object.
#'
#' @return The updated NIF object
#' @export
#'
#' @examples
#' add_tad(examplinib_poc_nif)
add_tad <- function(nif) {
  nif %>%
    as.data.frame() %>%
    mutate(admin_time = case_when(
      EVID == 1 ~ TIME)) %>%
    group_by(ID, PARENT) %>%
    fill(admin_time, .direction = "down") %>%
    mutate(TAD = TIME - admin_time) %>%
    select(-admin_time) %>%
    new_nif()
}


#' This function reduces a NIF data set on the subject level by excluding all
#' administrations after the last observation
#'
#' @param nif NIF dataset as created by make_raw_nif() and potentially modified
#' for additional covariates.
#' @return A NIF dataset
#' @import dplyr
#' @export
clip_nif <- function(nif) {
  last_obsobs <- nif %>%
    dplyr::filter(EVID == 0) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(last.obs = max(TIME)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(USUBJID, last_obsobs)

  ret <- nif %>%
    dplyr::left_join(last_obsobs, by = "USUBJID") %>%
    dplyr::filter(TIME <= last_obsobs)
  return(new_nif(ret))
}


#' Add a dose level (`DL`) column to NIF data set.
#'
#' Dose level is defined as the starting dose. For data sets with single drug
#'   administration, `DL`is a numerical value, for drug combinations, it is
#'   a character value specifying the `PARENT` and dose level for the individual
#'   components.
#'
#' @param obj A NIF dataset.
#'
#' @return A NIF dataset.
#' @export
add_dose_level <- function(obj) {
  temp <- obj %>%
    as.data.frame() %>%
    filter(METABOLITE == FALSE) %>%
    filter(PARENT != "", !is.na(DOSE), AMT != 0, EVID == 1) %>%
    group_by(ID, ANALYTE) %>%
    arrange(ID, TIME, ANALYTE) %>%
    filter(TIME == min(TIME)) %>%
    select(ID, ANALYTE, DOSE) %>%
    group_by(ID)

  if (temp %>%
        ungroup() %>%
        distinct(ANALYTE) %>%
        nrow() == 1) {
    temp <- temp %>% mutate(DL = DOSE)
  } else {
    temp <- temp %>%
      mutate(DL = paste0(DOSE, "-", ANALYTE)) %>%
      arrange(ID) %>%
      arrange(factor(ANALYTE, levels = analytes(obj))) %>%
      summarize(DL = paste0(DL, collapse = "+"))
  }

  return(obj %>% left_join(temp %>% select(ID, DL), by = "ID"))
}


#' Add treatment day
#'
#' @param obj The NIF data set as data frame.
#'
#' @return The updated NIF data set as data frame.
add_trtdy <- function(obj) {
  obj %>%
    # assertr::verify(has_all_names("USUBJID", "DTC", "EVID")) %>%
    assertr::verify(has_all_names("ID", "DTC", "EVID")) %>%
    assertr::verify(is.POSIXct(DTC)) %>%
    # dplyr::group_by(USUBJID) %>%
    dplyr::group_by(ID) %>%
    dplyr::mutate(FIRSTTRTDTC = min(DTC[EVID == 1], na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    mutate(TRTDY = interval(date(FIRSTTRTDTC), date(DTC)) / days(1)) %>%
    mutate(TRTDY = case_when(TRTDY < 0 ~ TRTDY, .default = TRTDY + 1)) %>%
    select(-FIRSTTRTDTC)
}


#' Add baseline covariates to a NIF data set
#'
#' Lab parameters not found in LB will be reported in a warning message.
#'
#' @param obj NIF dataset.
#' @param lb SDTM LB domain as data frame.
#' @param lbspec The specimen, usually "BLOOD" or "URINE".
#' @param lbtestcd Lab parameters as encoded by LBTESTCD, as strings.
#' @param silent Boolean value to indicate whether warnings should be printed.
#' @return A NIF dataset
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom rlang .data
#' @export
add_bl_lab <- function(obj, lb, lbtestcd, lbspec = "", silent = FALSE) {
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
    dplyr::select(.data$USUBJID, .data$LBTESTCD, .data$LBSTRESN) %>%
    tidyr::pivot_wider(names_from = "LBTESTCD", values_from = "LBSTRESN") %>%
    dplyr::rename_with(~ stringr::str_c("BL_", .), .cols = -1)

  obj %>%
    as.data.frame() %>%
    dplyr::left_join(temp, by = "USUBJID") %>%
    new_nif()
}


#' Add lab covariate
#'
#' This functions adds columns for the lab parameters specified in `lbtestcd`,
#' in a time-varying way, i.e., the actual lab value at the time of the
#' observation or administration. This is in contrast to  [add_bl_lab()].
#'   In rows of missing lab data, the last value is carried forward.
#'
#'   Note that for some lab parameters, e.g., leukocytes, bili, etc., there may
#'   be observations both in serum and in urine. It is therefore necessary to
#'   specify the specimen tested. This corresponds to the `LBSPEC` field used in
#'   the LB SDTM domain.
#'
#' @param obj The NIF data set.
#' @param lb The LB SDTM domain
#' @param lbspec The specimen, e.g., SERUM.
#' @param lbtestcd Lab parameters to be included as character scalar or vector.
#' @param silent Boolean value to indicate whether warnings should be printed.
#' @return A NIF data set
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
    dplyr::select(
      .data$USUBJID, .data$date, .data$labdate, .data$LBTESTCD,
      .data$LBSTRESN
    ) %>%
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


#' Add lab value as observation
#'
#' @param obj The NIF data set.
#' @param lb The LB SDTM domain.
#' @param lbspec The LBSPECT.
#' @param lbtestcd The LBTESTCD.
#' @param cmt A numerical value to specify the compartment this observation is
#'   assigned to.
#' @param silent Boolean value to indicate whether warnings should be printed.
#'
#' @return The resulting NIF data set.
#' @export
add_lab_observation <- function(obj, lb, lbtestcd, cmt = NULL, lbspec = "",
                                silent = FALSE) {
  obj %>%
    verify(has_all_names("USUBJID", "TIME", "EVID"))

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
      " was not specified and has beenset to ", cmt
    ))
  }

  lb_params <- lb %>%
    verify(has_all_names("USUBJID", "LBDTC", "LBSPEC", "LBTESTCD", "LBDY")) %>%
    lubrify_dates() %>%
    dplyr::filter(USUBJID %in% (obj %>%
                                  subjects() %>%
                                  pull(USUBJID))) %>%
    mutate(DTC = LBDTC) %>%
    dplyr::filter(LBSPEC %in% lbspec) %>%
    dplyr::filter(LBTESTCD == lbtestcd) %>%
    left_join(obj %>%
                add_time() %>%
                as.data.frame() %>%
                distinct(USUBJID, FIRSTDTC),
              by = "USUBJID") %>%
    dplyr::mutate(
      ANALYTE = lbtestcd, PARENT = "", CMT = cmt, MDV = 0, EVID = 0,
      AMT = 0, RATE = 0
    ) %>%
    dplyr::mutate(TIME = round(as.numeric(difftime(DTC, FIRSTDTC, units = "h")),
      digits = 3
    )) %>%
    dplyr::mutate(DV = LBSTRESN) %>%
    dplyr::mutate(LNDV = log(DV)) %>%
    dplyr::mutate(NTIME = case_when(LBDY < 0 ~ LBDY * 24,
      .default = (LBDY - 1) * 24
    )) %>%
    dplyr::select(
      STUDYID, USUBJID, DTC, FIRSTDTC, ANALYTE, PARENT, CMT, EVID,
      TIME, NTIME, DV, LNDV, AMT, RATE
    )

  temp <- obj %>%
    as.data.frame() %>%
    bind_rows(lb_params) %>%
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(REF = row_number()) %>%
    dplyr::group_by(USUBJID) %>%
    tidyr::fill(starts_with("BL_"), .direction = "downup") %>%
    tidyr::fill(
      any_of(c(
        "ID", "AGE", "SEX", "RACE", "BMI", "ACTARMCD",
        "HEIGHT", "WEIGHT", "DOSE", "PART", "COHORT"
      )),
      .direction = "downup"
    ) %>%
    mutate(MDV = case_when(is.na(DV) ~ 1, .default = 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TIME = round(
      as.numeric(difftime(DTC, FIRSTDTC, units = "h")),
      digits = 3
    )) %>%
    add_trtdy() %>%
    add_tad() %>%
    index_nif()

  # return(new_nif(temp))
}
