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
#' @param cut_off_date The cut-off date as POSIXct.
#' @param extrt The treatment.
#'
#' @return The updated EX domain as data frame.
#' @noRd
#' @import cli
impute_exendtc_to_rfendtc <- function(
  ex,
  dm,
  extrt,
  cut_off_date = NULL,
  silent = NULL
) {
  # Validate ex domain
  expected_ex_columns <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
  missing_ex_columns <- setdiff(expected_ex_columns, names(ex))
  n <- length(missing_ex_columns)
  if (n > 0) {
    stop(paste0(
      "Missing ", plural("column", n > 1), " in domain EX: ",
      nice_enumeration(missing_ex_columns)
    ))
  }

  if (!extrt %in% unique(ex$EXTRT)) {
    stop(paste0("EXTRT ", extrt, " not found in EX domain!"))
  }

  if (!"IMPUTATION" %in% names(ex)) {
    ex <- mutate(ex, IMPUTATION = "")
  }

  dm <- lubrify_dates(dm)

  # identify rows to be imputed
  temp <- ex |>
    lubrify_dates() |>
    arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC) |>
    group_by(.data$USUBJID, .data$EXTRT) |>
    mutate(LAST_ADMIN = row_number() == max(row_number())) |>
    ungroup() |>
    filter(.data$EXTRT == extrt)

  to_be_imputed <- temp |>
    filter(.data$LAST_ADMIN == TRUE & is.na(.data$EXENDTC))

  n_sbs_to_be_imputed <- to_be_imputed |>
    distinct(.data$USUBJID) |>
    nrow()

  replace_n <- 0

  if (nrow(to_be_imputed) > 0) {
    # check whether RFENDTC is available
    if (!"RFENDTC" %in% names(dm)) {
      conditional_cli(
        {
          cli::cli_alert_warning("Imputation warning!")
          cli::cli_text(paste0(
            "Cannot impute missing EXENDTC in final administration episode of ",
            n_sbs_to_be_imputed, plural(" subject", n_sbs_to_be_imputed > 1),
            " because RFENDTC is missing in DM:"
          ))

          cli::cli_verbatim(
            df_to_string(to_be_imputed, indent = 2)
          )
          cli::cli_text()
        },
        silent = silent
      )
      return(ex)
    }

    # conduct imputation
    temp <- temp |>
      left_join(
        select(dm, c("USUBJID", "RFENDTC")),
        by = "USUBJID"
      )

    # Capture rows to be imputed for message (before imputation)
    rows_for_message <- temp |>
      filter(is.na(.data$EXENDTC) & !is.na(.data$RFENDTC) &
               .data$LAST_ADMIN == TRUE)

    replace_n <- nrow(rows_for_message)

    if (replace_n > 0) {
      conditional_cli(
        {
          cli_alert_info("Missing EXENDTC")
          cli_text(paste0(
            replace_n, " ",
            plural("subject", replace_n > 1),
            " had a missing EXENDTC in their final administration episode.\n",
            "In these cases, EXENDTC was imputed to RFENDTC."
          ))
          cli_verbatim(df_to_string(
            rows_for_message |>
              select(any_of(
                c("USUBJID", "EXTRT", "EXSEQ", "EXSTDTC", "EXENDTC", "RFENDTC")
              )),
            indent = 2
            # abbr_lines = 5, abbr_threshold = 20
          ))
          cli_text()
        },
        silent = silent
      )
    }

    temp <- temp |>
      mutate(IMPUTATION = case_when(
        (.data$LAST_ADMIN == TRUE & is.na(.data$EXENDTC) &
           !is.na(.data$RFENDTC)) ~ "missing EXENDTC set to RFENDTC",
        .default = .data$IMPUTATION
      )) |>
      mutate(EXENDTC = case_when(
        (.data$LAST_ADMIN == TRUE & is.na(.data$EXENDTC) &
           !is.na(.data$RFENDTC)) ~ .data$RFENDTC,
        .default = .data$EXENDTC
      )) |>
      select(-any_of(c("LAST_ADMIN", "RFENDTC")))

    # Merge temp back with original ex to preserve all rows (including
    # non-matching EXTRT) Get rows that were not filtered (non-matching EXTRT)
    other_rows <- ex |>
      lubrify_dates() |>
      filter(.data$EXTRT != extrt)

    # Combine filtered and imputed rows with other rows
    bind_rows(temp, other_rows) |>
      arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC)
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
#' global cut off date using [nif::impute_exendtc_to_cutoff()].
#'
#' As this function conducts rather aggressive imputations, the message output
#' is not optional, i.e., cannot be suppressed using the global 'silent' option,
#' but is issued in all cases.
#'
#' @param ex The updated EX domain as data frame.
#' @param silent Suppress messages.
#' @return The updated EX domain as data frame.
#' @noRd
impute_missing_exendtc <- function(ex, silent = NULL) {
  # Input validation
  expected_columns <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")

  missing_columns <- setdiff(expected_columns, names(ex))
  n <- length(missing_columns)
  if (n > 0) {
    stop(paste0(
      "Missing ", plural("column", n > 1), " in domain EX: ",
      nice_enumeration(missing_columns)
    ))
  }

  if (!"IMPUTATION" %in% names(ex)) {
    ex <- mutate(ex, IMPUTATION = "")
  }

  temp <- ex |>
    lubrify_dates() |>
    arrange(.data$USUBJID, .data$EXSTDTC) |>
    group_by(.data$USUBJID, .data$EXTRT) |>
    mutate(next_start = lead(.data$EXSTDTC)) |>
    mutate(LAST_ADMIN = row_number() == max(row_number())) |>
    ungroup()

  to_replace <- temp |>
    filter(is.na(.data$EXENDTC) & .data$LAST_ADMIN == FALSE)

  if (nrow(to_replace) > 0) {
    conditional_cli(
      cli::cli({
        cli::cli_alert_warning("EXENDTC imputation:")
        cli::cli_text(paste(
          "In", nrow(to_replace), plural(" row", nrow(to_replace) > 1),
          "EXENDTC was missing and was imputed as the day before the next",
          "EXSTDTC:"
        ))
        cli::cli_verbatim(
          df_to_string(select(to_replace, any_of(
            c("USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC")
          )), indent = 2)
        )
        cli::cli_text()
      }),
      silent = silent
    )

    temp <- temp |>
      mutate(imputation_flag = (is.na(.data$EXENDTC) &
                                  .data$LAST_ADMIN == FALSE)) |>
      # mutate(EXENDTC = case_match(
      #   .data$imputation_flag,
      #   TRUE ~ .data$next_start - days(1),
      #   FALSE ~ .data$EXENDTC
      # )) |>

      # mutate(EXENDTC = recode_values(
      #   .data$imputation_flag,
      #   TRUE ~ .data$next_start - days(1),
      #   FALSE ~ .data$EXENDTC
      # )) |>

      mutate(EXENDTC = case_when(
        .data$imputation_flag == TRUE ~ .data$next_start - days(1),
        .data$imputation_flag == FALSE ~ .data$EXENDTC
      )) |>

      # mutate(IMPUTATION = case_match(
      #   .data$imputation_flag,
      #   TRUE ~ "EXENDTC imputed as the day before the next EXSTDTC",
      #   FALSE ~ .data$IMPUTATION
      # )) |>

      # mutate(IMPUTATION = recode_values(
      #   .data$imputation_flag,
      #   TRUE ~ "EXENDTC imputed as the day before the next EXSTDTC",
      #   FALSE ~ .data$IMPUTATION
      # )) |>

      mutate(IMPUTATION = case_when(
        .data$imputation_flag == TRUE ~ "EXENDTC imputed as the day before the next EXSTDTC",
        .data$imputation_flag == FALSE ~ .data$IMPUTATION
      )) |>

      select(-"imputation_flag")
  }

  temp |>
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
#' @noRd
impute_exendtc_to_cutoff <- function(
  ex,
  cut_off_date = NA,
  silent = NULL
) {
  # input validation
  expected_columns <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
  missing_columns <- setdiff(expected_columns, names(ex))
  n <- length(missing_columns)
  if (n > 0) {
    stop(paste0(
      "Missing ", plural("column", n > 1), " in domain EX: ",
      nice_enumeration(missing_columns)
    ))
  }

  temp <- ex |>
    lubrify_dates() |>
    # identify last administration per subject and EXTRT
    arrange(.data$USUBJID, .data$EXTRT, .data$EXSTDTC) |>
    group_by(.data$USUBJID, .data$EXTRT) |>
    mutate(LAST_ADMIN = row_number() == max(row_number())) |>
    mutate(flag = .data$LAST_ADMIN == TRUE & is.na(.data$EXENDTC)) |>
    ungroup()

  to_replace <- temp |>
    filter(.data$flag == TRUE)

  if (nrow(to_replace) > 0) {
    conditional_cli(
      {
        cli::cli_alert_warning("Final EXENDTC imputation!")
        cli::cli_text(paste0(
          "In ", nrow(to_replace), plural(" subject", nrow(to_replace) > 1),
          " a missing EXENDTC for the last administration episode was",
          " replaced by the cut off date (",
          format(cut_off_date, format = "%Y-%m-%d %H:%M"), "):"
        ))
        cli::cli_verbatim(
          df_to_string(
            to_replace |>
              select(all_of(c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"))),
            indent = 2
            # abbr_lines = 5, abbr_threshold = 20
          )
        )
        cli::cli_text()
      },
      silent = silent
    )

    temp <- temp |>
      mutate(EXENDTC = case_when(
        .data$flag == TRUE ~ cut_off_date,
        .default = .data$EXENDTC
      )) |>
      mutate(IMPUTATION = case_when(
        .data$flag == TRUE ~ "missing EXENDTC set to data cutoff",
        .default = .data$IMPUTATION
      ))
  }

  temp |>
    select(-c("LAST_ADMIN", "flag"))
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
#' @import cli
#' @noRd
impute_admin_from_pcrftdtc <- function(
  obj, pc, analyte, pctestcd, silent = NULL
) {
  # validate inputs
  if (!pctestcd %in% pc$PCTESTCD) {
    conditional_cli({
      cli_alert_info(paste0("Analyte ", pctestcd, " not found in PCTESTCD"))
      cli_text(paste0(
        "Administrations times for ", analyte, " ",
        "cannot be derived from PCRFDTC and will be taken from EXSTDTC/EXENDTC!"
      ))
    }, silent = silent)
  }

  pc_ref <- pc |>
    lubrify_dates() |>
    filter(.data$PCTESTCD == pctestcd) |>
    mutate(ANALYTE = analyte) |>
    decompose_dtc("PCRFTDTC") |>
    select(c(
      "USUBJID", "ANALYTE", "PCRFTDTC_date",
      "PCRFTDTC_time"
    )) |>
    distinct()

  temp <- obj |>
    lubrify_dates() |>
    decompose_dtc("DTC") |>
    left_join(pc_ref,
      by = c("USUBJID", "ANALYTE", "DTC_date" = "PCRFTDTC_date")
    )

  if (!"IMPUTATION" %in% names(temp)) {
    temp <- mutate(temp, IMPUTATION = "")
  }

  conflicting_rows <- which(
    (!is.na(temp$PCRFTDTC_time) &
       !is.na(temp$DTC_time) &
       temp$DTC_time != temp$PCRFTDTC_time) |
      (is.na(temp$DTC_time) & !is.na(temp$PCRFTDTC_time))
  )

  if (length(conflicting_rows) != 0) {
    conditional_cli(
      {
        cli_alert_warning(paste0(
          "Inconsistent PCRFTDTC and EXSTDTC/EXENDTC"
        ))
        cli_text(paste0(
          "For ", length(conflicting_rows), " administrations of ", analyte,
          ", administration times were different between PCRFDTC and ",
          "EXSTDTC/EXENDTC. In these cases, PCRFTDTC was prioritized: "
        ))
        cli_verbatim(
          df_to_string(
            temp[
              conflicting_rows,
              c("USUBJID", "ANALYTE", "DTC", "PCRFTDTC_time")
            ]
            # abbr_lines = 5, abbr_threshold = 20
          )
        )
        cli_text()
      },
      silent = silent
    )

    temp <- temp |>
      mutate(DTC_time = case_when(
        row_number() %in% conflicting_rows ~ .data$PCRFTDTC_time,
        .default = .data$DTC_time
      )) |>
      mutate(IMPUTATION = case_when(
        row_number() %in% conflicting_rows ~
          "admin time from PCRFTDTC",
        .default = .data$IMPUTATION
      ))
  }

  temp |>
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) |>
    select(-c("PCRFTDTC_time", "DTC_date", "DTC_time"))
}


#' derive DTC_time in EX after expansion
#'
#' @param ex The expanded EX domain.
#'
#' @returns The expanded EX domain with DTC_time imputed.
#' @noRd
derive_ex_dtc_time <- function(ex) {
  ex |>
    group_by(.data$USUBJID, .data$EXTRT, .data$EXENDTC_date) |>

    # make DTC_time field
    mutate(DTC_time = case_when(
      row_number() == 1 & n() == 1 ~ .data$EXSTDTC_time,
      row_number() == n() & !is.na(EXENDTC_time) ~ .data$EXENDTC_time,
      .default = .data$EXSTDTC_time
    )) |>

    # make IMPUTATION field
    mutate(IMPUTATION = case_when(
      row_number() == n() & !is.na(EXENDTC_time) ~ "",

      row_number() == 1 & !is.na(EXSTDTC_time) ~ "",

      row_number() == n() & is.na(EXENDTC_time) &
        !is.na(EXSTDTC_time) ~ "time carried forward",

      row_number() != n() & row_number() != 1 &
        !is.na(EXSTDTC_time) ~ "time carried forward",

      row_number() != n() & is.na(EXSTDTC) ~ "no time information",

      .default = "no time information"
    )) |>
    ungroup() |>
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time))
}


#' Remove administrations with EXSTDTC after EXENDTC
#'
#' @param ex The ex domain as data frame.
#' @param dm The dm domain as data frame.
#' @param silent Suppress messages, defaults to nif_option setting, if NULL.
#' @param extrt The treatment as character.
#'
#' @return A data frame.
#' @noRd
filter_exendtc_after_exstdtc <- function(ex, dm, extrt, silent = NULL) {
  # Input validation
  expected_ex_columns <- c("USUBJID", "EXSTDTC", "EXENDTC")
  missing_ex_columns <- setdiff(expected_ex_columns, names(ex))
  n <- length(missing_ex_columns)
  if (n > 0) {
    stop(paste0(
      "Missing ", plural("column", n > 1), " in domain EX: ",
      nice_enumeration(missing_ex_columns)
    ))
  }

  # Convert dates to POSIXct for proper comparison
  ex <- lubrify_dates(ex)
  dm <- lubrify_dates(dm)

  temp <- ex |>
    filter(.data$EXTRT == extrt) |>
    decompose_dtc(c("EXSTDTC", "EXENDTC")) |>
    filter(
      # either the start date is after the end date
      (as.Date(.data$EXSTDTC_date) > as.Date(.data$EXENDTC_date)) |
        # or the start and end dates are the same, both times are not NA, and
        # the start datetime is still after the end datetime
        (as.Date(.data$EXSTDTC_date) == as.Date(.data$EXENDTC_date) &
           !is.na(.data$EXSTDTC_time) & !is.na(.data$EXENDTC_time) &
           .data$EXSTDTC > .data$EXENDTC)
    ) |>
    left_join(
      dm |>
        select(any_of(c("USUBJID", "RFSTDTC", "RFENDTC"))),
      by = "USUBJID"
    )

  if (nrow(temp) > 0) {
    conditional_cli(
      {
        cli::cli_alert_warning("EXSTDTC after EXENDTC!")

        cli::cli_text(paste0(
          nrow(temp), " administration ", plural("episode", nrow(temp) > 1),
          " for ", extrt,
          " had an EXENDTC before the EXSTDTC and ",
          plural("was", nrow(temp) > 1),
          " removed from the data set:\n"
        ))

        cli::cli_verbatim(
          df_to_string(
            select(temp, any_of(
              c("USUBJID", "EXTRT", "EXSEQ", "EXSTDTC", "EXENDTC", "RFENDTC")
            )),
            indent = 2
          )
        )
        cli::cli_text()
      },
      silent = silent
    )
  }

  ex |>
    filter(.data$EXTRT == extrt) |>
    decompose_dtc(c("EXSTDTC", "EXENDTC")) |>
    filter(
      !((as.Date(.data$EXSTDTC_date) > as.Date(.data$EXENDTC_date)) |
          (as.Date(.data$EXSTDTC_date) == as.Date(.data$EXENDTC_date) &
             !is.na(.data$EXSTDTC_time) & !is.na(.data$EXENDTC_time) &
             .data$EXSTDTC > .data$EXENDTC)))
}



#' Get administration time from the PCRFTDTC where available
#'
#' @param date The dates to get reference PCRFTDTC times for, as character.
#' @param sdtm The sdtm data.
#' @param analyte The analyte as character.
#' @param silent Suppress messages.
#'
#' @returns A character vector with the administration times, if they can be
#' derived from PCRFTDTC. The vector has the same length as the input date
#' vector.
#' @noRd
get_admin_time_from_pcrfdtc <- function(
    usubjid, date, sdtm, pctestcd = NULL, silent = NULL
  ) {
  # validate inputs
  validate_argument(usubjid, "character")
  validate_argument(date, "character", allow_multiple = TRUE)
  validate_sdtm(sdtm, "pc")
  validate_argument(pctestcd, "character", allow_null = TRUE)
  validate_argument(silent, "logical", allow_null = TRUE)

  pc <- domain(sdtm, "pc")

  # if PCRFTDTC is not available, return NA
  if (!"PCRFTDTC" %in% names(pc)) {
    return(rep(NA, length(date)))
  }

  if (is.null(pctestcd))
    pctestcd <- unique(pc$PCTESTCD)

  missing_pctestcd <- setdiff(pctestcd, unique(pc$PCTESTCD))
  if (length(missing_pctestcd) > 0)
    stop("missing PCTESTCD ", nice_enumeration(missing_pctestcd))

  temp <- pc |>
    filter(.data$USUBJID == usubjid) |>
    filter(.data$PCTESTCD %in% pctestcd) |>
    filter(!is.na(.data$PCRFTDTC)) |>
    lubrify_dates() |>
    distinct(.data$USUBJID, .data$PCRFTDTC) |>
    decompose_dtc("PCRFTDTC") |>
    arrange(PCRFTDTC_date) |>
    reframe(
      USUBJID,
      PCRFTDTC_date,
      PCRFTDTC_time,
      n = n(),
      .by = any_of(c("PCRFTDTC_date")))

  # check for duplicate times
  n_dupl <- filter(temp, n > 1) |>
    nrow()
  if (n_dupl > 0)
    stop(paste0(n_dupl, "duplicate administration times by USUBJID, and date"))

  return(temp[match(date, temp$PCRFTDTC_date), "PCRFTDTC_time"])
}




#' Standard imputation rules
#'
#' A list containing the standard imputation functions to be used in
#' add_administration().
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{admin_pre_expansion}{A function to conduct imputations on the EX domain
#'   before expansion of the administration episodes.}
#'   \item{admin_post_expansion}{A function to conduct imputations on the
#'   administration data table after expansion of the administration episodes}
#'   \item{obs_raw}{A function to conduct imputations on the observation data.
#'   At this stage, time fields other than NTIME are not derived yet.}
#'   \item{obs_final}{A function to conduct imputations on the nif data table
#'   including the new observations, before finalization. New observations are
#'   flagged by .current_observation == TRUE.}
#' }
#'
#' @export
imputation_standard <- list(
  admin_pre_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    dm <- lubrify_dates(domain(sdtm, "dm"))

    ex |>
      impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    # impute missing administration times from PCRFTDTC where available
    if ("pc" %in% names(sdtm$domains)) {
      pc <- lubrify_dates(domain(sdtm, "pc"))
      if ("PCRFTDTC" %in% names(pc)) {
        ex <- impute_admin_from_pcrftdtc(
          ex, pc, analyte, analyte, silent = silent)
      }
    }
    return(ex)
  },

  obs_raw = function(obs, silent) {
    obs
  },

  obs_final = function(obs, silent) {
    obs
  }
)


#' Void imputation rule set
#'
#' * No administration time imputations
#' * No imputations on observations
#'
imputation_none <- list(
  admin_pre_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    ex
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    ex
  },

  obs_raw = function(obs, silent) {
    obs
  },

  obs_final = function(obs, silent) {
    obs
  }
)


#' Alternative imputation rule set
#'
#' * No administration time imputations
#' * TAFD is set to 0 for predose observations
#'
imputation_1 <- list(
  admin_pre_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    ex
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    # ex_date <- ex$EXDTC
    # temp <- get_admin_time_from_pcrfdtc()
    ex
  },

  obs_raw = function(obs, silent) {
    obs
  },

  obs_final = function(obs, silent) {
    obs |>
      mutate(TAFD = case_when(
        .current_observation == TRUE & TAFD < 0 ~ 0,
        .default = TAFD)
      )
  }
)

