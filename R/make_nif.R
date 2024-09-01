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
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_exendtc_to_rfendtc <- function(ex, dm) {
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
      ), "\n"
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
#' subject and EXTRT. For these cases, missing EXENDT can be imputed to the
#' global cut off date using [impute_exendtc_to_cutoff()].
#'
#' As this function conducts rather aggressive imputations, the message output
#' is not optional, i.e., cannot be suppressed using the blobal 'silent' option,
#' but is issued in all cases.
#'
#' @param ex The updated EX domain as data frame.
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_missing_exendtc <- function(ex) {
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
                            "EXSTDTC", "EXENDTC")), indent = "  "),
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
#' @param cut.off.date The cut-off date.
#' @return The updated EX domain as data frame.
#' @import assertr
#' @keywords internal
impute_exendtc_to_cutoff <- function(ex, cut_off_date = NA) {
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
      format(cut_off_date, format = "%Y-%m-%d %H:%M"), ":\n",
      df_to_string(to_replace %>% select(all_of(c("USUBJID", "EXTRT",
        "EXSTDTC", "EXENDTC")))), "\n")

    temp <- temp %>%
      mutate(EXENDTC = case_when(
        (LAST_ADMIN == TRUE & is.na(.data$EXENDTC)) ~ cut_off_date,
        .default = .data$EXENDTC
      )) %>%
      mutate(IMPUTATION = case_when(
        LAST_ADMIN == TRUE & is.na(.data$EXENDTC) ~
          "missing EXENDTC set to data cutoff",
        .default = .data$IMPUTATION
      ))
  }
  return(temp %>% select(-"LAST_ADMIN"))
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
impute_admin_times_from_pcrftdtc <- function(obj, pc, analyte, pctestcd) {
  pc_ref <- pc %>%
    filter(PCTESTCD == pctestcd) %>%
    mutate(ANALYTE = analyte) %>%
    decompose_dtc("PCRFTDTC") %>%
    select(c("USUBJID", "ANALYTE", "PCRFTDTC_date",
             "PCRFTDTC_time")) %>%
    distinct()

  obj %>%
    decompose_dtc("DTC") %>%
    left_join(pc_ref,
              by = c("USUBJID", "ANALYTE", "DTC_date" = "PCRFTDTC_date")) %>%
    mutate(IMPUTATION = case_when(
      !is.na(PCRFTDTC_time) ~
        "admin time imputed from PCRFTDTC",
      .default = .data$IMPUTATION)) %>%
    mutate(DTC_time = case_when(!is.na(PCRFTDTC_time) ~ PCRFTDTC_time,
                                .default = .data$DTC_time)) %>%
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    select(-c("PCRFTDTC_time", "DTC_date", "DTC_time"))
}


#' Remove administrations with EXSTDTC after EXENDTC
#'
#' @param ex The ex domain as data frame.
#' @param dm The dm domain as data frame.
#'
#' @return A data frame.
#' @keywords internal
filter_EXSTDTC_after_EXENDTC <- function(ex, dm) {
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
      df_to_string(select(temp, "USUBJID", "EXTRT", "EXSEQ",
                          "EXSTDTC", "EXENDTC", "RFENDTC"),
                   indent = "  "),
      "\n"))
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


#' Guess the most likely PCSPEC
#'
#' The PC specimen is selected based on the likelihood in the order of 'plasma'
#' < 'serum' < 'blood'.
#' @param pc A data frame.
#' @return The imputed spec as character.
#' @export
#' @keywords internal
guess_pcspec <- function(pc) {
  pcspecs <- unique(pc$PCSPEC)
  standard_specs <- c(
    "PLASMA", "Plasma", "plasma", "SERUM", "Serum", "serum",
    "BLOOD", "Blood", "blood"
  )

  spec <- standard_specs[standard_specs %in% pcspecs][1]
  conditional_message("No specimen specified. Set to ", spec,
                      " as the most likely.", "\n")
  return(spec)
}


#' Guess the most likely LBSPEC
#'
#' @param lb The LB SDTM domain as data frame.
#' @return the imputed LBSPEC as character.
#' @export
#' @keywords internal
guess_lbspec <- function(lb) {
  lbspecs <- unique(lb$LBSPEC)
  standard_specs <- c("SERUM", "serum", "URINE", "urine")
  spec <- standard_specs[standard_specs %in% lbspecs][1]
  conditional_message("No specimen specified. Set to '", spec,
                      "' as the most likely.", "\n")
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


#' Create the drug mapping data frame for PC observations
#'
#' @param sdtm_data The sdtm data as SDTM object.
#' @return A data frame.
#' @keywords internal
make_drug_mapping <- function(sdtm_data) {
  drug_mapping <- sdtm_data$analyte_mapping %>%
    filter(PCTESTCD %in% unique(sdtm_data$pc$PCTESTCD)) %>%
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
#' @param subject_filter The filtering to apply to the DM domain.
#' @param keep Columns to keep, as character.
#'
#' @return A data table.
#' @import assertr
#' @import tidyselect
#' @export
#' @keywords internal
make_subjects <- function(dm, vs,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  keep = "") {
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

  baseline_covariates <- vs %>%
    lubrify_dates() %>%
    left_join(select(dm, c("USUBJID", "RFSTDTC")), by = "USUBJID") %>%
    {if("VSBLFL" %in% names(vs)) filter(., VSBLFL == "Y") else
      filter(., VSDTC < RFSTDTC)} %>%
    filter(VSTESTCD %in% c("WEIGHT", "HEIGHT")) %>%
    group_by(.data$USUBJID, .data$VSTESTCD) %>%
    summarize(mean = mean(.data$VSSTRESN), .groups = "drop") %>%
    pivot_wider(names_from = "VSTESTCD", values_from = "mean")

  if ("HEIGHT" %in% colnames(baseline_covariates) &&
      "WEIGHT" %in% colnames(baseline_covariates)) {
    baseline_covariates <- baseline_covariates %>%
      mutate(BMI = .data$WEIGHT / (.data$HEIGHT / 100)^2)
  }

  out <- dm %>%
    verify(has_all_names("USUBJID", "SEX", "ACTARMCD", "RFXSTDTC")) %>%
    lubrify_dates() %>%
    filter(eval(parse(text = subject_filter))) %>%
    left_join(baseline_covariates, by = "USUBJID") %>%
    recode_sex() %>%
    mutate(ID = NA) %>%
    relocate("ID") %>%
    arrange("ID") %>%
    select(., any_of(c(
      "ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "HEIGHT",
      "WEIGHT", "BMI", "ACTARMCD", "RFXSTDTC", keep)))
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
#' @param observation_filter The filtering to apply to the observation source
#'   data.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param NTIME_lookup A data frame with two columns, a column that defines the
#'   custom nominal time information in the target domain (e.g., 'PCELTM'), and
#'   'NTIME'. This data frame is left_joined into the observation data frame
#'   to provide the NTIME field.
#' @param testcd The observation variable, as character.
#' @param TESTCD_field The xxTESTCD field. Defaults to the two-character domain
#'   name followed by 'TESTCD', if NULL.
#' @param keep Columns to keep, as character.
#' @param factor Multiplier for the DV field, as numeric.
#' @param coding_table Coding table to translate a categorical values into
#'   numerical values, as data frame. The data frame must have at least one
#'   column that matches a column in the domain, and a numerical 'DV' column
#'   that provides the recoding result.
#'
#' @return A data frame.
#' @keywords internal
#' @import stringr
#'
make_observation <- function(
    sdtm,
    domain,
    testcd,
    analyte = NULL,
    parent = NA,
    cmt = NA,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    TESTCD_field = NULL,
    DTC_field = NULL,
    DV_field = NULL,
    coding_table = NULL,
    factor = 1,
    NTIME_lookup = NULL,
    keep = NULL
    ) {
  if(is.null(DTC_field)) DTC_field <- paste0(str_to_upper(domain), "DTC")
  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(str_to_upper(domain),
                                                   "TESTCD")
  if(is.null(analyte)) analyte <- testcd
  if(is.null(parent)) parent <- analyte

  sbs <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"), subject_filter,
                       keep)

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
            mutate(NTIME = pt_to_hours(.data$PCELTM))
          } else {
            NTIME_lookup <- obj %>%
              distinct(.data$PCELTM) %>%
              mutate(NTIME = as.numeric(stringr::str_extract(
                .data$PCELTM, "PT([.0-9]+)H", group = 1)))
          }
        } else {
          conditional_message("PC.PCELM is not defined. Provide a NTIME lookup",
                              "table to define nominal time!")
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

  # apply coding table, if not NULL
  if(!is.null(coding_table)) {
    if(!any(names(coding_table) %in% names(obj))){
      stop("Coding table cannot be applied to data set!")
    }
    if(!is.numeric(coding_table$DV)){
      stop("DV field in coding table must be numeric!")
    }
    suppressMessages(
      obj <- obj %>%
        left_join(coding_table)
    )
  } else {
    obj <- obj %>%
      mutate(DV = .data[[DV_field]] * factor)
  }

  obj %>%
    filter(eval(parse(text = observation_filter))) %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    mutate(
      DTC = .data[[DTC_field]],
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
    {if(is.numeric(.$DV)) mutate(., DV = DV * factor) else .} %>%

    {if(!is.null(NTIME_lookup)) suppressMessages(
      left_join(., NTIME_lookup)) else
      mutate(., NTIME = NA)} %>%

    inner_join(sbs, by = "USUBJID") %>%
    group_by(.data$USUBJID) %>%
    mutate(TRTDY = as.numeric(
      difftime(date(.data$DTC), date(safe_min(.data$RFXSTDTC))),
      units = "days") + 1) %>%
    ungroup() %>%
    filter(!is.na(.data$DTC)) %>%
    new_nif()
}


#' Compile administration data frame
#'
#' @details
#' A discussion on EC vs EX is provided [here](https://www.cdisc.org/kb/ecrf/exposure-collected#:~:text=In%20the%20SDTMIG%2C%20the%20Exposure,data%20collected%20on%20the%20CRF.)
#'
#' @details
#' # Time imputations and filtering
#'
#' The following time imputations and filters are applied in the given
#' order:
#'
#' ## 1. [impute_exendtc_to_rfendtc()]
#'
#' If EXENDTC is missing in the last administration episode for a given subject,
#' it is replaced with DM.RFENDTC, if available.
#'
#' ## 2. [filter_EXSTDTC_after_EXENDTC()]
#'
#' Administration episodes in which EXSTDTC is after EXENDT are deleted from the
#' data set.
#'
#' ## 3. [impute_exendtc_to_cutoff()]
#'
#' If in the last administration episode per subject and treatment, EXENDTC is
#' missing, for example because the treatment is still ongoing at the time of
#' the SDTM generation, EXENDTC is replaced with the cut-off date.
#'
#' ## 4. [impute_missing_exendtc()]
#'
#' If in any further episode, EXENDTC is missing, it is replaced with the day
#' before the subsequent administration episode start (EXSTDTC). It should be
#' understood that this reflects a rather strong assumption, i.e., that the
#' treatment was continued into the next administration episode. This imputation
#' therefore issues a warning that cannot be suppressed.
#'
#' ## 5. Expand administration episodes
#'
#' All administration episodes, i.e., the intervals between EXSTDTC and EXENDTC
#' for a given row in EX, are expanded into a sequence of rows with one
#' administration day per row. The administration times for all rows except for
#' the last are taken from the time information in EXSTDTD, whereas the time
#' for the last administration event in the respective episode is taken from the
#' time information in EXENDTC.
#'
#' **Development note:** In the present version of the function, once-daily (QD)
#' dosing is assumed. Multiple-daily dosings are not supported. In future
#' versions, the dosing frequency provided in `EXDOSFRQ` may be taken into
#' account to adequately handle multiple daily administrations.
#'
#' ## 6. [impute_admin_times_from_pcrftdtc()]
#'
#' For administration days for which PK sampling events are recorded in PC, the
#' administration time is taken from PC.PCRFTDTC, if this field is available.
#'
#' **Development note:** This may be updated in future versions of the function
#' to work with multiple-daily administrations.
#'
#' ## 7. Carry forward time
#'
#' For all administration events per subject and treatment, missing time
#' information is finally carried forward from available time information.
#'
#' @param sdtm A sdtm object.
#' @param subject_filter The filtering to apply to the DM domain, as string,
#' @param extrt The EXTRT for the administration, as character.
#' @param analyte The name of the analyte as character.
#' @param cmt The compartment for the administration as numeric.
#' @param cut_off_date The data cut-off date as Posix date-time.
#' @param keep Columns to keep after cleanup, as character.
#'
#' @return A data frame.
#' @export
#' @importFrom assertthat assert_that
#' @keywords internal
#' @seealso [add_administration()]
make_administration <- function(sdtm, extrt, analyte = NA, cmt = 1,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    cut_off_date = NULL,
    keep = ""
    ) {
  dm <- domain(sdtm, "dm") %>% lubrify_dates()
  ex <- domain(sdtm, "ex") %>% lubrify_dates()
  pc <- domain(sdtm, "pc") %>% lubrify_dates()

  assert_that(
    extrt %in% ex$EXTRT,
    msg = paste0("Treatment '", extrt, "' not found in EX.EXTRT!")
  )

  if(is.na(analyte)) {analyte <- extrt}
  if(is.null(cut_off_date)) cut_off_date <- last_ex_dtc(ex)

  sbs <- make_subjects(dm, domain(sdtm, "vs"), subject_filter, keep)

  admin <- ex %>%
    # assertr::verify(extrt %in% EXTRT) %>%
    mutate(IMPUTATION = "") %>%
    filter(.data$EXTRT == extrt) %>%
    filter(.data$EXSTDTC <= cut_off_date) %>%
    decompose_dtc("EXSTDTC") %>%

    impute_exendtc_to_rfendtc(dm) %>%
    filter_EXSTDTC_after_EXENDTC(dm) %>%

    # time imputations
    impute_exendtc_to_cutoff(cut_off_date = cut_off_date) %>%
    impute_missing_exendtc() %>%
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
    unnest("DTC_date") %>%

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
      is.na(.data$DTC_time) == TRUE ~ "time carried forward",
      .default = .data$IMPUTATION)) %>%
    group_by(.data$USUBJID, .data$ANALYTE) %>%
    fill("DTC_time", .direction = "down") %>%
    ungroup() %>%

    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    select(-c("DTC_date", "DTC_time")) %>%
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
#' @export
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


#' Sort nif object and add REF field
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
#' @keywords internal
index_nif <- function(nif) {
  nif %>%
    as.data.frame() %>%
    dplyr::arrange(ID, TIME, -EVID) %>%
    dplyr::mutate(REF = row_number()) %>%
    dplyr::relocate(REF) %>%
    new_nif()
}


#' Append administration events
#'
#' Drug administration data is taken from the EX domain of the sdtm object. The
#' 'extrt' field specifies the drug name as represented in 'EX', however, a
#' different 'analyte' name can be assigned to match with that of the
#' pharmacokinetic observations for the parent drug in plasma.
#'
#' @param nif A nif object.
#' @inheritParams make_administration
#' @param silent `r lifecycle::badge("deprecated")` Dummy option for
#' compatibility, set the global option [nif_option()] with `silent = TRUE` to
#' suppress messages.
#'
#' @return A nif object.
#' @export
#' @examples
#' add_administration(new_nif(), examplinib_sad, "EXAMPLINIB")
#'
add_administration <- function(nif, sdtm, extrt, analyte = NA, cmt = 1,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    cut_off_date = NULL, keep = NULL, silent = deprecated()) {
  bind_rows(nif,
            make_administration(sdtm, extrt, analyte, cmt, subject_filter,
                                     cut_off_date, keep)) %>%
    normalize_nif(keep = keep)
}


#' Append observation events
#'
#' Observations can be pharmacokinetic observations (i.e., from the PC domain),
#' or any other class of observation from any other SDTM domain. The 'testcd'
#' specifies the value of the respective __TESTCD__ field (e.g., 'PCTESTCD',
#' 'VSTESTCD' or 'LBTESTCD') that defines the observation. Observation events
#' can be attached to an administered drug by specifying the 'parent' field.
#' This is required for, e.g., the time-after-dose ('TAD') and
#' time-after-first-dose ('TAFD') time calculation.
#'
#' Observations can be further specified with the 'observation_filter' term. The
#' filter term can refer to any field of the respective SDTM domain.
#'
#' A PK/PD model compartment can be specified with 'cmt' or will be
#' automatically assigned if `cmt = NULL`.
#'
#' @param nif A nif object.
#' @inheritParams make_observation
#' @param silent `r lifecycle::badge("deprecated")` Dummy option for
#' compatibility, set the global option [nif_option()] with `silent = TRUE` to
#' suppress messages.
#'
#' @return A nif object.
#' @seealso [add_administration()]
#' @export
#' @examples
#' add_observation(examplinib_fe_nif, examplinib_fe, "pc", "RS2023487A",
#'   parent = "RS2023")
#'
add_observation <- function(
    nif, sdtm, domain, testcd,
    analyte = NULL,
    parent = NULL,
    cmt = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    observation_filter = "TRUE",
    TESTCD_field = NULL,
    DTC_field = NULL,
    DV_field = NULL,
    coding_table = NULL,
    factor = 1,
    NTIME_lookup = NULL,
    keep = NULL,
    silent = deprecated()
  ) {
  if(length(parents(nif)) == 0)
    stop("Please add at least one administration first!")
  if(is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_message(paste0(
      "Compartment for ", testcd,
      " was not specified and has been set to ", cmt))
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
                                 parent, "!"))
    }
  }

  obj <- bind_rows(
    nif,
    make_observation(sdtm, domain, testcd, analyte, parent, cmt, subject_filter,
                     observation_filter, TESTCD_field, DTC_field, DV_field,
                     coding_table, factor, NTIME_lookup, keep)) %>%
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
        indent = "  "), "\n"))

    obj <- obj %>%
      filter(.data$NO_ADMIN_FLAG == 0)
  }

  obj %>%
    select(-c("NO_ADMIN_FLAG")) %>%
    normalize_nif(keep = keep)
}


#' Attach baseline covariate
#'
#' Baseline covariates, as specified by the 'testcd' field, can come from any
#' SDTM domain. By default, the baseline value is identified by `xxBLFL == "Y"`
#' in the respective SDTM domain. Alternatively, a custom observation filter can
#' be defined. The name of the baseline covariate is the 'testcd', prefixed with
#' 'BL_'.
#'
#' @param nif A nif object.
#' @param sdtm A sdtm object.
#' @param domain The domain as character.
#' @param testcd The covariate variable name as character.
#' @param DV_field The name of the DV field as character.
#' @param TESTCD_field The name of the TESTCD field. defaults to xxTESTCD with
#'   xx the domain name, as character.
#' @param observation_filter A filter term for the `domain`, as character.
#' @param subject_filter A filter term for the DM domain, as character.
#' @param baseline_filter A filter term to identify the baseline condition.
#'   within the `domain`. Defaults to "xxBLFL == 'Y'" (with xx the domain
#'   code).
#' @param summary_function The summary function to summarize multiple baseline
#'   values. Defaults to `mean`.
#' @param silent `r lifecycle::badge("deprecated")` Dummy option for
#' compatibility, set the global option [nif_option()] with `silent = TRUE` to
#' suppress messages.
#' @param coding_table A recoding table as data frame, or NULL. If present, the
#'   table needs to have a field that matches a column in the domain, and a
#'   field 'DV' that provides the re-coded value.
#'
#' @return A nif object.
#' @import assertr
#' @importFrom stats na.omit
#' @export
#' @examples
#' add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT")
#' add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT",
#'   baseline_filter = "VSBLFL == 'Y'")
add_baseline <- function(
    nif,
    sdtm,
    domain,
    testcd,
    DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = "TRUE",
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    baseline_filter = NULL,
    coding_table = NULL,
    summary_function = mean,
    silent = deprecated()) {

  if(is.null(DV_field)) DV_field <- paste0(str_to_upper(domain), "STRESN")
  if(is.null(TESTCD_field)) TESTCD_field <- paste0(
    str_to_upper(domain), "TESTCD")
  if(is.null(baseline_filter)) baseline_filter <- paste0(
    ".data[['", str_to_upper(domain), "BLFL']] == 'Y'")

  sbs <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"),
                       subject_filter, "")

  bl_field <- paste0("BL_", testcd)

  # baseline <- domain(sdtm, str_to_lower(domain)) %>%
  #   lubrify_dates() %>%
  #   filter(eval(parse(text = observation_filter))) %>%
  #   filter(.data[[TESTCD_field]] %in% testcd) %>%
  #   filter(eval(parse(text = baseline_filter))) %>%
  #   pivot_wider(names_from = all_of(TESTCD_field),
  #               values_from = all_of(DV_field)) %>%
  #   # select("USUBJID", {{testcd}}) %>%
  #   select(all_of(c("USUBJID", {{testcd}}))) %>%
  #
  #   group_by(.data$USUBJID) %>%
  #   summarize(across(all_of(testcd),
  #                    ~ summary_function(na.omit(.x, na.rm = TRUE)))) %>%
  #   rename_with(~bl_field, .cols = testcd) %>%
  #   ungroup()

  join_fields <- intersect(names(coding_table),
                           names(domain(sdtm, str_to_lower(domain))))
  if(!is.null(coding_table) & length(join_fields) == 0) {
    stop("Coding table cannot be applied - no valid data column!")
  } else {
    conditional_message(paste0("Recoding from ", join_fields))
  }

  baseline <- domain(sdtm, str_to_lower(domain)) %>%
    lubrify_dates() %>%
    filter(eval(parse(text = observation_filter))) %>%
    filter(.data[[TESTCD_field]] %in% testcd) %>%
    filter(eval(parse(text = baseline_filter))) %>%
    {if(is.null(coding_table)) mutate(., DV = .data[[DV_field]]) else
      left_join(., coding_table, by = join_fields)} %>%
    pivot_wider(names_from = all_of(TESTCD_field),
                values_from = DV) %>%
    # select("USUBJID", {{testcd}}) %>%
    select(all_of(c("USUBJID", {{testcd}}))) %>%
    group_by(.data$USUBJID) %>%
    summarize(across(all_of(testcd),
                     ~ summary_function(na.omit(.x, na.rm = TRUE)))) %>%
    rename_with(~bl_field, .cols = all_of(testcd)) %>%
    ungroup()

  out <- nif %>%
    coalesce_join(baseline, by = "USUBJID", join = 'left_join')

  return(out)
}



#' Attach time-varying covariate
#'
#' A time-varying covariate is added as a new field with daily time granularity
#' and carried forward for missing entries. The name of the covariate can be
#' specified by 'covariate'. By default, it is set to the 'testcd' (without any
#' prefix).
#'
#' @param nif A nif object.
#' @param sdtm The corresponding sdtm object.
#' @param domain The domain as character.
#' @param testcd The xxTESTCD with xx the domain name, as character.
#' @param DV_field The name of the DV field as character.
#' @param TESTCD_field The name of the TESTCD field. defaults to xxTESTCD (with
#'   xx the domain code), as character.
#' @param observation_filter A filter term for the `domain`, as character.
#' @param covariate The name of the covariate, defaults to the testcd if 'NULL'.
#' @param DTC_field The field to use as the date-time code for the observation.
#'   Defaults to the two-character domain name followed by 'DTC', if NULL.
#' @param silent `r lifecycle::badge("deprecated")` Dummy option for
#' compatibility, set the global option [nif_option()] with `silent = TRUE` to
#' suppress messages.
#'
#' @return A nif object.
#' @seealso add_baseline()
#' @export
#'
#' @examples
#' add_covariate(examplinib_poc_nif, examplinib_poc, "vs", "WEIGHT",
#'   covariate = "wt")
add_covariate <- function(nif, sdtm, domain, testcd,
    covariate = NULL,
    DTC_field = NULL, DV_field = NULL,
    TESTCD_field = NULL,
    observation_filter = "TRUE",
    silent = deprecated()
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
    filter(eval(parse(text = observation_filter))) %>%
    filter(.data[[TESTCD_field]] == testcd) %>%
    select("USUBJID", "DTC" = .data[[DTC_field]],
           !!COV_field := .data[[DV_field]]) %>%
    decompose_dtc("DTC") %>%
    select(-c("DTC", "DTC_time")) %>%
    # select(!c("DTC", "DTC_time")) %>%
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


#' Normalize nif object
#'
#' Order nif object, index and fill missing fields, and reduce to essential
#' columns.
#'
#' @param obj A nif object.
#' @param keep Fields to explicitly keep, as character.
#' @param cleanup Remove non-essential fields, as logical.
#'
#' @return A nif object.
#' @export
#'
#' @examples
#' normalize_nif(examplinib_sad_nif)
normalize_nif <- function(obj, cleanup = TRUE, keep = NULL) {
  selector <- unique(c("REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE",
                       "HEIGHT", "WEIGHT", "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD",
                       "PCELTM", "EVID", "AMT", "ANALYTE", "CMT",  "PARENT", "TRTDY",
                       "METABOLITE", "DOSE", "DV", "MDV", "ACTARMCD", "IMPUTATION",
                       "FOOD", "PART", "PERIOD", "COHORT", "FASTED", "RICH_N", "DI",
                       "TREATMENT"))

  obj %>%
    make_time() %>%
    arrange(.data$DTC) %>%
    mutate(ID = as.numeric(factor(.data$USUBJID, unique(.data$USUBJID)))) %>%
    index_nif() %>%
    group_by(.data$ID, .data$PARENT) %>%
    fill(any_of(c("DOSE", "EPOCH", "PART", "COHORT", "FOOD", "FASTED",
                  starts_with("BL_"))),
         .direction = "downup") %>%
    ungroup() %>%
    nif_cleanup(keep = keep) %>%
    new_nif()
}





#' Remove non-essential fields
#'
#' @param nif A nif object.
#' @param keep Fields to explicitly keep, as character.
#'
#' @return A nif object
#' @export
nif_cleanup <- function(nif, keep = NULL) {
  selector <- unique(c("REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE",
    "HEIGHT", "WEIGHT", "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD",
    "PCELTM", "EVID", "AMT", "ANALYTE", "CMT",  "PARENT", "TRTDY",
    "METABOLITE", "DOSE", "DV", "MDV", "ACTARMCD", "IMPUTATION",
    "FOOD", "PART", "PERIOD", "COHORT", "FASTED", "RICH_N", "DI",
    "TREATMENT", keep))
  selector <- selector[selector %in% names(nif)]
  nif %>%
    select(all_of(selector), starts_with("BL_"))
}


#' Automatically generate a nif object with pharmacokinetic observations
#'
#' Automatically generate a nif object from SDTM data. All treatments and
#' analytes must be defined in the `treatment_mapping` and `metabolite_mapping`
#' objects that are attached to the sdtm object.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param sdtm The source SDTM object.
#' @param bl_creat Include baseline creatinine, creatinine clearance and renal
#'   function class fields.
#' @param bl_odwg Include baseline ODWG hepatic function class.
#' @param keep Columns to keep, as character.
#'
#' @return A nif object.
#' @seealso [add_analyte_mapping()]
#' @seealso [add_metabolite_mapping()]
#' @import assertr
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' nif_auto(examplinib_sad)
nif_auto <- function(sdtm,
                     bl_creat = TRUE,
                     bl_odwg = TRUE,
                     keep = NULL
                     ) {
  ## TO DO:
  ## try generating analyte mapping automatically
  ##
  analyte_mapping <- sdtm$analyte_mapping
  if(nrow(analyte_mapping) == 0) {
    stop("Missing analyte mapping in sdtm object.")
  }

  analyte_mapping <- analyte_mapping %>%
    verify(has_all_names("EXTRT", "PCTESTCD", "ANALYTE")) %>%
    mutate(PARENT = PCTESTCD, METABOLITE = FALSE) %>%
    {if(!"ANALYTE" %in% names(.)) mutate(., ANALYTE = PCTESTCD) else .}

  metabolite_mapping <- sdtm$metabolite_mapping
  if(nrow(metabolite_mapping > 0)){
    metabolite_mapping <- metabolite_mapping %>%
      rename(ANALYTE = PCTESTCD_metab, PARENT = PCTESTCD_parent) %>%
      mutate(EXTRT = NULL, METABOLITE = TRUE, PCTESTCD = ANALYTE)
  }
  analytes <- bind_rows(analyte_mapping, metabolite_mapping) %>%
    distinct()

  nif <- new_nif()

  # Treatments
  # treatments <- analyte_mapping$EXTRT
  treatments <- na.omit(unique(analytes$EXTRT))

  conditional_message(
    paste0("Adding treatment(s): ", nice_enumeration(treatments)))

  for(i in seq(1:nrow(analytes))){
    if(!is.na(analytes[i, "EXTRT"])) {
      nif <- add_administration(nif, sdtm, analytes[i, "EXTRT"],
                                analyte = analytes[i, "ANALYTE"], keep = keep)
    }
  }

  # PC observations
  observations <- unique(analytes$ANALYTE)

  conditional_message(
    paste0("Adding PC observations(s): ", nice_enumeration(observations)))

  for(i in seq(1:nrow(analytes))) {
    nif <- add_observation(nif, sdtm, "pc", analytes[i, "PCTESTCD"],
            analyte = analytes[i, "ANALYTE"], parent = analytes[i, "PARENT"],
            keep = keep)
  }

  lb <- domain(sdtm, "lb")
  if(bl_creat == TRUE | bl_odwg == TRUE){
    if(is.null(lb)) {
      conditional_message("LB not found in sdtm object!")
    } else {
      # Baseline CREAT, CRCL and renal function class
      if(bl_creat == TRUE) {
        if(!"CREAT" %in% unique(lb$LBTESTCD)) {
          conditional_message("CREAT not found in LB!")
        } else {
          conditional_message("Adding baseline renal function")
          nif <- add_baseline(nif, sdtm, "lb", "CREAT") %>%
            {if(all(c("BL_CREAT", "AGE", "SEX", "RACE", "WEIGHT") %in% names(.)))
              add_bl_crcl(.) else .} %>%
            {if("BL_CRCL" %in% names(.)) add_bl_renal(.) else .}
        }
      }

      # Baseline ODWG hepatic function class
      if(bl_odwg == TRUE) {
        if(all(c("BILI", "ALT") %in% unique(lb$LBTESTCD))) {
          conditional_message("Adding baseline hepatic function")
          nif <- add_bl_odwg(nif, sdtm)
        } else {
          conditional_message("Cannot make BL_ODWG: BILI and AST not found in LB!")
        }
      }
    }
  }
  return(nif)
}














