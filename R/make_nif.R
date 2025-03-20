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
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_exendtc_to_rfendtc <- function(ex, dm) {
  dm %>%
    assertr::verify(assertr::has_all_names("USUBJID", "RFSTDTC", "RFENDTC"))

  if(!"IMPUTATION" %in% names(ex)) {
    ex <- mutate(ex, IMPUTATION = "")
  }

  temp <- ex %>%
    assertr::verify(
      assertr::has_all_names(
        "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
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
          select(c("USUBJID", "EXTRT", "EXSEQ", "EXSTDTC", "EXENDTC", "RFENDTC")),
        indent = 2
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
#' is not optional, i.e., cannot be suppressed using the global 'silent' option,
#' but is issued in all cases.
#'
#' @param ex The updated EX domain as data frame.
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_missing_exendtc <- function(ex) {
  temp <- ex %>%
    assertr::verify(assertr::has_all_names(
      "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
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
#' @param cut_off_date The cut-off date.
#' @return The updated EX domain as data frame.
#' @keywords internal
impute_exendtc_to_cutoff <- function(ex, cut_off_date = NA) {
  temp <- ex %>%
    assertr::verify(assertr::has_all_names(
      "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
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
    conditional_message("In ", nrow(to_replace), " subjects, EXENDTC for the ",
      "last row is absent and is replaced by the cut off date, ",
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
      )) %>%
      ungroup()
  }
  return(temp %>% select(-"LAST_ADMIN"))
}


#' Derive administration time from PCRFTDTC
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
        "admin time copied from PCRFTDTC",
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
      df_to_string(select(
        temp, "USUBJID", "EXTRT", "EXSEQ", "EXSTDTC", "EXENDTC", "RFENDTC"),
        indent = 2),
      "\n"))
  }
  ex %>%
    filter(.data$EXSTDTC <= .data$EXENDTC)
}


# Make EXSTDY and EXENDY by USUBJID, EXTRT
#
# Treatment days are calculated relative to RFSTDTC from DM.
#
# @details
# Caution: Currently, the function works only with treatment days after Day 1.
#  If this be ever used for days before Day 1, a term must be
# implemented to correct for the missing Day 0 in the time nomenclature.
#
# @param ex The EX domain as data frame.
# @param dm The DM domain as data frame.
# @return The enhanced EX domain as data frame.
# @keywords internal
# make_exstdy_exendy <- function(ex, dm) {
#   ex %>%
#     assertr::verify(assertr::has_all_names(
#       "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
#     assertr::verify(is.POSIXct(c(EXSTDTC, EXENDTC))) %>%
#     left_join(
#       dm %>%
#         distinct(.data$USUBJID, .data$RFSTDTC),
#       by = "USUBJID"
#     ) %>%
#     mutate(EXSTDY = floor(as.numeric(
#       difftime(.data$EXSTDTC, .data$RFSTDTC), units = "days")) + 1) %>%
#     mutate(EXENDY = floor(as.numeric(
#       difftime(.data$EXENDTC, .data$RFSTDTC), units = "days")) + 1) %>%
#     select(-RFSTDTC)
# }


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
    "PLASMA", "SERUM", "BLOOD"
  )
  temp <- match(toupper(pcspecs), standard_specs)
  spec <- pcspecs[order(temp)][1]
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
  standard_specs <- c("SERUM", "URINE")
  temp <- match(toupper(lbspecs), standard_specs)
  spec <- lbspecs[order(temp)][1]
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


# Create the drug mapping data frame for PC observations
#
# @param sdtm_data The sdtm data as SDTM object.
# @return A data frame.
# @keywords internal
# make_drug_mapping <- function(sdtm_data) {
#   drug_mapping <- sdtm_data$analyte_mapping %>%
#     filter(PCTESTCD %in% unique(sdtm_data$pc$PCTESTCD)) %>%
#     rbind(
#       data.frame(EXTRT = intersect(
#         unique(sdtm_data$ex$EXTRT),
#         unique(sdtm_data$pc$PCTESTCD)
#       )) %>%
#         mutate(PCTESTCD = EXTRT)
#     ) %>%
#     mutate(PARENT = PCTESTCD)
#
#   # add metabolite mapping, if available
#   if (nrow(sdtm_data$metabolite_mapping) != 0) {
#     drug_mapping <- drug_mapping %>%
#       rbind(
#         sdtm_data$metabolite_mapping %>%
#           rename(PARENT = PCTESTCD_parent, PCTESTCD = PCTESTCD_metab) %>%
#           mutate(EXTRT = "")
#       )
#   }
#
#   drug_mapping <- drug_mapping %>%
#     mutate(METABOLITE = (PCTESTCD != PARENT)) %>%
#     mutate(ANALYTE = PCTESTCD) %>%
#     distinct()
# }


#' Add TIME field to table
#'
#' TIME is created as the difference between the DTC field and the first DTC
#' field on the USUBJID level. TIME is in hours, rounded by 3 digits.
#' @param x The table as data frame.
#' @return A data frame with FIRSTDTC and TIME added.
#' @keywords internal
add_time <- function(x) {
  x %>%
    assertr::verify(assertr::has_all_names("USUBJID", "DTC")) %>%
    assertr::verify(is.POSIXct(.data$DTC)) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(TIME = round(
      as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "h")),
      digits = 3
    ))
}


# Reduce a NIF object on the subject level by excluding all
# administrations after the last observation
#
# @param nif A NIF dataset.
# @return A NIF dataset.
# @import dplyr
# @export
# @examples
#' clip_nif(examplinib_poc_nif)
# clip_nif <- function(nif) {
#   last_obs <- nif %>%
#     as.data.frame() %>%
#     dplyr::filter(EVID == 0) %>%
#     dplyr::group_by(.data$USUBJID) %>%
#     dplyr::mutate(last_obs = max(.data$TIME)) %>%
#     dplyr::ungroup() %>%
#     dplyr::distinct(.data$USUBJID, .data$last_obs)
#
#   ret <- nif %>%
#     dplyr::left_join(last_obs, by = "USUBJID") %>%
#     dplyr::filter(.data$TIME <= .data$last_obs)
#   return(new_nif(ret))
# }



#' Calculate time fields based on DTC
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
make_time <- function(obj) {
  # obj %>%
  #   as.data.frame() %>%
  #   assertr::verify(assertr::has_all_names(
  #     "ID", "DTC", "ANALYTE", "PARENT", "EVID")) %>%
  #   assertr::verify(is.POSIXct(.data$DTC)) %>%
  #   group_by(.data$ID) %>%
  #   mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   group_by(.data$ID, .data$PARENT) %>%
  #   mutate(FIRSTADMIN = min(.data$DTC[.data$EVID == 1], na.rm = TRUE)) %>%
  #   ungroup() %>%
  #   mutate(TIME = round(
  #     as.numeric(difftime(.data$DTC, .data$FIRSTDTC, units = "h")),
  #     digits = 3)) %>%
  #   mutate(TAFD = round(
  #     as.numeric(difftime(.data$DTC, .data$FIRSTADMIN, units = "h")),
  #     digits = 3)) %>%
  #   add_tad() %>%
  #   new_nif()

  obj %>%
    as.data.frame() %>%
    assertr::verify(assertr::has_all_names(
      "ID", "DTC", "ANALYTE", "PARENT", "EVID")) %>%
    assertr::verify(is.POSIXct(.data$DTC)) %>%
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


#' Calculate time fields based on TIME
#'
#' @param obj A nif object.
#'
#' @return A nif object.
make_time_from_TIME <- function(obj) {
  as.data.frame(obj) %>%
    assertr::verify(assertr::has_all_names(
      "ID", "CMT", "EVID")) %>%
    group_by(.data$ID) %>%
    mutate(.first_time = min(.data$TIME, na.rm = TRUE)) %>%
    mutate(.first_admin = min(.data$TIME[.data$EVID == 1], na.rm = TRUE)) %>%
    mutate(TAFD = round(.data$TIME - .data$.first_admin, digits = 3)) %>%
    arrange(.data$ID, .data$TIME, -.data$EVID) %>%
    mutate(.admin_time = case_when(.data$EVID == 1 ~ .data$TIME)) %>%
    # tidyr::fill(.data$.admin_time, .direction = "down") %>%
    tidyr::fill(".admin_time", .direction = "down") %>%
    mutate(TAD = .data$TIME - .data$.admin_time) %>%
    ungroup() %>%
    select(-c(".first_time", ".first_admin", ".admin_time")) %>%
    new_nif()
}


#' Sort nif object and add REF field
#'
#' The input data format expected by NONMEM requires all rows ordered by ID and
#' TIME, and indexed sequentially on a subject level with a REF field.
#' Re-indexing may be required if a NIF object is extended, e.g., by merging in
#' further data.
#'
#' @param nif NIF object.
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


#' DTC of first administration by subject
#'
#' @param x A nif object.
#'
#' @return A data frame.
first_admin_dtc <- function(x) {
  x %>%
    assertr::verify(assertr::has_all_names("USUBJID", "DTC", "EVID")) %>%
    assertr::verify(is.POSIXct(.data$DTC)) %>%
    filter(EVID == 1) %>%
    dplyr::group_by(.data$USUBJID) %>%
    dplyr::mutate(FIRSTDTC = min(.data$DTC, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    distinct(.data$USUBJID, .data$FIRSTDTC)
}


#' Add observation from non-SDTM-formatted data table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param nif A nif object.
#' @param raw The raw observation data frame.
#' @param analyte The analyte name as character.
#' @param parent The parent as character.
#' @param cmt The compartment for the analte as numeric.
#' @param observation_filter Filter term, as character.
#' @param USUBJID_field The field specifying the USUBJID, as character.
#' @param DTC_field The field specifying the DTC, as character.
#' @param NTIME_field The field specifying the NTIME, as character.
#' @param DV_field The field specifying the dependent variable, as character.
#' @param keep Columns to keep, as character.
#' @param debug Keep debug information.
#'
#' @return A nif object.
#' @export
import_observation <- function(
    nif,
    raw,
    analyte,
    parent = NULL,
    cmt = NULL,
    observation_filter = "TRUE",
    USUBJID_field = "USUBJID",
    DTC_field = NULL,
    NTIME_field = NA,
    DV_field = NULL,
    keep = NULL,
    debug = FALSE) {
  debug = isTRUE(debug) | isTRUE(nif_option_value("debug"))
  if(isTRUE(debug)) keep <- c(keep, "SRC_DOMAIN", "SRC_SEQ")

  if(length(parents(nif)) == 0)
    stop("Please add at least one administration first!")

  if(!all(c(DV_field, USUBJID_field) %in% names(raw)))
    stop(paste0("ERROR: DV field (", DV_field, ") and USUBJID field (",
                USUBJID_field, ") must both be present in the ",
                "input data frame!"))
  if(!any(c(NTIME_field, DTC_field) %in% names(raw)))
    stop(paste0("ERROR: One of the time fields (",
                nice_enumeration(c(NTIME_field, DTC_field), conjunction = "or"),
                ") must be present in the input data frame!"))
  if(is.null(cmt)) {
    cmt <- max(nif$CMT) + 1
    conditional_message(paste0(
      "Compartment was not specified and has been set to ", cmt))}

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

  sbs <- nif %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    select("USUBJID", "ID", any_of(fillable_nif_fields), starts_with("BL_"),
           any_of(keep)) %>%
    select(!any_of(c("SRC_DOMAIN", "SRC_SEQ"))) %>%
    mutate(IMPUTATION = "") %>%
    distinct()

  obs <- raw %>%
    filter(eval(parse(text = observation_filter))) %>%
    mutate(USUBJID = .data[[USUBJID_field]]) %>%
    {if(!is.na(NTIME_field)) mutate(., NTIME = .data[[NTIME_field]]) else .} %>%
    mutate(DV = .data[[DV_field]]) %>%
    select(any_of(c("USUBJID", "NTIME", "DV", DTC_field))) %>%
    mutate(
      ANALYTE = analyte,
      CMT = cmt,
      AMT = 0,
      DOSE = NA,
      PARENT = parent,
      METABOLITE = FALSE,
      EVID = 0,
      MDV = as.numeric(is.na(DV))) %>%
    inner_join(sbs, by = "USUBJID") %>%
    mutate(
      SRC_DOMAIN = "IMPORT",
      SRC_SEQ = NA) %>%
    as.data.frame()

  # derive time from DTC, if present, or generate DTC from first administration
  #   time in nif object
  if(!is.null(DTC_field)) {
    obs <- obs %>%
      mutate(DTC = .data[[DTC_field]]) %>%
    lubrify_dates()
  } else {
    obs <- obs %>%
      left_join(first_admin_dtc(nif), by = "USUBJID") %>%
      mutate(DTC = .data$FIRSTDTC + duration(hours = NTIME)) %>%
      select(!all_of("FIRSTDTC")) %>%
      mutate(IMPUTATION = paste0("DTC derived from ", NTIME_field))
  }

  out <- bind_rows(
    as.data.frame(nif),
    obs
  ) %>%
    normalize_nif(keep = keep)
  return(out)
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
#' @return A nif object.
#' @export
normalize_nif <- function(obj, cleanup = TRUE, keep = NULL) {
  selector <- unique(c(
    "REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE", "HEIGHT", "WEIGHT",
    "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD", "PCELTM", "EVID", "AMT",
    "ANALYTE", "CMT",  "PARENT", "TRTDY", "METABOLITE", "DOSE", "DV", "MDV",
    "ACTARMCD", "IMPUTATION", "FOOD", "PART", "PERIOD", "COHORT", "FASTED",
    "RICH_N", "DI", "TREATMENT"))

  obj %>%
    mutate(ID = as.numeric(factor(.data$USUBJID, unique(.data$USUBJID)))) %>%
    make_time() %>%
    arrange(.data$DTC) %>%
    index_nif() %>%
    group_by(.data$ID, .data$PARENT) %>%
    tidyr::fill(any_of(c("DOSE", "EPOCH", "PART", "COHORT", "FOOD", "FASTED",
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
#' @importFrom stats na.omit
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
    assertr::verify(assertr::has_all_names("EXTRT", "PCTESTCD", "ANALYTE")) %>%
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














