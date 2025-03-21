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














