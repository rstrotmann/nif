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


#' Add baseline lab covariate
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @details
#' Lab parameters not found in LB will be reported in a warning message.
#'
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
  lifecycle::deprecate_warn("0.48.2", "add_bl_lab()", "add_baseline()")
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


#' Add lab covariate
#'
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
  # lifecycle::deprecate_warn("0.48.2", "add_lab_covariate()", "add_observation()")
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
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @keywords internal
add_lab_observation <- function(obj, lb, lbtestcd, cmt = NULL, lbspec = "",
                                parent = NA, silent = FALSE) {
  lifecycle::deprecate_warn("0.48.2", "add_lab_observation()",
                            "add_observation()")
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


#' Add an observation from a generic source
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @keywords internal
add_generic_observation <- function(obj, source, DTC_field, selector, DV_field,
                                    analyte_name, cmt = NULL, parent = NA,
                                    silent = FALSE) {
  lifecycle::deprecate_warn("0.48.2", "add_generic_observation()",
                            "add_observation()")
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


#' Make a NIF object from SDTM-formatted data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
#' @import tidyr
#' @import dplyr
#' @keywords internal
make_nif <- function(
    sdtm_data,
    spec = NULL,
    silent = FALSE,
    truncate_to_last_observation = TRUE,
    truncate_to_last_individual_obs = FALSE,
    use_pctptnum = FALSE,
    analyte_cmt_mapping = NULL) {
  lifecycle::deprecate_warn("0.48.2", "make_nif()", "new_nif()")
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


#' Make administration data set from EX domain
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
  lifecycle::deprecate_warn("0.48.2", "make_admin()", "make_administration()")

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


#' Make observation data set from PC
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
  lifecycle::deprecate_warn("0.48.2", "make_obs()", "make_observation()")
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


#' Add time-variant lab covariate
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Add column named 'TV_xxx' with the time-varying values for the respective
#' lab covariate. Last values are carried forward.
#' @details
#' Lab parameters not found in LB will be reported in a warning message.
#' @inheritParams add_bl_lab
#' @return A NIF object.
#' @export
add_tv_lab <- function(obj, lb, lbtestcd, lbspec = NULL, silent = FALSE) {
  lifecycle::deprecate_warn("0.48.2", "add_tv_lab()", "add_covariate()")
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
#' @description
#' `r lifecycle::badge("deprecated")`
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
  lifecycle::deprecate_warn("0.48.2", "add_tv_vs()", "add_covariate()")

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


#' Compress a NIF object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
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
  lifecycle::deprecate_warn("0.48.2", "compress()", "")
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


#' Fill all baseline and time-varying covariates
#'
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
fill_bl_tv <- function(obj) {
  lifecycle::deprecate_warn("0.48.2", "fill_bl_tv()", "")
  obj %>%
    arrange(ID, TIME) %>%
    fill(starts_with("BL_"), .direction = "down") %>%
    fill(starts_with("TV_"), .direction = "down")
}


