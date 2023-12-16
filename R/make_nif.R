## BUGS TO BE FIXED
## -------------------------------------------
##
## PROBLEM:
##
## Expansion of administrations in make_admin:
##
## The end DTC of an administration (EXENDTC) may be missing, and
## currently, it is assumed that that administration epoch continues
## until the cut-off date (confirm).
##
## However, there may be further administration epochs following,
## and there seems to be an implicit agreement that in the case of
## missing EXENDTC, the administration continues until the start of
## the next epoch, see the example below.
##
## This is not adequately reflected in make_admin() and needs to be
## implemented.
##
##
## STUDYID DOMAIN           USUBJID EXSEQ EXGRPID EXSPID EXTRT EXDOSE EXDOSU EXDOSFRM EXDOSFRQ EXROUTE
## 1 2019240001     EX 20192400013011005     1              1 M1774    180     mg  CAPSULE       QD    ORAL
## 2 2019240001     EX 20192400013011005     2              2 M1774    180     mg  CAPSULE       QD    ORAL
## 3 2019240001     EX 20192400013011005     3              4 M1774    150     mg  CAPSULE       QD    ORAL
## 4 2019240001     EX 20192400013011005     4              6 M1774    150     mg  CAPSULE       QD    ORAL
## EXADJ     EPOCH          EXSTDTC          EXENDTC EXSTDY EXENDY EXENRTPT EXENTPT
## 1               TREATMENT 2022-06-29T15:55 2022-07-12T16:00      1     14
## 2               TREATMENT 2022-07-27T12:00                      29     NA
## 3 ADVERSE EVENT TREATMENT 2022-08-24T13:45 2022-09-06T12:00     57     70
## 4               TREATMENT 2022-09-21T14:23       2022-10-04     85     98
##
##
## SOLUTION:
##
## Change administraiton expansion algorithm in make_admin()
## accordingly.
##
## -------------------------------------------
##
## PROBLEM:
##
## NCA for multiple dose administration does not world_bank_pop
##
## NCA for nif.001 produces the error:
##
##   Error in check.interval.specification(intervals) :
##   interval specification has no rows
##
## Is this due to the multiple administrations?
##
## -------------------------------------------
##
## PROBLEM:
##
## Implement LOQ!


#' Render data frame object to string
#'
#' This function renders a data.frame into a string similar to its representation
#'  when printed without line numbers
#'
#' @param df The data.frame to be rendered
#' @param indent A string that defines the left indentation of the rendered
#'   output.
#' @param header Boolean to indicate whether the header row is to be included.
#' @param n The number of lines to be included, or all if NULL.
#' @return The output as string.
#' @import utils
#' @export
df.to.string <- function(df, indent="", n=NULL, header=T){
  df <- as.data.frame(df) %>%
    mutate(across(everything(), as.character))
  max.widths <- as.numeric(lapply(
    rbind(df, names(df)),
    FUN=function(x) max(sapply(as.character(x), nchar), na.rm=TRUE)))
  line = df[1,]

  render.line <- function(line){
    out <- indent
    for(i in 1:length(line)){
      out <- paste0(out, sprintf(paste0("%-", max.widths[i]+3, "s"),
                                 as.character(line[i])))
    }
    return(out)
  }

  if(header==T) {
    out <- render.line(data.frame(as.list(names(df))))
  } else {
    out <- ""
  }

  if(!is.null(n)){
    df <- utils::head(df, n=n)
  }
  for(i in 1:nrow(df)){
    out <- paste(out, render.line(df[i,]), sep="\n")
  }
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
recode_sex <- function(obj){
  obj %>%
    dplyr::mutate(SEX=as.numeric(
      dplyr::case_match(as.character(SEX),
                        "M"~0,
                        "F"~1,
                        "1"~1,
                        "0"~0,
                        .default=NA)))
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
#'
#' @examples
#' standardize_date_format(examplinib_fe$domains[["ex"]], c("RFSTDTC", "EXSTDTC", "EXENDTC"))
standardize_date_format <- function(obj, fields=NULL) {
  obj %>%
    dplyr::mutate_at(fields, function(x) {
      lubridate::as_datetime(x, format=dtc_formats)
    })
}


#' Convert date fields to ISO 8601 format
#'
#' @param obj A data frame.
#' @param fields Date variable names as character.
#'
#' @return A data frame.
#' @export
isofy_date_format <- function(obj, fields=NULL) {
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
  obj %>% dplyr::mutate_at(vars(ends_with("DTC")),
    function(x) {
      if(!is.POSIXct(x)) {
        x <- lubridate::as_datetime(x, format=dtc_formats)}
      return(x)})
}


#' Convert all DTC fields into ISO 8601 string format
#'
#' @param obj A data frame.
#'
#' @return A data frame.
isofy_dates <- function(obj) {
  obj %>%
    dplyr::mutate_at(vars(ends_with("DTC")), ~format(., "%Y-%m-%dT%H:%M"))
}


#' Compose DTC from date and time components
#'
#' @param date A date in POSIX or character format.
#' @param time A time in character format.
#'
#' @return A POSICct object
compose_dtc <- function(date, time) {
  data.frame(date=as.character(date), time=as.character(time)) %>%
    mutate(time=case_when(is.na(time)~"", .default=time)) %>%
    mutate(DTC=str_trim(paste(as.character(date), time))) %>%
    mutate(DTC=lubridate::as_datetime(DTC,
      format=c("%Y-%m-%d %H:%M", "%Y-%m-%d"))) %>%
    pull(DTC)
}


#' Extract date component of a POSICct object
#'
#' @param dtc The POSIX-formatted datetime.
#'
#' @return The date as character.
extract_date <- function(dtc) {
  format(dtc, format="%Y-%m-%d")
}


#' Extract time component of a POSICct object
#'
#' @param dtc The POSIX-formatted datetime.
#'
#' @return The time as character.
extract_time <- function(dtc) {
  format(dtc, format="%H:%M")
}


impute.missing.end.time <- function(ex, silent=F) {
  ## Issue message about imputations
  temp <- admin %>%
    dplyr::filter(is.na(end.time) & !is.na(start.time))
  if(nrow(temp) != 0){
    temp <- temp %>%
      dplyr::select(USUBJID, EXSEQ, EXTRT, EXSTDTC, end) %>%
      df.to.string()
    if(!silent){
      message(paste("Administration end time was imputed to start time",
                    "for the following entries:\n", temp))
    }
  }
  ## conduct missing end time imputation
  admin <- admin %>%
    dplyr::mutate(end.time=case_when(is.na(end.time) & !is.na(start.time) ~ start.time,
                                     .default=end.time))
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
#' @param ex EX domain
#' @param cut.off.date The cut-off date to be used where no EXENDTC is recorded,
#'  in POSIX format.
#' @param drug_mapping A data frame with the columns of EXTRT and PCTESTCD
#'  that associate both.
#' @param impute.missing.end.time A boolean value to indicate whether in rows
#'  in EX where EXENDTC does not include a time, the time should be copied from
#'  EXSTDTC.
#' @param silent Boolean value to indicate whether warnings should be printed.
#'
#' @return A tibble with individual administrations
#' @import lubridate
#' @import dplyr
#' @import tidyr
#' @import assertr
make_admin <- function(ex,
                       drug_mapping,
                       cut.off.date,
                       impute.missing.end.time=TRUE,
                       silent=F){
  # Assertions
  ex <- ex %>%
    verify(has_all_names("STUDYID", "USUBJID", "EXSEQ", "EXTRT", "EXSTDTC", "EXENDTC",
                         "EXDOSE", "EPOCH"))

  admin <- ex %>%
    lubrify_dates() %>%
    mutate(EXSTDTC_has_time=has_time(EXSTDTC),
           EXENDTC_has_time=has_time(EXENDTC)) %>%

    ### The following should be probably done after expansion!
    # filter for entries with start before cut-off
    dplyr::filter(EXSTDTC <= cut.off.date) %>%
    dplyr::group_by(USUBJID, EXTRT) %>%
    dplyr::arrange(EXSTDTC) %>%
    dplyr::mutate(next_start=lead(EXSTDTC)) %>%
    dplyr::ungroup() %>%

    dplyr::mutate(end=case_when(is.na(EXENDTC)~next_start-days(1),
                                .default=EXENDTC)) %>%
    dplyr::mutate(end=case_when(is.na(end)~cut.off.date,
                                .default=end)) %>%
    dplyr::select(-next_start) %>%

    # convert EXSTDTC to to datetime object, start date and start time
    mutate(start.date=extract_date(EXSTDTC)) %>%
    dplyr::mutate(start.time=case_when(
      EXSTDTC_has_time==T ~ extract_time(EXSTDTC),
      .default=NA)) %>%

    mutate(end.date=extract_date(end)) %>%
    dplyr::mutate(end.time=case_when(EXENDTC_has_time==T ~ extract_time(end),
                                     .default=NA))

  # for rows with recorded start.time but missing end.time, impute end.time to
  #   be equal to start.time
  ##
  ## to do:
  ##
  ## apply imputation only for entries that have an end DATE!!
  ##

  if(impute.missing.end.time){
    ## Issue message about imputations
    temp <- admin %>%
      dplyr::filter(is.na(end.time) & !is.na(start.time))
    if(nrow(temp) != 0){
      temp <- temp %>%
        dplyr::select(USUBJID, EXSEQ, EXTRT, EXSTDTC, end) %>%
        df.to.string()
      # if(!silent){
      #   message(paste("Administration end time was imputed to start time",
      #                 "for the following entries:\n", temp))
      # }
      conditional_message("Administration end time was imputed to start time ",
        "for the following entries:\n", temp, silent=silent)
    }
    ## conduct missing end time imputation
    admin <- admin %>%
      dplyr::mutate(end.time=case_when(is.na(end.time) & !is.na(start.time) ~ start.time,
        .default=end.time))
  }

  # Derive EXSTDTC and EXENDTC if not available in the original data set
  if(!"EXSTDY" %in% names(admin)) {
    admin <- admin %>%
      arrange(USUBJID, EXSTDTC) %>%
      group_by(USUBJID, EXTRT) %>%
      mutate(ref=EXSTDTC[1]) %>%
      mutate(EXSTDY=floor(as.numeric(difftime(EXSTDTC, ref, units="days"))+1)) %>%
      mutate(EXENDY=floor(as.numeric(difftime(end, ref, units="days"))+1)) %>%
      as.data.frame()
  }

  ret <- admin %>%
    # expand dates from start date to end date, time is end.time for last row,
    #   otherwise start.time
    dplyr::group_by(STUDYID, USUBJID, EXTRT, EXDOSE, EXSEQ, EXSTDTC, EXENDTC,
      EXSTDY, EXENDY, start.time, end.time, EPOCH) %>%

    # dplyr::group_by_all() %>%
    tidyr::expand(date=seq(as.Date(start.date), as.Date(end.date), by="1 day")) %>%

    dplyr::mutate(time=case_when(
      row_number()==n() ~ end.time,
      .default=start.time)) %>%
    dplyr::mutate(EXDY=EXSTDY+(row_number()-1)) %>%
    dplyr::ungroup() %>%

    mutate(DTC=compose_dtc(date, time)) %>%

    # set treatment, standard fields
    dplyr::mutate(NTIME=0, DV=NA, LNDV=NA, DOSE=EXDOSE, AMT=EXDOSE, EVID=1) %>%
    dplyr::mutate(TYPE=NA, CMT=1, PCTPTNUM=0, ANALYTE=NA, MDV=1)

    # apply drug mapping, introducing the field PCTESTCD
    if(nrow(drug_mapping)>0) {
      ret <- ret %>%
        dplyr::left_join(drug_mapping, by="EXTRT")
    } else {
      stop("no drug mapping")
    }
  return(ret %>% as.data.frame())
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
                     time_mapping=NULL,
                     spec=NULL,
                     silent=F,
                     use_pctptnum=F){
  # Assertions
  pc <- pc %>%
    verify(has_all_names("PCSPEC", "PCDTC", "PCSTRESN", "PCTESTCD"))

  # Filter for specific specimen, guess specimen if none defined
  pcspecs <- pc %>%
    dplyr::distinct(PCSPEC) %>%
    dplyr::pull(PCSPEC)

  standard_specs <- c("PLASMA", "Plasma", "plasma", "SERUM", "Serum", "serum",
                      "BLOOD", "Blood", "blood")
  if(length(spec)==0) {
    spec <- standard_specs[standard_specs %in% pcspecs][1]
    # if(!silent){
    #   message(paste("No specimen specified. Set to", spec, "as the most likely."))
    # }
    conditional_message("No specimen specified. Set to ", spec,
      " as the most likely.", silent=silent)
  }
  obs <- pc %>%
    dplyr::filter(PCSPEC %in% spec)

  # filter for PC data marked as 'not done'
  if("PCSTAT" %in% colnames(obs)){
    nd <- obs %>%
      dplyr::filter(PCSTAT=="NOT DONE") %>%
      nrow()
    if(nd>0 & !silent){
      message(paste(
        nd,
        "samples are marked as 'not done' and were removed fromthe data set."))
    }
    obs <- obs %>%
      dplyr::filter(PCSTAT!="NOT DONE")
  }

  # identify observation date and time
  obs <- obs %>%
    # extract date and time of observation
    mutate(DTC=PCDTC) %>%
    lubrify_dates() %>%
    mutate(start.date=extract_date(DTC)) %>%
    dplyr::mutate(start.time=case_when(has_time(PCDTC) ~ extract_time(DTC),
                                       .default=NA))

  # identify nominal time
  if(use_pctptnum) {
    obs <- obs %>%
      dplyr::mutate(NTIME=as.numeric(PCTPTNUM))
  } else {
    if("PCELTM" %in% names(pc)){
      obs <- obs %>%
        dplyr::mutate(NTIME=as.numeric(
          stringr::str_extract(PCELTM, "PT([.0-9]+)H", group=1)))
    } else {
      if(is.null(time_mapping) | nrow(time_mapping)==0){
        stop(paste("No PCELM in PC. Please add time mapping to SDTM data set",
          "(see '?add_time_mapping()' for details)."))
      }
      obs <- obs %>%
        dplyr::left_join(time_mapping, by="PCTPT")
    }
  }

  obs <- obs %>%
    dplyr::mutate(EVID=0, CMT=2, AMT=0, DV=PCSTRESN/1000, LNDV=log(DV)) %>%
    dplyr::mutate(MDV=case_when(is.na(DV) ~ 1, .default=0)) %>%
    dplyr::mutate(ANALYTE=PCTESTCD)

  return(obs %>% as.data.frame())
}


#' Extract last observation time from observation tibble
#'
#' .
#'
#' @param obs tibble as created with make_obs
#' @return A datetime object representing the last recorded observation time
#' @import dplyr
last_obs_dtc <- function(obs){
  return(max(obs$DTC, na.rm=T))
}


#' Last administration DTC
#'
#' @param ex The EX domain as data table.
#'
#' @return The last administration in DTC format.
#' @export
last_ex_dtc <- function(ex) {
  return(max(ex$EXENDTC, na.rm=T))
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
impute.administration.time <- function(admin, obs) {
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
    mutate(dtc.date=extract_date(DTC)) %>%
    mutate(ref.date=extract_date(PCRFTDTC)) %>%
    mutate(ref.time=extract_time(PCRFTDTC)) %>%
    dplyr::filter(dtc.date == ref.date) %>%
    dplyr::group_by(USUBJID, dtc.date, PCTESTCD) %>%
    dplyr::distinct(ref.time)

  ret <- admin %>%
    mutate(dtc.date=extract_date(date)) %>%
    dplyr::left_join(reference, by=c("USUBJID"="USUBJID", "dtc.date"="dtc.date",
                                "EXTRT"="PCTESTCD")) %>%

    # take time for administration from PCREFDTC where time is not available
    #   from EX
    dplyr::mutate(admin.time=case_when(
      !is.na(ref.time) ~ ref.time,
      .default=time)) %>%
    mutate(admin.time=case_when(is.na(admin.time)~"", .default=admin.time)) %>%

    dplyr::group_by(USUBJID, EXTRT) %>%
    dplyr::arrange(date) %>%
    # carry forward/back last administration time
    tidyr::fill(admin.time, .direction="downup") %>%
    dplyr::ungroup() %>%

    mutate(DTC=compose_dtc(date, admin.time)) %>%
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
baseline_covariates <- function(vs, silent=F) {
  temp <- vs %>%
    filter(VSTESTCD %in% c("HEIGHT", "WEIGHT"))

  bl_cov <- NULL

  if("VISIT" %in% names(temp)) {
    if("SCREENING" %in% str_to_upper(unique(temp$VISIT))) {
      bl_cov <- temp %>%
        filter(str_to_upper(VISIT)=="SCREENING")
    } else {
      if("VSBLFL" %in% names(temp)) {
        bl_cov <- temp %>%
          filter(VSBLFL=="Y")
      }
    }
  } else {
    if("VSBLFL" %in% names(temp)) {
      bl_cov <- temp %>%
        filter(VSBLFL=="Y")
    } else {
      # if(!silent){
      #   message("Baseline VS data could not be identified!")
      # }
      conditional_message("Baseline VS data could not be identified!",
                          silent=silent)
    }
  }

  bl_cov <- bl_cov %>%
    dplyr::group_by(USUBJID, VSTESTCD) %>%
    dplyr::summarize(mean=mean(VSSTRESN), .groups="drop") %>%
    tidyr::pivot_wider(names_from=VSTESTCD, values_from=mean)

  # Calculate BMI if height and weight are available
  if("HEIGHT" %in% colnames(bl_cov) & "WEIGHT" %in% colnames(bl_cov)) {
    bl_cov <- bl_cov %>%
      mutate(BMI=WEIGHT/(HEIGHT/100)^2)
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
conditional_message <- function(msg, ..., silent=F){
  parameters <- c(as.list(environment()), list(...))
  parameters <- lapply(parameters, as.character)
  if(!silent) {
    message(paste(as.character(parameters[names(parameters) != "silent"])))
  }
}


#' Make raw NIF data set from list of SDTM domains
#'
#' @description This function makes a basic NONMEM input file (NIF) data set, following the
#'   conventions summarized in
#'   [Bauer, CPT Pharmacometrics Syst. Pharmacol.
#'   (2019)](https://doi.org/10.1002/psp4.12404).
#'
#'   For SDTM data sets in which the administered drugs (EXTRT in domain EX) and
#'   the corresponding analyte (PCTESTCD in domain PC) have different names,
#'   treatment-to-analyte mappings must be added to the sdtm object using
#'   [add_mapping()].
#'
#' @section Imputations:
#'   Subjects with administration but no observations for the respective
#'   analyte are deleted from the data set. For further imputations, see the
#'   vignette "nif-imputations".
#'
#' @section Output fields:
#'   * `ID` Subject identification number
#'   * `TIME` Recorded time of administration or observation events in hours
#'        relative to the first individual event.
#'   * `AMT` Dose administered for dosing record, or zero for observations.
#'   * `DOSE` Dose in mg for administrations and post-dose observations.
#'   * `DV` The dependent variable, i.e., observed concentration, or zero for
#'        administration records, in mg/l.
#'   * `LNDV` The natural Log of DV.
#'   * `RATE` Rate of infusion of drug or zero if drug is given as a bolus.
#'   * `MDV` One for missing DV, else zero.
#'   * `EVID` Event ID: 0 for observations, 1 for administrations.
#'   * `CMT` Pharmacokinetic compartment. Will be set to 1 for administrations and 2 for
#'        observations. Should be changed afterwards, if needed.
#'   * `DTC` The date-time of the data record.
#'   * `FIRSTDTC` Date and time of first event per subject. This field is used
#'        internally for the calculation of `TIME`. Although it is not needed
#'        for NONMEM analysis, it is provided for subsequent NIF file building
#'        steps, e.g., addition of further time-dependent endpoints.
#'   * `FIRSTADMINDTC` The date-time of the first administration of the
#'        respective parent drug for the respective subject.
#'   * `FIRSTTRTDTC` The date-time of the first administration of any parent
#'        drug for the respective subject.
#'   * `ANALYTE` The analyte or drug in the data record.
#'   * `TRTDY` The treatment day, i.e., the relative day after the first
#'        treatment for the respective subject.
#'
#' @param sdtm.data A list of SDTM domains as data tables, e.g., as loaded using
#'   read_sas_sdtm(). As a minimum, dm, vs, pc and ex are needed.
#' @param spec The specimen to be represented in the NIF data set as string
#'   (e.g., "BLOOD", "PLASMA", "URINE", "FECES"). When spec is an empty string
#'   (""), which is the default setting, the most likely specimen, i.e., "BLOOD"
#'   or "PLASMA" is selected, depending what is found in the PC data.
#' @param impute.missing.end.time A logic value to indicate whether in rows
#'   in EX where EXENDTC does not include a time, the time should be copied from
#'   EXSTDTC.
#' @param impute.administration.time A boolean value to indicate whether the
#'  time of administration is to be imputed from the PCRFDTC field from the
#'  PC domain. This field is 'permissible' and may not be present in certain
#'  SDTM data.
#' @param truncate.to.last.observation Boolean to indicate whether the data set
#'   should be truncated to the last observation. In this case, administrations
#'   after the last observation time point will deleted.
#' @param silent Boolean value to indicate whether warnings should be printed.
#' @param use_pctptnum Boolean to indicate whether to derive nominal time
#'   (`NTIME`) from `PCTPTNUM`.
#'
#' @return A NIF object.
#' @seealso [add_analyte_mapping()]
#' @seealso [add_time_mapping()]
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' make_nif(examplinib_fe)
#'
make_nif <- function(
    sdtm.data,
    spec=NULL,
    impute.missing.end.time=TRUE,
    impute.administration.time=TRUE,
    silent=F,
    truncate.to.last.observation=FALSE,
    use_pctptnum=TRUE) {
  vs <- sdtm.data$domains[["vs"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()
  ex <- sdtm.data$domains[["ex"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()
  pc <- sdtm.data$domains[["pc"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()
  dm <- sdtm.data$domains[["dm"]] %>%
    dplyr::select(-DOMAIN) %>%
    lubrify_dates()

  # Get baseline covariates on subject level from VS
  bl.cov <- baseline_covariates(vs, silent=silent)

  # create drug mapping
  drug_mapping <- sdtm.data$analyte_mapping %>%
    rbind(
      data.frame(
        EXTRT=intersect(unique(ex$EXTRT), unique(pc$PCTESTCD))
      ) %>%
        mutate(PCTESTCD=EXTRT)) %>%
    mutate(PARENT=PCTESTCD)

  # add metabolite mapping, if available
  if(nrow(sdtm.data$metabolite_mapping)!=0){
    drug_mapping <- drug_mapping %>%
      rbind(
        sdtm.data$metabolite_mapping %>%
          rename(PARENT=PCTESTCD_parent, PCTESTCD=PCTESTCD_metab) %>%
          mutate(EXTRT=""))
  }
  drug_mapping <- drug_mapping %>%
    mutate(METABOLITE=(PCTESTCD!=PARENT)) %>%
    distinct()

  # make observations
  obs <- make_obs(pc, time_mapping=sdtm.data$time_mapping,
                  spec=spec, silent=silent, use_pctptnum=use_pctptnum) %>%
    left_join(drug_mapping, by="PCTESTCD")

  # define cut-off date
  if(truncate.to.last.observation==TRUE){
    cut.off.date <- last_obs_dtc(obs)
    # if(!silent) {
    #   message(paste("Data cut-off was set to last observation time,", cut.off.date))
    # }
    conditional_message("Data cut-off was set to last observation time, ",
                        cut.off.date, silent=silent)
  } else {
    cut.off.date <- last_ex_dtc(ex)
  }

  # identify subjects with observations by analyte
  obs.sbs <- obs %>%
    filter(!is.na(PCSTRESN)) %>%
    tidyr::unite("ut", USUBJID, PCTESTCD, remove=FALSE) %>%
    dplyr::distinct(USUBJID, PCTESTCD, ut)

  # make administrations
  admin <- make_admin(
    ex,
    drug_mapping = drug_mapping,
    cut.off.date,
    impute.missing.end.time=impute.missing.end.time,
    silent=silent)

  # Remove all administrations with PCTESTCD==NA
  #  those rows may come from treatments that have no analyte mapping
  admin <- admin %>%
    dplyr::filter(!is.na(PCTESTCD))

  # filter admin for subjects who actually have observations
  no.obs.sbs <- admin %>%
    tidyr::unite("ut", USUBJID, PCTESTCD, remove=FALSE) %>%
    dplyr::filter(!(ut %in% obs.sbs$ut)) %>%
    dplyr::distinct(USUBJID, PCTESTCD, ut) %>%
    as.data.frame()
  # Issue message about excluded administrations, if applicable
  if(nrow(no.obs.sbs)>0) {
    out <- no.obs.sbs %>%
      dplyr::arrange(PCTESTCD, USUBJID) %>%
      dplyr::select(USUBJID, PCTESTCD) %>%
      df.to.string()
    # if(!silent){
    #   message(paste0("The following subjects had no observations for ",
    #     "the respective analyte and were removed from the data set:\n",
    #     out))
    # }
    conditional_message("The following subjects had no observations for ",
      "the respective analyte and were removed from the data set:\n",
      out, silent=silent)
  }
  # and filter out excluded administrations
  admin <- admin %>%
    tidyr::unite("ut", USUBJID, PCTESTCD, remove=FALSE) %>%
    dplyr::filter(ut %in% obs.sbs$ut) %>%
    dplyr::select(-ut)

  if(nrow(admin)==0) {
    stop(paste0("No subjects in the data set left after filtering out ",
    "subjects without observations.\n",
    "  In most cases, this is because of `PCTESTCD` not matching with a ",
    "corresponding `EXTRT`.\n",
    "  Consider adding analyte mappings to the SDTM data object!\n",
    "  For futher information, please see `?add_analyte_mapping`,",
    "`suggest_sdtm()` and the\n",
    "  vignette 'Creating NIF files from SDTM data'!"))
  }

  # impute administration times
  if(impute.administration.time==TRUE) {
    admin <- impute.administration.time(admin, obs)
  }

  # calculate age from birthday and informed consent signature date
  if("RFICDTC" %in% colnames(dm) & "BRTHDTC" %in% colnames(dm)) {
    dm <- dm %>%
      dplyr:: mutate(age1=floor(as.duration(interval(BRTHDTC, RFICDTC))/as.duration(years(1)))) %>%
      dplyr::mutate(AGE=case_when(is.na(AGE) ~ age1, .default=AGE)) %>%
      dplyr::select(-age1)
  }

  ## assemble NIF data set from administrations and observations and baseline
  #    data.
  nif <- obs %>%
    dplyr::bind_rows(
      admin %>%
        dplyr::filter(USUBJID %in% obs$USUBJID)) %>%
    dplyr::left_join(dm, by=c("USUBJID", "STUDYID")) %>%
    dplyr::left_join(bl.cov, by="USUBJID") %>%

    # individual first event
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(FIRSTDTC=min(DTC, na.rm=T)) %>%
    dplyr::ungroup() %>%

    # filter out rows without parent information
    dplyr::filter(PARENT != "") %>%

    # filter out observations without administration
    dplyr::group_by(USUBJID, PARENT) %>%
    dplyr::filter(sum(AMT)!=0) %>%
    dplyr::ungroup() %>%

    # identify first administration per PARENT and first treatment overall
    group_by(USUBJID, PARENT) %>%
    dplyr::mutate(FIRSTADMINDTC=min(DTC[EVID==1], na.rm=T)) %>%
    ungroup() %>%
    group_by(USUBJID) %>%
    mutate(FIRSTTRTDTC=min(FIRSTADMINDTC, na.rm=T)) %>%
    ungroup() %>%

    dplyr::arrange(USUBJID, PARENT, DTC, EVID) %>%
    dplyr::group_by(USUBJID, PARENT) %>%
    tidyr::fill(DOSE, .direction = "down") %>%
    tidyr::fill(any_of(c("AGE", "SEX", "RACE", "ETHNIC", "ACTARMCD", "HEIGHT", "WEIGHT", "COUNTRY", "ARM",
                "SUBJID")), .direction="down") %>%
    dplyr::ungroup() %>%

    dplyr::mutate(RATE=0) %>%

    # TIME is the difference in h to the first individual event time
    dplyr::mutate(TIME=round(
      as.numeric(difftime(DTC, FIRSTDTC, units="h")), digits=3)) %>%
    dplyr::mutate(ANALYTE=PCTESTCD) %>%

    # Treatment day
    mutate(TRTDY=interval(date(FIRSTTRTDTC),
                          date(DTC)) / days(1) +1) %>%
    # recode SEX
    recode_sex()

    nif <- nif %>%
    # create ID column
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(ID=as.numeric(as.factor(USUBJID))) %>%
    dplyr::relocate(ID) %>%
    new_nif() %>%
    index_nif()
  return(nif)
}


#' This function finalizes a NIF data set by indexing the rows
#'
#' @param nif NIF dataset as created by make_raw_nif() and potentially modified for additional covariates
#' @return A final NIF dataset including ID and REF fields
#' @import dplyr
#' @export
index_nif <- function(nif) {
  nif %>%
    as.data.frame() %>%
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(REF=row_number()) %>%
    dplyr::relocate(REF) %>%
    new_nif()
}


#' This function cleans up a NIF data set
#'
#' @param nif NIF dataset as created by make_raw_nif() and potentially modified for
#'  additional covariates.
#' @param ... Further optional parameters are fields to be included in the
#'  output. If none are provided, the standard set will be used.
#' @return A NIF dataset with only the specified fields
#' @import dplyr
#' @export
compress_nif <- function(nif, ...) {
  temp <- as.character(unlist(c(as.list(environment())[-1], list(...))))
  if(length(temp)==0) {
    columns <- standard_nif_fields
  } else {
    columns <- temp
  }
  nif %>%
    dplyr::select(any_of(columns)) %>%
    new_nif()
}


#' This function reduces a NIF data set on the subject level by excluding all administrations after
#'  the last observation
#'
#' @param nif NIF dataset as created by make_raw_nif() and potentially modified for
#'  additional covariates.
#' @return A NIF dataset
#' @import dplyr
#' @export
clip_nif <- function(nif){
  last.obs <- nif %>%
    dplyr::filter(EVID==0) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(last.obs= max(TIME)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(USUBJID, last.obs)

  ret <- nif %>%
    dplyr::left_join(last.obs, by="USUBJID") %>%
    dplyr::filter(TIME <= last.obs)
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
    filter(METABOLITE==FALSE) %>%
    filter(PARENT != "", !is.na(DOSE), AMT != 0, EVID==1) %>%
    group_by(ID, ANALYTE) %>%
    arrange(ID, TIME, ANALYTE) %>%
    filter(TIME==min(TIME)) %>%
    select(ID, ANALYTE, DOSE)  %>%
    group_by(ID)

  if(temp %>%
     ungroup() %>%
     distinct(ANALYTE) %>%
     nrow()==1) {
    temp <- temp %>% mutate(DL=DOSE)
  } else {
   temp <- temp %>%
    mutate(DL=paste0(DOSE, "-", ANALYTE)) %>%
    arrange(ID) %>%
    arrange(factor(ANALYTE, levels=analytes(obj))) %>%
    summarize(DL=paste0(DL, collapse="+"))
  }

  return(obj %>%
           left_join(temp %>%
                       select(ID, DL), by="ID"))
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
add_bl_lab <- function(obj, lb, lbtestcd, lbspec="", silent=F){
  temp <- lbtestcd %in% (lb %>%
                           dplyr::distinct(.data$LBTESTCD) %>%
                           dplyr::pull(.data$LBTESTCD))
  if(!all(temp)) {
    if(!silent) {
      message(paste0("The following was not found in lb: ", lbtestcd[!temp]))
    }
    lbtestcd <- lbtestcd[temp]
    if(length(lbtestcd)==0){
      return(obj)
    }
  }

  temp <- lb %>%
    dplyr::filter(.data$LBSPEC==lbspec) %>%
    dplyr::filter(.data$LBBLFL == "Y") %>%
    dplyr::filter(.data$LBTESTCD %in% lbtestcd) %>%
    dplyr::select(.data$USUBJID, .data$LBTESTCD, .data$LBSTRESN) %>%
    tidyr::pivot_wider(names_from="LBTESTCD", values_from="LBSTRESN") %>%
    dplyr::rename_with(~stringr::str_c("BL_", .), .cols=-1)

  obj %>%
    as.data.frame() %>%
    dplyr::left_join(temp, by="USUBJID") %>%
    new_nif()
}


#' Add lab covariate
#'
#' This functions adds columns for the lab parameters specified in `lbtestcd`, in
#'   a time-varying way, i.e., the actual lab value at the time of the observation
#'   or administration. This is in contrast to  [add_bl_lab()].
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
add_lab_covariate <- function(obj, lb, lbspec="SERUM", lbtestcd, silent=F){
  temp <- lbtestcd %in% (
    (lb %>% filter(.data$LBSPEC %in% lbspec) %>%
    dplyr::distinct(.data$LBTESTCD) %>%
    dplyr::pull(.data$LBTESTCD))
  )
  if(!all(temp)) {
    if(!silent){
      message(paste0("The following was not found in lb: ",
                     lbtestcd[!temp], " (", lbspec, ")\n"))
    }
    lbtestcd <- lbtestcd[temp]
    if(length(lbtestcd)==0){
      return(obj)
    }
  }

  lb.params <- lb %>%
    mutate(dtc=lubridate::as_datetime(
      .data$LBDTC, format=dtc_formats)) %>%
    mutate(date=format(.data$dtc, format="%Y-%m-%d")) %>%
    mutate(labdate=date) %>%
    dplyr::filter(.data$LBSPEC %in% lbspec) %>%
    dplyr::filter(.data$LBTESTCD %in% lbtestcd) %>%
    dplyr::select(.data$USUBJID, .data$date, .data$labdate, .data$LBTESTCD,
                  .data$LBSTRESN) %>%
    pivot_wider(names_from=.data$LBTESTCD, values_from=.data$LBSTRESN,
                values_fn=mean)

  temp <- obj %>%
    as.data.frame() %>%
    mutate(date=format(.data$DTC, format="%Y-%m-%d")) %>%
    bind_rows(lb.params) %>%
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
add_lab_observation <- function(obj, lb, lbtestcd, cmt=NA, lbspec="", silent=F) {
  test <- lbtestcd %in% unique(lb$LBTESTCD)
  if(!all(test)) {
    stop(paste0("The following parameters were not not found in lb: ",
                df.to.string(lbtestcd[!test], header=F)))
  }

  lb.params <- lb %>%
    dplyr::filter(USUBJID %in% (obj %>% subjects() %>% pull(USUBJID))) %>%
    dplyr::mutate(DTC=lubridate::as_datetime(
      LBDTC, format=dtc_formats)) %>%
    dplyr::filter(LBSPEC %in% lbspec) %>%
    dplyr::filter(LBTESTCD==lbtestcd) %>%

    left_join(obj %>%
                as.data.frame() %>%
                distinct(USUBJID, FIRSTDTC), by="USUBJID") %>%

    dplyr::mutate(ANALYTE=lbtestcd, PARENT="", CMT=cmt, MDV=0, EVID=0,
                  AMT=0, RATE=0) %>%
    dplyr::mutate(TIME=round(as.numeric(difftime(DTC, FIRSTDTC, units="h")),
                             digits=3)) %>%
    dplyr::mutate(DV=LBSTRESN) %>%
    dplyr::mutate(LNDV=log(DV)) %>%
    dplyr::mutate(NTIME=case_when(LBDY<0 ~ LBDY*24, .default=(LBDY-1)*24)) %>%
    dplyr::select(STUDYID, USUBJID, DTC, FIRSTDTC, ANALYTE, PARENT, CMT, EVID,
                  TIME, NTIME, DV, LNDV, AMT)

  temp <- obj %>%
    as.data.frame() %>%
    bind_rows(lb.params) %>%
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(REF=row_number()) %>%
    dplyr::group_by(USUBJID) %>%
    tidyr::fill(ID, AGE, SEX, RACE, BMI, ACTARMCD, HEIGHT, WEIGHT, DOSE,
                .direction="downup") %>%
    tidyr::fill(starts_with("BL_"), .direction="downup") %>%
    tidyr::fill(any_of(c("PART", "COHORT")),
                .direction="downup") %>%
    dplyr::ungroup()

  return(new_nif(temp))
}







