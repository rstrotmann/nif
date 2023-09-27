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










#' The list of expected date/time formats in the SDTM data
#'
dtc_formats <- c("%Y-%m-%dT%H:%M", "%Y-%m-%d", "%Y-%m-%dT%H:%M:%S")


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
  df <- as.data.frame(df)
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


has.time <- function(datetime) {
  stringr::str_detect(datetime, "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}")
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
#'  in "%Y-%m-%dT%H:%M" format.
#' @param drug_mapping A data frame with the columns of EXTRT and PCTESTCD
#'  that associate both.
#' @param impute.missing.end.time A logic value to indicate whether in rows
#'  in EX where EXENDTC does not include a time, the time should be copied from
#'  EXSTDTC.
#' @param silent Boolean value to indicate whether warnings should be printed.
#' @return A tibble with individual administrations
#' @import lubridate
#' @import dplyr
#' @import tidyr
make_admin <- function(ex,
                       drug_mapping,
                       cut.off.date,
                       impute.missing.end.time=TRUE,
                       silent=F){
  cutoff <- lubridate::as_datetime(
    cut.off.date,
    format=dtc_formats)

  ret <- ex

  # impute EXENDTC if not present (i.e., use cut-off date when administration ongoing)
  temp <- ret %>% dplyr::filter(EXENDTC=="")
  if(temp %>% nrow() > 0){
    out <- temp %>%
      dplyr::select(USUBJID, EXSEQ, EXTRT, EXSTDTC, EXENDTC) %>%
      df.to.string()
    if(!silent){
      message(paste0("In ", temp %>% nrow(),
        " administrations, EX contained no EXENDTC. ",
        "In these cases, EXENDTC was set to the cut off date (", cutoff, "):\n", out))
    }
  }

  ret <- ret %>%
    dplyr::mutate(EXENDTC=dplyr::case_when(
      EXENDTC=="" ~ format(cutoff, "%Y-%m-%dT%H:%M"),
      .default=EXENDTC)) %>%

    # convert EXSTDTC to to datetime object, start date and start time
    dplyr::mutate(start=lubridate::as_datetime(
      EXSTDTC, format=dtc_formats)) %>%
    dplyr::mutate(start.date=format(start, format="%Y-%m-%d")) %>%
    dplyr::mutate(start.time=case_when(
      has.time(EXSTDTC) ~ format(start, format="%H:%M"),
      .default=NA)) %>%

    # filter for entries with start before cut-off
    dplyr::filter(start <= cutoff) %>%

    # convert EXENDTC to datetime object, end date and end time
    dplyr::mutate(end=lubridate::as_datetime(
      EXENDTC, format=dtc_formats)) %>%
    dplyr::mutate(end.date=format(end, format="%Y-%m-%d")) %>%
    dplyr::mutate(end.time=case_when(
      has.time(EXENDTC) ~ format(end, format="%H:%M"),
      .default=NA))

  # for rows with recorded start.time but missing end.time, impute end.time to
  #   be equal to start.time
  if(impute.missing.end.time){
    temp <- ret %>%
      dplyr::filter(is.na(end.time) & !is.na(start.time))
    if(temp %>% nrow() != 0){
      temp <- temp %>%
        dplyr::select(USUBJID, EXSEQ, EXTRT, EXSTDTC, EXENDTC) %>%
        df.to.string()
      if(!silent){
        message(paste("Administration end time was imputed to start time",
                      "for the following entries:\n", temp))
      }
      ret <- ret %>%
        dplyr::mutate(end.time=case_when(is.na(end.time) & !is.na(start.time) ~ start.time,
          .default= end.time))
    }
  }

  ret <- ret %>%
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

    # set treatment, standard fields
    # dplyr::mutate(treatment=EXTRT) %>%
    dplyr::mutate(NTIME=0, DV=NA, LNDV=NA, DOSE=EXDOSE, AMT=EXDOSE, EVID=1) %>%
    dplyr::mutate(TYPE=NA, CMT=1, PCTPTNUM=0, ANALYTE=NA)

    # apply drug mapping, introducing the field PCTESTCD
    if(nrow(drug_mapping)>0) {
      ret <- ret %>%
        dplyr::left_join(drug_mapping, by="EXTRT")
    } else {
      # ret <- ret %>%
      #   dplyr::mutate(PCTESTCD=EXTRT)
      stop("no drug mapping")
    }
  return(ret %>% as.data.frame())
}


#' Make observation data set from PC
#'
#' Note that the DV is converted into mg/l assuming that PCSTRESN is provided
#'   in mg/ml
#'
#' @param pc The SDTM PC domain as a data.frame.
#' @param time_mapping The time mapping.
#' @param spec The specimen to be represented in the NIF data set as string
#'   (e.g., "BLOOD", "PLASMA", "URINE", "FECES"). When spec is an empty string
#'   (""), which is the default setting, the most likely specimen, i.e., "BLOOD"
#'   or "PLASMA" is selected, depending what is found in the PC data.
#' @param silent Boolean value to indicate whether warnings should be printed.
#' @return A tibble with individual observations with certain NONMEM input variables set
#' @import dplyr
#' @import lubridate
make_obs <- function(pc, time_mapping=NULL, spec=NULL, silent=F){
  # Filter for specific specimen, guess specimen if none defined
  pcspecs <- pc %>%
    dplyr::distinct(PCSPEC) %>% pull(PCSPEC)

  standard_specs <- c("PLASMA", "Plasma", "plasma", "SERUM", "Serum", "serum",
                      "BLOOD", "Blood", "blood")
  if(length(spec)==0) {
    spec <- standard_specs[standard_specs %in% pcspecs][1]
    if(!silent){
      message(paste("No specimen specified. Set to", spec, "as the most likely."))
    }
  }
  obs <- pc %>%
    dplyr::filter(PCSPEC %in% spec)

  # filter for PC data marked as 'not done'
  if("PCSTAT" %in% colnames(obs)){
    nd <- obs %>%
      dplyr::filter(PCSTAT=="NOT DONE") %>%
      nrow()
    if(nd>0 & !silent){
      message(paste(nd, "samples marked as 'not done' and removed from the data set."))
    }
    obs <- obs %>%
      dplyr::filter(PCSTAT!="NOT DONE")
  }

  obs <- obs %>%
    # extract date and time of observation
    dplyr::mutate(DTC=lubridate::as_datetime(PCDTC,
      format=dtc_formats)) %>%
    dplyr::mutate(start.date=format(DTC, format="%Y-%m-%d")) %>%
    dplyr::mutate(start.time=case_when(has.time(PCDTC) ~ format(DTC, format="%H:%M"),
      .default=NA))

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
last_obs_time <- function(obs){
  last <- obs %>%
    dplyr::distinct(USUBJID, DTC) %>%
    dplyr::group_by(USUBJID) %>%
    dplyr::summarize(last=max(DTC, na.rm=TRUE)) %>%
    dplyr::summarize(last=max(last))
  return(format(last$last, "%Y-%m-%dT%H:%M"))
}


#' Impute missing administration times
#'
#' This function fills in administration times from the PCREFDTC field when
#'  available, and carries forward last administration dates
#'
#' @param admin An admin data set as created by 'make_admin()'.
#' @param obs An observation data set as created by 'make_obs()'.
#' @return An admin data set.
#' @import tidyr
#' @import dplyr
impute.administration.time <- function(admin, obs){
  temp <- obs %>%
    dplyr::mutate(dtc=lubridate::as_datetime(
      PCDTC, format=dtc_formats)) %>%
    dplyr::mutate(dtc.date=format(dtc, format="%Y-%m-%d")) %>%
    dplyr::mutate(ref.dtc=lubridate::as_datetime(
      PCRFTDTC, format=dtc_formats)) %>%
    dplyr::mutate(ref.date=format(ref.dtc, format="%Y-%m-%d")) %>%
    dplyr::mutate(ref.time=format(ref.dtc, format="%H:%M")) %>%
    dplyr::filter(dtc.date == ref.date) %>%
    dplyr::group_by(USUBJID, dtc.date, PCTESTCD) %>%
    dplyr::distinct(ref.time)

  ret <- admin %>%
    dplyr::mutate(dtc.date=format(date, format="%Y-%m-%d")) %>%
    dplyr::left_join(temp, by=c("USUBJID"="USUBJID", "dtc.date"="dtc.date", "EXTRT"="PCTESTCD")) %>%
    # take PCREFDTC where time is not available from EX
    dplyr::mutate(admin.time=case_when(!is.na(ref.time) ~ ref.time, .default=time)) %>%
    dplyr::group_by(USUBJID, EXTRT) %>%
    dplyr::arrange(date) %>%
    # carry forward last administration time
    tidyr::fill(admin.time, .direction="down") %>%
    # carry back first administration time
    tidyr::fill(admin.time, .direction="up") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DTC=case_when((is.na(admin.time) | is.na(date)) ~ NA,
      .default=lubridate::as_datetime(paste(date, admin.time),
                                      format="%Y-%m-%d %H:%M"))) %>%
    dplyr::select(-admin.time, -ref.time)
  return(ret)
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
#'   'make_nif()' uses a cut-off time that is equal to the last observation
#'   time. All later administrations will not be in the data set.
#'
#'   Subjects-administration pairs that have no observations for the respective
#'   analyte will be deleted from the data set.
#'
#' @section Output fields:
#'   * `ID` Subject identification number
#'   * `TIME` Recorded time of administration or observation events in hours
#'        relative to the first individual event.
#'   * `AMT` Dose administered for dosing record, or zero for observations.
#'   * `DOSE` Dose in mg for administrations and post-dose observations.
#'   * `DV` The dependent variable, i.e., observed concentration, or zero for
#'        administration records.
#'   * `RATE` Rate of infusion of drug or zero if drug is given as a bolus.
#'   * `MDV` One for missing DV, else zero.
#'   * `EVID` Event ID: 0 for observations, 1 for administrations.
#'   * `CMT` Pharmacokinetic compartment. Will be set to 1 for administrations and 2 for
#'        observations. Should be changed afterwards, if needed.
#'   * `FIRSTDTC` Date and time of first event per subject. This field is used
#'        internally for the calculation of `TIME`. Although it is not needed
#'        for NONMEM analysis, it is provided for subsequent NIF file building
#'        steps, e.g., addition of further time-dependent endpoints.
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
#' @param silent Boolean value to indicate whether warnings should be printed.
#' @return A NIF data set as nif object.
#' @seealso [add_analyte_mapping()]
#' @seealso [add_time_mapping()]
#' @import tidyr
#' @import dplyr
#' @export
#' @examples
#' make_nif(examplinib)
#'
make_nif <- function(
    sdtm.data,
    spec=NULL,
    impute.missing.end.time=TRUE,
    silent=F) {
  vs <- sdtm.data$vs %>% dplyr::select(-DOMAIN)
  ex <- sdtm.data$ex %>% dplyr::select(-DOMAIN)
  pc <- sdtm.data$pc %>% dplyr::select(-DOMAIN)
  dm <- sdtm.data$dm %>% dplyr::select(-DOMAIN)

  # Get baseline covariates on subject level from VS
  bl.cov <- vs %>%
    dplyr::filter(EPOCH=="SCREENING") %>%
    dplyr::filter(VSTESTCD %in% c("HEIGHT", "WEIGHT")) %>%
    dplyr::group_by(USUBJID, VSTESTCD) %>%
    dplyr::summarize(mean=mean(VSSTRESN), .groups="drop") %>%
    tidyr::pivot_wider(names_from=VSTESTCD, values_from=mean)

  #   PCTESTCD pairs that use the same label for both:
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
                  spec=spec, silent=silent) %>%
    left_join(drug_mapping, by="PCTESTCD")

  # treatments <- unique(ex$EXTRT)
  # analytes <- unique(obs$PCTESTCD)
  # metabolites <- unique(sdtm.data$metabolite_mapping$PCTESTCD_metab)
  # parents <- analytes[(!analytes %in% metabolites) &
  #                       ((analytes %in% treatments) |
  #                          (analytes %in% unique(analyte_mapping$PCTESTCD)))]


  # complete the analyte mapping with the obvious pairings, i.e., those EXTRT-


  # define cut-off date
  cut.off.date <- last_obs_time(obs)
  if(!silent) {
    message(paste("Data cut-off was set to last observation time,", cut.off.date))
  }

  # identify subjects with observations by analyte
  obs.sbs <- obs %>%
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
    if(!silent){
      message(paste0("The following subjects had no observations for ",
        "the respective analyte and were removed from the data set:\n",
        out))
    }
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
  admin <- impute.administration.time(admin, obs)

  # calculate age from birthday and informed consent signature date
  if("RFICDTC" %in% colnames(dm) & "BRTHDTC" %in% colnames(dm)) {
    dm <- dm %>%
      dplyr::mutate(refdtc=lubridate::as_datetime(
        RFICDTC, format=dtc_formats)) %>%
      dplyr::mutate(brthyr=lubridate::as_datetime(BRTHDTC, format=c("%Y-%m-%d", "%Y-%m", "%Y"))) %>%
      dplyr:: mutate(age1=floor(as.duration(interval(brthyr, refdtc))/as.duration(years(1)))) %>%
      dplyr::mutate(AGE=case_when(is.na(AGE) ~ age1, .default=AGE)) %>%
      dplyr::select(-age1)
  }

  ## assemble NIF data set from administrations and observations and baseline
  #    data.
  nif <- obs %>%
    # join observations and administrations, then DM and baseline VS
    dplyr::bind_rows(admin %>%
                       dplyr::filter(USUBJID %in% obs$USUBJID)) %>%
    dplyr::left_join(dm, by=c("USUBJID", "STUDYID")) %>%
    dplyr::left_join(bl.cov, by="USUBJID") %>%

    # individual first event
    dplyr::group_by(USUBJID) %>%
    dplyr::mutate(FIRSTDTC=min(DTC)) %>%
    dplyr::ungroup() %>%

    # filter(!is.na(PARENT)) %>%
    filter(PARENT != "") %>%

    # filter out observations without administration
    dplyr::group_by(USUBJID, PARENT) %>%
    dplyr::filter(sum(AMT)!=0) %>%
    ungroup() %>%

  group_by(USUBJID, PARENT) %>%
    dplyr::mutate(first_admin_dtc=min(DTC[AMT!=0])) %>%
    ungroup()


  nif <- nif %>%
    # dplyr::mutate(DOSE=case_when(
    #   (AMT==0 & DTC<first_admin_dtc) ~ NA,
    #   .default=DOSE)) %>%

    # fill missing fields
    # dplyr::arrange(USUBJID, PCTESTCD, DTC, -EVID) %>%
    # dplyr::group_by(USUBJID, PCTESTCD) %>%
    # tidyr::fill(DOSE) %>%
    # tidyr::fill(AGE, SEX, RACE, ETHNIC, ACTARMCD, HEIGHT, WEIGHT, COUNTRY, ARM,
    #             SUBJID, .direction="down") %>%
    # dplyr::ungroup() %>%

    dplyr::arrange(USUBJID, PARENT, DTC, EVID) %>%
    dplyr::group_by(USUBJID, PARENT) %>%
    tidyr::fill(DOSE, .direction = "down") %>%
    tidyr::fill(AGE, SEX, RACE, ETHNIC, ACTARMCD, HEIGHT, WEIGHT, COUNTRY, ARM,
                SUBJID, .direction="down") %>%
    dplyr::ungroup() %>%

    dplyr::mutate(RATE=0) %>%

    # TIME is the difference in h to the first individual event time
    dplyr::mutate(TIME=round(
      as.numeric(difftime(DTC, FIRSTDTC, units="h")), digits=3)) %>%
    dplyr::mutate(ANALYTE=PCTESTCD) %>%

    # recode SEX
    recode_sex() %>%

    # create ID column
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(ID=as.numeric(as.factor(USUBJID))) %>%
    dplyr::relocate(ID) #%>%
  return(new_nif(nif))
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
    # (lb %>% filter(LBSPEC==lbspec)) %>%
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
    # dplyr::filter(LBSPEC==lbspec) %>%
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
add_lab_observation <- function(obj, lb, lbtestcd, cmt, lbspec="", silent=F) {
  # if(!lbtestcd %in% (
  #   lb %>%
  #   dplyr::distinct(LBTESTCD) %>%
  #   dplyr::pull(LBTESTCD))
  # ) {
  #   stop(paste0("The following was not found in lb: ", lbtestcd[lbtestcd]))
  # }

  test <- lbtestcd %in% unique(lb$LBTESTCD)
  if(!all(test)) {
    stop(paste0("The following parameters were not not found in lb: ",
                df.to.string(lbtestcd[!test], header=F)))
  }

  lb.params <- lb %>%
    mutate(DTC=lubridate::as_datetime(
      LBDTC, format=dtc_formats)) %>%
    # mutate(date=format(DTC, format="%Y-%m-%d")) %>%
    # mutate(labdate=date) %>%
    # dplyr::filter(LBSPEC==lbspec) %>%
    dplyr::filter(LBSPEC %in% lbspec) %>%
    dplyr::filter(LBTESTCD==lbtestcd) %>%

    left_join(obj %>%
                as.data.frame() %>%
                distinct(USUBJID, FIRSTDTC), by="USUBJID") %>%

    mutate(ANALYTE=lbtestcd, PARENT="", CMT=cmt, MDV=0, EVID=0, AMT=0, RATE=0) %>%
    dplyr::mutate(TIME=round(as.numeric(difftime(DTC, FIRSTDTC, units="h")), digits=3)) %>%
    mutate(DV=LBSTRESN) %>%
    mutate(NTIME=case_when(LBDY<0 ~ LBDY*24, .default=(LBDY-1)*24)) %>%
    select(STUDYID, USUBJID, DTC, FIRSTDTC, ANALYTE, PARENT, CMT, EVID, TIME, NTIME, DV, AMT)

  temp <- obj %>%
    as.data.frame() %>%
    bind_rows(lb.params) %>%
    dplyr::arrange(USUBJID, TIME, -EVID) %>%
    dplyr::mutate(REF=row_number()) %>%
    group_by(USUBJID) %>%
    fill(ID, DOSE, AGE, SEX, RACE, ACTARMCD, HEIGHT, WEIGHT, .direction="downup") %>%
    ungroup()

  return(new_nif(temp))
}







