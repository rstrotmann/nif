#' nif class constructor
#'
#' @param obj A data frame containing the actual NIF data or a sdtm object.
#' @import dplyr
#' @return A nif object from the input data set.
#' @export
new_nif <- function(obj) {
  if(class(obj)[1] == "sdtm"){
    temp <- make_nif(obj)
  } else {
    temp <- obj
    class(temp) <- c("nif", "data.frame")
  }
  return(temp)
}


#' print() implementation for nif objects
#'
#' @param x A nif object.
#' @param ... Additional parameters
#'
#' @export
print.nif <- function(x, ...){
  cat(paste("NONMEM input file (NIF) data set with data from",
            length(studies(x)),
            "studies\n"))
  n.obs <- x %>%
    filter(EVID==0) %>%
    nrow()
  cat(paste(n.obs, "observations from",
            subjects(x) %>% nrow(), "subjects\n"))

  n.sex <- x %>%
    dplyr::distinct(USUBJID, SEX) %>%
    dplyr::group_by(SEX) %>%
    dplyr::summarize(n=n())

  n_males <- n.sex %>%
    dplyr::filter(SEX==0) %>%
    dplyr::pull(n)
  if(length(n_males)==0) {n_males=0}

  n_females <- n.sex %>%
    dplyr::filter(SEX==1) %>%
    dplyr::pull(n)
  if(length(n_females)==0) {n_females=0}

  cat(paste0("Males: ", n_males, ", females: ", n_females, " (",
             round(n_females/(n_males + n_females)*100, 1), "%)\n\n"))

  cat(paste0("Studies:\n", paste(studies(x), collapse="\n"), "\n\n"))

  cat("Dose levels:\n")
  cat(df.to.string(dose_levels(x, grouping=any_of(c("PART", "COHORT", "GROUP")))))
  cat("\n\n")

  cat("Columns:\n")
  cat(paste(names(x), collapse=", "), "\n")

  temp <- x[1:15, names(x) %in% c("ID", "NTIME", "TIME", "ANALYTE", "EVID",
                                   "CMT", "AMT", "DOSE", "DV")] %>%
    as.data.frame()

  temp <- temp %>%
    df.to.string()
  cat(paste0("\nFirst rows of NIF data (selected columns):\n", temp))
  invisible(x)
}


#' Subjects within a NIF object
#'
#' @param obj A NIF object
#' @import dplyr
#' @return A data frame of all ID - USUBJID pairs in the data set.
#' @export
subjects <- function(obj) {
  obj %>%
    as.data.frame() %>%
    dplyr::distinct(ID, USUBJID) #%>%
    # dplyr::pull(USUBJID)
}

#' Get USUBJID of subject
#'
#' @param obj A NIF object.
#' @param id A subject ID in numeric form.
#'
#' @return The USUBJID as character.
#' @export
usubjid <- function(obj, id) {
  return(subjects(obj)[id, "USUBJID"])
}

#' Subjects with dose reduction
#'
#' @param obj A NIF data set object.
#' @param analyte The analyte of interest as string.
#'
#' @return The IDs.
#' @export
dose_red_sbs <- function(obj, analyte="") {
  if(analyte!="") {
    obj <- obj %>%  filter(ANALYTE %in% analyte)
  }

  obj %>%
    as.data.frame() %>%
    index_nif() %>%
    filter(EVID==1) %>%
    group_by(ID, ANALYTE) %>%
    mutate(initial_dose=DOSE[row_number()==1]) %>%
    filter(DOSE < initial_dose & DOSE != 0) %>%
    ungroup() %>%
    distinct(ID) %>%
    pull(ID)
}

#' Identify subjects with rich sampling
#'
#' @param obj The NIF dataset.
#' @param analyte The analyte. If the analyte is NA, the most likely will be
#'   selected.
#' @param max_time The end of the target interval across which the number of
#'   samples is determined. If NA, the full treatment interval is selected.
#' @param n The sample number cut-off.
#'
#' @return A list of IDs in numeric format.
#' @export
rich_sampling_sbs <- function(obj, analyte=NA, max_time=NA, n=4) {
  if(is.na(analyte)) {
    analyte <- guess_analyte(obj)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID==0, ANALYTE==analyte) %>%
    group_by(ID) %>%
    mutate(end_rich=case_when(is.na(max_time)~max(TIME), .default=max_time)) %>%
    ungroup() %>%
    filter(TIME < end_rich) %>%
    group_by(ID, USUBJID) %>%
    summarize(n_obs=n(), .groups="drop") %>%
    filter(n_obs>n) %>%
    pull(ID)
}

#' Studies within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A character vector of all STUDYIDs in the data set.
#' @export
studies <- function(obj) {
  obj %>%
    dplyr::distinct(STUDYID) %>%
    dplyr::pull(STUDYID)
}


#' Doses within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A number vector of all doses (AMT) in the data set.
#' @export
doses <- function(obj){
  obj %>%
    dplyr::filter(AMT!=0) %>%
    dplyr::distinct(AMT) %>%
    dplyr::arrange(as.numeric(AMT)) %>%
    dplyr::pull(AMT)
}


#' Dose levels
#'
#' In studies with multiple administrations, dose modifications may occur. The
#' `dose_levels()` function only summarizes the initial (i.e., the first)
#' administrations.
#'
#' @param obj A nif object.
#' @param grouping Further fields to be included (and to be grouped by) in the
#'   output.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' dose_levels(examplinib_fe_nif)
#' dose_levels(examplinib_fe_nif, grouping="SEX")
#' dose_levels(examplinib_fe_nif, grouping=c("SEX", "FASTED"))
dose_levels <- function(obj, grouping=NULL) {
  obj %>%
    filter(METABOLITE==FALSE) %>%
    filter(PARENT != "", !is.na(DOSE), AMT != 0) %>%
    group_by(ID, ANALYTE, across(any_of(grouping))) %>%
    arrange(ID, TIME) %>%
    filter(TIME==min(TIME)) %>%
    select(ID, ANALYTE, DOSE, any_of(grouping)) %>%
    pivot_wider(names_from="ANALYTE", values_from="DOSE", values_fill = 0) %>%
    group_by(across(c(-ID))) %>%
    summarize(N=n()) %>%
    as.data.frame()
}


#' Analytes within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A character vector of all analytes in the data set.
#' @export
analytes <- function(obj){
  obj %>%
    dplyr::distinct(ANALYTE) %>%
    dplyr::pull(ANALYTE)
}


#' Implementation of the head function
#'
#' @param n Number of rows to return.
#' @param obj A nif object
#'
#' @import dplyr
#' @return None
#' @import utils
#' @export
head <- function(obj, n=6) {
  obj %>%
    as.data.frame() %>%
    utils::head(n=n)
}


#' Export a nif object as csv file
#'
#' @param obj A nif object.
#' @param filename The filename for the exported file.
#' @import dplyr
#' @export
write_csv.nif <- function(obj, filename){
  temp <- obj %>%
    dplyr::mutate(across(c("TIME", "LNDV"), round, 3))
  write.csv(temp, filename, quote=F, row.names=F)
}



#' Write as space-delimited, fixed-width file as required by NONMEM
#'
#' @param obj The NIF data set.
#' @param fields The fields to export.
#' @param filename The filename as string.
#'
#' @importFrom gdata write.fwf
#' @export
write_nif <- function(obj, filename, fields=NULL) {
  if(is.null(fields)) {fields=standard_nif_fields}
  temp <- obj %>%
    compress_nif(fields) %>%
    as.data.frame() %>%
    dplyr::mutate_at(c("TIME", "DV", "LNDV"), signif, 4) %>%
    mutate_at(.vars=vars(RATE, DV, LNDV),
              .funs=function(x){case_when(is.na(x)~".",
                                          .default=as.character(x))}) %>%
    mutate_all(as.character)

  temp <- rbind(colnames(temp), temp)

  temp %>%
    write.fwf(file=filename, colnames=FALSE)

}


#' Standard nif fields
#'
#' @return A character vector of the standard NIF fields
#' @export
standard_nif_fields <- c("REF", "STUDYID", "ID", "USUBJID", "NTIME", "TIME",
                         "ANALYTE", "AMT", "RATE", "DV", "LNDV", "CMT", "EVID",
                         "DOSE", "AGE", "SEX", "RACE", "HEIGHT", "WEIGHT",
                         "ACTARMCD", "ANALYTE", "PARENT", "METABOLITE", "TRTDY")


# carry_forward_fields <- function(obj, fields=standard_nif_fields) {
#   obj %>%
#     as.data.frame() %>%
#     dplyr::group_by(ID, ANALYTE) %>%
#     tidyr::fill(all_of(fields), .direction="downup") %>%
#     new_nif() %>%
#     as.data.frame() %>%
#     head()
# }


#' Index dosing invervals
#'
#' This function adds a column `DI` that indicates the dosing interval. All
#' baseline observations before the first dosing interval get assigned to the
#' first dosing interval, too.
#' In addition to `DI`, the function also calls `index_nif()`, thus creating
#' the field `REF` as a side effect.
#'
#' @param obj The NIF data set.
#'
#' @return A new NIF data set.
#' @export
index_dosing_interval <- function(obj){
  obj <- obj %>%
    index_nif() %>%
    select(-any_of("DI"))

  di <- obj %>%
    as.data.frame() %>%
    filter(EVID==1) %>%
    # group_by(ID, ANALYTE) %>%
    group_by(ID, PARENT) %>%
    arrange(TIME) %>%
    mutate(DI=row_number()) %>%
    ungroup() %>%
    select(REF, DI) %>%
    as.data.frame()

  obj %>%
    as.data.frame() %>%
    left_join(di, by="REF") %>%
    # group_by(ID, ANALYTE) %>%
    group_by(ID) %>%
    arrange(REF) %>%
    fill(DI, .direction="down") %>%
    as.data.frame() %>%
    # all baseline before the first administration gets assigned to the first
    #   dosing interval, too:
    fill(DI, .direction="up") %>%
    ungroup() %>%
    new_nif()
}


#' Number of observations per dosing interval
#'
#' This function returns the number of observations by subject and analyte per
#' dosing interval.
#'
#' @param obj The NIF data set.
#' @param analyte The analyte to filter for as character. Optional.
#'
#' @return A data frame.
#' @export
#' @examples
#' n_observations_per_dosing_interval(examplinib_fe_nif)
#'
n_observations_per_dosing_interval <- function(obj, analyte=NULL) {
  if(!is.null(analyte)) {
    obj <- obj %>%
      filter(ANALYTE %in% analyte)
  }

  obj %>%
    index_dosing_interval() %>%
    group_by(across(any_of(c("ID", "USUBJID", "ANALYTE", "PARENT", "DI")))) %>%
    summarize(N=sum(EVID==0), .groups="drop") %>%
    as.data.frame()
}


#' Number of administrations per subject
#'
#' @param obj A NIF data set.
#'
#' @return A data frame.
#' @export
#' @examples
#' n_administrations(examplinib_fe_nif)
#'
n_administrations <- function(obj) {
  obj %>%
    index_dosing_interval() %>%
    group_by(across(any_of(c("ID", "USUBJID", "ANALYTE", "PARENT")))) %>%
    summarize(N=max(DI), .groups="drop") %>%
    as.data.frame()
}



#' Maximal administration time
#'
#' This function returns the time in hours of the last administration within
#' the data set.
#'
#' @param obj The NIF data set
#' @param analytes The analyte or analytes to filter for.
#'
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_admin_time(examplinib_fe_nif)
#'
max_admin_time <- function(obj, analytes=NULL) {
  if(!is.null(analytes)) {
    obj <- obj %>%
      filter(ANALYTE %in% analytes)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID==1) %>%
    pull(TIME) %>%
    max()
}

#' Maximal ovservation time
#'
#' This function returns the time in hours of the last observation within the
#' data set.
#'
#' @param obj The NIF data set
#' @param analytes The analyte or analytes to filter for.
#'
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_observation_time(examplinib_fe_nif)
#'
max_observation_time <- function(obj, analytes=NULL) {
  if(!is.null(analytes)) {
    obj <- obj %>%
      filter(ANALYTE %in% analytes)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    pull(TIME) %>%
    max(na.rm=TRUE)
}

#' Guess the most likely meant analyte
#'
#' @param obj A NIF data set
#'
#' @return The ANALYTE as character
guess_analyte <- function(obj) {
  temp <- obj %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    group_by(ANALYTE, METABOLITE) %>%
    summarize(n=n(), .groups="drop") %>%
    arrange(METABOLITE, -n)
  (temp %>% pull(ANALYTE))[1]
}


# add_day <- function(obj) {
#   temp <- obj %>%
#     as.data.frame() %>%
#     filter(EVID==1) %>%
#     dplyr::group_by(ID) %>%
#     dplyr::mutate(FIRSADMINDATE=date(min(DTC))) %>%
#     dplyr::ungroup() %>%
#     # as.data.frame() %>%
#     distinct(ID, FIRSADMINTIME)
#
#   ret <- obj %>%
#     left_join(temp, by="ID") %>%
#
# }
