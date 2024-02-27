#' nif class constructor
#'
#' @param obj A data frame containing the actual NIF data or a sdtm object.
#' @import dplyr
#' @return A nif object from the input data set.
#' @export
new_nif <- function(obj) {
  if (class(obj)[1] == "sdtm") {
    temp <- make_nif(obj)
  } else {
    temp <- obj %>% as.data.frame()
    class(temp) <- c("nif", "data.frame")
  }
  comment(temp) <- paste0("created with nif ", packageVersion("nif"))
  return(temp)
}


#' print() implementation for nif objects
#'
#' @param x A nif object.
#' @param color Colored output.
#' @param ... Additional parameters
#'
#' @export
#' @noRd
print.nif <- function(x, color=FALSE, ...) {
  hline <- paste0(rep("\U2500", 8), collapse="")
  cat(paste0(hline, " NONMEM input file (NIF) object ", hline, "\n"))

  if(length(studies(x)) == 1) {
    cat(paste0(guess_analyte(x), " data from one study\n"))
  } else{
    cat(paste0(guess_analyte(x), " data from ", length(studies(x)),
               " studies\n"))
  }
  n_obs <- x %>%
    filter(.data$EVID == 0) %>%
    nrow()
  cat(paste(
    n_obs, "observations from",
    subjects(x) %>% nrow(), "subjects\n"
  ))

  if ("SEX" %in% names(x)) {
    n_sex <- x %>%
      dplyr::distinct(.data$USUBJID, .data$SEX) %>%
      dplyr::group_by(.data$SEX) %>%
      dplyr::summarize(n = n())

    n_males <- n_sex %>%
      dplyr::filter(.data$SEX == 0) %>%
      dplyr::pull(.data$n)
    if (length(n_males) == 0) {
      n_males <- 0
    }

    n_females <- n_sex %>%
      dplyr::filter(.data$SEX == 1) %>%
      dplyr::pull(.data$n)
    if (length(n_females) == 0) {
      n_females <- 0
    }

    cat(paste0(
      "Males: ", n_males, ", females: ", n_females, " (",
      round(n_females / (n_males + n_females) * 100, 1), "%)\n\n"
    ))
  }

  cat("Columns:\n")
  cat(paste(names(x), collapse = ", "), "\n")

  temp <- x %>%
    as.data.frame() %>%
    select(any_of(c(
      "ID", "NTIME", "TIME", "TAD", "ANALYTE",
      "EVID",  "CMT", "AMT", "DOSE", "DV"
    ))) %>%
    head(10)

  temp <- temp %>%
    # mutate(across(where(is.numeric), round, 3)) %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    df_to_string(color = color)
  cat(paste0("\nNIF data (selected columns):\n", temp, "\n"))

  footer <- paste0(positive_or_zero(nrow(x)-10), " more rows")
  if(color == TRUE) {
    cat(paste0("\u001b[38;5;248m", footer, "\u001b[0m"))
  } else {
    cat(footer)
  }
  invisible(x)
}


#' Subject information
#'
#' This function summarizes baseline information for a subject or a list of
#' subjects, including sex, age, weight, height, BMI, ACTARMCD, analytes, IMPs
#' and baseline covariates as available.
#' @details
#' The output is an object of the class 'subject_info' which is a wrapper for
#' the named list of the above.
#' The field `administrations` is not printed automatically but can be accessed
#' as list item (see example).
#' @param obj A NIF object.
#' @param id The USUBJID or ID.
#' @export
#' @noRd
#' @examples
#' subject_info(examplinib_poc_nif, 1)
#' unclass(subject_info(examplinib_poc_nif, 1))
#' subject_info(examplinib_poc_nif, 1)$administrations
subject_info.nif <- function(obj, id) {
  temp <- obj %>%
    as.data.frame() %>%
    filter(.data$ID %in% id | .data$USUBJID %in% id) %>%
    filter(!is.na(.data$DOSE))

  out <- temp %>%
    select(any_of(
      c("USUBJID", "ID", "SEX", "AGE", "RACE", "WEIGHT", "HEIGHT", "BMI",
        "ACTARMCD", "PART", "COHORT")
    ), starts_with("BL_")) %>%
    distinct_all() %>%
    as.list()

  out$ANALYTE <- temp %>%
    distinct(.data$ANALYTE) %>%
    pull(.data$ANALYTE)

  out$IMP <- temp %>%
    filter(.data$PARENT != "") %>%
    distinct(.data$PARENT) %>%
    pull(.data$PARENT)

  out$administrations <- temp %>%
    add_trtdy() %>%
    filter(EVID == 1) %>%
    select(.data$USUBJID, .data$TIME, .data$ANALYTE, .data$DTC, .data$TRTDY) %>%
    arrange(.data$ANALYTE, .data$TIME) %>%
    select(.data$ANALYTE, .data$TIME, .data$TRTDY) %>%
    as.data.frame()

  class(out) <- c("subject_info", "data.frame")
  return(out)
}


#' Implementation of print for subject info
#'
#' @param x A data frame.
#' @param ... Optional further parameters.
#' @export
#' @noRd
print.subject_info <- function(x, ...) {
  temp <- lapply(x, function(i) {
    paste(i, collapse = ", ")
  }) %>%
    as.data.frame() %>%
    select(-any_of(c("administrations"))) %>%
    t()
  colnames(temp) <- NULL
  print(temp, quote = FALSE, col.names = FALSE)
  invisible(x)
}


#' Unique subjects within a data set
#'
#' @param obj The data set, either a `nif` or a `sdtm` object.
#' @return A data frame.
#' @export
#' @examples
#' subjects(examplinib_fe_nif)
#' subjects(examplinib_poc)
subjects <- function(obj) {
  UseMethod("subjects")
}


#' Unique subjects within a NIF object
#'
#' @param obj A NIF object.
#' @import dplyr
#' @return A data frame of all ID - USUBJID pairs in the data set.
#' @export
#' @examples
#' subjects(examplinib_fe_nif)
subjects.nif <- function(obj) {
  obj %>%
    as.data.frame() %>%
    {if(!"USUBJID" %in% names(.)) mutate(., USUBJID = NA) else .} %>%
    dplyr::select(any_of(c("ID", "USUBJID"))) %>%
    distinct()
}


#' Get the USUBJID of subject
#'
#' @param obj A NIF object.
#' @param id A subject ID in numeric form.
#'
#' @return The USUBJID as character.
#' @export
#' @examples
#' usubjid(examplinib_fe_nif)
usubjid <- function(obj, id) {
  return(subjects(obj)[id, "USUBJID"])
}


#' Parent compounds within a NIF object
#'
#' @param obj A NIF object.
#' @return The parent compunds as character.
#' @export
#' @examples
#' parents(examplinib_poc_nif)
#' parents(examplinib_poc_min_nif)
parents <- function(obj) {
  obj %>%
    ensure_parent() %>%
    as.data.frame() %>%
    distinct(PARENT) %>%
    filter(PARENT != "") %>%
    pull(PARENT)
}


#' Subjects with dose reduction
#'
#' @param obj A NIF object object.
#' @param analyte The analyte of interest as string. considers all analytes if
#'   analyte is NULL (default).
#' @return The IDs.
#' @export
#' @examples
#' dose_red_sbs(examplinib_poc_nif)
dose_red_sbs <- function(obj, analyte = NULL) {
  if(!"ANALYTE" %in% names(obj)) {
    obj <- obj %>% mutate(ANALYTE=CMT)
  }

  if (!is.null(analyte)) {
    obj <- obj %>% filter(ANALYTE %in% analyte)
  }

  obj %>%
    as.data.frame() %>%
    index_nif() %>%
    filter(EVID == 1) %>%
    group_by(ID, CMT) %>%
    mutate(initial_dose = AMT[row_number() == 1]) %>%
    filter(AMT < initial_dose & AMT != 0) %>%
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
#' @return A list of IDs in numeric format.
#' @export
#' @examples
#' rich_sampling_sbs(examplinib_poc_nif, n=6)
rich_sampling_sbs <- function(obj, analyte = NA, max_time = NA, n = 4) {
  if (is.na(analyte)) {
    analyte <- guess_analyte(obj)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID == 0, ANALYTE == analyte) %>%
    group_by(.data$ID) %>%
    mutate(end_rich = case_when(is.na(max_time) ~ max(.data$TIME),
      .default = max_time
    )) %>%
    ungroup() %>%
    filter(.data$TIME < .data$end_rich) %>%
    group_by(.data$ID, .data$USUBJID) %>%
    summarize(n_obs = n(), .groups = "drop") %>%
    filter(.data$n_obs > n) %>%
    pull(.data$ID)
}


#' Studies within a nif object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A character vector of all STUDYIDs in the data set.
#' @export
#' @examples
#' studies(examplinib_poc_nif)
studies <- function(obj) {
  if("STUDYID" %in% names(obj)){
    return(
      obj %>%
        dplyr::distinct(.data$STUDYID) %>%
        dplyr::pull(.data$STUDYID)
    )
  } else {
    return(NA)
  }
}


#' Ensure that the ANALYTE field is present
#'
#' If 'ANALYTE' is not in the column names, the field is created based on the
#' compartment (CMT).
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @export
#' @examples
#' head(ensure_analyte(examplinib_poc_nif))
#' head(ensure_analyte(examplinib_poc_min_nif))
ensure_analyte <- function(obj) {
  obj %>%
    {if(!"ANALYTE" %in% names(obj))
      mutate(., ANALYTE = "default") else .}
}


#' Ensure that the DOSE field is present
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @export
#' @examples
#' ensure_dose(examplinib_poc_min_nif)
ensure_dose <- function(obj) {
  obj %>%
    {if(!"DOSE" %in% names(obj))
      index_nif(.) %>%
        mutate(DOSE = case_when(.data$EVID == 1 ~ AMT, .default = NA)) %>%
        fill(DOSE, .direction = "downup")
      else .
    }
}

#' Ensure that the PARENT field is present in a NIF file.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @export
#' @examples
#' head(ensure_parent(examplinib_poc_nif))
#' head(ensure_parent(examplinib_poc_min_nif))
ensure_parent <- function(obj) {
  obj <- obj %>%
    {if(!"PARENT" %in% names(obj))
      mutate(., PARENT = "default") else .}
}


#' Ensure that the METABOLITE field is present in a NIF file.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
#' @export
#' @examples
#' head(ensure_metabolite(examplinib_poc_nif))
#' head(ensure_metabolite(examplinib_poc_min_nif))
ensure_metabolite <- function(obj) {
  obj <- obj %>%
    {if(!"METABOLITE" %in% names(obj))
      mutate(., METABOLITE = FALSE) else .}
}


#' Ensure that TAD is present in the NIF object
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @export
#' @examples
#' head(ensure_tad(examplinib_poc_nif))
#' head(ensure_tad(examplinib_poc_min_nif))
ensure_tad <- function(obj) {
  obj <- obj %>%
    {if(!"TAD" %in% names(obj))
      add_tad(.) else .}
}


#' Ensure that NTIME is present in the NIF object
#'
#' Experimental, do not use in production! Uses assumptions about the sampling
#' time points.
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
ensure_ntime <- function(obj) {
  # if(!"NTIME" %in% names(obj)) {
  #   late_labels <- seq(24, ceiling(max_observation_time(obj)/24)*24, 24)
  #   late_breaks <- late_labels + 12
  #
  #   obj %>%
  #     nif::ensure_parent() %>%
  #     nif::add_tad() %>%
  #     as.data.frame() %>%
  #     mutate(NTIME = as.numeric(as.character(cut(TAD,
  #       # breaks=c(-Inf, 0, 0.25,  .5,   1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24),
  #       breaks=  c(c(-Inf, 0, 0.125, .375, .75, 1.25, 1.75, 2.5, 3.5, 5,
  #                  7, 9, 11, 14, 20, 24), late_breaks),
  #       labels=c(c(0, 0, 0.25, .5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 16, 24),
  #                late_labels)))))
  # } else {
  #   return(obj)
  # }
}


#' Doses within a NIF object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A number vector of all doses (AMT) in the data set.
#' @export
#' @examples
#' doses(examplinib_poc_nif)
doses <- function(obj) {
  obj %>%
    dplyr::filter(AMT != 0) %>%
    dplyr::distinct(AMT) %>%
    dplyr::arrange(as.numeric(AMT)) %>%
    dplyr::pull(AMT)
}


#' Dose levels within a NIF object
#'
#' This function summarizes the doses in the individual first administration by
#' subject and drug, and the number of subjectes treated at this dose level.
#' Subsequent dose modifications are ignored.
#'
#' @param obj A rich or minimal NIF object.
#' @param group Further fields to be included (and to be grouped by) in the
#'   output.
#' @param cmt The compartment (CMT) as numeric.
#' @return A data frame.
#' @export
#' @examples
#' dose_levels(examplinib_fe_nif)
#' dose_levels(examplinib_fe_nif, group = "SEX")
#' dose_levels(examplinib_fe_nif, group = c("SEX", "FASTED"))
#' dose_levels(examplinib_sad_min_nif)
dose_levels <- function(obj, cmt = 1, group = NULL) {
  obj %>%
    ensure_analyte() %>%
    filter(AMT != 0) %>%
    group_by(ID, ANALYTE, across(any_of(group))) %>%
    arrange(ID, TIME) %>%
    filter(TIME == min(TIME)) %>%
    select(ID, ANALYTE, AMT, any_of(group)) %>%
    pivot_wider(
      names_from = "ANALYTE",
      values_from = "AMT", values_fill = 0) %>%
    group_by(across(c(-ID))) %>%
    summarize(N = n()) %>%
    as.data.frame()
}


#' Analytes within a NIF object
#'
#' All analytes found in the NIF object. If the field 'ANALYTE' is not present,
#' The analyte title is derived from the compartment.
#' @param obj A NIF object
#' @import dplyr
#' @return Character.
#' @export
#' @examples
#' analytes(examplinib_fe_nif)
#' analytes(examplinib_poc_nif)
#' analytes(examplinib_poc_min_nif)
analytes <- function(obj) {
  ensure_analyte(obj) %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    distinct(ANALYTE) %>%
    pull(ANALYTE)
}


#' Analytes and compartments within a NIF object
#'
#' @param obj A NIF object.
#' @return A data frame.
#' @export
#' @examples
#' analytes_cmts(examplinib_poc_nif)
#' analytes_cmts(examplinib_poc_min_nif)
analytes_cmts <- function(obj) {
  obj %>%
    ensure_analyte() %>%
    filter(EVID==0) %>%
    distinct(across(any_of(c("ANALYTE", "CMT")))) %>%
    as.data.frame()
}


#' Analyte - compartment mapping
#'
#' @param obj A NIF object
#' @return A data frame
#' @export
#' @examples
#' cmt_mapping(examplinib_poc_nif)
#' cmt_mapping(examplinib_poc_min_nif)
cmt_mapping <- function(obj) {
  obj %>%
    ensure_analyte() %>%
    # {if(!"ANALYTE" %in% names(obj)) mutate(., ANALYTE = NA) else .} %>%
    filter(EVID==0) %>%
    distinct(across(any_of(c("ANALYTE", "CMT")))) %>%
    as.data.frame()
}


#' Return the first lines of a nif object
#'
#' @param x A nif object.
#' @param ... Further arguments.
#'
#' @import dplyr
#' @return A data frame.
#' @import utils
#' @export
#' @noRd
head.nif <- function(x, ...) {
  x <- x %>%
    as.data.frame()
  NextMethod("head")
}


#' Export a NIF object as csv file
#'
#' @param obj A NIF object.
#' @param filename The filename for the exported file.
#' @import dplyr
#' @export
write_csv.nif <- function(obj, filename) {
  temp <- obj %>%
    as.data.frame() %>%
    dplyr::mutate(across(c("TIME", "TAD", "DV", "LNDV"), round, 3))
  write.csv(temp, filename, quote = FALSE, row.names = FALSE)
}


#' Write as space-delimited, fixed-width file as required by NONMEM or a
#' character-separated file
#'
#' All numeric fields are reduced to 4 significant places. All fields are
#' converted to character, and NA-values are converted to '.'.
#' @param obj The NIF object.
#' @param fields The fields to export. If NULL (default), all fields will be
#' exported.
#' @param filename The filename as string. If not filename is specified, the
#' file is printed only.
#' @param sep The separating character, e.g. ',' or ';'. If NULL (default), the
#' output has a space-separated, fixed-width format.
#' @importFrom gdata write.fwf
#' @export
#' @examples
#' write_nif(examplinib_fe_nif)
write_nif <- function(obj, filename = NULL, fields = NULL, sep=NULL) {
  double_fields <- c("NTIME", "TIME", "TAD", "AMT", "RATE", "DV", "LNDV", "DOSE",
                     "AGE", "HEIGHT", "WEIGHT", "BMI")
  bl_fields <- names(obj)[str_detect(names(obj), "^BL_")]
  int_fields <- c("REF", "ID", "MDV", "CMT", "EVID", "SEX", "TRTDY")
  num_fields <- c(double_fields, int_fields, bl_fields)

  if (is.null(fields)) {
    fields <- names(obj)
  }

  temp <- obj %>%
    as.data.frame() %>%
    mutate(across(any_of(num_fields), signif, 4))

  if("METABOLITE" %in% names(obj)) {
    temp <- temp %>%
      mutate(METABOLITE = case_when(
        is.na(METABOLITE) ~ FALSE,
        .default = METABOLITE))}

  temp <- temp %>%
    mutate(across(any_of(num_fields),
                function(x) {case_when(
                  is.na(x) ~ ".",
                  .default = as.character(x))})) %>%
  mutate_all(as.character)

  if (is.null(filename)) {
    print(temp, row.names = FALSE, col.names = FALSE)
  } else {
    if(is.null(sep)){
      temp <- rbind(colnames(temp), temp)
      write.fwf(temp, file = filename, colnames = FALSE)
    } else {
      write.table(temp, file = filename, row.names = FALSE,
                  sep = sep, dec = ".", quote = FALSE)
    }
  }
}


#' Write as comma-separated file, complying with the format used by Monolix
#'
#' @param obj The NIF object.
#' @param filename The filename as string. If not filename is specified, the
#' file is printed only.
#' @param fields The fields to export. If NULL (default), all fields will be
#' exported.
#' @return Nothing.
#' @export
#' @examples
#' write_monolix(examplinib_fe_nif)
write_monolix <- function(obj, filename = NULL, fields = NULL) {
  double_fields <- c("NTIME", "TIME", "TAD", "AMT", "RATE", "DV", "LNDV", "DOSE",
                     "AGE", "HEIGHT", "WEIGHT", "BMI")
  bl_fields <- names(obj)[str_detect(names(obj), "^BL_")]
  int_fields <- c("REF", "ID", "MDV", "CMT", "EVID", "SEX", "TRTDY")
  num_fields <- c(double_fields, int_fields, bl_fields)

  if (is.null(fields)) {
    fields <- names(obj)
  }

  temp <- obj %>%
    as.data.frame() %>%
    mutate(across(any_of(num_fields), signif, 4)) %>%
    mutate(ADM = case_when(AMT != 0 ~ "1", .default = ".")) %>%
    mutate(YTYPE = case_when(ADM == "1" ~ ".",
                             .default=as.character(CMT-1)))

  if("METABOLITE" %in% names(obj)) {
    temp <- temp %>%
      mutate(METABOLITE = case_when(
        is.na(METABOLITE) ~ FALSE,
        .default = METABOLITE))}

  temp <- temp %>%
    mutate(across(any_of(num_fields),
                  function(x) {case_when(
                    is.na(x) ~ ".",
                    .default = as.character(x))})) %>%
    mutate_all(as.character) %>%
    mutate(Y = DV)

  if (is.null(filename)) {
    print(temp, row.names = FALSE, col.names = FALSE)
  } else {
      write.table(temp, file = filename, row.names = FALSE,
                  sep = ",", dec = ".", quote = FALSE)
  }
}


#' Standard nif fields
#'
#' @return A character vector of the standard NIF fields
#' @export
standard_nif_fields <- c(
  "REF", "STUDYID", "ID", "USUBJID", "NTIME", "TIME", "TAD", "TAFD", "ANALYTE",
  "AMT", "RATE", "DV", "LNDV", "MDV", "CMT", "EVID", "DOSE", "AGE", "SEX",
  "RACE", "HEIGHT", "WEIGHT", "BMI", "ACTARMCD", "ANALYTE", "PARENT",
  "METABOLITE", "TRTDY", "DI", "PART", "COHORT", "FASTED", "DTC", "RICH_N"
)

fillable_nif_fields <- c(
  "SUBJID", "STUDYID", "AGE", "SEX", "RACE", "ETHNIC", "COUNTRY",
  "HEIGHT", "WEIGHT", "BMI", "ACTARMCD", "ARM", "PART", "COHORT", "FASTED",
  "IMPT_TIME"
)


#' Index dosing intervals
#'
#' This function adds a column `DI` that indicates the dosing interval. All
#' baseline observations before the first dosing interval get assigned to the
#' first dosing interval, too. In addition to `DI`, the function also calls
#' `index_nif()`, thus creating the field `REF` as a side effect.
#' @param obj The NIF object.
#' @return A new NIF object.
#' @export
#' @examples
#' head(index_dosing_interval(examplinib_fe_nif))
#' head(index_dosing_interval(examplinib_poc_nif))
#' head(index_dosing_interval(examplinib_poc_min_nif))
index_dosing_interval <- function(obj) {
  obj <- obj %>%
    ensure_parent() %>%
    index_nif() %>%
    select(-any_of("DI"))

  di <- obj %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    group_by(ID, PARENT) %>%
    arrange(TIME) %>%
    mutate(DI = row_number()) %>%
    ungroup() %>%
    select(REF, DI)

  obj %>%
    left_join(di, by = "REF") %>%
    group_by(ID, PARENT) %>%
    arrange(REF) %>%
    fill(DI, .direction = "downup") %>%
    ungroup() %>%
    new_nif()
}


#' Number of administrations per subject
#'
#' This function returns the number of administrations per `ID` and `PARENT`.
#'
#' @param obj A NIF object.
#'
#' @return A data frame.
#' @export
#' @examples
#' n_administrations(examplinib_poc_nif)
#' n_administrations(examplinib_poc_min_nif)
n_administrations <- function(obj) {
  obj %>%
    index_dosing_interval() %>%
    group_by(across(any_of(c("ID", "USUBJID", "PARENT")))) %>%
    summarize(N = max(DI), .groups = "drop") %>%
    as.data.frame()
}


#' Maximal administration time
#'
#' This function returns the time in hours of the last administration within
#' the data set.
#'
#' @param obj The NIF object
#' @param analyte The analyte or analytes to filter for.
#'
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_admin_time(examplinib_fe_nif)
#' max_admin_time(examplinib_poc_nif)
#' max_admin_time(examplinib_poc_min_nif)
#' max_admin_time(examplinib_poc_min_nif, analyte = "CMT2")
max_admin_time <- function(obj, analyte = NULL) {
  times <- obj %>%
    ensure_analyte() %>%
    {if(!is.null(analyte)) filter(., ANALYTE %in% analyte) else .} %>%
    filter(EVID == 1) %>%
    pull(TIME)

  if(length(times) == 0) {
    return(NA)
  } else {
    return(max(times, na.rm = TRUE))
  }
}


#' Maximal observation time
#'
#' This function returns the time in hours of the last observation relative to
#' the first observation within the data set.
#'
#' @param analyte The analyte as character. If `NULL` (default), all analytes
#'   are selected.
#' @param obj The NIF object
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_observation_time(examplinib_fe_nif)
#' max_observation_time(examplinib_poc_nif)
#' max_observation_time(examplinib_poc_min_nif)
#' max_observation_time(examplinib_poc_min_nif, analyte = "CMT4")
max_observation_time <- function(obj, analyte = NULL) {
  times <- obj %>%
    ensure_analyte() %>%
    {if(!is.null(analyte)) filter(., ANALYTE %in% analyte) else .} %>%
    filter(EVID == 0) %>%
    pull(TIME)

  if(length(times) == 0) {
    return(NA)
  } else {
    return(max(times, na.rm = TRUE))
  }
}


#' Guess the most likely analyte based on its prevalence in the NIF object
#'
#' @param obj A NIF object.
#' @return The analyte as character.
#' @export
#' @examples
#' guess_analyte(examplinib_poc_nif)
#' guess_analyte(examplinib_poc_min_nif)
guess_analyte <- function(obj) {
  temp <- obj %>%
    ensure_analyte() %>%
    ensure_metabolite() %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    group_by(ANALYTE, METABOLITE) %>%
    summarize(n = n(), .groups = "drop") %>%
    filter(METABOLITE == FALSE) %>%
    filter(n == max(n)) %>%
    arrange(ANALYTE)
  return(as.character(temp[1, "ANALYTE"]))
}


#' Add dose level (`DL`) column
#'
#' Dose level is defined as the starting dose. For data sets with single drug
#' administration, `DL`is a numerical value, for drug combinations, it is a
#' character value specifying the `PARENT` and dose level for the individual
#' components.
#' @param obj A NIF dataset.
#' @return A NIF dataset.
#' @export
#' @examples
#' head(add_dose_level(examplinib_sad_nif))
#' head(add_dose_level(examplinib_sad_min_nif))
add_dose_level <- function(obj) {
  temp <- obj %>%
    ensure_dose() %>%
    ensure_analyte() %>%
    ensure_parent() %>%
    ensure_metabolite() %>%
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


#' Add time-after-dose (TAD) field
#'
#' @param nif A NIF object.
#'
#' @return The updated NIF object
#' @export
#' @examples
#' add_tad(examplinib_poc_nif)
#' add_tad(examplinib_poc_min_nif)
add_tad <- function(nif) {
  nif %>%
    ensure_parent() %>%
    as.data.frame() %>%
    mutate(admin_time = case_when(
      EVID == 1 ~ TIME)) %>%
    arrange(TIME) %>%
    group_by(ID, PARENT) %>%
    fill(admin_time, .direction = "down") %>%
    ungroup() %>%
    mutate(TAD = TIME - admin_time) %>%
    select(-admin_time) %>%
    new_nif() %>%
    index_nif()
}


#' Add time after first dose column
#'
#' @param nif A NIF object.
#' @return A NIF object.
#' @export
#' @import assertr
#' @examples
#' add_tafd(examplinib_poc_nif)
add_tafd <- function(nif) {
  nif %>%
    assertr::verify(has_all_names("ID", "TIME", "EVID")) %>%
    ensure_parent() %>%
    arrange(ID, PARENT, TIME) %>%
    group_by(ID, PARENT) %>%
    mutate(first_admin = min(TIME[EVID == 1])) %>%
    mutate(TAFD = TIME-.data$first_admin) %>%
    mutate(TAFD = case_when(TAFD < 0 ~ 0, .default = .data$TAFD)) %>%
    select(-.data$first_admin) %>%
    new_nif()
}


#' Add treatment day ('TRTDY') column
#'
#' @param obj The NIF object as data frame.
#' @return The updated NIF object as data frame.
#' @import assertr
#' @export
#' @examples
#' head(add_trtdy(examplinib_poc_nif))
add_trtdy <- function(obj) {
  obj %>%
    assertr::verify(has_all_names("ID", "DTC", "EVID")) %>%
    assertr::verify(is.POSIXct(.data$DTC)) %>%
    dplyr::group_by(.data$ID) %>%
    dplyr::mutate(FIRSTTRTDTC = min(.data$DTC[.data$EVID == 1],
                                    na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    mutate(TRTDY = interval(date(.data$FIRSTTRTDTC),
                            date(.data$DTC)) / days(1)) %>%
    mutate(TRTDY = case_when(.data$TRTDY < 0 ~ .data$TRTDY,
                             .default = .data$TRTDY + 1)) %>%
    select(-.data$FIRSTTRTDTC) %>%
    new_nif()
}


#' Add baseline creatinine clearance field.
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#' Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
#' @return A NIF object.
#' @seealso [egfr_mdrd()]
#' @seealso [egfr_cg()]
#' @seealso [egfr_raynaud()]
#' @export
#' @examples
#' head(add_bl_crcl(examplinib_poc_nif))
add_bl_crcl <- function(obj, method = egfr_cg) {
  if ("BL_CREAT" %in% colnames(obj)) {
    obj %>%
      as.data.frame() %>%
      mutate(BL_CRCL = method(BL_CREAT, AGE, SEX, RACE, WEIGHT,
        molar = TRUE
      )) %>%
      new_nif()
  } else {
    obj %>%
      mutate(BL_CRCL = as.numeric(NA))
  }
}


#' Add baseline renal function class
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#' Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
#' @return A NIF object.
#' @export
#' @examples
#' as.data.frame(add_bl_renal(examplinib_poc_nif))
add_bl_renal <- function(obj, method = egfr_cg) {
  obj %>%
    add_bl_crcl(method = method) %>%
    mutate(BL_RENAL = as.character(
      cut(BL_CRCL,
        breaks = c(0, 30, 60, 90, Inf),
        labels = c("severe", "moderate", "mild", "normal")
      )
    )) %>%
    mutate(BL_RENAL = factor(BL_RENAL,
      levels = c("", "normal", "mild", "moderate", "severe")
    ))
}


#' Add baseline and change from baseline fields
#'
#' @details
#' Output fields:
#' * `DVBL` Baseline value for the dependent variable DV.
#' * `DVCFB` Change from baseline for the dependent variable DV.
#' @details The Baseline is calculated as the median of the DV for all times
#' lower or equal to zero.
#' @param obj A NIF object.
#' @param summary_function The function to derive the baseline. This function is
#'   applied over the DV values at TIME less than or equal to zero. The default
#'   is `median`. Alternatively, `mean`, `min` or `max` can be considered.
#' @return A NIF object
#' @export
#' @examples
#' head(add_cfb(examplinib_poc_nif))
#' head(add_cfb(examplinib_poc_min_nif))
add_cfb <- function(obj, summary_function = median) {
  obj %>%
    ensure_analyte() %>%
    as.data.frame() %>%
    group_by(ID, ANALYTE) %>%
    mutate(DVBL = summary_function(DV[TIME <= 0], na.rm = TRUE)) %>%
    mutate(DVCFB = DV - DVBL) %>%
    new_nif()
}


#' Add the number of observations per dosing interval
#'
#' This function adds a variable, `OPDI`, to the NIF object that indicates the
#' number of observations per analyte and dosing interval. This field can be
#' helpful to identify dosing intervals across which rich sampling was
#' conducted.
#' @param obj A NIF object.
#' @return Result as NIF object.
#' @export
#' @examples
#' add_obs_per_dosing_interval(examplinib_poc_nif)
#' add_obs_per_dosing_interval(examplinib_poc_min_nif)
add_obs_per_dosing_interval <- function(obj) {
  obj %>%
    index_nif() %>%
    select(-any_of("DI")) %>%
    index_dosing_interval() %>%
    group_by(across(any_of(c("ID", "USUBJID", "ANALYTE", "PARENT", "DI")))) %>%
    mutate(OPDI = sum(EVID == 0))
}


#' Identify and index rich PK sampling intervals
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Currently experimental. Don't use in production!
#'
#' Adds the fields `DI` (dosing interval per analyte), `RICHINT` (rich sampling
#' interval), and `RICH_N` (index of the rich sampling interval by analyte).
#' @details
#' This function identifies rich sampling intervals by the number of
#' observations that follow an administration. A number of `min_n` or more
#' observations before the next administration is interpreted as a rich sampling
#' interval and the corresponding observations are flagged with `RICHINT` ==
#' TRUE. The index of the rich sampling intervals per subject and analyte is
#' reported in the `RICH_N` field.
#' @param obj The NIF object.
#' @param min_n The minimum number of PK samples per analyte to qualify as rich
#'   sampling.
#' @param analyte The analyte as character. If `NA` (default), the most likely
#' will be selected automatically.
#'
#' @return A new NIF object.
#' @export
index_rich_sampling_intervals <- function(obj, analyte = NA, min_n = 4) {
  if (is.na(analyte)) {
    analyte <- guess_analyte(obj)
  }

  temp <- obj %>%
    as.data.frame() %>%
    filter(ANALYTE %in% analyte) %>%
    index_nif() %>%
    index_dosing_interval() %>%
    add_obs_per_dosing_interval() %>%
    mutate(RICHINT_TEMP = (OPDI >= min_n)) %>%

    # add last observation before administration to rich interval
    group_by(ID, ANALYTE) %>%
    mutate(LEAD = lead(RICHINT_TEMP)) %>%
    mutate(RICHINT = RICHINT_TEMP | (LEAD & EVID == 0)) %>%
    fill(RICHINT, .direction = "down") %>%
    ungroup() %>%
    select(-c("RICHINT_TEMP", "LEAD")) %>%
    group_by(ID, ANALYTE) %>%

    # `FLAG` marks the start of all rich dosing intervals and the start of the
    #  respective following non-rich dosing intervals
    mutate(FLAG = (RICHINT != lag(RICHINT) | row_number() == 1)) %>%
    ungroup() %>%

    # `rich_start` marks the start of a rich sampling interval
    mutate(rich_start = FLAG == TRUE & RICHINT == TRUE) %>%
    arrange(ID, ANALYTE, -rich_start) %>%
    group_by(ID, ANALYTE) %>%
    mutate(RICH_N = case_when(rich_start == TRUE ~ row_number(),
                              .default = NA)) %>%
    ungroup() %>%
    arrange(REF) %>%
    group_by(RICHINT) %>%
    fill(RICH_N, .direction = "down") %>%
    ungroup() %>%
    select(-c("rich_start", "FLAG")) %>%
    as.data.frame()

  return(new_nif(temp))
}


#' Filter NIF object for specific USUBJID
#'
#' @param obj The NIF object.
#' @param usubjid The USUBJID as character
#' @return A NIF object.
#' @export
#' @noRd
#' @examples
#' filter_subject(examplinib_poc_nif, subjects(examplinib_poc_nif)[1, "USUBJID"])
filter_subject.nif <- function(obj, usubjid) {
  obj %>%
    filter(.data$USUBJID %in% usubjid)
}













