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
  return(temp)
}


#' print() implementation for nif objects
#'
#' @param x A nif object.
#' @param ... Additional parameters
#' @export
#' @noRd
print.nif <- function(x, ...) {
  cat(paste(
    "NONMEM input file (NIF) data set with data from",
    length(studies(x)),
    "studies\n"
  ))
  n_obs <- x %>%
    filter(EVID == 0) %>%
    nrow()
  cat(paste(
    n_obs, "observations from",
    subjects(x) %>% nrow(), "subjects\n"
  ))

  if ("SEX" %in% names(x)) {
    n_sex <- x %>%
      dplyr::distinct(USUBJID, SEX) %>%
      dplyr::group_by(SEX) %>%
      dplyr::summarize(n = n())

    n_males <- n_sex %>%
      dplyr::filter(SEX == 0) %>%
      dplyr::pull(n)
    if (length(n_males) == 0) {
      n_males <- 0
    }

    n_females <- n_sex %>%
      dplyr::filter(SEX == 1) %>%
      dplyr::pull(n)
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
    head(15)

  temp <- temp %>%
    df_to_string()
  cat(paste0("\nFirst rows of NIF data (selected columns):\n", temp))
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
    filter(ID %in% id | USUBJID %in% id) %>%
    filter(!is.na(DOSE))

  out <- temp %>%
    select(any_of(
      c(
        "USUBJID", "ID", "SEX", "AGE", "RACE", "WEIGHT", "HEIGHT", "BMI",
        "ACTARMCD", "PART", "COHORT"
      )
    ), starts_with("BL_")) %>%
    distinct_all() %>%
    as.list()

  out$ANALYTE <- temp %>%
    distinct(ANALYTE) %>%
    pull(ANALYTE)

  out$IMP <- temp %>%
    filter(PARENT != "") %>%
    distinct(PARENT) %>%
    pull(PARENT)

  out$administrations <- temp %>%
    add_trtdy() %>%
    filter(EVID == 1) %>%
    select(USUBJID, TIME, ANALYTE, DTC, TRTDY) %>%
    arrange(ANALYTE, TIME) %>%
    select(ANALYTE, TIME, TRTDY) %>%
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
#' @noRd
#' @examples
#' subjects(examplinib_fe_nif)
subjects.nif <- function(obj) {
  obj %>%
    as.data.frame() %>%
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
parents <- function(obj) {
  if("PARENT" %in% names(obj)) {
    return(
      obj %>%
      as.data.frame() %>%
      distinct(PARENT) %>%
      filter(PARENT != "") %>%
      pull(PARENT))
  } else {
    return("UNKNOWN")
  }
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
    group_by(ID) %>%
    mutate(end_rich = case_when(is.na(max_time) ~ max(TIME),
      .default = max_time
    )) %>%
    ungroup() %>%
    filter(TIME < end_rich) %>%
    group_by(ID, USUBJID) %>%
    summarize(n_obs = n(), .groups = "drop") %>%
    filter(n_obs > n) %>%
    pull(ID)
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
        dplyr::distinct(STUDYID) %>%
        dplyr::pull(STUDYID)
    )
  } else {
    return(NA)
  }
}


#' Doses within a nif object
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
#' This function summarizes the doses in the individual first administration
#' by subject and drug, and the number of subjectes treated at this dose level.
#' Subsequent dose modifications are ignored.
#' @param obj A NIF object.
#' @param group Further fields to be included (and to be grouped by) in the
#'   output.
#' @return A data frame.
#' @export
#' @examples
#' dose_levels(examplinib_fe_nif)
#' dose_levels(examplinib_fe_nif, group = "SEX")
#' dose_levels(examplinib_fe_nif, group = c("SEX", "FASTED"))
dose_levels <- function(obj, cmt = 1, group = NULL) {
  # obj %>%
  #   filter(METABOLITE == FALSE) %>%
  #   filter(PARENT != "", !is.na(DOSE), AMT != 0) %>%
  #   group_by(ID, ANALYTE, across(any_of(group))) %>%
  #   arrange(ID, TIME) %>%
  #   filter(TIME == min(TIME)) %>%
  #   select(ID, ANALYTE, DOSE, any_of(group)) %>%
  #   pivot_wider(
  #     names_from = "ANALYTE",
  #     values_from = "DOSE", values_fill = 0) %>%
  #   group_by(across(c(-ID))) %>%
  #   summarize(N = n()) %>%
  #   as.data.frame()

  if("ANALYTE" %in% names(obj)) {
    obj %>%
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
  } else {
    obj %>%
      filter(CMT %in% cmt) %>%
      mutate(cmt_name = paste0("CMT", CMT)) %>%
      filter(AMT != 0) %>%
      group_by(ID, cmt_name, across(any_of("group"))) %>%
      arrange(ID, TIME) %>%
      filter(TIME == min(TIME)) %>%
      select(ID, cmt_name, AMT, any_of(group)) %>%
      pivot_wider(
        names_from = "cmt_name",
        values_from = "AMT", values_fill = 0) %>%
      group_by(across(c(-ID))) %>%
      summarize(N = n()) %>%
      as.data.frame()
  }
}


#' Analytes within a nif object
#'
#' If the field 'ANALYTE' is not present, 'CMT' is used instead.
#'
#' @param obj A nif object
#' @import dplyr
#' @return A character vector of all analytes in the data set.
#' @export
#' @examples
#' analytes(examplinib_fe_nif)
#' analytes(examplinib_poc_nif)
#'
analytes <- function(obj) {
  if("ANALYTE" %in% names(obj)) {
    obj %>%
      dplyr::distinct(ANALYTE) %>%
      dplyr::pull(ANALYTE)
  } else {
    as.character(unique(obj$CMT))
  }
}


#' Analytes and compartments within a NIF object
#'
#' @param obj A NIF object.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' analytes_cmts(examplinib_poc_nif)
analytes_cmts <- function(obj) {
  obj %>%
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


#' Export a nif object as csv file
#'
#' @param obj A nif object.
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
#' All numeric fields are reduced to 4 significant places. All fields are con-
#' verted to character, and NA-values are converted to '.'.
#'
#' @param obj The NIF object.
#' @param fields The fields to export. If NULL (default), all fields will be
#' exported.
#' @param filename The filename as string. If not filename is specified, the
#' file is printed only.
#' @param sep The separating character, e.g. ',' or ';'. If NULL (default), the
#' output has a space-separated, fixed-width format.
#'
#' @importFrom gdata write.fwf
#' @export
#' @examples
#' write_nif(examplinib_fe_nif)
#'
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
#'
#' @return Nothing.
#' @export
#'
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
  "REF", "STUDYID", "ID", "USUBJID", "NTIME", "TIME", "TAD",
  "ANALYTE", "AMT", "RATE", "DV", "LNDV", "MDV", "CMT",
  "EVID", "DOSE", "AGE", "SEX", "RACE", "HEIGHT",
  "WEIGHT", "BMI", "ACTARMCD", "ANALYTE", "PARENT",
  "METABOLITE", "TRTDY", "BL_CRCL", "PART", "COHORT",
  "FASTED", "DTC"
)


#' Index dosing invervals
#'
#' This function adds a column `DI` that indicates the dosing interval. All
#' baseline observations before the first dosing interval get assigned to the
#' first dosing interval, too.
#' In addition to `DI`, the function also calls `index_nif()`, thus creating
#' the field `REF` as a side effect.
#'
#' @param obj The NIF object.
#'
#' @return A new NIF object.
#' @export
#' @examples
#' as.data.frame(index_dosing_interval(examplinib_fe_nif))
#'
index_dosing_interval <- function(obj) {
  obj <- obj %>%
    index_nif() %>%
    select(-any_of("DI"))

  di <- obj %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    group_by(ID, PARENT) %>%
    arrange(TIME) %>%
    mutate(DI = row_number()) %>%
    ungroup() %>%
    select(REF, DI) %>%
    as.data.frame()

  obj %>%
    as.data.frame() %>%
    left_join(di, by = "REF") %>%
    group_by(ID) %>%
    arrange(REF) %>%
    fill(DI, .direction = "down") %>%
    as.data.frame() %>%
    # all baseline before the first administration gets assigned to the first
    #   dosing interval, too:
    fill(DI, .direction = "up") %>%
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
#'
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
#' @param analytes The analyte or analytes to filter for.
#'
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_admin_time(examplinib_fe_nif)
#'
max_admin_time <- function(obj, analytes = NULL) {
  if (!is.null(analytes)) {
    obj <- obj %>%
      filter(ANALYTE %in% analytes)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    pull(TIME) %>%
    max()
}


#' Maximal ovservation time
#'
#' This function returns the time in hours of the last observation relative to
#' the first observation within the data set.
#'
#' @param obj The NIF object
#' @param analytes The analyte or analytes to filter for.
#'
#' @return A scalar representing the time in hours.
#' @export
#' @examples
#' max_observation_time(examplinib_fe_nif)
#'
max_observation_time <- function(obj, analytes = NULL) {
  if (!is.null(analytes)) {
    obj <- obj %>%
      filter(ANALYTE %in% analytes)
  }

  obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    pull(TIME) %>%
    max(na.rm = TRUE)
}


#' Guess the most likely analyte based on its prevalence in the NIF object
#'
#' @param obj A NIF object.
#'
#' @return The analyte as character.
#' @export
#' @examples
#' guess_analyte(examplinib_poc_nif)
#'
guess_analyte <- function(obj) {
  temp <- obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    group_by(ANALYTE, METABOLITE) %>%
    summarize(n = n(), .groups = "drop") %>%
    arrange(METABOLITE, -n)
  (temp %>% pull(ANALYTE))[1]
}


#' Add baseline creatinine clearance field.
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#' Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
#'
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
#'
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
#'
#' @details
#' The Baseline is calculated as the median of the DV for all times lower or
#' equal to zero.
#'
#' @param obj A NIF object.
#' @param summary_function The function to derive the baseline. This function is
#' applied over the DV values at TIME less than or equal to zero. The default is
#' `median`. Alternatively, `mean`, `min` or `max` can be considered.
#'
#' @return A NIF object
#' @export
#' @examples
#' head(add_cfb(examplinib_poc_nif))
#'
add_cfb <- function(obj, summary_function = median) {
  obj %>%
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
#'
#' @param obj A NIF object.
#'
#' @return Result as NIF object.
#' @export
#' @examples
#' add_obs_per_dosing_interval(examplinib_poc_nif)
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
#' Currently experimental. Don't use in production!
#'
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
  obj1 <- obj %>%
    filter(ANALYTE %in% analyte) %>%
    index_nif() %>%
    index_dosing_interval() %>%
    add_obs_per_dosing_interval() %>%
    as.data.frame()

  temp <- obj1 %>%
    mutate(RICHINT_TEMP = (OPDI > min_n)) %>%
    # add last observation before administration to rich interval
    group_by(ID, ANALYTE) %>%
    mutate(LEAD = lead(RICHINT_TEMP)) %>%
    mutate(RICHINT = RICHINT_TEMP | (LEAD & EVID == 0)) %>%
    fill(RICHINT, .direction = "down") %>%
    ungroup() %>%
    select(-c("RICHINT_TEMP", "LEAD")) %>%
    group_by(ID, ANALYTE) %>%
    mutate(FLAG = (RICHINT != lag(RICHINT) | row_number() == 1)) %>%
    ungroup() %>%
    as.data.frame()

  rich_index <- temp %>%
    filter(FLAG == TRUE & RICHINT == TRUE) %>%
    group_by(ID, ANALYTE) %>%
    mutate(RICH_N = row_number()) %>%
    ungroup() %>%
    select(REF, RICH_N)

  temp %>%
    left_join(rich_index, by = "REF") %>%
    group_by(RICHINT) %>%
    fill(RICH_N, .direction = "down") %>%
    ungroup() %>%
    new_nif()
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
    filter(USUBJID %in% usubjid)
}













