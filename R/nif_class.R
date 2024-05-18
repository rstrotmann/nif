#' nif class constructor
#'
#' @param ... Further arguments.
#' @param obj A data frame containing the actual NIF data or a sdtm object.
#'
#' @import dplyr
#' @return A nif object from the input data set.
#' @export
new_nif <- function(obj = NULL, ...) {
  if(is.null(obj)) {
    temp <- data.frame(matrix(nrow = 0, ncol = length(minimal_nif_fields)))
    colnames(temp) <- minimal_nif_fields
    class(temp) <- c("nif", "data.frame")
    temp %>%
      order_nif_columns()
  } else {
    if (class(obj)[1] == "sdtm") {
      temp <- nif_auto(obj, ...)
    } else {
      temp <- as.data.frame(obj)
      class(temp) <- c("nif", "data.frame")
    }
    class(temp) <- c("nif", "data.frame")
    order_nif_columns(temp)
  }
}


#' Establish standard order of nif object columns
#'
#' @param obj A data frame.
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#' order_nif_columns(examplinib_poc_nif)
order_nif_columns <- function(obj) {
  selector <- unique(c("REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE",
    "HEIGHT", "WEIGHT", "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD",
    "PCELTM", "EVID", "AMT", "ANALYTE", "CMT",  "PARENT", "TRTDY",
    "METABOLITE", "DOSE", "DV", "MDV", "ACTARMCD", "IMPUTATION",
    "FOOD", "PART", "PERIOD", "COHORT", "FASTED", "RICH_N", "DI",
    "TREATMENT"))

  obj %>%
    relocate(any_of(selector))
}


#' print() implementation for nif objects
#'
#' @param x A nif object.
#' @param color Colored output.
#' @param ... Additional parameters
#'
#' @export
#' @noRd
print.nif <- function(x, color = FALSE, ...) {
  debug <- is_true(nif_option_value("debug"))
  if(debug == TRUE) {
    print(x %>%
      as.data.frame())
  } else {
    hline <- "-----"
    # hline <- paste0(rep("\U2500", 8), collapse="")
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
    select(c("USUBJID", "TIME", "ANALYTE", "DTC", "TRTDY")) %>%
    arrange(.data$ANALYTE, .data$TIME) %>%
    select(c("ANALYTE", "TIME", "TRTDY")) %>%
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
    return(NULL)
  }
}


#' Ensure that the ANALYTE field is present
#'
#' If 'ANALYTE' is not in the column names, the field is created based on the
#' compartment (CMT).
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
ensure_analyte <- function(obj) {
  obj %>%
    {if(!"ANALYTE" %in% names(obj))
      mutate(., ANALYTE = CMT) else .}
}


#' Ensure that the DOSE field is present
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
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
ensure_parent <- function(obj) {
  admin_cmt <- obj %>%
    as.data.frame() %>%
    filter(EVID == 1) %>%
    distinct(CMT)

  obj <- obj %>%
  {if(!"PARENT" %in% names(obj))
    mutate(., PARENT = admin_cmt[[1]]) else .}
  return(obj)
}


#' Ensure that the METABOLITE field is present in a NIF file.
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
ensure_metabolite <- function(obj) {
  obj %>%
    {if(!"METABOLITE" %in% names(obj))
      mutate(., METABOLITE = FALSE) else .}
}


#' Ensure that TAD is present in the NIF object
#'
#' @param obj A NIF object.
#' @return A NIF object.
#' @keywords internal
ensure_tad <- function(obj) {
  obj <- obj %>%
    {if(!"TAD" %in% names(obj))
      add_tad(.) else .}
}


#' Ensure TAFD is present in the NIF object
#'
#' @param obj A nif object.
#'
#' @return A nif object.
#' @keywords internal
ensure_tafd <- function(obj) {
  obj <- obj %>%
    {if(!"TAFD" %in% names(obj))
      add_tafd(.) else .}
}


#' Ensure that all time fields are in the nif object
#'
#' @param obj A nif object.
#'
#' @return A nif object.
#' @keywords internal
ensure_time <- function(obj) {
  obj <- obj %>%
    {if(!all(c("TIME", "TAD", "TAFD") %in% names(obj)))
      make_time(.) else .}
}


ensure_cfb <- function(obj) {
  obj <- obj %>%
    # {if(!all(c("ID", "TIME", "ANALYTE", "EVID", "DV") %in% names(obj)))
    #   add_cfb(.) else .}
    {if(!"DVCFB" %in% names(obj)) add_cfb(.) else .}
}


#' Doses in a nif or sdtm object
#'
#' @param obj A nif or sdtm object.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' doses(examplinib_poc_nif)
doses <- function(obj) {
  UseMethod("doses")
}


#' Doses within a NIF object
#'
#' @param obj A nif object
#' @import dplyr
#' @return A number vector of all doses (AMT) in the data set.
#' @export
#' @examples
#' doses(examplinib_poc_nif)
doses.nif <- function(obj) {
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
#' dose_levels(new_nif())
dose_levels <- function(obj, cmt = 1, group = NULL) {
  temp <- obj %>%
    ensure_analyte() %>%
    filter(AMT != 0) %>%
    group_by(ID, ANALYTE, across(any_of(group))) %>%
    arrange(ID, TIME)

  if(nrow(temp) == 0) {
    return(NULL)
  } else {
    temp %>%
      filter(TIME == min(TIME)) %>%
      select(ID, ANALYTE, AMT, any_of(group)) %>%
      pivot_wider(
        names_from = "ANALYTE",
        values_from = "AMT", values_fill = 0) %>%
      group_by(across(c(-ID))) %>%
      summarize(N = n()) %>%
      as.data.frame()
  }
}


#' Analytes within a NIF or SDTM object
#'
#' @param obj A nif or sdtm object.
#' @import dplyr
#' @return Character.
#' @export
#' @examples
#' analytes(examplinib_fe_nif)
#' analytes(examplinib_poc_nif)
#' analytes(examplinib_poc_min_nif)
analytes <- function(nif) {
  UseMethod("analytes")
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
analytes.nif <- function(obj) {
  ensure_analyte(obj) %>%
    as.data.frame() %>%
    filter(EVID==0) %>%
    distinct(ANALYTE) %>%
    pull(ANALYTE)
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
analytes.data.frame <- function(obj) {
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
    filter(EVID==0) %>%
    distinct(across(any_of(c("ANALYTE", "CMT")))) %>%
    as.data.frame()
}


treatments <- function(obj) {
  UseMethod("treatments")
}


#' Treatments in a nif object
#'
#' @param obj A nif object.
#'
#' @return Character.
#' @export
#'
#' @examples
#' treatments(examplinib_poc_nif)
treatments.nif <- function(obj) {
  obj %>%
    ensure_parent() %>%
    as.data.frame() %>%
    distinct(PARENT) %>%
    filter(PARENT != "") %>%
    pull(PARENT)
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


# ' Export a NIF object as csv file
# '
# ' @param obj A NIF object.
# ' @param filename The filename for the exported file.
# ' @import dplyr
# ' @export
# write_csv.nif <- function(obj, filename) {
#   temp <- obj %>%
#     as.data.frame() %>%
#     dplyr::mutate(across(c("TIME", "TAD", "DV", "LNDV"), round, 3))
#   write.csv(temp, filename, quote = FALSE, row.names = FALSE)
# }


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
write_nif <- function(obj, filename = NULL, fields = NULL, sep = NULL) {
  temp <- obj %>%
    as.data.frame() %>%
    mutate_if(is.numeric, round, digits = 4) %>%
    mutate_all(as.character()) %>%
    mutate_all(function(x) {
      case_when(is.na(x) ~ ".",.default = as.character(x))
    })

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


#' Minimal nif fields
#'
#' @return A character vector of the minimal NIF fields
#' @export
minimal_nif_fields <- c(
  "ID", "TIME", "AMT", "CMT", "EVID", "DOSE", "DV", "MDV"
)


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
  "IMPUTATION"
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
#' n_administrations(new_nif())
n_administrations <- function(obj) {
  if(nrow(obj) == 0) {
    return(mutate(obj, N = NA))
  } else {
    obj %>%
      index_dosing_interval() %>%
      group_by(across(any_of(c("ID", "USUBJID", "PARENT")))) %>%
      summarize(N = max(DI), .groups = "drop") %>%
      as.data.frame()
  }
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


#' Maximal time in nif object
#'
#' @param obj A nif object.
#' @param analyte The analyte to filter for, as character.
#' @param only_observations Maximal ovservation time as logical.
#' @param time_field The field to use as the time metric, as character.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' max_time(examplinib_poc_nif)
#' max_time(examplinib_poc_nif, analyte = "RS2023")
#' max_time(examplinib_poc_nif, only_observations = TRUE)
max_time <- function(obj, time_field = "TIME", analyte = NULL,
                     only_observations = TRUE) {
  times <- obj %>%
    ensure_analyte() %>%
    {if(!is.null(analyte)) filter(., .data$ANALYTE %in% analyte) else .} %>%
    {if(only_observations == TRUE) filter(., .data$EVID == 0 & !is.na(.data$DV)) else .} %>%
    # pull(TIME)
    pull(.data[[time_field]])

  if(length(times) == 0) return(NA)
  return(max(times, na.rm = TRUE))
}


#' Guess the most likely analyte based on its prevalence in the NIF object
#'
#' @param obj A NIF object.
#' @return The analyte as character.
#' @keywords internal
guess_analyte <- function(obj) {
  temp <- obj %>%
    as.data.frame() %>%
    ensure_analyte() %>%
    ensure_metabolite() %>%
    as.data.frame() %>%
    filter(EVID == 0)

  if(length(analytes(temp)) > 0) {
    temp %>%
      filter(METABOLITE == FALSE) %>%
      group_by(ANALYTE) %>%
      # filter(sum(EVID == 0) > 0) %>%
      summarize(n = n(), .groups = "drop") %>%
      filter(n == max(n)) %>%
      arrange(ANALYTE) %>%
      slice(1) %>%
      pull("ANALYTE")
  } else {
    return(NA)
  }
}


#' Guess the most likely parent based on its prevalence in the NIF object
#'
#' @param obj A nif object.
#'
#' @return The parent as character
#' @keywords internal
guess_parent <- function(obj) {
  temp <- obj %>%
    as.data.frame() %>%
    ensure_analyte() %>%
    ensure_metabolite() %>%
    ensure_parent() %>%
    as.data.frame() %>%
    filter(EVID == 0)

  if(length(analytes(temp)) > 0) {
    temp %>%
      filter(METABOLITE == FALSE) %>%
      group_by(PARENT) %>%
      summarize(n = n(), .groups = "drop") %>%
      filter(n == max(n)) %>%
      arrange(PARENT) %>%
      slice(1) %>%
      pull("PARENT")
  } else {
    return(NULL)
  }
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
#' @return A NIF object.
#' @export
#' @examples
#' add_tad(examplinib_poc_nif)
#' add_tad(examplinib_poc_min_nif)
add_tad <- function(nif) {
  nif %>%
    ensure_parent() %>%
    as.data.frame() %>%
    mutate(admin_time = case_when(
      .data$EVID == 1 ~ .data$TIME)) %>%
    arrange(.data$ID, .data$PARENT, .data$TIME, -.data$EVID) %>%
    group_by(.data$ID, .data$PARENT) %>%
    fill(admin_time, .direction = "down") %>%
    ungroup() %>%
    mutate(TAD = .data$TIME - .data$admin_time) %>%
    select(-"admin_time") %>%
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
    arrange(.data$ID, .data$PARENT, .data$TIME) %>%
    group_by(.data$ID, .data$PARENT) %>%
    mutate(first_admin = min(.data$TIME[.data$EVID == 1])) %>%
    mutate(TAFD = .data$TIME-.data$first_admin) %>%
    mutate(TAFD = case_when(.data$TAFD < 0 ~ 0, .default = .data$TAFD)) %>%
    select(-c("first_admin")) %>%
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
    select(-FIRSTTRTDTC) %>%
    new_nif()
}


# add_ntafd <- function(obj) {

# }

#' Add baseline creatinine clearance field.
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#'   Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
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
      mutate(BL_CRCL = method(BL_CREAT, AGE, SEX, RACE, WEIGHT,
        molar = TRUE
      ))
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
      levels = c("normal", "mild", "moderate", "severe")
    ))
}


#' Add baseline lean body mass (LBM)
#'
#' @param obj A nif object.
#' @param method The function to calculate LBM, i.e., [lbm_boer()],
#' [lbm_hume()] or [lbm_peters()].
#'
#' @return A nif object.
#' @seealso [lbm_hume()]
#' @seealso [lbm_boer()]
#' @seealso [lbm_peters()]
#' @import assertr
#' @export
add_bl_lbm <- function(obj, method = lbm_boer) {
  # if ("BL_CREAT" %in% colnames(obj)) {
  #   obj %>%
  #     mutate(BL_CRCL = method(BL_CREAT, AGE, SEX, RACE, WEIGHT,
  #                             molar = TRUE
  #     ))
  # } else {
  #   obj %>%
  #     mutate(BL_CRCL = as.numeric(NA))
  # }
  obj %>%
    verify(has_all_names("WEIGHT", "HEIGHT", "SEX")) %>%
    mutate(BL_LBM = method(.data$WEIGHT, .data$HEIGHT, .data$SEX))
}


#' Add baseline hepatic function class
#'
#' Based on the NCI ODWG criteria with TB the total (direct and indirect) serum
#' bilirubin, and AST aspartate aminotransferase.
#'
#' * normal: TB & AST ≤ upper limit of normal (ULN)
#'
#' * mild hepatic dysfunction: TB > ULN to 1.5 x ULN or AST > ULN
#'
#' * moderate hepatic dysfunction: TB >1.5–3 x ULN, any AST
#'
#' * severe hepatic dysfunction: TB >3 - 10 x ULN, any AST
#'
#' @param obj A nif object.
#' @param sdtm The corresponding sdtm object.
#' @param baseline_filter A filter term to identify the baseline condition, as
#'   character.
#' @param summary_function The summary function to summarize multiple baseline
#'   values. Defaults to `mean`
#' @param observation_filter The filter term for the observation source data, as
#'   character.
#'
#' @return A nif object.
#' @export
add_bl_odwg <- function(obj, sdtm,
                        observation_filter = "LBSPEC != 'URINE'",
                        baseline_filter = "LBBLFL == 'Y'",
                        summary_function = mean) {
  lb1 <- sdtm %>%
    domain("lb") %>%
    # filter(.data$LBSPEC != "URINE") %>%
    filter(eval(parse(text = baseline_filter))) %>%
    filter(eval(parse(text = observation_filter))) %>%
    # mutate(LBDTC = .data$LBDTC) %>%
    filter(.data$LBTESTCD %in% c("AST", "BILI")) %>%
    # mutate(LBTESTCD = .data$LBTESTCD) %>%
    # mutate(LB1BLFL = .data$LBBLFL) %>%
    mutate(LBTESTCD = paste0(.data$LBTESTCD, "_X_ULN"),
           LBSTRESN = .data$LBSTRESN / .data$LBSTNRHI)

  sdtm$domains[["lb"]] <- lb1

  obj %>%
    add_baseline(sdtm, "lb", "BILI_X_ULN", baseline_filter = baseline_filter) %>%
    add_baseline(sdtm, "lb", "AST_X_ULN", baseline_filter = baseline_filter) %>%
    mutate(BL_ODWG = case_when(
      .data$BL_BILI_X_ULN > 3 & .data$BL_BILI_X_ULN <= 10 ~ "severe",
      .data$BL_BILI_X_ULN > 1.5 & .data$BL_BILI_X_ULN <= 3 ~ "moderate",
      (.data$BL_BILI_X_ULN > 1 & .data$BL_BILI_X_ULN <= 1.5) |
        .data$BL_AST_X_ULN > 1 ~ "mild",
      .data$BL_BILI_X_ULN <= 1 & .data$BL_BILI_X_ULN <= 1 &
        .data$BL_AST_X_ULN <= 1 ~ "normal",
      .default = NA
    )) %>%
    mutate(BL_ODWG = factor(.data$BL_ODWG,
      levels = c("normal", "mild", "moderate", "severe")))
}


#' Add baseline and change from baseline fields
#'
#' @details
#' Output fields:
#' * `DVBL` Baseline value for the dependent variable DV.
#' * `DVCFB` Change from baseline for the dependent variable DV.
#' @details The Baseline is calculated as the median of the DV for all times
#' lower or equal to zero.
#'
#' @param obj A NIF object.
#' @param summary_function The function to derive the baseline. This function is
#'   applied over the DV values at TIME less than or equal to zero. The default
#' @param baseline_filter A filter term to identify the baseline condition.
#'   is `median`. Alternatively, `mean`, `min` or `max` can be considered.
#' @return A NIF object
#' @importFrom stats na.omit
#' @export
#' @examples
#' head(add_cfb(examplinib_poc_nif))
#' head(add_cfb(examplinib_poc_min_nif))
add_cfb <- function(obj, baseline_filter = "TIME <= 0",
                    summary_function = median) {
  # obj %>%
  #   ensure_analyte() %>%
  #   as.data.frame() %>%
  #   group_by(ID, ANALYTE) %>%
  #   # mutate(DVBL = summary_function(DV[TIME <= 0], na.rm = TRUE)) %>%
  #   mutate(DVBL = summary_function(na.omit(DV[TIME <= 0]))) %>%
  #   mutate(DVCFB = DV - DVBL) %>%
  #   new_nif()

  # f <- function(x) {
  #   temp <- x[eval(parse(text = baseline_filter))]
  #   temp <- na.omit(temp)
  #   return(summary_function(temp))
  # }

  obj %>%
    ensure_analyte() %>%
    as.data.frame() %>%
    group_by(ID, ANALYTE) %>%
    # mutate(DVBL = summary_function(DV[TIME <= 0], na.rm = TRUE)) %>%
    # mutate(DVBL = f(DV)) %>%

    mutate(DVBL = summary_function(
      na.omit(DV[eval(parse(text = baseline_filter))]))) %>%
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
#' sampling.
#' @param analyte The analyte as character. If `NA` (default), the most likely
#' will be selected automatically.
#'
#' @return A new NIF object.
#' @export
index_rich_sampling_intervals <- function(obj, analyte = NULL, min_n = 4) {
  if (is.null(analyte)) {
    analyte <- guess_analyte(obj)
  }

  obj %>%
    ensure_analyte() %>%
    as.data.frame() %>%
    arrange(.data$ID, .data$TIME, .data$ANALYTE) %>%
    index_dosing_interval() %>%
    add_obs_per_dosing_interval() %>%
    mutate(RICHINT_TEMP = (.data$OPDI >= min_n)) %>%

    # add last observation before administration to rich interval
    # group_by(ID, ANALYTE) %>%
    # mutate(RICHINT = RICHINT_TEMP | (lead(RICHINT_TEMP) & EVID == 0)) %>%
    # fill(RICHINT, .direction = "down") %>%
    # ungroup() %>%
    # select(-c("RICHINT_TEMP")) %>%

    group_by(.data$ID, .data$ANALYTE, .data$DI) %>%
    # group_by(DI) %>%
    mutate(RICH_START = case_when(
      # row_number() == 1 & RICHINT_TEMP == TRUE ~ dense_rank(DI),
      row_number() == 1 & .data$RICHINT_TEMP == TRUE ~ TRUE,
      .default = FALSE)) %>%
    # mutate(test = dense_rank(DI)) %>%
    ungroup() %>%

    group_by(.data$ID, .data$ANALYTE, .data$RICH_START) %>%
    mutate(RICH_N = case_when(
      .data$RICHINT_TEMP == TRUE & .data$RICH_START == TRUE ~ row_number(),
      .default = NA)) %>%
    ungroup() %>%

    group_by(.data$ID, .data$ANALYTE, .data$DI, .data$RICHINT_TEMP) %>%
    fill(.data$RICH_N, .direction = "down") %>%
    ungroup() %>%

    select(-c("RICHINT_TEMP", "RICH_START")) %>%
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
    filter(.data$USUBJID %in% usubjid)
}













