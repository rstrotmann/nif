#' nif class constructor
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param ... Further arguments.
#' @param obj A data frame containing the actual NIF data or a sdtm object.
#' @param silent Suppress messages.
#'
#' @import dplyr
#' @return A nif object from the input data set.
#' @export
new_nif <- function(obj = NULL, ..., silent = NULL) {
  lifecycle::deprecate_warn("0.61.1", "new_nif()", "nif()")

  if (is.null(obj)) {
    temp <- data.frame(matrix(nrow = 0, ncol = length(minimal_nif_fields)))
    colnames(temp) <- minimal_nif_fields
    class(temp) <- c("nif", "data.frame")
    temp |>
      order_nif_columns()
  } else {
    if (inherits(obj, "sdtm")) {
      temp <- nif_auto(obj, ..., silent = silent)
    } else {
      temp <- as.data.frame(obj)
      class(temp) <- c("nif", "data.frame")
    }
    class(temp) <- c("nif", "data.frame")
    order_nif_columns(temp)
  }
}


#' nif class constructor
#'
#' @param obj A data frame containing the actual NIF data or a sdtm object.
#' @param silent suppress messages.
#' @param ... Further arguments.
#'
#' @returns A nif object.
#' @export
#' @examples
#' nif()
nif <- function(obj = NULL, ..., silent = NULL) {
  if (is.null(obj)) {
    temp <- data.frame(matrix(nrow = 0, ncol = length(minimal_nif_fields)))
    colnames(temp) <- minimal_nif_fields
    class(temp) <- c("nif", "data.frame")
    temp |>
      order_nif_columns()
  } else {
    if (inherits(obj, "sdtm")) {
      temp <- nif_auto(obj, ..., silent = silent)
    } else {
      temp <- as.data.frame(obj)
      class(temp) <- c("nif", "data.frame")
    }
    class(temp) <- c("nif", "data.frame")
    order_nif_columns(temp)
  }
}


#' Convert data frame to nif object
#'
#' @param obj A data frame.
#'
#' @return A nif object.
#' @noRd
as_nif <- function(obj) {
  if (!inherits(obj, "data.frame")) {
    stop("obj must be a data frame!")
  }
  out <- as.data.frame(obj)
  class(out) <- c("nif", "data.frame")
  order_nif_columns(out)
}


#' Establish standard order of nif object columns
#'
#' @param obj A data frame.
#'
#' @return A data.frame.
#' @noRd
order_nif_columns <- function(obj) {
  selector <- unique(c(
    "REF", "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE",
    "HEIGHT", "WEIGHT", "BMI", "DTC", "TIME", "NTIME", "TAFD", "TAD",
    "PCELTM", "EVID", "AMT", "ANALYTE", "CMT", "PARENT", "TRTDY",
    "METABOLITE", "DOSE", "MDV", "ACTARMCD", "IMPUTATION",
    "FOOD", "PART", "PERIOD", "COHORT", "FASTED", "RICH_N", "DI",
    "TREATMENT", "SRC_DOMAIN", "SRC_SEQ"
  ))

  obj |>
    relocate(any_of(selector))
}


#' print() implementation for nif objects
#'
#' @param x A nif object.
#' @param color Colored output.
#' @param ... Additional parameters
#'
#' @export
#' @importFrom rlang hash
#' @noRd
print.nif <- function(x, color = FALSE, ...) {
  debug <- rlang::is_true(nif_option_value("debug"))

  # print as normal data frame if minimal fields not present
  if (debug == TRUE || !all(minimal_nif_fields %in% names(x))) {
    print(as.data.frame(x))
  } else {
    hline <- "-----"
    cat(paste0(hline, " NONMEM Input Format (NIF) data ", hline, "\n"))

    n_obs <- x |>
      filter(!"EVID" %in% names(x) | .data$EVID == 0) |>
      nrow()
    n_subs <- subjects(x) |>
      nrow()
    n_studies <- length(unique(x$STUDYID))

    cat(paste(
      n_obs,
      plural("observation", n_obs != 1), "from",
      n_subs, plural("subject", n_subs != 1),
      ifelse("STUDYID" %in% names(x),
        paste("across", n_studies, plural("study", n_studies > 1)),
        ""
      ),
      "\n"
    ))

    if ("ANALYTE" %in% names(x)) {
      cat(paste(
        "Analytes:", nice_enumeration(unique(x$ANALYTE)), "\n"
      ))
    } else {
      obs_cmt <- sapply(
        unique(x[x$EVID == 0, "CMT"]),
        function(x) paste0("'", x, "'")
      )
      cat(paste(
        length(obs_cmt),
        plural("compartment", length(obs_cmt) != 1),
        "with observations:", nice_enumeration(obs_cmt)
      ), "\n")
    }

    if ("SEX" %in% names(x)) {
      n_sex <- x |>
        dplyr::distinct(.data$USUBJID, .data$SEX) |>
        dplyr::group_by(.data$SEX) |>
        dplyr::summarize(n = n())

      n_males <- n_sex |>
        dplyr::filter(.data$SEX == 0) |>
        dplyr::pull(.data$n)
      if (length(n_males) == 0) {
        n_males <- 0
      }

      n_females <- n_sex |>
        dplyr::filter(.data$SEX == 1) |>
        dplyr::pull(.data$n)
      if (length(n_females) == 0) {
        n_females <- 0
      }

      cat(paste0(
        n_males, plural(" male", n_males != 1),
        " (", round(n_males / (n_males + n_females) * 100, 1), "%), ",
        n_females, plural(" female", n_males != 1),
        " (", round(n_females / (n_males + n_females) * 100, 1), "%)\n"
      ))
    }

    cat("\nColumns:\n")
    cat(str_wrap(paste(names(x), collapse = ", "),
                 width = 80, indent = 2, exdent = 2),  "\n\n")

    # version hash
    cat(str_glue("\nHash: {rlang::hash(x)}\n\n"))

    temp <- x |>
      as.data.frame() |>
      select(any_of(c(
        "ID", "NTIME", "TIME", "TAD", "ANALYTE",
        "EVID", "CMT", "AMT", "DOSE", "DV"
      ))) |>
      head(10)

    temp <- temp |>
      mutate(across(where(is.numeric), ~ round(., 3))) |>
      df_to_string(color = color, indent = 2)
    cat(paste0("\nData (selected columns):\n", temp, "\n"))

    footer <- paste0(positive_or_zero(nrow(x) - 10), " more rows")

    if (color == TRUE) {
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
  temp <- obj |>
    as.data.frame() |>
    filter(.data$ID %in% id | .data$USUBJID %in% id) |>
    filter(!is.na(.data$DOSE))

  out <- temp |>
    select(any_of(
      c(
        "USUBJID", "ID", "SEX", "AGE", "RACE", "WEIGHT", "HEIGHT", "BMI",
        "ACTARMCD", "PART", "COHORT"
      )
    ), starts_with("BL_")) |>
    distinct_all() |>
    as.list()

  out$ANALYTE <- temp |>
    distinct(.data$ANALYTE) |>
    pull(.data$ANALYTE)

  out$IMP <- temp |>
    filter(.data$PARENT != "") |>
    distinct(.data$PARENT) |>
    pull(.data$PARENT)

  out$administrations <- temp |>
    add_trtdy() |>
    filter(.data$EVID == 1) |>
    select(c("USUBJID", "TIME", "ANALYTE", "DTC", "TRTDY")) |>
    arrange(.data$ANALYTE, .data$TIME) |>
    select(c("ANALYTE", "TIME", "TRTDY")) |>
    as.data.frame()

  class(out) <- c("subject_info", "data.frame")
  out
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
  }) |>
    as.data.frame() |>
    select(-any_of(c("administrations"))) |>
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
#' @keywords internal
#' @examples
#' subjects(examplinib_fe_nif)
subjects.nif <- function(obj) {
  # input validation
  validate_nif(obj)

  # if (!"ID" %in% names(obj)) {
  #   stop("ID column missing!")
  # }

  if (!"USUBJID" %in% names(obj))
    obj <- mutate(obj, USUBJID = NA)

  obj |>
    as.data.frame() |>
    select(any_of(c("ID", "USUBJID"))) |>
    distinct()
}


#' Get the USUBJID of subject
#'
#' @param obj A NIF object.
#' @param id A subject ID in numeric form.
#' @param silent Suppress messages, defaults to nif_option setting.
#'
#' @return The USUBJID as character.
#' @export
#' @examples
#' usubjid(examplinib_fe_nif, 1, silent = FALSE)
usubjid <- function(obj, id, silent = NULL) {
  # input validation
  validate_nif(obj)
  validate_numeric_param(id, "id", allow_multiple = TRUE, allow_null = TRUE)
  if (!"USUBJID" %in% names(obj)) {
    stop("USUBJID field not found")
  }

  sbs <- subjects(obj)
  missing_id <- setdiff(id, unique(sbs$ID))
  matching_id <- intersect(id, unique(sbs$ID))
  if (length(missing_id) > 0) {
    conditional_message(
      plural("ID", length(missing_id) > 1), " not found: ",
      nice_enumeration(missing_id),
      silent = silent
    )
  }

  sbs[sbs$ID == matching_id, "USUBJID"]
}


#' Parent compounds within a NIF object
#'
#' @param obj A NIF object.
#' @return The parent compounds as character.
#' @noRd
parents <- function(obj) {
  # input validation
  validate_nif(obj)

  obj |>
    ensure_parent() |>
    as.data.frame() |>
    distinct(.data$PARENT) |>
    filter(.data$PARENT != "") |>
    pull(.data$PARENT)
}


#' Subjects with dose reduction
#'
#' @param obj A NIF object object.
#' @param analyte The analyte of interest as string. considers all analytes if
#'   analyte is NULL (default).
#' @return A data frame with the ID and, if available, the USUBJID of subjects
#'   with dose reductions.
#' @export
#' @examples
#' dose_red_sbs(examplinib_poc_nif)
dose_red_sbs <- function(obj, analyte = NULL) {
  # input validation
  validate_nif(obj)
  validate_char_param(analyte, "analyte", allow_null = TRUE)

  if (!"ANALYTE" %in% names(obj)) {
    obj <- obj |>
      mutate(ANALYTE = .data$CMT)
  }

  if (!is.null(analyte)) {
    obj <- obj |>
      filter(.data$ANALYTE %in% analyte)
  }

  temp <- obj |>
    as.data.frame() |>
    index_nif() |>
    filter(.data$EVID == 1)

  treatments <- unique(temp$ANALYTE)
  if (length(treatments) > 1) {
    stop(
      "Multiple treatments in data set (",
      nice_enumeration(treatments),
      "). Please specify exactly one treatment."
    )
  }

  temp |>
    arrange(.data$TIME) |>
    group_by(.data$ID, .data$ANALYTE) |>
    mutate(initial_dose = .data$AMT[row_number() == 1]) |>
    filter(.data$AMT < .data$initial_dose & .data$AMT != 0) |>
    ungroup() |>
    select(any_of(c("ID", "USUBJID"))) |>
    distinct()
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
#' rich_sampling_sbs(examplinib_poc_nif, n = 6)
rich_sampling_sbs <- function(
    obj,
    analyte = NA,
    max_time = NA,
    n = 4) {
  if (is.na(analyte)) {
    analyte <- guess_analyte(obj)
  }

  obj |>
    as.data.frame() |>
    filter(.data$EVID == 0, .data$ANALYTE == analyte) |>
    group_by(.data$ID) |>
    mutate(end_rich = case_when(is.na(max_time) ~ max(.data$TIME),
      .default = max_time
    )) |>
    ungroup() |>
    filter(.data$TIME < .data$end_rich) |>
    group_by(.data$ID, .data$USUBJID) |>
    summarize(n_obs = n(), .groups = "drop") |>
    filter(.data$n_obs > n) |>
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
  # input validation
  validate_nif(obj)

  if ("STUDYID" %in% names(obj)) {
    obj |>
      distinct(.data$STUDYID) |>
      pull(.data$STUDYID)
  } else {
    NULL
  }
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
#' @keywords internal
#' @examples
#' doses(examplinib_poc_nif)
doses.nif <- function(obj) {
  # input validation
  validate_nif(obj)

  obj |>
    filter(.data$AMT != 0) |>
    distinct(.data$AMT) |>
    arrange(as.numeric(.data$AMT)) |>
    pull(.data$AMT)
}


#' Dose levels within a NIF object
#'
#' This function summarizes the doses in the individual first administration by
#' subject and drug, and the number of subjects treated at this dose level.
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
#' dose_levels(nif())
dose_levels <- function(obj, cmt = 1, group = NULL) {
  # input validation
  validate_nif(obj)
  validate_numeric_param(cmt, "cmt")
  validate_char_param(group, "group", allow_null = TRUE, allow_multiple = TRUE)

  expected_fields <- c("ID", "AMT", "TIME", group)
  missing_fields <- setdiff(expected_fields, names(obj))
  if (length(missing_fields) > 0) {
    stop(paste0(
      "Required ", plural("fields", length(missing_fields) > 1),
      "missing in object: ", nice_enumeration(missing_fields)
    ))
  }

  temp <- obj |>
    ensure_analyte() |>
    filter(.data$EVID == 1) |>
    group_by(.data$ID, .data$ANALYTE, across(any_of(group))) |>
    arrange(.data$ID, .data$TIME)

  if (nrow(temp) == 0) {
    NULL
  } else {
    temp |>
      filter(.data$TIME == min(.data$TIME)) |>
      select("ID", "ANALYTE", "AMT", any_of(group)) |>
      tidyr::pivot_wider(
        names_from = "ANALYTE",
        values_from = "AMT", values_fill = 0
      ) |>
      group_by(across(c(-c("ID")))) |>
      summarize(N = n()) |>
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
analytes <- function(obj) {
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
#' @noRd
#' @examples
#' analytes(examplinib_fe_nif)
#' analytes(examplinib_poc_nif)
#' analytes(examplinib_poc_min_nif)
analytes.nif <- function(obj) {
  # input validation
  validate_nif(obj)

  ensure_analyte(obj) |>
    as.data.frame() |>
    filter(.data$EVID == 0) |>
    distinct(.data$ANALYTE) |>
    pull(.data$ANALYTE)
}


#' Analyte overview
#'
#' Overview on the analytes included in a nif object and their respective parent
#' analytes.
#'
#' @param obj A nif object.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' analyte_overview(examplinib_poc_nif)
analyte_overview <- function(obj) {
  obj |>
    ensure_analyte() |>
    ensure_parent() |>
    as.data.frame() |>
    distinct(.data$ANALYTE, .data$PARENT)
}


#' Analytes within a NIF object
#'
#' All analytes found in the NIF object. If the field 'ANALYTE' is not present,
#' The analyte title is derived from the compartment.
#' @param obj A NIF object
#' @import dplyr
#' @return Character.
#' @export
#' @keywords internal
#' @examples
#' analytes(examplinib_fe_nif)
#' analytes(examplinib_poc_nif)
#' analytes(examplinib_poc_min_nif)
analytes.data.frame <- function(obj) {
  ensure_analyte(obj) |>
    as.data.frame() |>
    filter(.data$EVID == 0) |>
    distinct(.data$ANALYTE) |>
    pull(.data$ANALYTE)
}


#' Analyte - compartment mapping
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param obj A NIF object
#' @return A data frame
#' @export
#' @keywords internal
#' @examples
#' cmt_mapping(examplinib_poc_nif)
#' cmt_mapping(examplinib_poc_min_nif)
cmt_mapping <- function(obj) {
  lifecycle::deprecate_warn("0.57.11", "cmt_mapping()", "compartments()")

  obj |>
    ensure_analyte() |>
    filter(.data$EVID == 0) |>
    distinct(across(any_of(c("ANALYTE", "CMT")))) |>
    as.data.frame()
}


#' Compartments used in a nif object
#'
#' @param obj A NIF object
#' @return A data frame
#' @export
#' @examples
#' compartments(examplinib_poc_nif)
#' compartments(examplinib_poc_min_nif)
compartments <- function(obj) {
  validate_nif(obj)

  obj |>
    ensure_analyte() |>
    filter(.data$EVID == 0) |>
    distinct(across(any_of(c("ANALYTE", "CMT")))) |>
    as.data.frame()
}


#' Treatments in a nif or sdtm object
#'
#' @param obj A nif or sdtm object.
#'
#' @return Character.
#' @export
treatments <- function(obj) {
  UseMethod("treatments")
}


#' Treatments in a nif object
#'
#' @param obj A nif object.
#'
#' @return Character.
#' @export
#' @keywords internal
#'
#' @examples
#' treatments(examplinib_poc_nif)
treatments.nif <- function(obj) {
  obj |>
    ensure_analyte() |>
    filter(.data$EVID == 1) |>
    distinct(.data$ANALYTE) |>
    pull(.data$ANALYTE)
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
  x <- x |>
    as.data.frame()
  NextMethod("head")
}


#' Minimal nif fields
#'
#' @return A character vector of the minimal NIF fields
#' @noRd
minimal_nif_fields <- c(
  "ID", "TIME", "AMT", "CMT", "EVID", "DV"
)


#' Standard nif fields
#'
#' @return A character vector of the standard NIF fields
#' @noRd
standard_nif_fields <- c(
  "REF", "STUDYID", "ID", "USUBJID", "NTIME", "TIME", "TAD", "TAFD", "ANALYTE",
  "AMT", "RATE", "DV", "LNDV", "MDV", "CMT", "EVID", "DOSE", "AGE", "SEX",
  "RACE", "HEIGHT", "WEIGHT", "BMI", "ACTARMCD", "ANALYTE", "PARENT",
  "METABOLITE", "TRTDY", "DI", "PART", "COHORT", "FASTED", "DTC", "RICH_N"
)


#' Fillable nif fields
#'
#' @return A character vector of the fillable NIF fields
#' @noRd
fillable_nif_fields <- unique(c(
  "SUBJID", "STUDYID", "AGE", "SEX", "RACE", "ETHNIC", "COUNTRY",
  "HEIGHT", "WEIGHT", "BMI", "ACTARMCD", "ARM", "PART", "COHORT", "FASTED",
  "IMPUTATION",
  "DOSE", "EPOCH", "PART", "COHORT", "FOOD", "FASTED"
))


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
  obj <- obj |>
    ensure_parent() |>
    index_nif() |>
    select(-any_of("DI"))

  di <- obj |>
    as.data.frame() |>
    filter(.data$EVID == 1) |>
    group_by(.data$ID, .data$PARENT) |>
    arrange(.data$TIME) |>
    mutate(DI = row_number()) |>
    ungroup() |>
    select("REF", "DI")

  obj |>
    left_join(di, by = "REF") |>
    group_by(.data$ID, .data$PARENT) |>
    arrange(.data$REF) |>
    tidyr::fill("DI", .direction = "downup") |>
    ungroup() |>
    nif()
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
#' n_administrations(nif())
n_administrations <- function(obj) {
  if (nrow(obj) == 0) {
    mutate(obj, N = NA)
  } else {
    obj |>
      index_dosing_interval() |>
      group_by(across(any_of(c("ID", "USUBJID", "PARENT")))) |>
      summarize(N = max(.data$DI), .groups = "drop") |>
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
#' max_admin_time(examplinib_poc_min_nif, analyte = "CMT1")
max_admin_time <- function(obj, analyte = NULL) {
  times <- obj |>
    ensure_analyte() |>
    as.data.frame() |>
    filter(is.null(analyte) | .data$ANALYTE %in% analyte) |>
    filter(.data$EVID == 1) |>
    pull(.data$TIME)

  if (length(times) == 0 || all(is.na(times))) {
    NA
  } else {
    max(times, na.rm = TRUE)
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
  times <- obj |>
    ensure_analyte() |>
    filter(is.null(analyte) | .data$ANALYTE %in% analyte) |>
    filter(.data$EVID == 0) |>
    pull(.data$TIME)

  if (length(times) == 0 || all(is.na(times))) {
    NA
  } else {
    max(times, na.rm = TRUE)
  }
}


#' Maximal time in nif object
#'
#' @param obj A nif object.
#' @param analyte The analyte to filter for, as character.
#' @param only_observations Maximal observation time as logical.
#' @param time_field The field to use as the time metric, as character.
#'
#' @return Numeric.
#' @export
#'
#' @examples
#' max_time(examplinib_poc_nif)
#' max_time(examplinib_poc_nif, analyte = "RS2023")
#' max_time(examplinib_poc_nif, only_observations = TRUE)
max_time <- function(
    obj,
    time_field = "TIME",
    analyte = NULL,
    only_observations = TRUE) {
  times <- obj |>
    ensure_analyte() |>
    filter(is.null(analyte) | .data$ANALYTE %in% analyte) |>
    filter(only_observations == FALSE | (.data$EVID == 0 & !is.na(.data$DV))) |>
    pull(.data[[time_field]])

  if (length(times) == 0 || all(is.na(times))) {
    return(NA)
  }
  max(times, na.rm = TRUE)
}


#' Guess the most likely analyte based on its prevalence in the NIF object
#'
#' @param obj A NIF object.
#' @return The analyte as character.
#' @keywords internal
#' @noRd
guess_analyte <- function(obj) {
  temp <- obj |>
    ensure_analyte() |>
    ensure_metabolite() |>
    filter(.data$EVID == 0)

  if (length(analytes(temp)) > 0) {
    temp |>
      filter(.data$METABOLITE == FALSE) |>
      group_by(.data$ANALYTE) |>
      summarize(n = n(), .groups = "drop") |>
      filter(n == max(.data$n)) |>
      arrange(.data$ANALYTE) |>
      slice(1) |>
      pull("ANALYTE")
  } else {
    NA
  }
}


#' Guess the most likely parent based on its prevalence in the NIF object
#'
#' @param obj A nif object.
#'
#' @return The parent as character
#' @keywords internal
#' @noRd
guess_parent <- function(obj) {
  validate_nif(obj)

  imp <- obj |>
    ensure_analyte() |>
    filter(.data$EVID == 1) |>
    reframe(n = n(), .by = "ANALYTE") |>
    arrange(-.data$n, .data$ANALYTE)

  # if administrations in data set, return analyte with most observations
  if (nrow(imp) > 0) {
    imp[1, ] |>
      pull(.data$ANALYTE)
  } else {
    obs <- obj |>
      ensure_analyte() |>
      filter(!"METABOLITE" %in% names(obj) | .data$METABOLITE == FALSE) |>
      reframe(n = n(), .by = "ANALYTE") |>
      arrange(-n, "ANALYTE")

    if (nrow(obs) > 0) {
      obs[1, "ANALYTE"]
    } else {
      NULL
    }
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
  temp <- obj |>
    ensure_dose() |>
    ensure_analyte() |>
    ensure_parent() |>
    ensure_metabolite() |>
    as.data.frame() |>
    filter(.data$METABOLITE == FALSE) |>
    filter(
      .data$PARENT != "", !is.na(.data$DOSE), .data$AMT != 0,
      .data$EVID == 1
    ) |>
    group_by(.data$ID, .data$ANALYTE) |>
    arrange(.data$ID, .data$TIME, .data$ANALYTE) |>
    filter(.data$TIME == min(.data$TIME)) |>
    select("ID", "ANALYTE", "DOSE") |>
    group_by(.data$ID)

  if (ungroup(temp) |> distinct(.data$ANALYTE) |> nrow() == 1) {
    temp <- temp |>
      mutate(DL = .data$DOSE)
  } else {
    temp <- temp |>
      mutate(DL = paste0(.data$DOSE, "-", .data$ANALYTE)) |>
      arrange(.data$ID) |>
      arrange(factor(.data$ANALYTE, levels = analytes(obj))) |>
      summarize(DL = paste0(.data$DL, collapse = "+"))
  }

  left_join(obj, select(temp, c("ID", "DL")), by = "ID")
}


#' Add baseline creatinine clearance field.
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#'   Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
#' @return A NIF object.
#' @seealso [nif::egfr_mdrd()]
#' @seealso [nif::egfr_cg()]
#' @seealso [nif::egfr_raynaud()]
#' @export
#' @examples
#' head(add_bl_crcl(examplinib_poc_nif))
add_bl_crcl <- function(obj, method = egfr_cg) {
  missing_columns <- setdiff(c( "BL_CREAT", "AGE", "SEX", "RACE", "WEIGHT"),
                            names(obj))
  if (length(missing_columns) > 0)
    stop(paste0(
      "Missing coluns: ", nice_enumeration(missing_columns), "!"
    ))

  if ("BL_CREAT" %in% colnames(obj)) {
    obj |>
      mutate(BL_CRCL = method(
        .data$BL_CREAT, .data$AGE, .data$SEX, .data$RACE, .data$WEIGHT,
        molar = TRUE
      ))
  } else {
    obj |>
      mutate(BL_CRCL = as.numeric(NA))
  }
}


#' Add baseline renal function class
#'
#' If baseline creatinine clearance (BL_CRCL) is not included in the input, it
#' will be calculated first.
#'
#' @param obj A NIF object.
#' @param method The function to calculate eGFR (CrCL) from serum creatinine.
#' Currently either: egfr_mdrd, egfr_cg or egfr_raynaud
#' @return A NIF object.
#' @export
#' @examples
#' head(add_bl_renal(examplinib_poc_nif), 5)
add_bl_renal <- function(obj, method = egfr_cg) {
  if (!"BL_CRCL" %in% names(obj))
    obj <- add_bl_crcl(obj, method = method)

  obj |>
    mutate(BL_RENAL = as.character(
      cut(.data$BL_CRCL,
        breaks = c(0, 30, 60, 90, Inf),
        labels = c("severe", "moderate", "mild", "normal")
      )
    )) |>
    mutate(BL_RENAL = factor(.data$BL_RENAL,
      levels = c("normal", "mild", "moderate", "severe")
    ))
}


#' Add baseline lean body mass (LBM)
#'
#' @param obj A nif object.
#' @param method The function to calculate LBM, i.e., [nif::lbm_boer()],
#' [nif::lbm_hume()] or [nif::lbm_peters()].
#'
#' @return A nif object.
#' @seealso [nif::lbm_hume()]
#' @seealso [nif::lbm_boer()]
#' @seealso [nif::lbm_peters()]
#' @export
add_bl_lbm <- function(obj, method = lbm_boer) {
  # input validation
  validate_nif(obj)
  missing_fields <- setdiff(c("WEIGHT", "HEIGHT", "SEX"), names(obj))
  if (length(missing_fields) > 0)
    stop(paste0("Missing fields: ", nice_enumeration(missing_fields)))

  obj |>
    # assertr::verify(assertr::has_all_names("WEIGHT", "HEIGHT", "SEX")) |>
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
  lb1 <- sdtm |>
    domain("lb") |>
    filter(eval(parse(text = baseline_filter))) |>
    filter(eval(parse(text = observation_filter))) |>
    filter(.data$LBTESTCD %in% c("AST", "BILI")) |>
    mutate(
      LBTESTCD = paste0(.data$LBTESTCD, "_X_ULN"),
      LBSTRESN = .data$LBSTRESN / .data$LBSTNRHI
    )

  sdtm$domains[["lb"]] <- lb1

  obj |>
    add_baseline(sdtm, "lb", "BILI_X_ULN", baseline_filter = baseline_filter) |>
    add_baseline(sdtm, "lb", "AST_X_ULN", baseline_filter = baseline_filter) |>
    mutate(BL_ODWG = case_when(
      .data$BL_BILI_X_ULN > 3 & .data$BL_BILI_X_ULN <= 10 ~ "severe",
      .data$BL_BILI_X_ULN > 1.5 & .data$BL_BILI_X_ULN <= 3 ~ "moderate",
      (.data$BL_BILI_X_ULN > 1 & .data$BL_BILI_X_ULN <= 1.5) |
        .data$BL_AST_X_ULN > 1 ~ "mild",
      .data$BL_BILI_X_ULN <= 1 & .data$BL_BILI_X_ULN <= 1 &
        .data$BL_AST_X_ULN <= 1 ~ "normal",
      .default = NA
    )) |>
    mutate(BL_ODWG = factor(.data$BL_ODWG,
      levels = c("normal", "mild", "moderate", "severe")
    ))
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
  obj |>
    index_nif() |>
    select(-any_of("DI")) |>
    index_dosing_interval() |>
    group_by(across(any_of(c("ID", "USUBJID", "ANALYTE", "PARENT", "DI")))) |>
    mutate(OPDI = sum(.data$EVID == 0))
}


#' Identify and index rich PK sampling intervals
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

  obj |>
    ensure_analyte() |>
    arrange(.data$ID, .data$TIME, .data$ANALYTE) |>
    index_dosing_interval() |>
    add_obs_per_dosing_interval() |>
    mutate(RICHINT_TEMP = (.data$OPDI >= min_n)) |>
    group_by(.data$ID, .data$ANALYTE, .data$DI) |>
    mutate(RICH_START = case_when(
      row_number() == 1 & .data$RICHINT_TEMP == TRUE ~ TRUE,
      .default = FALSE
    )) |>
    ungroup() |>
    group_by(.data$ID, .data$ANALYTE, .data$RICH_START) |>
    mutate(RICH_N = case_when(
      .data$RICHINT_TEMP == TRUE & .data$RICH_START == TRUE ~ row_number(),
      .default = NA
    )) |>
    ungroup() |>
    group_by(.data$ID, .data$ANALYTE, .data$DI, .data$RICHINT_TEMP) |>
    tidyr::fill("RICH_N", .direction = "down") |>
    ungroup() |>
    select(-c("RICHINT_TEMP", "RICH_START")) |>
    nif()
}


#' Filter NIF object for specific USUBJID
#'
#' @param obj The NIF object.
#' @param usubjid The USUBJID as character
#' @return A NIF object.
#' @export
#' @noRd
#' @examples
#' filter_subject(
#' examplinib_poc_nif,
#' subjects(examplinib_poc_nif)[1, "USUBJID"])
filter_subject.nif <- function(obj, usubjid) {
  filter(obj, .data$USUBJID %in% usubjid)
}


#' Last recorded date-time
#'
#' @param obj A nif, sdtm or domain domain object.
#'
#' @returns A POSIXct scalar.
#' @export
last_dtc <- function(obj) {
  UseMethod("last_dtc")
}


#' Last date in nif object
#'
#' @param obj A nif object.
#'
#' @returns A POSIXct scalar.
#' @export
#' @keywords internal
#'
#' @examples
#' last_dtc(examplinib_sad_nif)
last_dtc.nif <- function(obj) {
  validate_nif(obj)

  out <- NULL
  if ("DTC" %in% names(obj)) {
    out <- max(obj$DTC)
  }
  out
}


#' Last date in data.frame
#'
#' @param obj A data frame.
#'
#' @returns A POSIXct scalar.
#' @noRd
last_dtc_data_frame <- function(obj) {
  if (!inherits(obj, "data.frame")) {
    stop("obj must be a data frame!")
  }

  out <- NULL

  temp <- obj |>
    lubrify_dates() |>
    select(where(
      function(x) {
        is.POSIXct(x) & !all(is.na(x))
      }
    ))

  if (ncol(temp) > 0) {
    out <- as.POSIXct(max(unlist(lapply(temp, max, na.rm = TRUE))))
  }

  out
}
