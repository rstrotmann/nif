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
  nif(obj = obj, ..., silent = silent)
}


#' Order nif rows and add REF column
#'
#' Order rows by ID, TIME/DTC and EVID and then assign sequential REF
#'
#' @param obj A nif object.
#'
#' @returns A nif object.
#' @noRd
arrange_and_add_ref <- function(obj) {
  # Create temporary column for descending EVID sort
  if ("EVID" %in% names(obj)) {
    obj <- obj |>
      dplyr::mutate(.EVID_sort = -.data$EVID)
  }

  obj <- obj |>
    dplyr::arrange(across(any_of(c("ID", "TIME", "DTC", ".EVID_sort"))))

  # Remove temporary column
  if (".EVID_sort" %in% names(obj)) {
    obj <- obj |>
      dplyr::select(-".EVID_sort")
  }

  obj |>
    dplyr::mutate(REF = row_number()) |>
    dplyr::relocate("REF")
}


#' Assign unique ID
#'
#' Re-assign unique IDs per subject based on USUBJID or SUBJID, and STUDYID,
#' if available.
#'
#' @param obj A nif object
#'
#' @returns A inf object with unique IDs
#' @noRd
index_id <- function(obj) {
  if (!any(c("USUBJID", "SUBJID", "ID") %in% names(obj))) {
    stop("Input must have at least one of USUBJID, SUBJID or ID columns!")
  }

  # prefer USUBJID over SUBJID over ID to identify subjects
  if ("USUBJID" %in% names(obj)) {
    obj <- mutate(obj, .temp_id = .data$USUBJID)
  } else {
    if ("SUBJID" %in% names(obj)) {
      obj <- mutate(obj, .temp_id = .data$SUBJID)
    } else {
      obj <- mutate(obj, .temp_id = .data$ID)
    }
  }

  out <- obj |>
    unite(".temp_id", any_of(any_of(c("STUDYID", ".temp_id"))),
      remove = FALSE
    ) |>
    arrange(across(any_of(c("STUDYID", "USUBJID", "SUBJID", "ID")))) |>
    mutate(ID = as.numeric(
      factor(
        .data$.temp_id,
        levels = unique(.data$.temp_id)
      )
    )) |>
    select(-".temp_id")

  class(out) <- c("nif", "data.frame")
  out
}


#' nif class constructor
#'
#' Create an empty nif object or a nif object from sdtm data or from a data
#' table.
#'
#' If no obj argument is provided, an empty, minimal nif object will be
#' generated. If a data frame is provided for obj, it is converted into a nif
#' object. Minimally expected fields are ID, TIME, AMT, CMT, EVID and DV. If ID
#' is missing but USUBJID is available, ID will be derived.
#'
#' If the input is a sdtm object, a pharmacokinetic nif object is automatically
#' generated. Analyte mapping formulae can be supplied as the ... argument. For
#' details, see `nif_auto()`.
#'
#' @param obj A data frame containing the actual NIF data or a sdtm object.
#' @param silent suppress messages.
#' @param ... Further arguments.
#' @seealso [nif::nif_auto()]
#'
#' @return A nif object.
#' @export
#' @examples
#' nif()
nif <- function(obj = NULL, ..., silent = NULL) {
  # Case 1: Empty minimal nif object
  if (is.null(obj)) {
    # empty nif object
    fields <- c("REF", minimal_nif_fields)
    temp <- data.frame(matrix(nrow = 0, ncol = length(fields)))
    colnames(temp) <- fields
    class(temp) <- c("nif", "data.frame")

    return(order_nif_columns(temp))
  }

  # Case 2: Nif object from a sdtm object using nif_auto()
  if (inherits(obj, "sdtm")) {
    temp <- nif_auto(obj, ..., silent = silent) |>
      arrange_and_add_ref() |>
      order_nif_columns()

    class(temp) <- c("nif", "data.frame")
    return(temp)
  }

  # Error case: neither nif or data.frame
  if (!is.data.frame(obj)) {
    stop("obj must be a data frame or sdtm object")
  }

  # Case 3: Nif object from nif object or from data frame
  # validate inputs
  missing_min_fields <- setdiff(minimal_nif_fields, names(obj))
  if (length(missing_min_fields) > 0) {
    stop(paste0(
      "Missing essential fields: ",
      nice_enumeration(missing_min_fields)
    ))
  }

  if (any(is.na(obj$ID))) {
    stop("ID colum must not contain NA values!")
  }

  # Check correct type for essential columns
  min_obj <- obj |>
    select(all_of(minimal_nif_fields))
  non_num_fields <- names(min_obj)[!unlist(lapply(min_obj, numeric_or_na))]
  if (length(non_num_fields) > 0) {
    stop(paste0(
      "Non-numeric essential fields: ",
      nice_enumeration(non_num_fields)
    ))
  }

  # Case 4: Nif object from nif object
  if (inherits(obj, "nif")) {
    out <- obj |>
      arrange_and_add_ref() |>
      order_nif_columns()

    return(out)
  }

  # Case 5: Nif object from data frame
  if (inherits(obj, "data.frame")) {
    # make ID only if ID is not yet present
    if (!"ID" %in% names(obj))
      obj <- index_id(obj)

    out <- obj |>
      arrange_and_add_ref() |>
      order_nif_columns()

    class(out) <- c("nif", "data.frame")
    return(out)
  }

  stop("obj must be a data frame or sdtm object")
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
    cat(str_wrap(
      paste(names(x), collapse = ", "),
      width = 80, indent = 2, exdent = 2
    ), "\n\n")

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
#' head(subjects(examplinib_fe_nif))
#' head(subjects(examplinib_poc))
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
#' head(subjects(examplinib_fe_nif))
subjects.nif <- function(obj) {
  # input validation
  validate_nif(obj)

  if (!"USUBJID" %in% names(obj)) {
    obj <- mutate(obj, USUBJID = NA)
  }

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
  # validate_numeric_param(id, "id", allow_multiple = TRUE, allow_null = TRUE)
  validate_argument(id, "numeric", allow_multiple = TRUE, allow_null = TRUE)
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
#' @param analyte The treatment of interest as character. Automatically selects
#' the treatment if NULL but fails if there are multiple treatments.
#' @return A data frame with the ID and, if available, the USUBJID of subjects
#' with dose reductions.
#' @export
#' @examples
#' dose_red_sbs(examplinib_poc_nif)
#' dose_red_sbs(examplinib_poc_nif, "RS2023")
#'
dose_red_sbs <- function(obj, analyte = NULL) {
  # input validation
  validate_nif(obj)
  validate_argument(analyte, "character", allow_null = TRUE)

  obj <- ensure_analyte(obj)

  available_treatments <- treatments(obj)

  if (is.null(analyte)) {
    if (length(available_treatments) == 0)
      stop("No treatment found!")

    if (length(available_treatments) == 1)
      analyte <- available_treatments

    if (length(available_treatments) > 1)
      stop(paste0(
        "Multiple treatments in data set (",
        nice_enumeration(available_treatments),
        "). Please specify exactly one treatment."))

  }

  if (!analyte %in% available_treatments)
    stop(paste0("Treatment ", analyte, " not found!"))

  # if (!is.null(analyte)) {
    obj <- obj |>
      filter(.data$ANALYTE %in% analyte)
  # }

  obj |>
    as.data.frame() |>
    filter(.data$ANALYTE %in% analyte) |>
    arrange_and_add_ref() |>
    filter(.data$EVID == 1) |>
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
  n = 4
) {
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

  if ("DOSE" %in% names(obj)) {
    return(unique(obj$DOSE))
  }

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
        values_from = "AMT",
        values_fill = 0
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


#' Analyte-to-compartment mapping
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @param obj A NIF object
#' @return A data frame
#' @export
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
    arrange_and_add_ref() |>
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
#' head(n_administrations(examplinib_poc_nif))
#' head(n_administrations(examplinib_poc_min_nif))
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
  only_observations = TRUE
) {
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
#' head(add_obs_per_dosing_interval(examplinib_poc_nif))
#' head(add_obs_per_dosing_interval(examplinib_poc_min_nif))
add_obs_per_dosing_interval <- function(obj) {
  obj |>
    arrange_and_add_ref() |>
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
#'   examplinib_poc_nif,
#'   subjects(examplinib_poc_nif)[1, "USUBJID"]
#' ) |>
#' head()
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
