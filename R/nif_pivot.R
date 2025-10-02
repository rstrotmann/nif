#' Convert nif object to wide data frame by NTIME
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param analyte Analytes to include, defaults to all, if NULL.
#' @param duplicates Selection how to deal with duplicate observations with
#'   respect to the ID, ANALYTE, NTIME and TRTDY fields:
#'   * 'stop': Stop execution and produce error message
#'   * 'ignore': Include duplicates in the data set
#'   * 'identify': Return a list of duplicate entries
#'   * 'resolve': Resolve duplicates, applying the `duplicate_function` to the
#'   duplicate entries.
#' @param duplicate_function Function to resolve duplicate values, defaults to
#'   `mean`.
#' @param silent Suppress messages.
#' @param keep Fields to keep in pivoted output. Defaults to standard subject-
#'   level covariates, if NULL.
#'
#' @returns A data frame.
#' @export
pivot_analytes <- function(
    obj,
    analyte = NULL,
    keep = NULL,
    duplicates = "stop",
    duplicate_function = mean,
    silent = NULL) {
  # input validation
  validate_nif(obj)
  validate_char_param(
    analyte, "analyte", allow_null = TRUE, allow_multiple = TRUE)
  validate_logical_param(silent, "silent", allow_null = TRUE)
  validate_char_param(keep, "keep", allow_null = TRUE, allow_multiple = TRUE)

  valid_duplicate_values <- c("stop", "identify", "resolve")
  if(!duplicates %in% valid_duplicate_values)
    stop(paste0(
      "Invalid value for 'duplicates' - must be one of ",
      nice_enumeration(valid_duplicate_values, conjunction = "or")))

  # assemble keep fields
  missing_keep <- setdiff(keep, names(obj))
  if(length(missing_keep) > 0)
    stop(paste0(
     "Missing keep ", plural("field", length(missing_keep) > 1), ": ",
     nice_enumeration(missing_keep)))

  if(is.null(keep)) {
    basic_keep = c(
      "ID", "STUDYID", "USUBJID", "AGE", "SEX", "RACE", "HEIGHT", "WEIGHT",
      "BMI", "ACTARMCD", "PART", "PERIOD", "COHORT")
    keep <- intersect(
      names(obj),
      unique(c(basic_keep, names(obj)[grepl("BL_.*", names(obj))])))
  }

  # validate keep fields
  temp <- obj %>%
    as.data.frame() %>%
    select(any_of(c("ID", keep))) %>%
    distinct() %>%
    reframe(n = n(), .by = "ID") %>%
    filter(n != 1)
  if(nrow(temp) > 0) {
    stop(paste0(
      "Keep fields (", nice_enumeration(keep), ") are not unique for the ",
      "following subjects: ", nice_enumeration(temp$ID)))
  }

  # ensure NTIME
  if(!"NTIME" %in% names(obj)) {
    stop("'NTIME' not found in nif object!")
  }

  # ensure TRTDY
  obj <- obj %>%
    add_trtdy()

  # prepare observation data
  if(is.null(analyte)) {
    analyte = analytes(obj)
  }
  obs <- obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    filter(ANALYTE %in% analyte) #%>%


  # deal with NTIME of NA
  na_overview <- obs %>%
    reframe(n_obs = n(), n_NA = sum(is.na(NTIME)), .by = c(ANALYTE))
  temp <- filter(na_overview, .data$n_obs == .data$n_NA)
  if(nrow(temp) > 0) {
    message(
      "NTIME is all NA for ", plural("analyte", nrow(temp) > 1), " ",
      nice_enumeration(temp$ANALYTE), "!")
  }
  if(sum(na_overview$n_NA) != 0) {
    conditional_message(
      sum(na_overview$n_NA),
      " observations with NTIME of NA were excluded:\n",
      df_to_string(na_overview, indent = 2), "\n",
      silent = silent
    )
  }
  obs <- filter(obs, !is.na(NTIME))

  # apply exclusions
  if("EXCL" %in% names(obs)) {
    temp <- obs %>%
      filter(.data$EXCL == TRUE) %>%
      reframe(n = n(), .by = any_of(c("ANALYTE", "EXCL_REASON")))

    if(nrow(temp) > 0) {
      conditional_message(
        sum(temp$n), " observations were excluded:\n",
        df_to_string(temp, indent = 2), "\n",
        silent = silent
      )
    }
    obs <- obs %>%
      filter(.data$EXCL == FALSE)
  }

  obs <- obs %>%
    select(any_of(c("ID", keep, "TRTDY", "NTIME", "ANALYTE", "DV")))


  # duplicate handling
  dup <- find_duplicates(
    obs, c("ID", "ANALYTE", "NTIME", "TRTDY"))

  if(!is.null(dup)) {
    dup_overview <- dup %>%
      reframe(n = n(), .by = "ANALYTE")

    if(nrow(dup_overview) > 0) {
      if(duplicates == "stop") {
        stop(paste0(
          "Duplicate observations found with respect to NTIME and TRTDY:\n",
          df_to_string(dup_overview, indent = 2), "\n\n",
          "Identify duplicates using the `duplicates = 'identify'` parameter, ",
          "or have duplicates automatically resolved with `duplicates = 'resolve'` ",
          "where the resolution function is specified by the `duplicate_function` ",
          "parameter (default is `mean`)."
        ))
      }

      if(duplicates == "identify") {
        message(paste0(
          "Duplicate observations found with respect to NTIME and TRTDY:\n",
          df_to_string(dup_overview, indent = 2), "\n\n",
          "Only duplicate observations returned!"))

        return(dup)
      }

      if(duplicates == "resolve") {
        temp <- obs %>%
          select(-any_of(c("TIME", "TAD", "DTC", "TAFD", "TIME_DEV")))

        obs = resolve_duplicates(
          temp,
          fields = c("ID", "ANALYTE", "NTIME", "TRTDY"),
          duplicate_function = duplicate_function,
          na.rm = TRUE)

        conditional_message(
          sum(dup_overview$n),
          " duplicate observations with respect to NTIME and TRTDY ",
          "were resolved:\n",
          df_to_string(dup_overview, indent = 2),
          silent = silent)
      }
    }
  }

  # convert to wide table
  out <- obs %>%
    pivot_wider(
      names_from = ANALYTE, values_from = DV,
      id_cols = setdiff(names(.), c("ANALYTE", "DV"))
    )

  return(out)
}



#' Correlate two observations by their actual observation time
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param indep_analyte The independent analyte as character.
#' @param dep_analyte The dependent analyte as character.
#' @param window The allowed time window between the independent and dependent
#'   analyte observations in hours.
#' @param duplicate_function A function to resolve duplicate values for
#'   observations of the dependent analyte. Defaults to `mean`.
#'
#' @returns A data frame.
#' @export
pivot_analytes1 <- function(
    obj,
    indep_analyte,
    dep_analyte,
    window = 10/60,
    duplicate_function = mean) {
  # validate input
  validate_nif(obj)
  validate_analyte(
    obj, indep_analyte, allow_multiple = FALSE, allow_null = FALSE)
  validate_analyte(
    obj, dep_analyte, allow_multiple = FALSE, allow_null = FALSE)
  validate_numeric_param(window, "window")

  # observations
  obs <- obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    filter(ANALYTE %in% c(indep_analyte, dep_analyte)) %>%
    filter(!is.nan(DV) & !is.na(DV))

  # function definitions
  time_match <- function(x_ref, y) {
    target_dtc <- obs[obs$REF == x_ref, "DTC"]
    dtcs <- y$DTC

    index <- abs(as.numeric(dtcs - target_dtc, units = "hours")) < window
    y_ref <- y[index, "REF"]
    if(length(y_ref) == 0) y_ref = NA
    return(y_ref)
  }

  pivot_line <- function(x_ref, y) {
    y_ref <- time_match(x_ref, y)
    out <- NULL
    if(!all(is.na(y_ref))) {
      yval <- duplicate_function(filter(obs, REF %in% y_ref)$DV, na.rm = TRUE)

      out <- obs %>%
        filter(REF == x_ref) %>%
        rename(.X = DV) %>%
        mutate(.Y = yval)
    }
    return(out)
  }

  x <- filter(obs, ANALYTE == indep_analyte)
  y <- filter(obs, ANALYTE == dep_analyte)

  temp <- bind_rows(lapply(x$REF, function(x) pivot_line(x, y))) %>%
    filter(!is.nan(.data$.X) & !is.nan(.data$.Y)) %>%
    rename(!!indep_analyte := .data$.X) %>%
    rename(!!dep_analyte := .data$.Y)

  return(temp)
}






