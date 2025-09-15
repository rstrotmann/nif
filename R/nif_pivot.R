#' Convert nif object to wide data frame by NTIME
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param analyte Analytes to include, defaults to all, if NULL.
#' @param duplicates Selection how to deal with duplicate observations with
#'   respect to the USUBJID, ANALYTE and DTC fields:
#'   * 'stop': Stop execution and produce error message
#'   * 'ignore': Include duplicates in the data set
#'   * 'identify': Return a list of duplicate entries
#'   * 'resolve': Resolve duplicates, applying the `duplicate_function` to the
#'   duplicate entries.
#' @param duplicate_function Function to resolve duplicate values, defaults to
#'   `mean`.
#' @param silent Suppress messages.
#'
#' @returns A data frame.
#' @export
pivot_analytes <- function(
    obj,
    analyte = NULL,
    duplicates = "stop",
    duplicate_function = mean,
    silent = NULL) {
  # input validation
  validate_nif(obj)
  validate_char_param(
    analyte, "analyte", allow_null = TRUE, allow_multiple = TRUE)
  validate_logical_param(silent, "silent", allow_null = TRUE)

  valid_duplicate_values <- c("stop", "identify", "resolve")
  if(!duplicates %in% valid_duplicate_values)
    stop(paste0(
      "Invalid value for 'duplicates' - must be one of ",
      nice_enumeration(valid_duplicate_values, conjunction = "or")))

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
    filter(ANALYTE %in% analyte) %>%
    # remove debug fields
    select(-any_of(c("SRC_DOMAIN", "SRC_SEQ")))

  # ensure NTIME
  if(!"NTIME" %in% names(obj)) {
    stop("'NTIME' not found in nif object!")
  }
  na_overview <- obs %>%
    reframe(n_obs = n(), n_NA = sum(is.na(NTIME)), .by = c(ANALYTE))
  temp <- filter(na_overview, n_obs == n_NA)
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
      filter(EXCL == TRUE) %>%
      reframe(n = n(), .by = any_of(c("ANALYTE", "EXCL_REASON")))

    if(nrow(temp) > 0) {
      conditional_message(
        sum(temp$n), " observations were excluded:\n",
        df_to_string(temp, indent = 2), "\n",
        silent = silent
      )
    }
    obs <- obs %>%
      filter(EXCL == FALSE)
  }


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
    select(-any_of(c(
      "MDV", "METABOLITE", "PARENT", "REF",
      "TIME", "TAD", "DTC", "TAFD", "TIME_DEV", "CMT"))) %>%
    pivot_wider(
      names_from = ANALYTE, values_from = DV,
      id_cols = setdiff(names(.), c("ANALYTE", "DV"))
    )

  return(out)
}
