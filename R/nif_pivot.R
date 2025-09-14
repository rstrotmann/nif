#' Convert nif object to wide data frame by NTIME
#'
#' @param obj A nif object.
#' @param analyte Analytes to include, defaults to all, if NULL.
#' @param duplicates
#' @param duplicate_function
#' @param silent
#'
#' @returns A data frame.
#' @export
#'
#' @examples
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
    filter(ANALYTE %in% analyte)

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
      "Some observations with NTIME of NA were excluded:\n",
      df_to_string(na_overview, indent = 2),
      silent = silent
    )
  }
  obs <- filter(obs, !is.na(NTIME))

  # duplicate handling
  dup <- find_duplicates(
    obs, c("ID", "ANALYTE", "NTIME", "TRTDY"))

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

      f <- function(x) {
        if(na.rm == TRUE)
          duplicate_function(x[!is.na(x)])
        else
          duplicate_function(x)
      }

      # result <- temp %>%
      #   select(c("ID", "ANALYTE", "NTIME", "TRTDY", "DV")) %>%
      #   reframe(
      #     DV = f(DV),
      #     .by = all_of(setdiff(names(temp), c("DV")))
      #   )

      result <- temp %>%
        reframe(
          DV = f(DV),
          # .by = all_of(c("ID", "ANALYTE", "NTIME", "TRTDY"))
          .by = all_of(setdiff(names(.), c("DV", "REF")))
        )



      observation = resolve_duplicates(
        temp,
        fields = c("ID", "ANALYTE", "NTIME", "TRTDY"),
        duplicate_function = duplicate_function,
        na.rm = TRUE)
      conditional_message(
        "In observations for ", testcd, " (analyte '", analyte, "'), ",
        n_dupl, " duplicates were resolved!",
        silent = silent
      )
    }
  }








  # convert to wide table
  out <- obs %>%
    select(ID, NTIME, ANALYTE, DV, TRTDY) %>%
    pivot_wider(names_from = ANALYTE, values_from = DV)


}
