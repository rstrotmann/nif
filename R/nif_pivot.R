#' Correlate observations
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param obj A nif object.
#' @param indep_analyte The independent analyte as character.
#' @param dep_analyte The dependent analyte(s) as character.
#' @param window The allowed time window between the independent and dependent
#'   analyte observations in hours.
#' @param time_field Time variable to use for the correlation of observations,
#'   can be 'DTC' or 'TIME' (default).
#' @param duplicate_function A function to resolve duplicate values for
#'   observations of the dependent analyte. Defaults to `mean`.
#'
#' @returns A data frame.
correlate_obs <- function(
    obj,
    indep_analyte,
    dep_analyte,
    window = 10/60,
    time_field = "TIME",
    duplicate_function = mean) {
  # validate input
  validate_nif(obj)
  if(!"REF" %in% names(obj)) {
    stop("Input must contain the 'REF' field!")
  }
  validate_analyte(
    obj, indep_analyte, allow_multiple = FALSE, allow_null = FALSE)
  validate_analyte(
    obj, dep_analyte, allow_multiple = TRUE, allow_null = FALSE)
  validate_numeric_param(window, "window")

  # validate time parameter
  validate_char_param(time_field, "time_field")
  allowed_time_fields <- c("DTC", "TIME")
  if(!time_field %in% allowed_time_fields) {
    stop("'time_field' must be one of: ",
         nice_enumeration(allowed_time_fields, conjunction = "or"))
  }
  if(!time_field %in% names(obj)) {
    stop("Time field '", time_field, "' not found in input!")
  }

  # get observations
  obs <- obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    filter(ANALYTE %in% c(indep_analyte, dep_analyte)) %>%
    filter(!is.nan(DV) & !is.na(DV))

  x <- filter(obs, ANALYTE == indep_analyte)
  y <- filter(obs, ANALYTE %in% dep_analyte)

  # function definitions
  time_match <- function(x_ref) {
    indep <- x[x$REF == x_ref,]
    indep_tm <- indep[[time_field]]
    indep_id <- indep$ID
    target <- y[y$ID == indep_id,]
    target_tm <- target[[time_field]]

    # Handle TIME (numeric) vs DTC (POSIXct) differently
    if(time_field == "TIME") {
      time_matches <- abs(target_tm - indep_tm) < window
    } else {
      time_matches <- abs(
        as.numeric(difftime(target_tm, indep_tm, units = "hours"))) < window
    }

    y_ref <- target[time_matches, "REF"]
    if(length(y_ref) == 0) y_ref = NA
    return(y_ref)
  }

  correlate_line <- function(x_ref) {
    indep <- filter(x, REF == x_ref) %>%
      mutate(!!indep_analyte := DV)
    y_ref <- time_match(x_ref)

    # Return NULL if no match
    if(all(is.na(y_ref))) {
      return(NULL)
    }

    match <- filter(y, REF %in% y_ref)

    temp <- match %>%
      reframe(
        DV = duplicate_function(DV),
        TIME = duplicate_function(TIME), .by = ANALYTE)
    dep <- temp %>%
      pivot_longer(cols = c("DV", "TIME"),
                   names_to = "param", values_to = "value") %>%
      mutate(param = case_match(
        param, "DV" ~ ANALYTE,
        .default = paste(ANALYTE, param, sep = "_"))) %>%
      select(-ANALYTE) %>%
      pivot_wider(names_from = param, values_from = value)
    out <- bind_cols(indep, dep)
    return(out)
  }

  temp <- bind_rows(lapply(x$REF, function(x) correlate_line(x)))
  return(temp)
}




