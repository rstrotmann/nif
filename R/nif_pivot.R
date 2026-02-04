#' Correlate observations
#'
#' Identify observations of the dependent variables that are timely correlated
#' to the independent variable, i.e., are within a specified time window to the
#' observations of the independent variable. If multiple dependent observations
#' of a given analyte fall into the window, they are summarized using the
#' 'duplicate_function'. The output data frame is a wide table based on the
#' independent observations, with the (summarized) dependent observations and
#' the respective summarized observation times for the dependent observations as
#' additional columns.
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
#' @return A data frame.
#' @export
correlate_obs <- function(
  obj,
  indep_analyte,
  dep_analyte,
  window = 10 / 60,
  time_field = "TIME",
  duplicate_function = mean
) {
  # validate input
  validate_nif(obj)

  if (!"REF" %in% names(obj)) {
    obj <- arrange_and_add_ref(obj)
  }
  validate_analyte(
    obj, indep_analyte,
    allow_multiple = FALSE, allow_null = FALSE
  )
  validate_analyte(
    obj, dep_analyte,
    allow_multiple = TRUE, allow_null = FALSE
  )
  validate_numeric_param(window, "window")

  # validate time parameter
  validate_char_param(time_field, "time_field")
  allowed_time_fields <- c("DTC", "TIME")
  if (!time_field %in% allowed_time_fields) {
    stop(
      "'time_field' must be one of: ",
      nice_enumeration(allowed_time_fields, conjunction = "or")
    )
  }
  if (!time_field %in% names(obj)) {
    stop("Time field '", time_field, "' not found in input!")
  }

  # get observations
  obs <- obj |>
    ensure_analyte() |>
    as.data.frame() |>
    filter(.data$EVID == 0) |>
    filter(.data$ANALYTE %in% c(indep_analyte, dep_analyte)) |>
    filter(!is.nan(.data$DV) & !is.na(.data$DV))

  x <- filter(obs, .data$ANALYTE == indep_analyte)
  y <- filter(obs, .data$ANALYTE %in% dep_analyte)

  # function definitions
  time_match <- function(x_ref) {
    indep <- x[x$REF == x_ref, ]
    indep_tm <- indep[[time_field]]
    indep_id <- indep$ID
    target <- y[y$ID == indep_id, ]
    target_tm <- target[[time_field]]

    # Handle TIME (numeric) vs DTC (POSIXct) differently
    if (time_field == "TIME") {
      time_matches <- abs(target_tm - indep_tm) < window
    } else {
      time_matches <- abs(
        as.numeric(difftime(target_tm, indep_tm, units = "hours"))
      ) < window
    }

    y_ref <- target[time_matches, "REF"]
    if (length(y_ref) == 0) y_ref <- NA

    y_ref
  }

  correlate_line <- function(x_ref) {
    indep <- filter(x, .data$REF == x_ref) |>
      mutate(.temp_dv = .data$DV)

    names(indep)[names(indep) == ".temp_dv"] <- indep_analyte

    y_ref <- time_match(x_ref)

    # Return NULL if no match
    if (all(is.na(y_ref))) {
      return(NULL)
    }

    match <- filter(y, .data$REF %in% y_ref)

    temp <- match |>
      reframe(
        DV = duplicate_function(.data$DV),
        TIME = duplicate_function(.data$TIME), .by = "ANALYTE"
      )
    dep <- temp |>
      pivot_longer(
        cols = c("DV", "TIME"),
        names_to = "param", values_to = "value"
      ) |>

      # mutate(param = case_match(
      #   .data$param, "DV" ~ .data$ANALYTE,
      #   .default = paste(.data$ANALYTE, .data$param, sep = "_")
      # )) |>

      mutate(param = recode_values(
        .data$param, "DV" ~ .data$ANALYTE,
        default = paste(.data$ANALYTE, .data$param, sep = "_")
      )) |>

      select(-c("ANALYTE")) |>
      pivot_wider(names_from = "param", values_from = "value")
    bind_cols(indep, dep)
  }

  bind_rows(lapply(x$REF, function(x) correlate_line(x)))
}
