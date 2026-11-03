#' Check if a string matches ISO 8601 date-time format
#'
#' This function checks whether a character string complies with the ISO 8601
#' standard for date-time representation (combined date and time). Unlike the
#' more general `is_iso8601_format()` function, this specifically checks for the
#' presence of both date and time components.
#'
#' Valid formats include:
#' - Extended format with separators: "2023-10-15T14:30:00"
#' - Basic format without separators: "20231015T143000"
#' - With timezone information: "2023-10-15T14:30:00Z" or
#' "2023-10-15T14:30:00+02:00"
#' - With fractional seconds: "2023-10-15T14:30:00.123"
#' - Using space instead of T separator: "2023-10-15 14:30:00" (non-strict mode
#' only)
#'
#' @param x A character string or vector of strings to check.
#' @param strict Logical, whether to strictly enforce ISO 8601 specification.
#'   Default is FALSE, which allows some common variations like space instead of
#'   'T' separator.
#'
#' @return Logical value indicating whether the string is in ISO 8601 date-time
#'   format.
#' @noRd
#'
#' @examples
#' is_iso8601_datetime("2023-10-15T14:30:00") # TRUE
#' is_iso8601_datetime("2023-10-15 14:30:00") # TRUE (with default strict=FALSE)
#' is_iso8601_datetime("2023-10-15 14:30:00", TRUE) # FALSE (with strict=TRUE)
#' is_iso8601_datetime("2023-10-15T14:30:00Z") # TRUE
#' is_iso8601_datetime("2023-10-15T14:30:00+02:00") # TRUE
#' is_iso8601_datetime("20231015T143000") # TRUE
#' is_iso8601_datetime("2023-10-15") # FALSE (no time component)
#' is_iso8601_datetime("14:30:00") # FALSE (no date component)
is_iso8601_datetime <- function(x, strict = FALSE) {
  if (!is.character(x)) {
    stop("Input must be a character string")
  }

  # If x is NA, return NA
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  # Date patterns
  date_extended <- "\\d{4}-\\d{2}-\\d{2}" # YYYY-MM-DD
  date_basic <- "\\d{4}\\d{2}\\d{2}" # YYYYMMDD

  # Time patterns
  time_extended <- "\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?"
  time_basic <- "\\d{2}\\d{2}\\d{2}(?:\\.\\d+)?"

  # Timezone pattern
  timezone <- "(?:Z|[+-]\\d{2}(?::\\d{2}|\\d{2}))?"

  # Separators
  strict_separator <- "T"
  relaxed_separator <- "[ T]"
  separator <- if (strict) strict_separator else relaxed_separator

  # Combined patterns
  # Extended format: YYYY-MM-DDThh:mm:ss(.sss)(Z|+/-hh:mm)
  datetime_extended <- paste0(
    "^", date_extended, separator, time_extended, timezone, "$"
  )

  # Basic format: YYYYMMDDThhmmss(.sss)(Z|+/-hhmm)
  datetime_basic <- paste0(
    "^", date_basic, strict_separator, time_basic, timezone, "$"
  )

  # Mix of extended date with basic time: YYYY-MM-DDThhmmss(.sss)(Z|+/-hhmm)
  datetime_mixed1 <- paste0(
    "^", date_extended, separator, time_basic, timezone, "$"
  )

  # Mix of basic date with extended time: YYYYMMDDThh:mm:ss(.sss)(Z|+/-hh:mm)
  datetime_mixed2 <- paste0(
    "^", date_basic, strict_separator, time_extended, timezone, "$"
  )

  # For each element in the input vector
  result <- sapply(x, function(str) {
    if (is.na(str)) {
      return(NA)
    }

    # Check if the string matches any of the patterns
    grepl(datetime_extended, str) ||
      grepl(datetime_basic, str) ||
      grepl(datetime_mixed1, str) ||
      grepl(datetime_mixed2, str)
  })

  as.logical(result)
}


#' Check if a string matches ISO 8601 date format
#'
#' This function checks whether a character string complies with the ISO 8601
#' standard for date representation (without time components). Unlike the more
#' general `is_iso8601_format()` function or the `is_iso8601_datetime()`
#' function, this specifically validates date-only formats.
#'
#' Valid formats include:
#' - Extended format with separators: "2023-10-15"
#' - Basic format without separators: "20231015"
#' - Reduced precision (year-month): "2023-10" or "202310"
#' - Reduced precision (year only): "2023"
#'
#' @param x A character string or vector of strings to check.
#' @param allow_reduced_precision Logical, whether to allow reduced precision
#'   formats (year-month or year only). Default is TRUE.
#'
#' @return Logical value indicating whether the string is in ISO 8601 date
#'   format.
#' @noRd
#'
#' @examples
#' is_iso8601_date("2023-10-15") # TRUE
#' is_iso8601_date("20231015") # TRUE
#' is_iso8601_date("2023-10") # TRUE
#' is_iso8601_date("2023") # TRUE
#' is_iso8601_date("2023-10", FALSE) # FALSE
#' is_iso8601_date("2023/10/15") # FALSE (not ISO 8601 format)
#' is_iso8601_date("2023-10-15T14:30:00") # FALSE (has time component)
is_iso8601_date <- function(x, allow_reduced_precision = TRUE) {
  if (!is.character(x)) {
    stop("Input must be a character string")
  }

  # If x is NA, return NA
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  # Date patterns
  date_extended <- "^\\d{4}-\\d{2}-\\d{2}$" # YYYY-MM-DD
  date_basic <- "^\\d{4}\\d{2}\\d{2}$" # YYYYMMDD

  # Reduced precision date patterns (if allowed)
  year_month_extended <- "^\\d{4}-\\d{2}$" # YYYY-MM
  year_month_basic <- "^\\d{4}\\d{2}$" # YYYYMM
  year_only <- "^\\d{4}$" # YYYY

  # For each element in the input vector
  result <- sapply(x, function(str) {
    if (is.na(str)) {
      return(NA)
    }

    # Check if the string matches full date patterns
    is_full_date <- grepl(date_extended, str) || grepl(date_basic, str)

    # If it's a full date or we don't allow reduced precision, return result
    if (is_full_date || !allow_reduced_precision) {
      return(is_full_date)
    }

    # Otherwise also check reduced precision formats
    is_reduced_precision <- grepl(year_month_extended, str) ||
      grepl(year_month_basic, str) ||
      grepl(year_only, str)

    is_full_date || is_reduced_precision
  })

  # Ensure logical return type
  as.logical(result)
}


#' Check ISO 8601 PT format
#'
#' @param x A character string or vector of strings to check.
#'
#' @returns A logical vector.
#' @noRd
#'
#' @examples
#' is_iso8601_pt("PT10M")
is_iso8601_pt <- function(x) {
  if (!is.character(x)) {
    stop("Input must be a character string")
  }

  # ISO 8601 PT pattern
  pt_pattern <- "^-?PT([0-9.]*H)?([0-9]*M)?([0-9]*S)?$"

  result <- sapply(x, function(s) {
    if (is.na(s))
      return(NA)

    grepl(pt_pattern, s)
  })

  as.logical(result)
}


#' The list of expected date/time formats as per ISO 8601
#' @keywords internal
dtc_formats <- c(
  "%Y-%m-%dT%H:%M",
  "%Y-%m-%d",
  "%Y-%m",
  "%Y-%m-%dT%H:%M:%S",
  "%Y",
  "%Y-%m-%d %H:%M",
  "%Y-%m-%d %H:%M:%S"
)


#' Convert date fields to POSIX format
#'
#' Convert date-time code (DTC) variables from the ISO
#' 8601 (https://w.wiki/8Bzr) format used in SDTM (i.e., something like
#' "2001-01-02T09:59" where date and time are separated by "T") to standard
#' POSIXct format. The names of the variables to be converted need to be
#' provided by `fields`.
#' @param obj A data frame.
#' @param fields Date variable names as character.
#' @return A data frame
#' @noRd
standardize_date_format <- function(obj, fields = NULL) {
  obj |>
    dplyr::mutate_at(fields, function(x) {
      lubridate::as_datetime(x, format = dtc_formats)
    })
}


#' Convert all date fields to ISO 8601 format
#'
#' Change date-time columns in the input data frame form POSIXct to ISO 8601
#' format.
#' @param obj A data frame.
#' @param fields Date variable names as character.
#' @return A data frame.
#' @keywords internal
#' @noRd
isofy_date_format <- function(obj, fields = NULL) {
  obj |>
    dplyr::mutate_at(fields, function(x) {
      format(x, "%Y-%m-%dT%H:%M")
    })
}


#' Convert all DTC fields from ISO 8601 into POSIXct
#'
#' Change all columns in the input data frame that end with 'DTC' to standard
#' POSIXct format.
#'
#' @param obj A data frame.
#' @param col Columns to convert. Defaults to all columns ending in "DTC", if
#' NULL.
#'
#' @return A data frame.
#' @noRd
lubrify_dates <- function(obj, col = NULL) {
  # input validation
  validate_df_argument(obj)
  validate_argument(col, "character", allow_multiple = TRUE, allow_null = TRUE)

  if (!is.null(col)) {
    missing_columns <- setdiff(col, names(obj))
    n_missing <- length(missing_columns)
    if (n_missing > 0) {
      stop(paste0(
        plural("Column", n_missing > 1), " not found in data frame: ",
        nice_enumeration(missing_columns)
      ))
    }
    obj |>
      mutate(across(all_of(col), ~ as_datetime(.x, format = dtc_formats)))
  } else {
    obj |>
      dplyr::mutate_at(
        vars(ends_with("DTC")),
        function(x) {
          if (all(is.na(x)))
            return(as_datetime(x))

          if (!is.POSIXct(x)) {
            x <- lubridate::as_datetime(x, format = dtc_formats)
          }
          x
        }
      )
  }
}


#' Convert all DTC fields into ISO 8601 string format
#'
#' Change all columns in the input data frame that end with 'DTC' from POSIXct
#' to character using the ISO 8601 format. Seconds will be ignored, the
#' resolution is only to minutes.
#'
#' @param obj A data frame.
#' @return A data frame.
#' @keywords internal
#' @noRd
isofy_dates <- function(obj) {
  obj |>
    dplyr::mutate_at(vars(ends_with("DTC")), ~ format(., "%Y-%m-%dT%H:%M"))
}


#' Test whether string represents ISO 8601-formatted date-time
#'
#' The expected format is "dddd-dd-ddTdd:dd" with "d" a digit. This function
#' tests whether the above is part of the input, i.e., date-time formats that
#' also include seconds information are also recognized.
#' @param x The input as character.
#' @return Boolean.
#' @noRd
#' @examples
#' is_iso_date_time("2023-09-27T15:04")
#' is_iso_date_time("2023-09-27T15:04:00")
#' is_iso_date_time(c("2023-03-21T11:55", "2023-07-18"))
is_iso_date_time <- function(x) {
  str_detect(x, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}")
}


#' Test whether string represents ISO 8601-formatted date
#'
#' The expected format is "dddd-dd-dd" with "d" a digit. This function tests
#' whether the above is part of the input, i.e., ISO 8601-formatted date-time
#' objects like "dddd-dd-ddTdd:dd" are also recognized. Dates with missing day
#' of month are not accepted.
#' @param x The input as character.
#' @return Boolean.
#' @noRd
#' @examples
#' is_iso_date("2023-09-27")
#' is_iso_date(c("2023-03-21T11:55", "2023-07-18"))
is_iso_date <- function(x) {
  str_detect(x, "\\d{4}-\\d{2}-\\d{2}")
}


#' Convert ISO 8601-formatted duration to hours
#'
#' @param iso The duration as ISO 8601-formatted string.
#'
#' @return Duration in hours.
#' @noRd
#' @examples
#' pt_to_hours(c("PT1H15M", "PT1.5H", "-PT4H30M"))
pt_to_hours <- function(iso) {
  temp <- str_extract(
    iso,
    regex("(-)?PT(([0-9.]*)H)?(([0-9.]*)M)?"),
    group = c(1, 3, 5)
  )
  # length-1 input returns a vector; coerce to a 1-row matrix
  if (is.null(dim(temp))) {
    temp <- matrix(temp, nrow = 1)
  }
  colnames(temp) <- c("SIGN", "H", "MIN")

  as.data.frame(temp) |>
    mutate(sign = case_when(
      .data$SIGN == "-" ~ -1,
      .default = 1
    )) |>

    mutate(hours = case_when(
      is.na(.data$H) ~ 0,
      .default = as.numeric(.data$H)
    )) |>
    mutate(mins = case_when(
      is.na(.data$MIN) ~ 0,
      .default = as.numeric(.data$MIN)
    )) |>
    mutate(out = case_when(
      is.na(.data$H) & is.na(.data$MIN) ~ NA,
      .default = .data$sign * (.data$hours + .data$mins / 60)
    )) |>
    pull(.data$out)
}


#' Convert nominal time labels to hours
#'
#' PCTPT is sponsor-defined text and not drawn from a CDISC CT codelist. This is
#' why there is variation across studies and sponsors. This function attempts to
#' derive the nominal sampling time from the PCTPT field based on common
#' patterns.
#'
#' @param x A character vector.
#'
#' @returns A numerical vector with NTIME in hours. Time labels not recognized
#' are NA.
#' @noRd
pctpt_to_hours <- function(x) {
  x <- toupper(x)
  temp <- data.frame(
    pre = ifelse(stringr::str_detect(x, "^PRE[- ]?DOSE$"), 0, NA),
    h = as.numeric(stringr::str_extract(
      x, "([0-9]+\\.?[0-9]*)\\s*(H|HR|HRS|HOUR|HOURS)", group = 1)),
    m = as.numeric(stringr::str_extract(
      x, "([0-9]+\\.?[0-9]*)\\s*(M|MIN|MINS|MINUTE|MINUTES)", group = 1)) / 60
  ) |>
    mutate(NTIME = case_when(
      is.na(.data$pre) & is.na(.data$h) & is.na(.data$m) ~ NA,
      .default = rowSums(across(c("pre", "h", "m")), na.rm = TRUE))
    )

  temp$NTIME
}


#' Extract NTIME from PC domain
#'
#' @param pc The PC domain as data frame.
#'
#' @returns A numeric vector of NTIMES.
#' @noRd
extract_pc_ntime <- function(pc) {
  if (!is.data.frame(pc))
    stop("input must be a data frame")

  # first priority: PCELTM
  if ("PCELTM" %in% names(pc)) {
    return(pt_to_hours(pc$PCELTM))
  }

  # second priority: PCTPT
  if ("PCTPT" %in% names(pc)) {
    return(pctpt_to_hours(pc$PCTPT))
  }

  # otherwise fail
  return(rep(NA_real_, nrow(pc)))
}



#' Compose DTC from date and time components
#'
#' Convert the date and time provided separately as character to a POSIXct
#' date-time object.
#'
#' @param date A date in POSIX or character format.
#' @param time A time in character format.
#'
#' @return A POSIXct object.
#' @keywords internal
#' @noRd
#' @examples
#' compose_dtc(date = "2022-09-29", time = "09:30")
compose_dtc <- function(date, time) {
  n_date <- length(date)
  n_time <- length(time)
  if (n_date != n_time) {
    if (n_date == 1L && n_time > 0L) {
      date <- rep(date, n_time)
    } else if (n_time == 1L && n_date > 0L) {
      time <- rep(time, n_date)
    } else {
      stop(
        "arguments imply differing number of rows: ",
        n_date, ", ", n_time
      )
    }
  }

  date <- as.character(date)
  time <- as.character(time)
  time[is.na(time)] <- ""
  stamp <- str_trim(paste(date, time))

  tryCatch(
    lubridate::as_datetime(
      stamp,
      format = c("%Y-%m-%d %H:%M", "%Y-%m-%d")
    ),
    warning = function(w) {
      message(paste0("Warning from composing DTC: ", w))
    }
  )
}


#' Decompose DTC field into date and time components
#'
#' For each `dtc_field`, adds `{field}_date` and `{field}_time`. If both
#' companion columns already exist, that field is left unchanged (fast path).
#' If only one companion exists, both are dropped and recomputed from the DTC
#' column.
#'
#' @param obj A data frame.
#' @param dtc_field The field to decompose as character.
#'
#' @return A data frame.
#' @noRd
decompose_dtc <- function(obj, dtc_field) {
  # input validation
  if (!is.data.frame(obj)) {
    stop("obj must be a data frame!")
  }
  validate_argument(dtc_field, "character", allow_multiple = TRUE)

  missing_fields <- setdiff(dtc_field, names(obj))
  n_missing <- length(missing_fields)
  if (n_missing > 0) {
    stop(paste0(
      plural("Column", n_missing > 1), " not found in obj: ",
      nice_enumeration(missing_fields)
    ))
  }

  # business logic
  date_cols <- paste0(dtc_field, "_date")
  time_cols <- paste0(dtc_field, "_time")
  has_date <- date_cols %in% names(obj)
  has_time_col <- time_cols %in% names(obj)
  complete <- has_date & has_time_col
  partial <- (has_date | has_time_col) & !complete

  if (any(partial)) {
    obj <- select(obj, -any_of(c(date_cols[partial], time_cols[partial])))
  }

  to_compute <- dtc_field[!complete]
  if (length(to_compute) == 0L) {
    return(obj)
  }

  tryCatch(
    obj <- mutate(
      obj,
      across(
        all_of(to_compute),
        list(
          date = extract_date,
          time = function(x) {
            if_else(has_time(x), extract_time(x), NA_character_)
          }
        ),
        .names = "{.col}_{.fn}"
      )
    ),
    warning = function(w) {
      message(paste0("Warning decomposing DTC: ", w))
    }
  )

  obj
}


#' Extract the date component of a POSIXct object
#'
#' @param dtc The POSIX-formatted datetime.
#' @return The date as character.
#' @keywords internal
#' @noRd
extract_date <- function(dtc) {
  format(dtc, format = "%Y-%m-%d")
}


#' Extract time component of a POSIXct object
#'
#' @param dtc The POSIX-formatted datetime.
#' @return The time as character.
#' @keywords internal
#' @noRd
extract_time <- function(dtc) {
  format(dtc, format = "%H:%M")
}


#' Check whether POSIX datetime object includes time information
#'
#' @param obj POSIX datetime object.
#'
#' @return A Boolean value.
#' @keywords internal
#' @noRd
has_time <- function(obj) {
  if (is.POSIXct(obj)) {
    as.numeric(obj) %% 86400 != 0
  } else {
    grepl(".*T[0-9]{2}:[0-9]{2}", obj)
  }
}


#' Convert trial day to elapsed days
#'
#' This function corrects for trial day 1 actually indicating zero elapsed days
#' since the treatment start.
#'
#' @param x The trial day as numeric.
#'
#' @return The number of elapsed days as numeric.
#' @noRd
#'
trialday_to_day <- function(x) {
  if (any(x[which(!is.na(x))] == 0)) stop("Trial day cannot be zero!")
  x + (x > 0) * -1
}
