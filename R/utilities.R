#' Issue message based on silent flag
#'
#' @param ... Message components.
#' @param silent Logical flag to suppress messages. If NULL, will check .nif_env
#'              environment (defaults to FALSE if not found).
#' @return Nothing.
#' @keywords internal
conditional_message <- function(..., silent = NULL) {
  # Get silent flag from parameter or environment
  if (is.null(silent)) {
    silent <- tryCatch({
      get("silent", envir = .nif_env)
    }, error = function(e) {
      FALSE
    })
  }

  # Safely convert arguments to character
  args <- tryCatch({
    lapply(list(...), as.character)
  }, error = function(e) {
    warning("Failed to convert some arguments to character")
    list(...)
  })

  # Print message if not silent
  if (!isTRUE(silent)) {
    message(paste(args, collapse = ""))
  }

  invisible(NULL)
}


#' Debug output
#'
#' Print an object only if the global option 'debug' is `TRUE`.
#'
#' @param obj An object.
#'
#' @return Nothing
#' @export
#' @seealso [nif_option()]
print_debug <- function(obj) {
  if (rlang::is_true(nif_option_value("debug"))) print(obj)
}


#' Re-code SEX field in a data frame
#'
#' @param obj The data.frame containing a SEX field
#' @return The output data frame with SEX coded as:
#'   - 0: "M", "男", "0"
#'   - 1: "F", "女", "1"
#'   - NA: Any other values (with warning)
#' @import dplyr
#' @keywords internal
recode_sex <- function(obj) {
  # Input validation
  if (!is.data.frame(obj)) {
    stop("Input must be a data frame")
  }
  if (!"SEX" %in% names(obj)) {
    stop("Input data frame must contain 'SEX' column")
  }

  # Store original values for warning message
  orig_vals <- unique(obj$SEX[!is.na(obj$SEX)])

  result <- obj %>%
    mutate(SEX = as.numeric(
      case_match(str_trim(toupper(as.character(.data$SEX))),
        "M" ~ 0, "F" ~ 1, "1" ~ 1, "0" ~ 0,
        "\u7537" ~ 0, "\u5973" ~ 1,  # 男, 女
        .default = NA
      )
    ))

  # Warn about invalid values that were converted to NA
  valid_vals <- c("m", "f", "M", "F", "0", "1", "\u7537", "\u5973")
  invalid_vals <- setdiff(orig_vals, valid_vals)
  if (length(invalid_vals) > 0) {
    warning("Invalid sex values converted to NA: ",
            paste(invalid_vals, collapse = ", "))
  }

  return(result)
}


#' Positive value or zero if negative
#'
#' @param x Numeric.
#' @return Numeric.
#' @export
#' @keywords internal
#' @examples
#' positive_or_zero(2)
#' positive_or_zero(-2)
#' positive_or_zero(c(2, 1, 0, -1, -2))
positive_or_zero <- function(x) {
  x[which(x < 0 | is.na(x))] <- 0
  return(x)
}


#' Convert indent level to padding string of spaces
#'
#' @param indent The indent level as numeric.
#'
#' @return A character string
#' @keywords internal
indent_string <- function(indent = 0) {
  paste(replicate(positive_or_zero(indent), " "), collapse = "")
}


#' Render data frame object to string
#'
#' This function renders a data.frame into a string similar to its
#' representation when printed without line numbers
#'
#' @param df The data.frame to be rendered.
#' @param indent Indentation level, as numeric.
#' @param header Boolean to indicate whether the header row is to be included.
#' @param color Print headers in grey as logical.
#' @param n The number of lines to be included, or all if NULL.
#' @param show_none Show empty data frame as 'none', as logical.
#' @param header_sep Show separation line after header, as logical.
#' @param na_string String to use for NA values. Defaults to "NA".
#'
#' @return The output as string.
#' @import utils
#' @keywords internal
df_to_string <- function(
    df,
    indent = 0,
    n = NULL,
    header = TRUE,
    header_sep = FALSE,
    color = FALSE,
    show_none = FALSE,
    na_string = "NA"
    ) {

  # Input validation
  if(is.null(df))
    return("")

  # if (!is.data.frame(df) & !is_tibble(df)) {
  if(!inherits(df, "data.frame")){
    stop("Input must be a data frame")
  }


  if (!is.numeric(indent) || indent < 0) {
    stop("Indent must be a non-negative number")
  }

  # Handle empty data frame early
  if (nrow(df) == 0) {
    if (show_none) {
      return(paste0(indent_string(indent), "none\n"))
    }
    return("")
  }

  # Convert all columns to character, handling NA values
  df <- as.data.frame(df) %>%
    mutate(across(everything(), ~ifelse(is.na(.), na_string, as.character(.))))

  # Calculate maximum width for each column including headers
  max_widths <- sapply(
    seq_along(df),
    function(i) max(
      nchar(names(df)[i]),
      max(nchar(as.character(df[[i]])), na.rm = TRUE)
    )
  )

  # Create the padding function
  pad_element <- function(element, width) {
    sprintf(paste0("%-", width, "s   "), element)
  }

  # Create line renderer
  render_line <- function(line) {
    paste0(
      indent_string(indent),
      paste0(
        mapply(
          pad_element,
          element = as.character(line),
          width = max_widths
        ),
        collapse = ""
      )
    )
  }

  # Build output starting with header if requested
  output_parts <- character(0)

  if (header) {
    header_line <- render_line(names(df))
    if (color) {
      header_line <- paste0("\u001b[38;5;248m", header_line, "\u001b[0m")
    }
    output_parts <- c(output_parts, header_line)

    if (header_sep) {
      separator <- paste0(
        indent_string(indent),
        paste(mapply(function(w) paste(rep("-", w), collapse = ""),
                    max_widths),
              collapse = "   ")
      )
      output_parts <- c(output_parts, separator)
    }
  }

  # Add data rows
  data_rows <- if (!is.null(n)) utils::head(df, n = n) else df
  row_strings <- apply(data_rows, 1, render_line)
  output_parts <- c(output_parts, row_strings)

  # Combine all parts with newlines
  paste(output_parts, collapse = "\n")
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
#' Convert date-time code (DTC) variables from the [ISO
#' 8601](https://w.wiki/8Bzr) format used in SDTM (i.e., something like
#' "2001-01-02T09:59" where date and time are separated by "T") to standard
#' POSIXct format. The names of the variables to be converted need to be
#' provided by `fields`.
#' @param obj A data frame.
#' @param fields Date variable names as character.
#' @return A data frame
#' @export
standardize_date_format <- function(obj, fields = NULL) {
  obj %>%
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
isofy_date_format <- function(obj, fields = NULL) {
  obj %>%
    dplyr::mutate_at(fields, function(x) {
      format(x, "%Y-%m-%dT%H:%M")
    })
}


#' Convert all DTC fields from ISO 8601 into POSIXct
#'
#' Change all columns in the input data frame that end with 'DTC' to standard
#' POSIXct format.
#' @param obj A data frame.
#' @return A data frame.
#' @seealso [isofy_dates()]
#' @keywords internal
lubrify_dates <- function(obj) {
  obj %>% dplyr::mutate_at(
    vars(ends_with("DTC")),
    function(x) {
      if (!is.POSIXct(x)) {
        x <- lubridate::as_datetime(x, format = dtc_formats)
      }
      return(x)
    }
  )
}


#' Convert all DTC fields into ISO 8601 string format
#'
#' Change all columns in the input data frame that end with 'DTC' from POSIct to
#' character using the ISO 8601 format. Seconds will be ignored, the resolution
#' is only to minutes.
#'
#' @param obj A data frame.
#' @return A data frame.
#' @seealso [lubrify_dates()]
#' @keywords internal
isofy_dates <- function(obj) {
  obj %>%
    dplyr::mutate_at(vars(ends_with("DTC")), ~ format(., "%Y-%m-%dT%H:%M"))
}


#' Test whether string represents ISO 8601-formatted date-time
#'
#' The expected format is "dddd-dd-ddTdd:dd" with "d" a digit. This function
#' tests whether the above is part of the input, i.e., date-time formats that
#' also include seconds information are also recognized.
#' @param x The input as character.
#' @return Boolean.
#' @export
#' @keywords internal
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
#' @export
#' @keywords internal
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
#' @export
#' @keywords internal
#' @examples
#' pt_to_hours(c("PT1H15M", "PT1.5H", "-PT4H30M"))
pt_to_hours <- function(iso) {
  temp <- str_extract(
    iso,
    regex("(-)?PT(([0-9.]*)H)?(([0-9.]*)M)?"),
    group = c(1, 3, 5))

  # if(nrow(temp) == 0)
  #   return(NULL)

  as.data.frame(temp) %>%
    mutate(sign = case_match(.[[1]], "-" ~ -1, .default = 1)) %>%
    mutate(hours = case_when(is.na(.[[2]]) ~ 0, .default = as.numeric(.[[2]]))) %>%
    mutate(mins = case_when(is.na(.[[3]]) ~ 0, .default = as.numeric(.[[3]]))) %>%
    mutate(out = case_when(
      is.na(.[[2]]) & is.na(.[[3]]) ~ NA,
      .default = .data$sign * (.data$hours + .data$mins/60))) %>%
    pull(.data$out)
}


#' Compose DTC from date and time components
#'
#' Convert the date and time provided separately as character to a POSIXct
#' date-time object.
#'
#' @param date A date in POSIX or character format.
#' @param time A time in character format.
#'
#' @return A POSICct object.
#' @export
#' @keywords internal
#' @examples
#' compose_dtc(date = "2022-09-29", time = "09:30")
compose_dtc <- function(date, time) {
  data.frame(date = as.character(date), time = as.character(time)) %>%
    mutate(time = case_when(is.na(.data$time) ~ "", .default = .data$time)) %>%
    mutate(DTC = str_trim(paste(as.character(.data$date), .data$time))) %>%
    mutate(DTC = lubridate::as_datetime(.data$DTC,
      format = c("%Y-%m-%d %H:%M", "%Y-%m-%d")
    )) %>%
    pull(.data$DTC)
}


#' Decompose DTC field into date and time components
#'
#' @param obj A data frame.
#' @param DTC_field The field to decompose as character.
#'
#' @return A data frame.
#' @export
#' @keywords internal
decompose_dtc <- function(obj, DTC_field) {
  dec_dtc <- function(fld) {
    DTC_date <- paste0(fld, "_date")
    DTC_time <- paste0(fld, "_time")
    obj %>%
      mutate(has_time = has_time(.data[[fld]])) %>%
      mutate({{ DTC_date }} := extract_date(.data[[fld]])) %>%
      mutate({{ DTC_time }} := case_when(
        .data$has_time == TRUE ~ extract_time(.data[[fld]]),
        .default = NA
      )) %>%
      select(-c("has_time"))
  }

  for (i in DTC_field) {
    obj <- dec_dtc(i)
  }
  return(obj)
}


#' Extract the date component of a POSICct object
#'
#' @param dtc The POSIX-formatted datetime.
#' @return The date as character.
#' @keywords internal
extract_date <- function(dtc) {
  format(dtc, format = "%Y-%m-%d")
}


#' Extract time component of a POSICct object
#'
#' @param dtc The POSIX-formatted datetime.
#' @return The time as character.
#' @keywords internal
extract_time <- function(dtc) {
  format(dtc, format = "%H:%M")
}


#' Check whether POSIX datetime object includes time information
#'
#' @param obj POSIX datetime object.
#'
#' @return A Boolean value.
#' @keywords internal
has_time <- function(obj) {
  if (is.POSIXct(obj)) {
    as.numeric(obj) %% 86400 != 0
  } else {
    grepl(".*T[0-9]{2}:[0-9]{2}", obj)
  }
}

#' Nice enumeration of multiple strings
#'
#' @param items Items to enumerate as character.
#' @param conjunction The conjunction between the last and penultimate items.
#'
#' @return Enumeration as character.
#' @export
#' @keywords internal
#'
#' @examples
#' nice_enumeration("A")
#' nice_enumeration(c("A", "B"))
#' nice_enumeration(c("A", "B", "C"))
#' nice_enumeration(c("A", "B", "C"), conjunction = "or")
nice_enumeration <- function(items, conjunction = "and") {
  if (length(items) == 1) {
    return(items[[1]])
  }
  if (length(items) > 1) {
    return(paste(
      paste(items[1:length(items) - 1], collapse = ", "), conjunction,
      items[length(items)]
    ))
  }
}


#' Return singular or plural form of word
#'
#' @param word Source word in singular form, as character.
#' @param plural Return plural form, as character.
#'
#' @return Character.
#' @export
#' @keywords internal
#' @examples
#' plural("subject", FALSE)
#' plural("subject", TRUE)
#' plural("study", FALSE)
#' plural("study", TRUE)
plural <- function(word, plural) {
  exceptions = tribble(
    ~singular, ~plural,
    "study", "studies",
    "Study", "Studies"
  )
  if(plural) {
    if(word %in% exceptions$singular)
      return(as.character(exceptions[exceptions$singular == word, "plural"]))
    else
      return(paste0(word, "s"))
  } else {
    return(word)
  }
}


#' Mean derivative that works with vectors containing NaN
#'
#' @param x The input as numeric.
#'
#' @return The mean as numeric.
#' @export
#' @keywords internal
safe_mean <- function(x, ...) {
  temp <- x[!is.nan(x) & !is.na(x)]
  if (length(temp) == 0) {
    return(NA)
  }
  out <- mean(temp, na.rm = T)
  attributes(out)$N <- length(temp[!is.na(temp)])
  return(out)
}


#' SD derivative that works with vectors containing NaN
#'
#' @param x The input as numeric.
#'
#' @return The mean as numeric.
#' @export
#' @keywords internal
safe_sd <- function(x, ...) {
  temp <- x[!is.nan(x) & !is.na(x)]
  if (length(temp) == 0) {
    return(NA)
  }
  out <- sd(temp, na.rm = T)
  attributes(out)$N <- length(temp[!is.na(temp)])
  return(out)
}


#' Min derivative that works with vectors containing NaN
#'
#' @param x The input as numeric.
#'
#' @return The mean as numeric.
#' @export
#' @keywords internal
safe_min <- function(x, ...) {
  temp <- x[!is.nan(x) & !is.na(x)]
  if (length(temp) == 0) {
    return(NA)
  }
  out <- min(temp, na.rm = T)
  attributes(out)$N <- length(temp[!is.na(temp)])
  return(out)
}


#' Difference, or NA, if difference is negative
#'
#' @param a A as numeric.
#' @param b B as numeric.
#'
#' @return Numeric.
#' @export
#' @keywords internal
pos_diff <- function(a, b) {
  data.frame(
    a = a,
    b = b
  ) %>%
    mutate(diff = case_when(a - b < 0 ~ NA, .default = a - b)) %>%
    pull(diff)
}



#' Coalescing join
#'
#' Source: https://www.r-bloggers.com/2023/05/replace-missing-value-from-other-columns-using-coalesce-join-in-dplyr/
#'
#' @param x Left, as data frame.
#' @param y Right, as data frame.
#' @param by The 'by' argument of the join functions.
#' @param keep 'left' means keep value from left table if values exist in both
#'  tables.
#' @param suffix Same as the suffix argument in dplyr joins.
#' @param join Choose a join type from the list. The default is full_join.
#'
#' @return A data frame.
#' @export
coalesce_join <- function(
    x, y, by = NULL,
    keep = c("left", "right"),
    suffix = c(".x", ".y"),
    join = c("full_join", "left_join", "right_join", "inner_join")) {
  keep <- match.arg(keep)

  # Confirm the join argument is in the list and matches the string to the
  # function
  join <- match.arg(join)
  join <- match.fun(join)

  # Depends on the keep argument, overwrite the duplicate value
  # If keep = "left", the value from the left table will be kept, vice versa.
  if (keep == "left") suffix_ <- suffix else suffix_ <- rev(suffix)

  join(x, y, by = by, suffix = suffix) %>%
    mutate(
      across( # Apply the coalesce function to all overlapped columns
        # Select columns ended with .x if keep = "left"; or .y if keep = "right"
        ends_with(suffix_[1]),
        # Replace .x in var.x with .y to generate var.y, if keep = "left"; or
        # vice versa
        ~ coalesce(., get(str_replace(cur_column(), suffix_[1], suffix_[2]))),
        # Remove the suffix from the combined columns
        .names = "{str_remove(.col, suffix_[1])}"
      ),
      # Remove the temporary columns ended with suffix
      .keep = "unused"
    )
}


#' Convert trial day to elapsed days
#'
#' This function corrects for trial day 1 actually indicating zero elapsed days
#' since the treatment start.
#'
#' @param x The trial day as numeric.
#'
#' @return The number of elapsed days as numeric.
#'
trialday_to_day <- function(x) {
  if(any(x[which(!is.na(x))] == 0)) stop("Trial day cannot be zero!")
  return(x + (x > 0) * -1)
}


#' Check if a string matches ISO 8601 date-time format
#'
#' This function checks whether a character string complies with the ISO 8601
#' standard for date-time representation (combined date and time). Unlike the more
#' general `is_iso8601_format()` function, this specifically checks for the
#' presence of both date and time components.
#'
#' Valid formats include:
#' - Extended format with separators: "2023-10-15T14:30:00"
#' - Basic format without separators: "20231015T143000"
#' - With timezone information: "2023-10-15T14:30:00Z" or "2023-10-15T14:30:00+02:00"
#' - With fractional seconds: "2023-10-15T14:30:00.123"
#' - Using space instead of T separator: "2023-10-15 14:30:00" (non-strict mode only)
#'
#' @param x A character string or vector of strings to check.
#' @param strict Logical, whether to strictly enforce ISO 8601 specification. Default is FALSE,
#'   which allows some common variations like space instead of 'T' separator.
#'
#' @return Logical value indicating whether the string is in ISO 8601 date-time format.
#' @export
#'
#' @examples
#' is_iso8601_datetime("2023-10-15T14:30:00")        # TRUE
#' is_iso8601_datetime("2023-10-15 14:30:00")        # TRUE (with default strict=FALSE)
#' is_iso8601_datetime("2023-10-15 14:30:00", TRUE)  # FALSE (with strict=TRUE)
#' is_iso8601_datetime("2023-10-15T14:30:00Z")       # TRUE
#' is_iso8601_datetime("2023-10-15T14:30:00+02:00")  # TRUE
#' is_iso8601_datetime("20231015T143000")            # TRUE
#' is_iso8601_datetime("2023-10-15")                 # FALSE (no time component)
#' is_iso8601_datetime("14:30:00")                   # FALSE (no date component)
is_iso8601_datetime <- function(x, strict = FALSE) {
  if (!is.character(x)) {
    stop("Input must be a character string")
  }

  # If x is NA, return NA
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  # Date patterns
  date_extended <- "\\d{4}-\\d{2}-\\d{2}"  # YYYY-MM-DD
  date_basic <- "\\d{4}\\d{2}\\d{2}"       # YYYYMMDD

  # Time patterns
  time_extended <- "\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?"  # HH:MM:SS(.sss)
  time_basic <- "\\d{2}\\d{2}\\d{2}(?:\\.\\d+)?"       # HHMMSS(.sss)

  # Timezone pattern
  timezone <- "(?:Z|[+-]\\d{2}(?::\\d{2}|\\d{2}))?"

  # Separators
  strict_separator <- "T"
  relaxed_separator <- "[ T]"
  separator <- if(strict) strict_separator else relaxed_separator

  # Combined patterns
  # Extended format: YYYY-MM-DDThh:mm:ss(.sss)(Z|±hh:mm)
  datetime_extended <- paste0(
    "^", date_extended, separator, time_extended, timezone, "$"
  )

  # Basic format: YYYYMMDDThhmmss(.sss)(Z|±hhmm)
  datetime_basic <- paste0(
    "^", date_basic, strict_separator, time_basic, timezone, "$"
  )

  # Mix of extended date with basic time: YYYY-MM-DDThhmmss(.sss)(Z|±hhmm)
  datetime_mixed1 <- paste0(
    "^", date_extended, separator, time_basic, timezone, "$"
  )

  # Mix of basic date with extended time: YYYYMMDDThh:mm:ss(.sss)(Z|±hh:mm)
  datetime_mixed2 <- paste0(
    "^", date_basic, strict_separator, time_extended, timezone, "$"
  )

  # For each element in the input vector
  result <- sapply(x, function(str) {
    if (is.na(str)) return(NA)

    # Check if the string matches any of the patterns
    grepl(datetime_extended, str) ||
    grepl(datetime_basic, str) ||
    grepl(datetime_mixed1, str) ||
    grepl(datetime_mixed2, str)
  })

  return(as.logical(result))
}


#' Check if a string matches ISO 8601 date format
#'
#' This function checks whether a character string complies with the ISO 8601
#' standard for date representation (without time components). Unlike the more
#' general `is_iso8601_format()` function or the `is_iso8601_datetime()` function,
#' this specifically validates date-only formats.
#'
#' Valid formats include:
#' - Extended format with separators: "2023-10-15"
#' - Basic format without separators: "20231015"
#' - Reduced precision (year-month): "2023-10" or "202310"
#' - Reduced precision (year only): "2023"
#'
#' @param x A character string or vector of strings to check.
#' @param allow_reduced_precision Logical, whether to allow reduced precision formats
#'   (year-month or year only). Default is TRUE.
#'
#' @return Logical value indicating whether the string is in ISO 8601 date format.
#' @export
#'
#' @examples
#' is_iso8601_date("2023-10-15")                  # TRUE
#' is_iso8601_date("20231015")                    # TRUE
#' is_iso8601_date("2023-10")                     # TRUE (with default allow_reduced_precision=TRUE)
#' is_iso8601_date("2023")                        # TRUE (with default allow_reduced_precision=TRUE)
#' is_iso8601_date("2023-10", FALSE)              # FALSE (with allow_reduced_precision=FALSE)
#' is_iso8601_date("2023/10/15")                  # FALSE (not ISO 8601 format)
#' is_iso8601_date("2023-10-15T14:30:00")         # FALSE (has time component)
is_iso8601_date <- function(x, allow_reduced_precision = TRUE) {
  if (!is.character(x)) {
    stop("Input must be a character string")
  }

  # If x is NA, return NA
  if (length(x) == 1 && is.na(x)) {
    return(NA)
  }

  # Date patterns
  date_extended <- "^\\d{4}-\\d{2}-\\d{2}$"  # YYYY-MM-DD
  date_basic <- "^\\d{4}\\d{2}\\d{2}$"       # YYYYMMDD

  # Reduced precision date patterns (if allowed)
  year_month_extended <- "^\\d{4}-\\d{2}$"   # YYYY-MM
  year_month_basic <- "^\\d{4}\\d{2}$"       # YYYYMM
  year_only <- "^\\d{4}$"                    # YYYY

  # For each element in the input vector
  result <- sapply(x, function(str) {
    if (is.na(str)) return(NA)

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

    return(is_full_date || is_reduced_precision)
  })

  # Ensure logical return type
  return(as.logical(result))
}


#' Find duplicate rows in a data frame
#'
#' This function identifies duplicate rows in a data frame based on specified fields.
#' It returns a data frame containing the duplicate rows and their counts.
#'
#' @param df A data frame to check for duplicates
#' @param fields A character vector of field names to check for duplicates. If NULL,
#'   defaults to c("USUBJID", "TIME", "ANALYTE") for NIF data.
#' @param count_only Logical indicating whether to return only the count of duplicates (default: FALSE)
#' @param return_all_cols Logical indicating whether to return all columns from the original data frame (default: TRUE)
#' @param additional_cols Character vector of additional columns to include in the output when return_all_cols is FALSE
#'
#' @return A data frame containing the duplicate rows and their counts, or just the count if count_only is TRUE
#' @export
find_duplicates <- function(df, fields = NULL, count_only = FALSE,
                            return_all_cols = TRUE, additional_cols = NULL) {
  if(is.null(fields)) {
    fields <- c("ID", "TIME", "ANALYTE")
  }

  # Check if all specified fields exist in the data frame
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0) {
    stop(paste("The following fields do not exist in the data frame:",
               paste(missing_fields, collapse = ", ")))
  }

  # Group by specified fields and count occurrences
  duplicates <- df %>%
    dplyr::group_by(across(all_of(fields))) %>%
    dplyr::summarize(count = n(), .groups = "drop") %>%
    dplyr::filter(count > 1)

  if (count_only) {
    return(nrow(duplicates))
  }

  # Join back with original data to get all columns
  if (nrow(duplicates) > 0) {
    if (return_all_cols) {
      result <- duplicates %>%
        dplyr::left_join(df, by = fields) %>%
        dplyr::arrange(across(all_of(fields)))
    } else {
      # If additional_cols is NULL, use fields
      cols_to_keep <- unique(c(fields, additional_cols))
      result <- duplicates %>%
        dplyr::left_join(
          df %>% dplyr::select(all_of(cols_to_keep)),
          by = fields
        ) %>%
        dplyr::arrange(across(all_of(fields)))
    }
    return(as.data.frame(result))
  } else {
    return(NULL)
  }
}


#' Remove duplicate rows from a data frame
#'
#' This function removes duplicate rows from a data frame based on specified fields,
#' applying a function to handle duplicate values in the dependent variable.
#'
#' @param df A data frame to remove duplicates from
#' @param fields A character vector of field names to check for duplicates. If NULL,
#'   defaults to c("USUBJID", "TIME", "ANALYTE") for NIF data.
#' @param duplicate_function A function to apply to duplicate values. Default is mean.
#'   The function should take a vector and return a single value.
#' @param dependent_variable The name of the field to apply the duplicate_function to.
#'   Defaults to "DV".
#' @param na.rm Logical indicating whether to remove NA values when applying the
#'   duplicate_function. Defaults to TRUE.
#'
#' @return A data frame with duplicate rows removed
#' @export
resolve_duplicates <- function(
    df,
    fields = NULL,
    duplicate_function = mean,
    dependent_variable = "DV",
    na.rm = TRUE) {
  if(is.null(fields)) {
    fields <- c("ID", "TIME", "ANALYTE")
  }

  # Check if all specified fields exist in the data frame
  missing_fields <- setdiff(fields, names(df))
  if (length(missing_fields) > 0) {
    stop(paste("The following fields do not exist in the data frame:",
               paste(missing_fields, collapse = ", ")))
  }

  # Check if dependent_variable exists in the data frame
  if (!dependent_variable %in% names(df)) {
    stop(paste("The dependent variable", dependent_variable,
               "does not exist in the data frame"))
  }

  # Validate that duplicate_function is a function
  if (!is.function(duplicate_function)) {
    stop("duplicate_function must be a function")
  }

  # Get all columns that are not in fields
  other_cols <- setdiff(names(df), fields)

  # Remove duplicates by applying the duplicate_function to the dependent variable
  # and keeping the first value of other columns
  # Handle NA values based on na.rm parameter
  # result <- df %>%
  #   dplyr::group_by(across(all_of(fields))) %>%
  #   dplyr::summarize(
  #     !!dependent_variable := {
  #       dv_values <- .data[[dependent_variable]]
  #       # Remove NA values if na.rm is TRUE
  #       if (na.rm) {
  #         dv_values <- dv_values[!is.na(dv_values)]
  #       }
  #       # Return NA if all values are NA or no values left after removing NAs
  #       if (length(dv_values) == 0) {
  #         NA
  #       } else {
  #         # Try to pass na.rm to duplicate_function if it accepts it
  #         tryCatch({
  #           duplicate_function(dv_values, na.rm = na.rm)
  #         }, error = function(e) {
  #           # If function doesn't accept na.rm parameter, call without it
  #           duplicate_function(dv_values)
  #         })
  #       }
  #     },
  #     across(all_of(setdiff(other_cols, dependent_variable)), ~ .[1]),
  #     .groups = "drop"
  #   )

  # df %>%
  #   dplyr::group_by(across(all_of(fields))) %>%
  #   dplyr::summarize(
  #     !!dependent_variable := duplicate_function(.data[[dependent_variable]]),
  #     across(all_of(setdiff(other_cols, dependent_variable)), ~ .[1]),
  #     .groups = "drop"
    # )

  f <- function(x) {
    if(na.rm == TRUE)
      duplicate_function(x[!is.na(x)])
    else
      duplicate_function(x)
  }

  result <- df %>%
    # dplyr::group_by(across(all_of(fields))) %>%
    reframe(
      !!dependent_variable := f(.data[[dependent_variable]]),
      # .by = all_of(fields)
      .by = all_of(setdiff(names(df), c(dependent_variable, "MDV")))
    ) %>% as.data.frame()


  return(as.data.frame(result))
}


#' Test whether a filter term is valid
#'
#' @param data A data frame.
#' @param filter_string A filter term as character.
#'
#' @returns Logical.
is_valid_filter <- function(data, filter_string) {
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data frame")
  }
  if (!is.character(filter_string) || length(filter_string) != 1) {
    stop("filter_string must be a single character string")
  }

  # Check for empty filter string
  if (nchar(trimws(filter_string)) == 0) {
    return(FALSE)
  }

  # Parse the filter expression
  filter_expr <- tryCatch({
    rlang::parse_expr(filter_string)
  }, error = function(e) {
    return(FALSE)
  })

  if (isFALSE(filter_expr)) {
    return(FALSE)
  }

  # Extract column names from the expression
  col_names <- tryCatch({
    all.vars(filter_expr)
  }, error = function(e) {
    return(FALSE)
  })

  if (isFALSE(col_names)) {
    return(FALSE)
  }

  # Check if all columns exist in the data frame
  if (!all(col_names %in% names(data))) {
    return(FALSE)
  }

  # Try to evaluate the filter expression
  result <- tryCatch({
    # Create a test row with NA values for all columns
    test_row <- data[1, , drop = FALSE]
    if (nrow(data) == 0) {
      # For empty data frames, create a row with appropriate types
      test_row <- data.frame(
        lapply(data, function(x) {
          if (is.numeric(x)) 0
          else if (is.character(x)) ""
          else if (is.logical(x)) FALSE
          else if (inherits(x, "Date")) as.Date("2000-01-01")
          else NA
        }),
        stringsAsFactors = FALSE
      )
    }

    # Try to evaluate the filter on the test row
    eval_result <- dplyr::filter(test_row, !!filter_expr)
    TRUE
  }, error = function(e) {
    FALSE
  })

  return(result)
}
