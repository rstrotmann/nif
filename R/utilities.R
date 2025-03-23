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
#' @param indent The indenbt level as numeric.
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
#' @param df The data.frame to be rendered
#' @param indent Indentation level, as numeric.
#' @param header Boolean to indicate whether the header row is to be included.
#' @param color Print headers in grey as logical.
#' @param n The number of lines to be included, or all if NULL.
#' @param show_none Show empty data frame as 'none', as logical.
#' @param header_sep Show separation line after header, as logical.
#'
#' @return The output as string.
#' @import utils
#' @keywords internal
df_to_string <- function(
    df, indent = 0, n = NULL, header = TRUE, header_sep = FALSE,
    color = FALSE, show_none = FALSE) {
  indent = indent_string(indent)
  df <- as.data.frame(df) %>%
    mutate(across(everything(), as.character))

  max_widths <- as.numeric(lapply(
    rbind(df, names(df)),
    FUN = function(x) max(sapply(as.character(x), nchar), na.rm = TRUE)
  ))

  header_separator <- paste(
    lapply(
      max_widths,
      function(n) paste(replicate(n, "-"), collapse = "")),
    collapse = "   ")

  render_line <- function(line) {
    paste0(
      indent,
      paste0(
        mapply(
          function(element, width) {
            format(element, width = width + 3)
          },
          element = as.character(line), width = max_widths
        ),
        collapse = ""
      )
    )
  }

  if (header == TRUE) {
    if (color == TRUE) {
      out <- paste0(
        "\u001b[38;5;248m",
        render_line(data.frame(as.list(names(df)))),
        "\u001b[0m"
      )
    } else {
      out <- render_line(data.frame(as.list(names(df))))
    }
  } else {
    out <- ""
  }

  if(isTRUE(header) & isTRUE(header_sep))
     out <- paste0(out, "\n", indent, header_separator)

  if (!is.null(n)) {
    df <- utils::head(df, n = n)
  }

  temp <- lapply(as.list(as.data.frame(t(df))), render_line)

  if (show_none & length(temp) == 0) {
    out <- paste0(indent, "none")
  } else {
    out <- paste(out, paste(temp, collapse = "\n"), sep = "\n")
  }
  return(out)
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
#' tests whether the above is part of the input, i.e., dat-time formats that
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
#' @param conjunction The conjunction between the last and penultmate items.
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



