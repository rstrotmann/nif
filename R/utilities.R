#' Issue message based on silent flag
#'
#' @param msg The message as character.
#' @param silent A Boolean.
#' @param ... Further message components.
#' @return Nothing.
#' @keywords internal
# conditional_message <- function(msg, ...) {
#   parameters <- c(as.list(environment()), list(...))
#   parameters <- lapply(parameters, as.character)
#   silent <- get("silent", .nif_env)
#   if (silent == FALSE) {
#     message(paste(as.character(parameters[names(parameters) != "silent"])))
#   }
# }
conditional_message <- function(...) {
  args <- lapply(list(...), as.character)
  if(exists("silent", envir = .nif_env)) {
    silent <- get("silent", .nif_env)
  } else {
    silent = FALSE
  }
  if (silent == FALSE) {
    message(paste(args, collapse = ""))
  }
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
  if(is_true(nif_option_value("debug"))) print(obj)
}


#' Recode SEX field in a data frame
#'
#' This function recodes the SEX field in a data frame. All numerical values are
#' kept while "M" is recoded to 0 and "F" to 1. If your downstream analysis
#' requires different coding, please manually re-code.
#' @param obj The data.frame containing a SEX field
#' @return The output data frame
#' @import dplyr
#' @keywords internal
recode_sex <- function(obj) {
  obj %>%
    mutate(SEX = as.numeric(
      case_match(as.character(.data$SEX),
                        "M" ~ 0, "F" ~ 1, "1" ~ 1, "0" ~ 0, .default = NA)
    ))
}


#' Render data frame object to string
#'
#' This function renders a data.frame into a string similar to its
#' representation when printed without line numbers
#'
#' @param df The data.frame to be rendered
#' @param indent A string that defines the left indentation of the rendered
#'   output.
#' @param header Boolean to indicate whether the header row is to be included.
#' @param color Print headers in grey as logical.
#' @param n The number of lines to be included, or all if NULL.
#' @param show_none Show empty data frame as 'none', as logical.
#'
#' @return The output as string.
#' @import utils
#' @keywords internal
df_to_string <- function(df, indent = "", n = NULL, header = TRUE,
                         color = FALSE, show_none = FALSE) {
  df <- as.data.frame(df) %>%
    mutate(across(everything(), as.character))
  max_widths <- as.numeric(lapply(
    rbind(df, names(df)),
    FUN = function(x) max(sapply(as.character(x), nchar), na.rm = TRUE)
  ))

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
    if(color == TRUE) {
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

  if (!is.null(n)) {
    df <- utils::head(df, n = n)
  }

  temp <- lapply(as.list(as.data.frame(t(df))), render_line)

  if(show_none & length(temp) == 0) {
    out <- paste0(indent, "none")
  } else {
    out <- paste(out, paste(temp, collapse = "\n"), sep = "\n")
  }
  return(out)
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


#' The list of expected date/time formats as per ISO 8601
#' @keywords internal
dtc_formats <- c("%Y-%m-%dT%H:%M", "%Y-%m-%d", "%Y-%m-%dT%H:%M:%S", "%Y")


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
#' @keywords internal
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
#' character using the ISO 8601 format.
#' @param obj A data frame.
#' @return A data frame.
#' @seealso [lubrify_dates()]
#' @keywords internal
isofy_dates <- function(obj) {
  obj %>%
    dplyr::mutate_at(vars(ends_with("DTC")), ~ format(., "%Y-%m-%dT%H:%M"))
}


#' Convert ISO 8601-formatted duration to hours
#'
#' @param iso The duration as ISO 8601-formatted string.
#'
#' @return Duration in hours.
#' @export
#'
#' @examples
#' pt_to_hours(c("PT1H15M", "PT1.5H"))
pt_to_hours <- function(iso) {
  as.numeric(duration(iso))/60/60
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
      mutate({{DTC_date}} := extract_date(.data[[fld]])) %>%
      mutate({{DTC_time}} := case_when(
        .data$has_time == TRUE ~ extract_time(.data[[fld]]),
        .default = NA)) %>%
      select(-c("has_time"))
  }

  for(i in DTC_field) {
    obj <- dec_dtc(i)
  }
  return(obj)
}


#' Extract thendate component of a POSICct object
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
#' @param datetime The datetime object as POSIX or as ISO 8601-formatted string.
#' @return A Boolean value.
#' @keywords internal
has_time <- function(obj) {
  if(is.POSIXct(obj)) {
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
  if(length(items) == 1) {
    return(items[[1]])
  }
  if(length(items) > 1) {
    return(paste(paste(items[1:length(items)-1], collapse = ", "), conjunction,
                 items[length(items)]))
  }
}


#' Add string to string, separated by a comma
#'
#' @param object The base string as character.
#' @param statement The string to be added as character.
#'
#' @return A string
#' @export
#' @keywords internal
#'
#' @examples
#' append_statement("A", "B")
#' append_statement("", "B")
#' append_statement(c("A1", "A2", "A3"), "B")
#' append_statement(c("A1", "", "A3", ""), "B")
append_statement <- function(object, statement) {
  i <- which(lapply(object, str_length) > 0)
  object[i] <- paste0(object[i], ", ")
  paste0(object, statement)
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
  if(length(temp) == 0) return(NA)
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
  if(length(temp) == 0) return(NA)
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
  if(length(temp) == 0) return(NA)
  out <- min(temp, na.rm = T)
  attributes(out)$N <- length(temp[!is.na(temp)])
  return(out)
}


#' Difference, or zero, if difference is negative
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

