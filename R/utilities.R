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
    dplyr::mutate(SEX = as.numeric(
      dplyr::case_match(as.character(SEX),
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
#'
#' @return The output as string.
#' @import utils
#' @keywords internal
df_to_string <- function(df, indent = "", n = NULL, header = TRUE,
                         color = FALSE) {
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
    # out <- render_line(data.frame(as.list(names(df))))
    if(color == TRUE) {
      out <- paste0(
        "\u001b[38;5;248m",
        render_line(data.frame(as.list(names(df)))),
        "\u001b[0m"
      )
      # out <- render_line(data.frame(as.list(names(df))))
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
  out <- paste(out, paste(temp, collapse = "\n"), sep = "\n")
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
  x[which(x <0)] <- 0
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
#' @export
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
isofy_dates <- function(obj) {
  obj %>%
    dplyr::mutate_at(vars(ends_with("DTC")), ~ format(., "%Y-%m-%dT%H:%M"))
}


#' Compose DTC from date and time components
#'
#' Convert the date and time provided separately as character to a POSIXct
#' date-time object.
#' @param date A date in POSIX or character format.
#' @param time A time in character format.
#' @return A POSICct object.
#' @keywords internal
compose_dtc <- function(date, time) {
  data.frame(date = as.character(date), time = as.character(time)) %>%
    mutate(time = case_when(is.na(.data$time) ~ "", .default = .data$time)) %>%
    mutate(DTC = str_trim(paste(as.character(.data$date), .data$time))) %>%
    mutate(DTC = lubridate::as_datetime(.data$DTC,
                                        format = c("%Y-%m-%d %H:%M", "%Y-%m-%d")
    )) %>%
    pull(.data$DTC)
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
#' @param datetime The datetime object in POSIX format.
#' @return A Boolean value.
#' @keywords internal
has_time <- function(datetime) {
  as.numeric(datetime) %% 86400 != 0
}


