#' Load NIF file
#'
#' @param filename The filename as character.
#' @param format The data format, as character. Can be only "csv".
#'
#' @return A nif object.
#' @export
read_nif <- function(filename, format="csv") {
  if(!format %in% c("csv"))
    stop(paste0("Unsupported data format: ", format))

  if(format == "csv") {
    raw <- read.csv(
      filename,
      sep=",",
      numerals=c("no.loss"))
  } else {
    return(NULL)
  }
  missing_fields <- minimal_nif_fields[!minimal_nif_fields %in% names(raw)]
  if(length(missing_fields) != 0)
    stop("Required fields missing!")
  out <- new_nif(raw) %>%
    ensure_analyte() %>%
    ensure_parent() %>%
    ensure_time()

  return(out)
}


#' Read NIF data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param filename File name as character.
#' @param format Format as character. Must be 'NONMEM'.
#'
#' @return A nif object.
#' @import readr
#' @export
import_nif <- function(filename, format = "NONMEM") {
  if(!format %in% c("NONMEM", "csv"))
    stop(paste0("Unsupported data format: ", format))

  is_nif_numeric <- function(col) {
    all(grepl("^-?[0-9.e-]+$", col))
  }

  num_cols <- function(x) {
    x %>%
      # select(-c("USUBJID", "STUDYID")) %>%
      select(-any_of(c("USUBJID", "STUDYID"))) %>%
      select(is_nif_numeric) %>%
      names()
  }

  if(format == "NONMEM"){
    header <- readLines(filename, n = 1)
    pos <- str_locate_all(header, "[A-Za-z_]+")[[1]][,1]
    widths <- (pos - lag(pos))[-1]

    invisible(
      # capture_output(
        raw <- readr::read_fwf(
          filename,
          col_positions = readr::fwf_widths(widths),
          show_col_types = FALSE
        )
      # )
    )

    nif <- raw[-1,] %>%
      mutate(across(
        everything(),
        ~replace(., . == ".", "0")
      ))
    names(nif) <- as.character(raw[1,])

    nif <- nif %>%
      mutate(across(c(num_cols(.)), ~as.numeric(.x))) %>%
      new_nif()

    return(nif)
  }

  if(format == "csv") {
    # invisible(capture_output(
      raw <- readr::read_csv(
        filename,
        show_col_types = FALSE
      )
    # ))
    return(new_nif(raw))
  }
}


#' Test vector whether consistent with NONMEM numeric format
#'
#' @param x The input, as character.
#'
#' @return Logical.
is_char_nonmem_numeric <- function(x) {
  grepl("^[+-]?[0-9]*\\.?[0-9]*(e[+-]?[0-9]+)?$", x)
}


#' Test whether character vector likely holds nueric values
#'
#' @param x The input as character.
#' @param min_prob The minimal fraction of values to correctly parse as numeric.
#'
#' @return Logical.
is_likely_numeric <- function(x, min_prob=0.9) {
  # assert_that(is.character(x))
  if(!is.character(x)) return(FALSE)
  overall_match <- is_char_nonmem_numeric(x)
  overall_prob <- length(overall_match[overall_match == TRUE])/length(overall_match)
  return(overall_prob >= min_prob)
}


#' Title
#'
#' @param x
#' @param min_prob
#' @param exclude
#' @param silent
#'
#' @return
convert_char_numeric <- function(
    x, min_prob=0.9, exclude=c("USUBJID", "STUDYID"), silent=NULL) {
  p <- is_likely_numeric(x, min_prob)
  if(!p) {
    stop("Vector cannot be converted to numeric!")
  }

  overall_match <- is_char_nonmem_numeric(x)
  not_numeric <- unique(x[overall_match == FALSE])

  if(length(not_numeric) >0) {
    conditional_message(
      "The following values are not numeric ",
      "and will be represented by 'NA': ",
      paste0(not_numeric, collapse=", "),
      silent = silent)
  }

  unlist(lapply(
    x,
    function(x) {
    x[x == "."] <- "NA"
    suppressWarnings(as.numeric(x))
  }))
}


#' Test vector whether consistent with Datetime as per ISO 8601
#'
#' @param x The input, as character.
#'
#' @return Logical.
is_char_datetime <- function(x) {
  grepl("^\\d{4}-\\d{2}-\\d{2}(T| )\\d{2}:\\d{2}(:\\d{2})?$", x)
}


#' Test vector whether consistent with Date format
#'
#' @param x The input, as character.
#'
#' @return Logical.
is_char_date <- function(x) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
}


#' Test whether character vector likely holds date/time values
#'
#' @param x The input as character.
#' @param min_prob The minimal fraction of values to correctly parse as numeric.
#'
#' @return Logical.
is_likely_datetime <- function(x, min_prob = 0.9) {
  # assert_that(is.character(x))
  if(!is.character(x)) return(FALSE)
  overall_match <- is_char_datetime(x) | is_char_date(x)
  overall_prob <- length(overall_match[overall_match == TRUE])/length(overall_match)
  return(overall_prob >= min_prob)
}


#' Convert character vector to DTC
#'
#' @param x The input as data frame.
#' @param min_prob The minimal fraction of values to correctly parse as numeric.
#' @param silent No message output.
#'
#' @return A data frame.
convert_char_datetime <- function(x, min_prob=0.9, silent=NULL) {
  p <- is_likely_datetime(x, min_prob)
  if(!p) {
    stop("Not a date/time vector!")
  }

  overall_match <- is_char_datetime(x) | is_char_date(x)
  no_datetime <- unique(x[overall_match == FALSE])

  if(length(which(overall_match == FALSE)) > 0) {
  conditional_message(
    "The following values are not valid date/times ",
    "and will be represented by 'NA': ",
    paste0(no_datetime, collapse=", "),
    silent = silent)
  }

  date_only <- is_char_date(x) & overall_match

  enum = length(which(date_only == TRUE))
  if(enum > 0) {
    denom <- length(x[overall_match])
    temp <- paste0(
      "No time information in ", enum, "/", denom,
      " (", round(enum/denom*100,1), "%) of the datetime values")
    sample_values <- unique(x[date_only])
    if(length(sample_values) < 20) {
      conditional_message(
        temp, "; ",
        paste0(sample_values, collapse = ", "),
        silent = silent
      )
    } else {
      conditional_message(
        temp, "!",
        silent = silent
      )
    }

    # conditional_message(
    #   "No time information in ", enum, "/", denom,
    #   " (", round(enum/denom*100,1), "%) ",
    #   "of the datetime values: ",
    #   paste0(x[date_only], collapse = ", "),
    #   silent = silent
    # )
  }

  suppressWarnings(
    lubridate::as_datetime(x, format = dtc_formats))
}


#' Import nif object from connection
#'
#' @param connection The connectino to read from.
#' @param format The input data format.
#'
#' @return A nif object.
#' @export
#'
#' @examples
import_from_connection <- function(
    connection, format = "NONMEM",
    no_numeric = c("USUBJID", "STUDYID")) {
  temp <- match.arg(format, choices = c("csv", "NONMEM"))
  assert_that(length(format) == 1, msg = "'format' must be a single value!")

  # read raw line data from connection
  lines <- readLines(connection, skipNul = TRUE)
  comment_lines <- which(substr(trimws(lines), 1, 1) == "#")
  empty_lines <- which(nchar(trimws(lines)) == 0)
  if(length(c(comment_lines, empty_lines)) != 0) {
    lines <- lines[-c(comment_lines, empty_lines)]
  }

  # NONMEN-format, i.e., fixed-width format, white space-separated columns
  if(format == "NONMEM"){
    # column positions
    max_width <- max(nchar(lines))
    col_start <- str_locate_all(lines[1], "[A-Za-z_]+")[[1]][,1]
    col_pos <- cbind(col_start, c(head(lead(col_start)-1, -1), max_width))

    row_vector <- function(line) {
      trimws(apply(col_pos, 1, function(x) substr(line, x[1], x[2])))
    }

    # make data frame from lines
    temp <- lapply(lines[-1], row_vector)
    raw <- data.frame(t(sapply(temp, c)))
    names(raw) <- row_vector(lines[1])
  }

  if(format == "csv") {
    raw <- data.frame(str_split(lines[-1], ",", simplify = TRUE))
    colnames(raw) <- str_split(lines[1], ",", simplify = TRUE)
  }

  raw <- raw %>%
    mutate(across(
      c(where(is_likely_numeric), -any_of(no_numeric)),
      convert_char_numeric
    )) %>%
    mutate(across(
      where(is.character) & where(is_likely_datetime),
      convert_char_datetime
    ))
  return(raw)
}








