#' Test vector whether consistent with Datetime as per ISO 8601
#'
#' @param x The input, as character.
#'
#' @return Logical.
#' @noRd
is_char_datetime <- function(x) {
  grepl("^\\d{4}-\\d{2}-\\d{2}(T| )\\d{2}:\\d{2}(:\\d{2})?$", x)
}


#' Test vector whether consistent with Date format
#'
#' @param x The input, as character.
#'
#' @return Logical.
#' @noRd
is_char_date <- function(x) {
  grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
}


#' Test whether character vector likely holds date/time values
#'
#' @param x The input as character.
#' @param min_prob The minimal fraction of values to correctly parse as numeric.
#'
#' @return Logical.
#' @noRd
is_likely_datetime <- function(x, min_prob = 0.9) {
  if(!is.character(x)) return(FALSE)
  overall_match <- is_char_datetime(x) | is_char_date(x)
  overall_prob <- length(overall_match[overall_match == TRUE])/length(overall_match)
  return(overall_prob >= min_prob)
}


#' Convert character vector to DTC
#'
#' @param x The input as character.
#' @param min_prob The minimal fraction of values to correctly parse as numeric.
#' @param silent No message output.
#'
#' @return POSIXct.
#' @noRd
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
  }

  suppressWarnings(
    lubridate::as_datetime(x, format = dtc_formats))
}


#' Rename columns in data frame based on function terms
#'
#' @param obj a data frame.
#' @param f a function.
#'
#' @returns a data frame.
#' @noRd
rename_by_formula <- function(obj, f) {
  # input validation
  if(!is.data.frame(obj))
    stop("obj must be a data frame!")
  if(!is_formula(f))
    stop("f must be a formula object!")

  to_col = rlang::f_lhs(f)
  from_term = rlang::f_rhs(f)
  tryCatch(
    error = function(e) {
      stop(paste0(
        "'", deparse(f), "' is not a valid renaming term, ", e
      ))
    },
    out <- obj %>%
      mutate(!!to_col := eval(from_term))
  )
  return(out)
}


#' Import nif object from connection
#'
#' @param connection The connection to read from.
#' @param format The input data format, can be 'csv' or 'fixed_width', or NULL
#'   (default) to automatically determine the format.
#' @param no_numeric Fields that will not be converted to numeric.
#' @param silent Suppress message output, as logical.
#' @param delimiter Delimiter character.
#' @param ... Renaming terms as function.
#'
#' @return A nif object.
#' @import stringr
#' @export
import_from_connection <- function(
    connection,
    ...,
    format = NULL,
    delimiter = ",",
    no_numeric = c("USUBJID", "STUDYID"),
    silent= NULL) {
  temp <- match.arg(format, choices = c("csv", "fixed_width", NULL))

  # Validate inputs
  if (!inherits(connection, "connection")) {
    stop("Input must be a connection object")
  }

  terms <- list(...)
  valid_terms <- lapply(terms, is_formula)
  terms <- terms[as.logical(valid_terms)]

  # read raw line data from connection
  lines <- readLines(connection, skipNul = TRUE)
  comment_lines <- which(substr(trimws(lines), 1, 1) == "#")
  empty_lines <- which(nchar(trimws(lines)) == 0)
  if(length(c(comment_lines, empty_lines)) != 0) {
    lines <- lines[-c(comment_lines, empty_lines)]
  }

  if (length(lines) == 0) {
    stop("No data found after removing comments and empty lines")
  }

  # Auto-detect format
  if(is.null(format)){
    # Check for quoted fields first
    first_line <- gsub('"[^"]*"', "", lines[1]) # Remove quoted content
    n_comma <- str_count(first_line, delimiter)
    n_space <- str_count(first_line, "\\s+")

    if(n_space == 0 & n_comma == 0)
      stop("Data format is not specified and can not be autmatically determined!")
    if(n_space > 0) format <- "fixed_width"
    if(n_comma > 0) format <- "csv"

    # Validate auto-detected format
    if (format == "fixed_width") {
      # Check if we can extract column headers
      col_headers <- str_extract_all(lines[1], "[A-Za-z_]+")[[1]]
      if (length(col_headers) < 2) {
        stop("Auto-detected fixed-width format, but couldn't properly identify column headers. Please specify format explicitly.")
      }
    } else if (format == "csv") {
      # Check if we have a consistent number of fields
      header_fields <- length(str_split(lines[1], delimiter, simplify = TRUE))
      if (length(lines) > 1) {
        first_row_fields <- length(str_split(lines[2], delimiter, simplify = TRUE))
        if (header_fields != first_row_fields) {
          stop("Auto-detected CSV format, but the number of fields is inconsistent. Please specify format explicitly or check delimiter setting.")
        }
      }
    }
  }

  # fixed-width format, white space-separated columns
  if(format == "fixed_width"){
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
    raw <- data.frame(str_split(lines[-1], delimiter, simplify = TRUE))
    colnames(raw) <- str_split(lines[1], delimiter, simplify = TRUE)
  }

  raw <- raw %>%
    mutate(across(
     -c(any_of(no_numeric)),
      ~type.convert(.x, as.is = TRUE, numerals = "no.loss",
                    na.strings = c("NA", "."))
    )) %>%
    mutate(across(
      where(is.character) & where(is_likely_datetime),
      convert_char_datetime
    ))

  # apply renaming
  for(f in terms) {
    raw <- rename_by_formula(raw, f)
  }

  missing_fields <- minimal_nif_fields[!minimal_nif_fields %in% names(raw)]

  if(length(missing_fields) != 0) {
    conditional_message(
      "Missing essential fields ",
      paste(missing_fields, collapse = ", "),
      " were set to 'NA'!",
      silent = silent
    )
    raw[missing_fields] <- NA
  }

  out <- new_nif(raw) #%>%
    # ensure_analyte() %>%
    # ensure_parent() #%>%
    # ensure_time()

  return(out)
}


#' Import nif file
#'
#' @param filename Filename as character.
#' @param format The input data format, can be 'csv' or 'fixed_width', or NULL
#'   (default) to automatically determine the format.
#' @param ... Renaming terms as function.
#' @param delimiter Delimiter character.
#' @param no_numeric Fields that will not be converted to numeric.
#' @param silent Suppress message output, as logical.
#'
#' @return A nif object.
#' @export
import_nif <- function(
    filename,
    ...,
    format = NULL,
    delimiter = ",",
    no_numeric = c("USUBJID", "STUDYID"),
    silent = NULL) {
  if(!file.exists(filename))
    stop(paste0(
      "File '", filename, "' not found."
    ))
  connection = file(filename)
  on.exit(close(connection))
  import_from_connection(
    connection,
    ...,
    format = format,
    delimiter = delimiter,
    no_numeric = no_numeric,
    silent = silent)
}




#' Write to pin board
#'
#' @param obj nif object.
#' @param board Path to pin board, as character, defaults to respective
#'   nif_option setting if NULL.
#' @param name Name for the nif pin object, as character. Defaults to
#'   'xx_nif' with xx the studies included.
#' @param title Title for the nif pin object, as character. Defaults to
#'   the studies included.
#' @param silent Suppress messages. Defaults to nif_option setting if NULL.
#'
#' @returns Nothing.
#' @importFrom pins pin_write board_folder
#' @export
pin_write.nif <- function(
    obj, board = NULL, name = NULL, title = NULL, silent = NULL) {
  # input validation
  validate_nif(obj)
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name", allow_null = TRUE)
  validate_char_param(title, "title", allow_null = TRUE)

  if(is.null(board))
    board <- nif_option_value("board")

  if(is.null(board) | is.na(board))
    stop("No pin board provided")

  board_obj <- pins::board_folder(board)

  if(!dir.exists(board))
    stop(paste0(
      "Board ", board, " does not exist"))

  if(is.null(name))
    name <- paste0(paste(studies(obj), collapse = "_"), "_nif")

  if(is.null(title))
    title <- paste(studies(obj), collapse = "_")

  msg <- capture.output(
    pins::pin_write(board_obj, obj, name = name, title = title, type = "rds"),
    type = "message")
  conditional_message(
    paste(msg, collapse = "\n"), silent = silent)
}


#' Read nif object from pinboard
#'
#' @param name The pin name, as character.
#' @param board The board folder, defaults to the respective nif_option setting.
#'
#' @returns A nif object.
#' @export
pin_read_nif <- function(name, board = NULL) {
  # input validation
  validate_char_param(board, "board", allow_null = TRUE)
  validate_char_param(name, "name")

  # get board
  if(is.null(board))
    board <- nif_option_value("board")

  if(is.null(board) | is.na(board))
    stop("No pin board provided")

  board_obj <- pins::board_folder(board)

  if(!dir.exists(board))
    stop(paste0(
      "Board ", board, " does not exist"))

  # read pin
  out <- pin_read(board_obj, name)

  validate_nif(out)
  return(out)
}
