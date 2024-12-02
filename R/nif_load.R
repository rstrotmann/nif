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










