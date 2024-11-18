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
#' @param filename Filename as character.
#' @param format Format as character. Must be 'NONMEM'.
#'
#' @return A nif object.
#' @export
import_nif <- function(filename, format = "NONMEM") {
  if(!format %in% c("NONMEM"))
    stop(paste0("Unsupported data format: ", format))

  # convert_nif_numeric <- function(col) {
  #   as.numeric(ifelse(col == ".", 0, col))
  # }

  is_nif_numeric <- function(col) {
    all(grepl("^-?[0-9.e-]+$", col))
  }

  num_cols <- function(x) {
    x %>%
      select(-c("USUBJID", "STUDYID")) %>%
      select(is_nif_numeric) %>%
      names()
  }

  if(format == "NONMEM"){
    header <- readLines(filename, n = 1)
    pos <- str_locate_all(header, "[A-Za-z_]+")[[1]][,1]
    widths <- (pos - lag(pos))[-1]

    raw <- readr::read_fwf(
      filename,
      col_positions = radr::fwf_widths(widths)
    )

    nif <- raw[-1,] %>%
      as.data.frame()
    names(nif) <- as.character(raw[1,])

    temp <- nif %>%
      # mutate(across(where(is_nif_numeric), as.numeric)) %>%
      mutate(across(c(num_cols(.)), ~as.numeric(.x, na.rm = TRUE)))


  }

  return(nif)
}










