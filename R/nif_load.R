#' Load NIF file
#'
#' @param filename The filename as character.
#' @param format The data format, as character. Can be only "csv".
#'
#' @return A nigitggf object.
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
