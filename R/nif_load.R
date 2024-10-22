read_nif <- function(filename, format="csv") {

  # if(format == "csv"){
    raw <- read.csv(
      filename,
      sep=",",
      numerals=c("no.loss"))
    missing_fields <- minimal_nif_fields[!minimal_nif_fields %in% names(raw)]
    if(length(missing_fields) != 0)
      stop("Required fields missing!")
  # }
  out <- new_nif(raw) %>%
    ensure_analyte() %>%
    ensure_parent() %>%
    ensure_time()

  return(out)
}
