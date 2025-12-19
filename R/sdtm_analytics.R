#' Number of DTC entries with missing time information
#'
#' @param sdtm A stm object.
#' @param fields The fields to test, as character.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' sdtm_missing_times(examplinib_poc, c("EXSTDTC", "EXENDTC"))
sdtm_missing_times <- function(sdtm, fields) {
  n_missing_time <- function(field) {
    dom <- domain(sdtm, tolower(str_sub(field, 1, 2)))
    dom %>%
      filter(has_time(.data[[field]]) == FALSE) %>%
      nrow()
  }

  n_total <- function(field) {
    dom <- domain(sdtm, tolower(str_sub(field, 1, 2)))
    dom %>%
      nrow()
  }

  data.frame(
    field = fields,
    missing = as.numeric(lapply(fields, n_missing_time)),
    total = as.numeric(lapply(fields, n_total))
  ) %>%
    mutate(percent_missing = round(.data$missing / .data$total * 100, 1))
}
