# data_issues <- function(obj) {
#   # input validation
#   if (!inherits(obj, "domain"))
#     stop("Input must be a SDTM domain")
#   if(!"DOMAIN" %in% names(obj))
#     stop("DOMAIN field missing in input")
#   domain_name <- unique(obj$DOMAIN)
#   if(length(domain_name) != 1)
#     stop("Multiple domain names!")
#
#   # Domain-specific assessments
#   if(toupper(domain_name) == "EX") {
#
#   }
#
#   return(NA)
# }


#' Identifies data inconsistencies and potential issues in the EX domain
#'
#' @param ex The EX domain as domain.
#'
#' @returns A list.
#' @export
ex_issues <- function(ex) {
  # validate ex domain
  expected_ex_columns <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
  missing_ex_columns <- setdiff(expected_ex_columns, names(ex))
  n_missing = length(missing_ex_columns)
  if(n_missing > 0)
    stop(paste0(
      "Missing ", plural("colum", n > 1), " in domain EX: ",
      nice_enumeration(missing_ex_columns)))

  missing_exendtc = ex %>%
    lubrify_dates() %>%
    arrange(.data$USUBJID, .data$EXSTDTC) %>%
    group_by(.data$USUBJID, .data$EXTRT) %>%
    mutate(.LAST_ADMIN = row_number() == max(row_number())) %>%
    ungroup() %>%
    filter(is.na(.data$EXENDTC))

  out <- list(
    missing_last_exendtc = missing_exendtc %>%
      filter(.data$.LAST_ADMIN == TRUE) %>%
      select(-c(".LAST_ADMIN")),

    missing_non_last_exendtc = missing_exendtc %>%
      filter(.data$.LAST_ADMIN == FALSE) %>%
      select(-c(".LAST_ADMIN")),

    exstdtc_after_exendtc = ex %>%
      lubrify_dates() %>%
      decompose_dtc(c("EXSTDTC", "EXENDTC")) %>%
      filter(
        as.Date(.data$EXENDTC_date) < as.Date(.data$EXSTDTC_date) |
        !is.na(EXSTDTC_time) & !is.na(EXENDTC_time) & EXENDTC < EXSTDTC
      )
  )

  return(out)
}
