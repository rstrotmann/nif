


# compare_ex_ec <- function(sdtm) {
#   if(!all(c("ex", "ec") %in% names(sdtm$domains)))
#     stop("EX and EC not both found in the sdtm object!")
#   ec <- domain(sdtm, "ec") %>%
#     assertr::verify(has_all_names("USUBJID", "ECTRT", "ECSTDTC", "ECENDTC")) %>%
#     filter(ECMOOD == "PERFORMED", !is.na(ECDOSE)) %>%
#     select(USUBJID, TRT = ECTRT, STDTC = ECSTDTC, ENDY = ECENDTC, DOSE = ECDOSE)
#   ex <- domain(sdtm, "ex") %>%
#     assertr::verify(has_all_names("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")) %>%
#     select(USUBJID, TRT = EXTRT, STDTC = EXSTDTC, ENDY = EXENDTC, DOSE = EXDOSE)
#
# }



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

  data.frame(field = fields,
             missing = as.numeric(lapply(fields, n_missing_time)),
             total = as.numeric(lapply(fields, n_total))) %>%
    mutate(percent_missing = round(.data$missing/.data$total*100, 1))
}





























