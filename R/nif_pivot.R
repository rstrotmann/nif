#' Calculate time deviations for observations
#'
#' Calculate the time difference in hours between the actual time after dose
#' (TAD) and the scheduled time (NTIME) of observations.
#'
#' If NTIME == 0, the data point is considered pre-dose, and instead of TAD, the
#' (negative) time difference to the next administration is calculated!
#'
#' @param obj A nif object.
#'
#' @returns A nif object with the 'TIME_DEV' field added
#' @export
#' @examples
#' library(dplyr)
#'
#' examplinib_poc_nif %>%
#'   add_time_deviation() %>%
#'   head()
#'
add_time_deviation <- function(obj) {
  # input validation
  validate_nif(obj)

  # ensure that TAD and NTIME fields are present and not NA
  required_fields <- c("TAD", "NTIME")
  missing_fields <- setdiff(required_fields, names(obj))
  if(length(missing_fields) > 0) {
    stop(paste0(
      "Missing ", plural("field", length(missing_fields) > 1),
      " in nif object: ", nice_enumeration(missing_fields)
    ))
  }

  # identify observations without NTIME
  temp <- obj %>%
    as.data.frame() %>%
    filter(EVID == 0) %>%
    reframe(n_obs = n(), n_missing_ntime = sum(is.na(NTIME)), .by = "ANALYTE")

  # calculate time deviation
  out <- obj %>%
    arrange(USUBJID, DTC, -EVID) %>%
    group_by(PARENT) %>%

    # identify DTC of next administration
    mutate(next_admin = case_when(EVID == 1 ~ DTC)) %>%
    fill(next_admin, .direction = "up") %>%

    # calculate time to next dose
    mutate(TTND = as.numeric(DTC - next_admin, units = "hours")) %>%

    ungroup() %>%

    # calculate time difference
    mutate(TIME_DEV = round(
      case_when(
        NTIME == 0 ~ .data$TTND,
        .default = .data$TAD - .data$NTIME),
      3)) %>%
    select(-c("TTND", "next_admin"))

  return(out)
}




