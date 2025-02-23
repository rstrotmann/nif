
#' Expand administration episodes
#'
#' @param ex The EX domain as data frame.
#'
#' @return A data frame.
expand_ex <- function(ex) {
  validate <- TRUE

  # Input validation
  if (validate) {
    if (!is.data.frame(ex)) stop("Input must be a data frame")
    ex %>%
      assertr::verify(assertr::has_all_names(
        "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"
    ))
  }

  # Helper function for date sequence generation
  date_list <- function(stdtc, endtc) {
    tryCatch({
      start_date <- as.Date(stdtc)
      end_date <- as.Date(endtc)
    },
    error = function(e) {
      warning("Failed to parse dates: ", stdtc, " to ", endtc)
    })

    if(!is.na(end_date) & end_date < start_date) {
      stop(paste0(
        "End date before start date for: ", stdtc, " to ", endtc
      ))
    }

    if(!is.na(endtc)){
      return(list(seq(start_date, end_date, by = "days")))
    } else {
      return(list(start_date))
    }
  }

  # Helper function for study day sequence
  exdy_list <- function(stdy, endy) {
    if(stdy > endy) {
      stop(paste0(
        "End day before start day for: ", stdy, " to ", endy
      ))
    }

    if(!is.na(endy)){
      return(list(seq(stdy, endy)))
    } else {
      return(list(stdy))
    }
  }

  ex %>%
    assertr::verify(assertr::has_all_names(
      "USUBJID", "EXTRT", "EXSTDTC", "EXENDTC"
      )) %>%
    {if(!"IMPUTATION" %in% names(.))
      mutate(., IMPUTATION = "") else .} %>%

    decompose_dtc(c("EXSTDTC", "EXENDTC")) %>%

    # expand dates
    # to do: implement dose frequencies other than QD (e.g., BID)
    rowwise() %>%
    mutate(DTC_date = date_list(.data$EXSTDTC_date, .data$EXENDTC_date)) %>%
    {if(all(c("EXSTDY", "EXENDY") %in% names(ex)))
      mutate(., EXDY = exdy_list(EXSTDY, EXENDY)) else .} %>%

    tidyr::unnest(any_of(c("DTC_date", "EXDY"))) %>%

    group_by(.data$USUBJID, .data$EXTRT, .data$EXENDTC_date) %>%

    mutate(DTC_time = case_when(
      row_number() == n() & !is.na(EXENDTC_time) ~ .data$EXENDTC_time,
      # row_number() == n() & is.na(EXENDTC_time) ~ .data$EXSTDTC_time,
      .default = .data$EXSTDTC_time
    )) %>%

    # make imputation field
    mutate(IMPUTATION = case_when(
      row_number() == n() & !is.na(EXENDTC_time) ~ .data$IMPUTATION, # no comment
      row_number() == n() & row_number() != 1 & is.na(EXENDTC_time) &
        !is.na(EXSTDTC_time) ~ "time carried forward",
      row_number() == 1 & !is.na(EXSTDTC_time) ~ .data$IMPUTATION, # no comment
      !is.na(EXSTDTC_time) ~ "time carried forward",
      .default = "no time information"
    )) %>%

    ungroup() %>%
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    select(-c("EXSTDTC_date", "EXSTDTC_time", "EXENDTC_date", "EXENDTC_time",
              "DTC_date", "DTC_time"))
}
