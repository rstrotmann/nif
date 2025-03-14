#' Compile subject information
#'
#' @param dm The DM domain as data table.
#' @param vs The VS domain as data table.
#' @param subject_filter The filtering to apply to the DM domain.
#' @param keep Columns to keep, as character.
#'
#' @return A data table.
#' @import tidyselect
#' @export
#' @keywords internal
make_subjects <- function(
    dm,
    vs = NULL,
    subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
    keep = "") {
  # if AGE is not present in DM, calculate age from birthday and informed
  #   consent signature date
  if ("RFICDTC" %in% colnames(dm) && "BRTHDTC" %in% colnames(dm)) {
    dm <- dm %>%
      lubrify_dates() %>%
      mutate(age_brthdtc = round(as.numeric(
        as.duration(.data$RFICDTC - .data$BRTHDTC), "years"), 0)) %>%
      {if("AGE" %in% names(dm))
        mutate(., AGE = case_when(
          is.na(.data$AGE) ~ .data$age_brthdtc,
          .default = .data$AGE))
        else
          mutate(., AGE = .data$age_brthdtc)} %>%
      select(-"age_brthdtc")
  }

  if(!is.null(vs)){
    baseline_covariates <- vs %>%
      lubrify_dates() %>%
      left_join(select(dm, c("USUBJID", "RFSTDTC")), by = "USUBJID") %>%
      {if("VSBLFL" %in% names(vs)) filter(., VSBLFL == "Y") else
        filter(., VSDTC < RFSTDTC)} %>%
      filter(VSTESTCD %in% c("WEIGHT", "HEIGHT")) %>%
      group_by(.data$USUBJID, .data$VSTESTCD) %>%
      summarize(mean = mean(.data$VSSTRESN), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = "VSTESTCD", values_from = "mean")

    if ("HEIGHT" %in% colnames(baseline_covariates) &&
        "WEIGHT" %in% colnames(baseline_covariates)) {
      baseline_covariates <- baseline_covariates %>%
        mutate(BMI = .data$WEIGHT / (.data$HEIGHT / 100)^2)
    }
  } else {
    baseline_covariates <- distinct(dm, .data$USUBJID)
  }

  out <- dm %>%
    assertr::verify(
      # assertr::has_all_names("USUBJID", "SEX", "ACTARMCD", "RFXSTDTC")
      assertr::has_all_names("USUBJID", "SEX", "ACTARMCD")
    ) %>%
    lubrify_dates() %>%
    filter(eval(parse(text = subject_filter))) %>%
    # {if(!is.null(vs)) left_join(baseline_covariates, by = "USUBJID") else .} %>%
    left_join(baseline_covariates, by = "USUBJID") %>%
    recode_sex() %>%
    mutate(ID = NA) %>%
    relocate("ID") %>%
    arrange("ID") %>%
    select(., any_of(c(
      "ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "HEIGHT",
      "WEIGHT", "BMI", "ACTARMCD", "RFXSTDTC", "RFSTDTC", keep)))
  return(out)
}
