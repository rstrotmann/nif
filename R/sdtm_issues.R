#' Identifies data inconsistencies and potential issues in the EX domain
#'
#' @param ex The EX domain as domain.
#' @param extrt The treatment, as character.
#' @param silent Suppress reporting, as logical.
#'
#' @returns A list.
#' @export
#' @examples
#' dummy <- ex_issues(domain(examplinib_poc_messy, "ex"), "EXAMPLINIB", silent = FALSE)
#'
ex_issues <- function(
    ex,
    extrt,
    silent = TRUE) {
  # validate ex domain
  expected_ex_columns <- c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")
  missing_ex_columns <- setdiff(expected_ex_columns, names(ex))
  n_missing = length(missing_ex_columns)
  if(n_missing > 0)
    stop(paste0(
      "Missing ", plural("colum", n > 1), " in domain EX: ",
      nice_enumeration(missing_ex_columns)))

  # validate treatment
  validate_char_param(extrt, "extrt")
  if(!extrt %in% unique(ex$EXTRT))
    stop(paste("Treatment not found: ", extrt))

  filtered_ex <- filter(ex, .data$EXTRT == extrt)

  missing_exendtc = filtered_ex %>%
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

    exstdtc_after_exendtc = filtered_ex %>%
      lubrify_dates() %>%
      decompose_dtc(c("EXSTDTC", "EXENDTC")) %>%
      filter(
        as.Date(.data$EXENDTC_date) < as.Date(.data$EXSTDTC_date) |
        !is.na(.data$EXSTDTC_time) & !is.na(.data$EXENDTC_time) &
          .data$EXENDTC < .data$EXSTDTC
      ) %>%
      as_tibble()
  )

  # Reporting
  if(silent == FALSE) {
    if(!is.null(out)) {
      if(any(lapply(out, nrow) > 0)) {
        cli::cli_h1(paste0("Data issues in EX for treatment ", extrt))

        if(nrow(out$missing_last_exendtc) > 0) {
          n_missing_final_exendtc <- nrow(out$missing_last_exendtc)
          cli::cli({
            cli::cli_h2("Missing EXENDTC in final administration episode")
            # cli::cli_alert_danger("Missing EXENDTC in final administration episode")
            cli_text(
              "The following ", plural("subject", n_missing_final_exendtc > 1),
              " had a missing EXENDTC in the final administration episode. ",
              "This may indicate ongoing treatment at the data cut off, ",
              "and the EXENDTC will be imputed to be the cut-off date.")
            cli::cli_text()
            cli::cli_verbatim(df_to_string(
              select(
                out$missing_last_exendtc,
                any_of(c("USUBJID", "EPOCHm", "EXTRT", "EXDOSE", "EXSEQ", "EXSTDTC", "EXENDTC"))),
              indent = 2))
          })
        }

        if(nrow(out$missing_non_last_exendtc) >0) {
          n_missing_exendtc <- nrow(out$missing_non_last_exendtc)
          cli::cli({
            cli::cli_h2("Missing EXENDTC in other administration episode")
            cli_text(
              "The following non-final administration ",
              plural("episode", n_missing_exendtc > 1),
              " had a missing EXENDTC. This usually implies incomplete data ",
              "cleaning. The EXENDTC will be imputed to the day before the next ",
              "administration episode.")
            cli::cli_text()
            cli::cli_verbatim(df_to_string(
              select(
                out$missing_non_last_exendtc,
                any_of(c("USUBJID", "EPOCHm", "EXTRT", "EXDOSE", "EXSEQ", "EXSTDTC", "EXENDTC"))),
              indent = 2))
          })
        }

        if(nrow(out$exstdtc_after_exendtc) > 0) {
          n_exstdtc_after_exendtc <- nrow(out$exstdtc_after_exendtc)
          cli::cli({
            cli::cli_h2("EXENDTC before EXSTDTC")
            cli_text(
              "The following administration ",
              plural("episode", n_exstdtc_after_exendtc > 1),
              " had an EXSTDTC after the EXENDTC. These administration episodes ",
              "will be removed from the data set:")
            cli::cli_text()
            cli::cli_verbatim(df_to_string(
              select(
                out$exstdtc_after_exendtc,
                any_of(c("USUBJID", "EPOCHm", "EXTRT", "EXDOSE", "EXSEQ", "EXSTDTC", "EXENDTC"))),
              indent = 2))
          })
        }
      }
    }
  }

  return(out)
}





