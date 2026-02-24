#' Standard imputation rule set
#'
#' Standard imputation rule set
#'
#' @format A list of the following functions:
#'
#' * admin_pre_expansion()
#' * admin_post_expansion()
#'
#' @details
#'
#' This imputation rule set includes the  following imputation steps:
#'
#' ## Treatment administrations:
#'
#' * Filter administrations to the cut-off date.
#'
#' * Impute missing EXENDTC values in the last administration episode to the
#' cut-off date.
#'
#' * Impute missing (non-last) EXENDTC values to the day before the start of the
#' subsequent administration episode.
#'
#' * Remove records where EXENDTC is before EXSTDTC.
#'
#' * Expand administration episodes from the EX domain between EXSTDTC and
#' EXENDTC.
#'
#' * For each administration event, take the administration time from
#' PCRFTDTC of the PC domain if there are related pharmacokinetic observations.
#' The name of the PK analyte (PCTESTCD) that corresponds with the administered
#' treatment (EXTRT) must be specified by the 'pctestcd' to
#' add_administration().
#'
#' * Unless imputed by the above rule, administrations inherit the
#' administration time from EXSTDTC or EXENDTC.
#'
#' * After the above imputations, carry forward the administration time for
#' subsequent administration events until the next imputed time.
#'
#' ## Observations
#'
#' * No imputations.
#'
#'@section Creating custom imputation rules:
#' You can create your own imputation rule set by providing a named list
#' with any combination of the four function slots: `admin_pre_expansion`,
#' `admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
#' receives specific arguments depending on its slot.
#'
#' @seealso add_administration()
#' @seealso add_observation()
#'
#' @family imputation rules
#'
#' @export
imputation_rules_standard <- list(
  admin_pre_expansion = function(
      ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
    ) {
    dm <- lubrify_dates(domain(sdtm, "dm"))

    ex |>
      filter(.data$EXSTDTC <= cut_off_date) |>
      impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  admin_post_expansion = function(
      ex, sdtm, extrt, analyte = NULL, pctestcd = NULL, cut_off_date = NULL,
      silent = NULL
    ) {
    # impute missing administration times from PCRFTDTC where available
    get_admin_time_from_pcrfdtc(ex, sdtm, extrt, pctestcd, silent)
  }
)


#' Minimal imputation rule set
#'
#' @format An empty list.
#'
#' @details
#'
#' This imputation rule set includes the following minimal imputation steps:
#'
#' ## Treatment administrations:
#'
#' * Filter administrations to the cut-off date.
#'
#' * Impute missing EXENDTC values in the last administration episode to the
#' cut-off date.
#'
#' * Impute missing (non-last) EXENDTC values to the day before the start of the
#' subsequent administration episode.
#'
#' * Remove records where EXENDTC is before EXSTDTC.
#'
#' * Expand administration episodes from the EX domain between EXSTDTC and
#' EXENDTC.
#'
#' * Administrations inherit the administration time from EXSTDTC or EXENDTC.
#'
#' * After the above imputations, the administration time is carried forward for
#' subsequent administration events until the next imputed time.
#'
#' @section Creating custom imputation rules:
#' You can create your own imputation rule set by providing a named list
#' with any combination of the four function slots: `admin_pre_expansion`,
#' `admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
#' receives specific arguments depending on its slot.
#'
#' @seealso add_administration()
#' @seealso add_observation()
#' @family imputation rules
#'
#' @export
imputation_rules_minimal <- list()



#' Alternative imputation rule set
#'
#' @format A list of the following functions:
#'
#' * admin_post_expansion()
#' * obs_final()
#'
#' @details
#'
#' This imputation rule set includes the following imputation steps:
#'
#' ## Treatment administrations:
#'
#' * Filter administrations to the cut-off date.
#'
#' * Impute missing EXENDTC values in the last administration episode to the
#' cut-off date.
#'
#' * Impute missing (non-last) EXENDTC values to the day before the start of the
#' subsequent administration episode.
#'
#' * Remove records where EXENDTC is before EXSTDTC.
#'
#' * Expand administration episodes from the EX domain between EXSTDTC and
#' EXENDTC.
#'
#' * For each administration event, take the administration time from
#' PCRFTDTC of the PC domain if there are related pharmacokinetic observations.
#' The name of the PK analyte (PCTESTCD) that corresponds with the administered
#' treatment (EXTRT) must be specified by the 'pctestcd' to
#' add_administration().
#'
#' * For administration events that have associated PK observations but PCRFTDTC
#' is not defined, back-calculate the administration time, if possible, from
#' the PK observations based on their nominal time (PCTPT).
#'
#' * Unless imputed by the above rules, administrations inherit the
#' administration time from EXSTDTC or EXENDTC.
#'
#' * After the above imputations, the administration time is carried forward for
#' subsequent administration events until the next imputed time.
#'
#'
#' ## Observations
#'
#' * For all predose observations, TAFD is set to zero.
#'
#' @section Creating custom imputation rules:
#' You can create your own imputation rule set by providing a named list
#' with any combination of the four function slots: `admin_pre_expansion`,
#' `admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
#' receives specific arguments depending on its slot.
#'
#' @seealso add_administration()
#'
#' @seealso add_observation()
#' @family imputation rules
#'
#' @export
imputation_rules_1 <- list(
  admin_post_expansion = function(
      ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
    ) {
    ex |>
      get_admin_time_from_ntime(
        sdtm, extrt = "TREATMENT_A", pctestcd = pctestcd, silent = FALSE
      ) |>
      get_admin_time_from_pcrfdtc(
        sdtm, extrt = "TREATMENT_A", pctestcd = pctestcd, silent = FALSE
      )
  },

  obs_final = function(obs, silent) {
    obs |>
      mutate(TAFD = case_when(
        .current_observation == TRUE & TAFD < 0 ~ 0,
        .default = TAFD)
      )
  }
)
