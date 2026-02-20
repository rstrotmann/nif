#' Standard imputation rule set
#'
#' A list containing the standard imputation functions to be used in
#' add_administration().
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{admin_pre_expansion}{A function to conduct imputations on the EX domain
#'   before expansion of the administration episodes.}
#'   \item{admin_post_expansion}{A function to conduct imputations on the
#'   administration data table after expansion of the administration episodes}
#'   \item{obs_raw}{A function to conduct imputations on the observation data.
#'   At this stage, time fields other than NTIME are not derived yet.}
#'   \item{obs_final}{A function to conduct imputations on the nif data table
#'   including the new observations, before finalization. New observations are
#'   flagged by .current_observation == TRUE.}
#' }
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
      ex, sdtm,
      extrt,analyte = NULL, pctestcd = NULL, cut_off_date = NULL, silent = NULL
    ) {
    # impute missing administration times from PCRFTDTC where available
    get_admin_time_from_pcrfdtc(ex, sdtm, extrt, pctestcd, silent)
  }
)


#' Void imputation rule set
#'
#' * No administration time imputations
#' * No imputations on observations
#'
#' @export
imputation_rules_none <- list(

)


#' Alternative imputation rule set
#'
#' * No administration time imputations
#' * TAFD is set to 0 for predose observations
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
