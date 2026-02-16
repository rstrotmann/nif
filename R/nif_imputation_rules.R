#' Standard imputation rules
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
imputation_standard <- list(
  admin_pre_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    dm <- lubrify_dates(domain(sdtm, "dm"))

    ex |>
      impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    # impute missing administration times from PCRFTDTC where available
    if ("pc" %in% names(sdtm$domains)) {
      pc <- lubrify_dates(domain(sdtm, "pc"))
      if ("PCRFTDTC" %in% names(pc)) {
        ex <- impute_admin_from_pcrftdtc(
          ex, pc, analyte, analyte, silent = silent)
      }
    }
    return(ex)
  },

  obs_raw = function(obs, silent) {
    obs
  },

  obs_final = function(obs, silent) {
    obs
  }
)


#' Void imputation rule set
#'
#' * No administration time imputations
#' * No imputations on observations
#'
imputation_none <- list(
  admin_pre_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    ex
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    ex
  },

  obs_raw = function(obs, silent) {
    obs
  },

  obs_final = function(obs, silent) {
    obs
  }
)


#' Alternative imputation rule set
#'
#' * No administration time imputations
#' * TAFD is set to 0 for predose observations
#'
imputation_1 <- list(
  admin_pre_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    ex
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    # ex_date <- ex$EXDTC
    # temp <- get_admin_time_from_pcrfdtc()
    ex |>
      get_admin_time_from_pcrfdtc(sdtm, extrt, analyte, silent)
  },

  obs_raw = function(obs, silent) {
    obs
  },

  obs_final = function(obs, silent) {
    obs |>
      mutate(TAFD = case_when(
        .current_observation == TRUE & TAFD < 0 ~ 0,
        .default = TAFD)
      )
  }
)
