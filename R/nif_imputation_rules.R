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
imputation_standard <- list(
  admin_pre_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    dm <- lubrify_dates(domain(sdtm, "dm"))

    ex |>
      filter(.data$EXSTDTC <= cut_off_date) |>
      impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    # impute missing administration times from PCRFTDTC where available
    ex |>
      # get time from PCRFTDTC
      get_admin_time_from_pcrfdtc(sdtm, extrt, analyte, silent) |>
      carry_forward_admin_time_imputations()
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
    dm <- domain(sdtm, "dm")
    ex |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  admin_post_expansion = function(ex, sdtm, extrt, analyte, cut_off_date, silent) {
    ex |>
      # carry forward imputed times
      # group_by(USUBJID, EXTRT, EXSTDTC_date) |>
      # mutate(IMPUTATION = case_when(
      #   is.na(.data$DTC_time) ~ "time carried forward",
      #   .default = .data$IMPUTATION
      # )) |>
      # fill(DTC_time, .direction = "down") |>
      # ungroup()
      carry_forward_admin_time_imputations()
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
    ex |>
      get_admin_time_from_pcrfdtc(sdtm, extrt, analyte, silent) |>
      get_admin_time_from_ntime(sdtm, extrt, analyte, silent) |>

      # first priority: Get admin time from PCRFTDTC
      mutate(IMPUTATION = case_when(
        !is.na(.data$.PCRFTDTC_DTC_time) ~ "time copied from PCRFTDTC",
        .default = .data$IMPUTATION
      )) |>
      mutate(DTC_time = case_when(
        !is.na(.data$.PCRFTDTC_DTC_time) ~ .data$.PCRFTDTC_DTC_time,
        .default = .data$DTC_time
      )) |>

      # second priority: Get admin time from NTIME
      mutate(IMPUTATION = case_when(
        (is.na(.data$.PCRFTDTC_DTC_time) & !is.na(.data$.NTIME_DTC_time)) ~ "time imputed from PCTPT",
        .default = .data$IMPUTATION
      )) |>
      mutate(DTC_time = case_when(
        (is.na(.data$.PCRFTDTC_DTC_time) & !is.na(.data$.NTIME_DTC_time)) ~ .data$.NTIME_DTC_time,
        .default = .data$DTC_time
      )) |>

      # carry forward imputed times
      group_by(USUBJID, EXTRT, EXSTDTC_date) |>
      mutate(IMPUTATION = case_when(
        is.na(.data$DTC_time) ~ "time carried forward",
        .default = .data$IMPUTATION
      )) |>
      fill(DTC_time, .direction = "down") |>
      ungroup() |>
      select(-c(".PCRFTDTC_DTC_time"))
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
