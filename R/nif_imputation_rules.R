#' Minimal imputation rule set
#'
#' Same pre-expansion steps as [nif::imputation_rules_standard()], but
#' post-expansion uses PCRFTDTC only (no NTIME back-calculation). No
#' data imputations for observations.
#'
#' @format A named list with these functions:
#' \describe{
#'   \item{admin_pre_expansion}{Data imputations before
#'   expansion of individual drug administration episodes.}
#'   \item{admin_post_expansion}{Data imputations after expansion
#'   of drug administration episodes.}
#'   \item{obs_raw}{Imputations on raw observation data.}
#'   \item{obs_final}{Imputations on  observation data}
#' }
#'
#' @details
#'
#' #' @section Imputation rules for drug administrations:
#'
#' ### Generic imputations
#'
#' These steps are always performed regardless of the specific rule set:
#'
#' * Missing end date-time of the last administration episode. If missing,
#' `EXENDTC` of the last administration episode for each subject is set to
#' `DM.RFENDTC`
#' * Expansion of individual administration episode, i.e., conversion of date
#' ranges between `EXSTDTC` and `EXENDTC` into one row for each individual
#' administration event. This step is conducted after `admin_pre_examsion()`
#' and `admin_post_expansion()`.
#' * Carry-forward of missing administration times after
#' `admin_post_expansion()`.
#'
#' ### `admin_pre_expansion`
#'
#' Same as [nif::imputation_rules_standard()]: `apply_cut_off_date`,
#' `impute_exendtc_to_cutoff`, `impute_missing_exendtc`, then
#' `filter_exendtc_after_exstdtc`.
#'
#' ### `admin_post_expansion`
#'
#' 1. `get_admin_time_from_pcrftdtc` — set `DTC_time` from `PC.PCRFTDTC` when
#'    related PK observations exist.
#'
#' After these imputations, remaining missing administration times are carried
#' forward.
#'
#' @section Imputation rules for observations:
#'
#' ## `obs_raw` / `obs_final`
#'
#' No observation imputations.
#'
#' @inheritSection imputation_rules_standard Creating custom imputation rules
#'
#' @seealso [nif::add_administration()], [nif::add_observation()],
#'   [nif::imputation_rules_standard()], [nif::imputation_rules_void()]
#'
#' @family imputation rules
#'
#' @export
imputation_rules_minimal <- list(
  # pre-expansion: impute EXENDTC where needed
  admin_pre_expansion = function(
      ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
    ) {
    dm <- lubrify_dates(domain(sdtm, "dm"))

    ex |>
      apply_cut_off_date(extrt, cut_off_date, silent = silent) |>
      impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  # post-expansion: impute time from PCRFTDTC where possible
  admin_post_expansion = function(
      ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
    ) {
    get_admin_time_from_pcrftdtc(ex, sdtm, extrt, pctestcd, silent)
  },

  # raw observations: no action
  obs_raw = function (obs, silent) {
    obs
  },

  # final observations: no action
  obs_final = function(obs, silent) {
    obs
  }
)


#' Void imputation rule set
#'
#' Empty rule list: no pre-/post-expansion or observation slot functions.
#' Host steps in `add_administration()` / `make_administration()` still run
#' (RFENDTC fill, episode expansion, time carry-forward).
#'
#' @format An empty list.
#'
#' @details
#' # Steps in this rule set
#'
#' This object defines no slot functions. Behavior is limited to host steps:
#'
#' * Last-episode `EXENDTC` from `DM.RFENDTC` (`impute_exendtc_to_rfendtc`).
#' * Episode expansion (`expand_ex`); times from `EXSTDTC` / `EXENDTC` when
#'   present.
#' * Administration time carry-forward after expansion.
#'
#' There is no cut-off filter, no EXENDTC cutoff/middle imputation via rules,
#' no PCRFTDTC or NTIME time imputation, and no observation imputations from
#' this object.
#'
#' @inheritSection imputation_rules_standard Creating custom imputation rules
#'
#' @seealso [nif::add_administration()], [nif::add_observation()],
#'   [nif::imputation_rules_standard()]
#' @family imputation rules
#'
#' @export
imputation_rules_void <- list()



#' Standard imputation rule set
#'
#' Imputations that are applied when administrations or observations are added
#' to a NIF data set are bundled in imputation rule sets.
#'
#' This is the default imputation rule set for [nif::add_administration()] and
#' [nif::add_observation()].
#'
#' @format A named list with these functions:
#' \describe{
#'   \item{admin_pre_expansion}{Data imputations before
#'   expansion of individual drug administration episodes.}
#'   \item{admin_post_expansion}{Data imputations after expansion
#'   of drug administration episodes.}
#'   \item{obs_raw}{Imputations on raw observation data.}
#'   \item{obs_final}{Imputations on  observation data}
#' }
#'
#' @details
#'
#' @section Imputation rules for drug administrations:
#'
#' ### Generic imputations
#'
#' These steps are always performed regardless of the specific rule set:
#'
#' * Missing end date-time of the last administration episode. If missing,
#' `EXENDTC` of the last administration episode for each subject is set to
#' `DM.RFENDTC`
#' * Expansion of individual administration episode, i.e., conversion of date
#' ranges between `EXSTDTC` and `EXENDTC` into one row for each individual
#' administration event. This step is conducted after `admin_pre_examsion()`
#' and `admin_post_expansion()`.
#' * Carry-forward of missing administration times after
#' `admin_post_expansion()`.
#'
#' ### `admin_pre_expansion`
#'
#' 1. `apply_cut_off_date` — delete episodes with `EXSTDTC` after the global
#'    cut-off date.
#' 2. `impute_exendtc_to_cutoff` — if the last episode still has missing
#'    `EXENDTC`, set it to the cut-off date (e.g. for ongoing treatment).
#' 3. `impute_missing_exendtc` — for non-last episodes with missing
#'    `EXENDTC`, set it to the day before the next episode's `EXSTDTC`.
#' 4. `filter_exendtc_after_exstdtc` — remove episodes where `EXENDTC` is
#'    before `EXSTDTC`.
#'
#' ### `admin_post_expansion`
#'
#' 1. `get_admin_time_from_pcrftdtc` — Use the `PCRFTDTC` field from the PC
#'    domain, if available, to complete missing administration times.
#' 2. `get_admin_time_from_ntime` — back-calculate administration time, if still
#'    missing from the nominal PK observation times in `PCTPT`, if available.
#'
#' After these imputations, remaining missing administration times are carried
#' forward.
#'
#' @section Imputation rules for observations:
#'
#' ### `obs_raw`
#'
#' * `impute_lloq_pc` — pharmacokinetic observations below the limit of
#'   quantification are set to `PCLLOQ / 2`.
#'
#' ### `obs_final`
#'
#' * For predose observations of the current analyte, set `TAFD` to zero when
#'   it would otherwise be negative.
#'
#' @section Creating custom imputation rules:
#' You can create further imputation rule sets by providing a named list
#' with any combination of the four function slots: `admin_pre_expansion`,
#' `admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
#' receives specific arguments depending on its slot. See the implementations
#' in this file for the expected signatures.
#'
#' @seealso [nif::add_administration()], [nif::add_observation()],
#'   [nif::imputation_rules_minimal()], [nif::imputation_rules_1()],
#'   [nif::imputation_rules_void()]
#'
#' @family imputation rules
#'
#' @export
imputation_rules_standard <- list(
  # pre-expansion: impute EXENDTC where needed
  admin_pre_expansion = function(
    ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
  ) {
    dm <- lubrify_dates(domain(sdtm, "dm"))

    ex |>
      apply_cut_off_date(extrt, cut_off_date, silent = silent) |>
      impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  # post expansion: impute NTIME from PCRFTDTC or from NTIME
  admin_post_expansion = function(
      ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
    ) {
    ex |>
      get_admin_time_from_ntime(
        sdtm, extrt = extrt, pctestcd = pctestcd, silent = silent
      ) |>
      get_admin_time_from_pcrftdtc(
        sdtm, extrt = extrt, pctestcd = pctestcd, silent = silent
      )
  },

  # raw observations: no action
  obs_raw = function (obs, silent) {
    # obs
    impute_lloq_pc(obs, silent = silent)
  },

  # final observations: Set TAFD to zero for predose
  obs_final = function(obs, silent) {
    obs |>
      mutate(TAFD = case_when(
        .current_observation == TRUE & TAFD < 0 ~ 0,
        .default = TAFD)
      )
  }
)



#' Alternative imputation rule set 1
#'
#' Same pre-expansion steps as [nif::imputation_rules_standard()], with a
#' different post-expansion time rule and no `obs_raw` imputation.
#'
#' @format A named list with these functions:
#' \describe{
#'   \item{admin_pre_expansion}{Data imputations before
#'   expansion of individual drug administration episodes.}
#'   \item{admin_post_expansion}{Data imputations after expansion
#'   of drug administration episodes.}
#'   \item{obs_raw}{Imputations on raw observation data.}
#'   \item{obs_final}{Imputations on  observation data}
#' }
#'
#' @details
#'
#' @section Imputation rules for drug administrations:
#'
#' ### Generic imputations
#'
#' These steps are always performed regardless of the specific rule set. Same
#' as [nif::imputation_rules_standard()]:
#' * RFENDTC fill
#' * `expand_ex`
#' * time carry-forward.
#'
#' ### `admin_pre_expansion`
#'
#' Same as [nif::imputation_rules_standard()].
#'
#' ### `admin_post_expansion`
#'
#' 1. Compute an NTIME-based administration time via
#'    `get_admin_time_from_ntime`
#' 2. Apply `get_admin_time_from_pcrftdtc` when PCRFTDTC is available.
#' 3. If PCRFTDTC is absent and the NTIME-derived time differs from the
#'    EX-derived time by more than 10 minutes, use the NTIME-derived time;
#'    otherwise keep the EX-derived time.
#'
#' After these imputations, remaining missing administration times are carried
#' forward.
#'
#' @section Imputation rules for observations:
#'
#' ### `obs_raw`
#'
#' No action (LLOQ imputation is not applied in this rule set).
#'
#' ### `obs_final`
#'
#' Same as [nif::imputation_rules_standard()]: predose `TAFD` is set to zero
#' when negative.
#'
#' @inheritSection imputation_rules_standard Creating custom imputation rules
#'
#' @seealso [nif::add_administration()], [nif::add_observation()],
#'   [nif::imputation_rules_standard()], [nif::imputation_rules_void()]
#'
#' @family imputation rules
#'
#' @export
imputation_rules_1 <- list(
  # pre-expansion: impute EXENDTC where needed
  admin_pre_expansion = function(
    ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
  ) {
    dm <- lubrify_dates(domain(sdtm, "dm"))

    ex |>
      apply_cut_off_date(extrt, cut_off_date, silent = silent) |>
      impute_exendtc_to_cutoff(cut_off_date = cut_off_date, silent = silent) |>
      impute_missing_exendtc(silent = silent) |>
      filter_exendtc_after_exstdtc(dm, extrt, silent = silent)
  },

  # post expansion: impute NTIME from PCRFTDTC or from NTIME
  admin_post_expansion = function(
    ex, sdtm, extrt, analyte, pctestcd, cut_off_date, silent
  ) {
    temp <- ex |>
      get_admin_time_from_ntime(
        sdtm, extrt, pctestcd, silent
      ) |>
      mutate(.ntime_time = .data$DTC_time) |>
      pull(.data$.ntime_time)

    ex |>
      mutate(.ntime_time = temp) |>
      get_admin_time_from_pcrftdtc(
        sdtm, extrt, pctestcd, silent
      ) |>
      mutate(.dtc_ex = compose_dtc(DTC_date, DTC_time)) |>
      mutate(.dtc_ntime = compose_dtc(DTC_date, .ntime_time)) |>
      mutate(DTC_time = case_when(
        !is.na(.data$.PCRFTDTC_DTC_time) ~ .data$.PCRFTDTC_DTC_time,
        difftime(.data$.dtc_ex, .data$.dtc_ntime, units = "mins") > 10 ~ .data$.ntime_time,
        .default = .data$DTC_time
      ))
  },

  # raw observations: no action
  obs_raw = function (obs, silent) {
    obs
    # impute_lloq_pc(obs, silent = silent)
  },

  # final observations: Set TAFD to zero for predose
  obs_final = function(obs, silent) {
    obs |>
      mutate(TAFD = case_when(
        .data$.current_observation == TRUE & .data$TAFD < 0 ~ 0,
        .default = .data$TAFD)
      )
  }
)
