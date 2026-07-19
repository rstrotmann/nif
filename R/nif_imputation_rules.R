#' Minimal imputation rule set
#'
#' Same pre-expansion steps as [nif::imputation_rules_standard()], but post-expansion
#' uses PCRFTDTC only (no NTIME back-calculation). Observation slots are
#' no-ops.
#'
#' @format A named list with these functions:
#' \describe{
#'   \item{admin_pre_expansion}{Cut-off filter and EXENDTC imputations (same as
#'   standard).}
#'   \item{admin_post_expansion}{Administration time from PCRFTDTC only.}
#'   \item{obs_raw}{Identity (no change).}
#'   \item{obs_final}{Identity (no change).}
#' }
#'
#' @details
#' # Steps in this rule set
#'
#' ## Host steps (not in this list)
#'
#' These always run in `add_administration()` / `make_administration()`,
#' regardless of the rule set:
#'
#' * Last-episode `EXENDTC` from `DM.RFENDTC` (`impute_exendtc_to_rfendtc`).
#' * Episode expansion (`expand_ex`) between pre- and post-expansion.
#' * Administration time carry-forward after post-expansion.
#'
#' ## `admin_pre_expansion`
#'
#' Same as [nif::imputation_rules_standard()]: `apply_cut_off_date`,
#' `impute_exendtc_to_cutoff`, `impute_missing_exendtc`, then
#' `filter_exendtc_after_exstdtc`.
#'
#' ## `admin_post_expansion`
#'
#' 1. `get_admin_time_from_pcrftdtc` — set `DTC_time` from `PC.PCRFTDTC` when
#'    related PK observations exist. Pass `pctestcd` to
#'    [nif::add_administration()] for the matching `PCTESTCD`.
#'
#' After this slot, host carry-forward fills remaining missing times. Unless
#' set by PCRFTDTC or carry-forward, times come from `EXSTDTC` / `EXENDTC`
#' during expansion.
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
#' Default imputation rule set for [nif::add_administration()] and
#' [nif::add_observation()]. Canonical documentation for the administration and
#' observation steps; other rule sets document only how they differ.
#'
#' @format A named list with these functions:
#' \describe{
#'   \item{admin_pre_expansion}{Cut-off filter and EXENDTC imputations before
#'   expansion.}
#'   \item{admin_post_expansion}{NTIME then PCRFTDTC administration-time
#'   imputation after expansion.}
#'   \item{obs_raw}{BLQ / LLOQ imputation on raw PC observations.}
#'   \item{obs_final}{Predose TAFD adjustment.}
#' }
#'
#' @details
#' # Steps in this rule set
#'
#' Documented in execution order. Helper names match the implementation.
#'
#' ## Host steps (not in this list)
#'
#' These always run in `add_administration()` / `make_administration()`,
#' regardless of the rule set:
#'
#' * Last-episode `EXENDTC` from `DM.RFENDTC` (`impute_exendtc_to_rfendtc`).
#' * Episode expansion (`expand_ex`) between pre- and post-expansion (QD;
#'   `EXDOSFRQ` is not used). Start/end times come from `EXSTDTC` / `EXENDTC`
#'   when present; other days are missing until later steps.
#' * Administration time carry-forward after post-expansion.
#'
#' ## `admin_pre_expansion`
#'
#' 1. `apply_cut_off_date` — delete episodes with `EXSTDTC` after the cut-off
#'    date.
#' 2. `impute_exendtc_to_cutoff` — if the last episode still has missing
#'    `EXENDTC`, set it to the cut-off date (e.g. ongoing treatment).
#' 3. `impute_missing_exendtc` — for non-last episodes with missing
#'    `EXENDTC`, set it to the day before the next episode's `EXSTDTC`.
#' 4. `filter_exendtc_after_exstdtc` — remove episodes where `EXENDTC` is
#'    before `EXSTDTC`.
#'
#' ## `admin_post_expansion`
#'
#' 1. `get_admin_time_from_ntime` — back-calculate administration time from PK
#'    nominal times (`PCTPT` / NTIME). When an estimate exists, it replaces
#'    the current `DTC_time` (including times from EX).
#' 2. `get_admin_time_from_pcrftdtc` — set `DTC_time` from `PC.PCRFTDTC` when
#'    available; this overwrites a prior NTIME estimate for that day. Pass
#'    `pctestcd` to [nif::add_administration()] for the matching `PCTESTCD`.
#'
#' After this slot, host carry-forward fills remaining missing times.
#'
#' ## `obs_raw`
#'
#' * `impute_lloq_pc` — pharmacokinetic observations below the limit of
#'   quantification are set to `PCLLOQ / 2`.
#'
#' ## `obs_final`
#'
#' * For predose observations of the current analyte, set `TAFD` to zero when
#'   it would otherwise be negative.
#'
#' @section Creating custom imputation rules:
#' You can create your own imputation rule set by providing a named list
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
#' Same pre-expansion steps as [nif::imputation_rules_standard()], with a different
#' post-expansion time rule (10-minute NTIME vs EX gate) and a no-op
#' `obs_raw`.
#'
#' @format A named list with these functions:
#' \describe{
#'   \item{admin_pre_expansion}{Cut-off filter and EXENDTC imputations (same as
#'   standard).}
#'   \item{admin_post_expansion}{NTIME and PCRFTDTC with a 10-minute EX
#'   comparison.}
#'   \item{obs_raw}{Identity (no LLOQ imputation).}
#'   \item{obs_final}{Predose TAFD adjustment (same as standard).}
#' }
#'
#' @details
#' # Steps in this rule set
#'
#' ## Host steps (not in this list)
#'
#' Same as [nif::imputation_rules_standard()]: RFENDTC fill, `expand_ex`, and
#' time carry-forward.
#'
#' ## `admin_pre_expansion`
#'
#' Same as [nif::imputation_rules_standard()].
#'
#' ## `admin_post_expansion`
#'
#' 1. Compute an NTIME-based administration time via
#'    `get_admin_time_from_ntime` (stored alongside the current EX-based
#'    time).
#' 2. Apply `get_admin_time_from_pcrftdtc` when PCRFTDTC is available
#'    (takes precedence).
#' 3. If PCRFTDTC is absent and the NTIME-derived time differs from the
#'    EX-derived time by more than 10 minutes, use the NTIME-derived time;
#'    otherwise keep the EX-derived time.
#'
#' After this slot, host carry-forward fills remaining missing times.
#'
#' ## `obs_raw`
#'
#' No-op (LLOQ imputation is not applied in this rule set).
#'
#' ## `obs_final`
#'
#' Same as [nif::imputation_rules_standard()]: predose `TAFD < 0` set to zero for
#' the current observation.
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
