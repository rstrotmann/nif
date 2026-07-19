# Standard imputation rule set

Default imputation rule set for
[`add_administration()`](add_administration.md) and
[`add_observation()`](add_observation.md). Canonical documentation for
the administration and observation steps; other rule sets document only
how they differ.

## Usage

``` r
imputation_rules_standard
```

## Format

A named list with these functions:

- admin_pre_expansion:

  Cut-off filter and EXENDTC imputations before expansion.

- admin_post_expansion:

  NTIME then PCRFTDTC administration-time imputation after expansion.

- obs_raw:

  BLQ / LLOQ imputation on raw PC observations.

- obs_final:

  Predose TAFD adjustment.

## Steps in this rule set

Documented in execution order. Helper names match the implementation.

### Host steps (not in this list)

These always run in [`add_administration()`](add_administration.md) /
`make_administration()`, regardless of the rule set:

- Last-episode `EXENDTC` from `DM.RFENDTC`
  (`impute_exendtc_to_rfendtc`).

- Episode expansion (`expand_ex`) between pre- and post-expansion (QD;
  `EXDOSFRQ` is not used). Start/end times come from `EXSTDTC` /
  `EXENDTC` when present; other days are missing until later steps.

- Administration time carry-forward after post-expansion.

### `admin_pre_expansion`

1.  `apply_cut_off_date` — delete episodes with `EXSTDTC` after the
    cut-off date.

2.  `impute_exendtc_to_cutoff` — if the last episode still has missing
    `EXENDTC`, set it to the cut-off date (e.g. ongoing treatment).

3.  `impute_missing_exendtc` — for non-last episodes with missing
    `EXENDTC`, set it to the day before the next episode's `EXSTDTC`.

4.  `filter_exendtc_after_exstdtc` — remove episodes where `EXENDTC` is
    before `EXSTDTC`.

### `admin_post_expansion`

1.  `get_admin_time_from_ntime` — back-calculate administration time
    from PK nominal times (`PCTPT` / NTIME). When an estimate exists, it
    replaces the current `DTC_time` (including times from EX).

2.  `get_admin_time_from_pcrftdtc` — set `DTC_time` from `PC.PCRFTDTC`
    when available; this overwrites a prior NTIME estimate for that day.
    Pass `pctestcd` to [`add_administration()`](add_administration.md)
    for the matching `PCTESTCD`.

After this slot, host carry-forward fills remaining missing times.

### `obs_raw`

- `impute_lloq_pc` — pharmacokinetic observations below the limit of
  quantification are set to `PCLLOQ / 2`.

### `obs_final`

- For predose observations of the current analyte, set `TAFD` to zero
  when it would otherwise be negative.

## Creating custom imputation rules

You can create your own imputation rule set by providing a named list
with any combination of the four function slots: `admin_pre_expansion`,
`admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
receives specific arguments depending on its slot. See the
implementations in this file for the expected signatures.

## See also

[`add_administration()`](add_administration.md),
[`add_observation()`](add_observation.md),
[`imputation_rules_minimal()`](imputation_rules_minimal.md),
[`imputation_rules_1()`](imputation_rules_1.md),
[`imputation_rules_void()`](imputation_rules_void.md)

Other imputation rules: [`imputation_rules_1`](imputation_rules_1.md),
[`imputation_rules_minimal`](imputation_rules_minimal.md),
[`imputation_rules_void`](imputation_rules_void.md)
