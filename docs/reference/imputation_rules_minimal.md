# Minimal imputation rule set

Same pre-expansion steps as
[`imputation_rules_standard()`](imputation_rules_standard.md), but
post-expansion uses PCRFTDTC only (no NTIME back-calculation).
Observation slots are no-ops.

## Usage

``` r
imputation_rules_minimal
```

## Format

A named list with these functions:

- admin_pre_expansion:

  Cut-off filter and EXENDTC imputations (same as standard).

- admin_post_expansion:

  Administration time from PCRFTDTC only.

- obs_raw:

  Identity (no change).

- obs_final:

  Identity (no change).

## Steps in this rule set

### Host steps (not in this list)

These always run in [`add_administration()`](add_administration.md) /
`make_administration()`, regardless of the rule set:

- Last-episode `EXENDTC` from `DM.RFENDTC`
  (`impute_exendtc_to_rfendtc`).

- Episode expansion (`expand_ex`) between pre- and post-expansion.

- Administration time carry-forward after post-expansion.

### `admin_pre_expansion`

Same as [`imputation_rules_standard()`](imputation_rules_standard.md):
`apply_cut_off_date`, `impute_exendtc_to_cutoff`,
`impute_missing_exendtc`, then `filter_exendtc_after_exstdtc`.

### `admin_post_expansion`

1.  `get_admin_time_from_pcrftdtc` — set `DTC_time` from `PC.PCRFTDTC`
    when related PK observations exist. Pass `pctestcd` to
    [`add_administration()`](add_administration.md) for the matching
    `PCTESTCD`.

After this slot, host carry-forward fills remaining missing times.
Unless set by PCRFTDTC or carry-forward, times come from `EXSTDTC` /
`EXENDTC` during expansion.

### `obs_raw` / `obs_final`

No observation imputations.

## Creating custom imputation rules

You can create your own imputation rule set by providing a named list
with any combination of the four function slots: `admin_pre_expansion`,
`admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
receives specific arguments depending on its slot. See the
implementations in this file for the expected signatures.

## See also

[`add_administration()`](add_administration.md),
[`add_observation()`](add_observation.md),
[`imputation_rules_standard()`](imputation_rules_standard.md),
[`imputation_rules_void()`](imputation_rules_void.md)

Other imputation rules: [`imputation_rules_1`](imputation_rules_1.md),
[`imputation_rules_standard`](imputation_rules_standard.md),
[`imputation_rules_void`](imputation_rules_void.md)
