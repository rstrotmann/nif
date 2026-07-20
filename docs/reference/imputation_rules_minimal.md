# Minimal imputation rule set

Same pre-expansion steps as
[`imputation_rules_standard()`](imputation_rules_standard.md), but
post-expansion uses PCRFTDTC only (no NTIME back-calculation). No data
imputations for observations.

## Usage

``` r
imputation_rules_minimal
```

## Format

A named list with these functions:

- admin_pre_expansion:

  Data imputations before expansion of individual drug administration
  episodes.

- admin_post_expansion:

  Data imputations after expansion of drug administration episodes.

- obs_raw:

  Imputations on raw observation data.

- obs_final:

  Imputations on observation data

## Details

\#' @section Imputation rules for drug administrations:

### Generic imputations

These steps are always performed regardless of the specific rule set:

- Missing end date-time of the last administration episode. If missing,
  `EXENDTC` of the last administration episode for each subject is set
  to `DM.RFENDTC`

- Expansion of individual administration episode, i.e., conversion of
  date ranges between `EXSTDTC` and `EXENDTC` into one row for each
  individual administration event. This step is conducted after
  `admin_pre_examsion()` and `admin_post_expansion()`.

- Carry-forward of missing administration times after
  `admin_post_expansion()`.

### `admin_pre_expansion`

Same as [`imputation_rules_standard()`](imputation_rules_standard.md):
`apply_cut_off_date`, `impute_exendtc_to_cutoff`,
`impute_missing_exendtc`, then `filter_exendtc_after_exstdtc`.

### `admin_post_expansion`

1.  `get_admin_time_from_pcrftdtc` — set `DTC_time` from `PC.PCRFTDTC`
    when related PK observations exist.

After these imputations, remaining missing administration times are
carried forward.

## Imputation rules for observations

### `obs_raw` / `obs_final`

No observation imputations.

## Creating custom imputation rules

You can create further imputation rule sets by providing a named list
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
