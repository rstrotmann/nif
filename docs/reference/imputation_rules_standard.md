# Standard imputation rule set

Imputations that are applied when administrations or observations are
added to a NIF data set are bundled in imputation rule sets.

## Usage

``` r
imputation_rules_standard
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

This is the default imputation rule set for
[`add_administration()`](add_administration.md) and
[`add_observation()`](add_observation.md).

## Imputation rules for drug administrations

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

1.  `apply_cut_off_date` — delete episodes with `EXSTDTC` after the
    global cut-off date.

2.  `impute_exendtc_to_cutoff` — if the last episode still has missing
    `EXENDTC`, set it to the cut-off date (e.g. for ongoing treatment).

3.  `impute_missing_exendtc` — for non-last episodes with missing
    `EXENDTC`, set it to the day before the next episode's `EXSTDTC`.

4.  `filter_exendtc_after_exstdtc` — remove episodes where `EXENDTC` is
    before `EXSTDTC`.

### `admin_post_expansion`

1.  `get_admin_time_from_pcrftdtc` — Use the `PCRFTDTC` field from the
    PC domain, if available, to complete missing administration times.

2.  `get_admin_time_from_ntime` — back-calculate administration time, if
    still missing from the nominal PK observation times in `PCTPT`, if
    available.

After these imputations, remaining missing administration times are
carried forward.

## Imputation rules for observations

### `obs_raw`

- `impute_lloq_pc` — pharmacokinetic observations below the limit of
  quantification are set to `PCLLOQ / 2`.

### `obs_final`

- For predose observations of the current analyte, set `TAFD` to zero
  when it would otherwise be negative.

## Creating custom imputation rules

You can create further imputation rule sets by providing a named list
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
