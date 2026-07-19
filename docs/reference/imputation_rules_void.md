# Void imputation rule set

Empty rule list: no pre-/post-expansion or observation slot functions.
Host steps in [`add_administration()`](add_administration.md) /
`make_administration()` still run (RFENDTC fill, episode expansion, time
carry-forward).

## Usage

``` r
imputation_rules_void
```

## Format

An empty list.

## Steps in this rule set

This object defines no slot functions. Behavior is limited to host
steps:

- Last-episode `EXENDTC` from `DM.RFENDTC`
  (`impute_exendtc_to_rfendtc`).

- Episode expansion (`expand_ex`); times from `EXSTDTC` / `EXENDTC` when
  present.

- Administration time carry-forward after expansion.

There is no cut-off filter, no EXENDTC cutoff/middle imputation via
rules, no PCRFTDTC or NTIME time imputation, and no observation
imputations from this object.

## Creating custom imputation rules

You can create your own imputation rule set by providing a named list
with any combination of the four function slots: `admin_pre_expansion`,
`admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
receives specific arguments depending on its slot. See the
implementations in this file for the expected signatures.

## See also

[`add_administration()`](add_administration.md),
[`add_observation()`](add_observation.md),
[`imputation_rules_standard()`](imputation_rules_standard.md)

Other imputation rules: [`imputation_rules_1`](imputation_rules_1.md),
[`imputation_rules_minimal`](imputation_rules_minimal.md),
[`imputation_rules_standard`](imputation_rules_standard.md)
