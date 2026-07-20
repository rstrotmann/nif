# Alternative imputation rule set 1

Same pre-expansion steps as
[`imputation_rules_standard()`](imputation_rules_standard.md), with a
different post-expansion time rule and no `obs_raw` imputation.

## Usage

``` r
imputation_rules_1
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

## Imputation rules for drug administrations

### Generic imputations

These steps are always performed regardless of the specific rule set.
Same as [`imputation_rules_standard()`](imputation_rules_standard.md):

- RFENDTC fill

- `expand_ex`

- time carry-forward.

### `admin_pre_expansion`

Same as [`imputation_rules_standard()`](imputation_rules_standard.md).

### `admin_post_expansion`

1.  Compute an NTIME-based administration time via
    `get_admin_time_from_ntime`

2.  Apply `get_admin_time_from_pcrftdtc` when PCRFTDTC is available.

3.  If PCRFTDTC is absent and the NTIME-derived time differs from the
    EX-derived time by more than 10 minutes, use the NTIME-derived time;
    otherwise keep the EX-derived time.

After these imputations, remaining missing administration times are
carried forward.

## Imputation rules for observations

### `obs_raw`

No action (LLOQ imputation is not applied in this rule set).

### `obs_final`

Same as [`imputation_rules_standard()`](imputation_rules_standard.md):
predose `TAFD` is set to zero when negative.

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

Other imputation rules:
[`imputation_rules_minimal`](imputation_rules_minimal.md),
[`imputation_rules_standard`](imputation_rules_standard.md),
[`imputation_rules_void`](imputation_rules_void.md)
