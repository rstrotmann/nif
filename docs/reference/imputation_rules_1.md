# Alternative imputation rule set 1

Same pre-expansion steps as
[`imputation_rules_standard()`](imputation_rules_standard.md), with a
different post-expansion time rule (10-minute NTIME vs EX gate) and a
no-op `obs_raw`.

## Usage

``` r
imputation_rules_1
```

## Format

A named list with these functions:

- admin_pre_expansion:

  Cut-off filter and EXENDTC imputations (same as standard).

- admin_post_expansion:

  NTIME and PCRFTDTC with a 10-minute EX comparison.

- obs_raw:

  Identity (no LLOQ imputation).

- obs_final:

  Predose TAFD adjustment (same as standard).

## Steps in this rule set

### Host steps (not in this list)

Same as [`imputation_rules_standard()`](imputation_rules_standard.md):
RFENDTC fill, `expand_ex`, and time carry-forward.

### `admin_pre_expansion`

Same as [`imputation_rules_standard()`](imputation_rules_standard.md).

### `admin_post_expansion`

1.  Compute an NTIME-based administration time via
    `get_admin_time_from_ntime` (stored alongside the current EX-based
    time).

2.  Apply `get_admin_time_from_pcrftdtc` when PCRFTDTC is available
    (takes precedence).

3.  If PCRFTDTC is absent and the NTIME-derived time differs from the
    EX-derived time by more than 10 minutes, use the NTIME-derived time;
    otherwise keep the EX-derived time.

After this slot, host carry-forward fills remaining missing times.

### `obs_raw`

No-op (LLOQ imputation is not applied in this rule set).

### `obs_final`

Same as [`imputation_rules_standard()`](imputation_rules_standard.md):
predose `TAFD < 0` set to zero for the current observation.

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

Other imputation rules:
[`imputation_rules_minimal`](imputation_rules_minimal.md),
[`imputation_rules_standard`](imputation_rules_standard.md),
[`imputation_rules_void`](imputation_rules_void.md)
