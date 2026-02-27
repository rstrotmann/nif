# Minimal imputation rule set

Minimal imputation rule set

## Usage

``` r
imputation_rules_minimal
```

## Format

An empty list.

## Details

This imputation rule set includes the following minimal imputation
steps:

### Treatment administrations:

- Expand administration episodes from the EX domain between EXSTDTC and
  EXENDTC.

- Administrations inherit the administration time from EXSTDTC or
  EXENDTC.

- The administration time is carried forward for subsequent
  administration events until the next imputed time.

## Creating custom imputation rules

You can create your own imputation rule set by providing a named list
with any combination of the four function slots: `admin_pre_expansion`,
`admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
receives specific arguments depending on its slot.

## See also

add_administration()

add_observation()

Other imputation rules: [`imputation_rules_1`](imputation_rules_1.md),
[`imputation_rules_standard`](imputation_rules_standard.md)
