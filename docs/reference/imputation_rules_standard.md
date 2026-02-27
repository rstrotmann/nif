# Standard imputation rule set

Standard imputation rule set

## Usage

``` r
imputation_rules_standard
```

## Format

A list of the following functions:

- admin_pre_expansion()

- admin_post_expansion()

## Details

This imputation rule set includes the following imputation steps:

### Treatment administrations:

- Filter administrations to the cut-off date.

- Impute missing EXENDTC values in the last administration episode to
  the cut-off date.

- Impute missing (non-last) EXENDTC values to the day before the start
  of the subsequent administration episode.

- Remove records where EXENDTC is before EXSTDTC.

- Expand administration episodes from the EX domain between EXSTDTC and
  EXENDTC.

- For each administration event, take the administration time from
  PCRFTDTC of the PC domain if there are related pharmacokinetic
  observations. The name of the PK analyte (PCTESTCD) that corresponds
  with the administered treatment (EXTRT) must be specified by the
  'pctestcd' to add_administration().

- Unless imputed by the above rule, administrations inherit the
  administration time from EXSTDTC or EXENDTC.

- After the above imputations, carry forward the administration time for
  subsequent administration events until the next imputed time.

### Observations

- No imputations.

## Creating custom imputation rules

You can create your own imputation rule set by providing a named list
with any combination of the four function slots: `admin_pre_expansion`,
`admin_post_expansion`, `obs_raw`, and `obs_final`. Each function
receives specific arguments depending on its slot.

## See also

add_administration()

add_observation()

Other imputation rules: [`imputation_rules_1`](imputation_rules_1.md),
[`imputation_rules_minimal`](imputation_rules_minimal.md)
