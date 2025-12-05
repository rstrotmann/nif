# Impute missing EXENDTC to the day before the next EXSTDTC.

In some cases, EX does not contain EXENDTC for administration episodes
that are not the very last administration episode. This should only
occur when non-clean clinical data is analyzed, e.g., in the context of
an interim analysis. In most cases, such instances must be manually
resolved. There could be AE information with consequences of "drug
withdrawn" available that may be helpful, or other information from
clinical context can be used to determine how many IMP administrations
were done in the respective interval. This function should only be used
if no other information is available as it takes a worst-case approach,
i.e., assumes that IMP was administered up to the day before the
subsequent administration interval. Note that this imputation does not
apply to the last administration per subject and EXTRT. For these cases,
missing EXENDTC can be imputed to the global cut off date using
[`impute_exendtc_to_cutoff()`](impute_exendtc_to_cutoff.md).

## Usage

``` r
impute_missing_exendtc(ex)
```

## Arguments

- ex:

  The updated EX domain as data frame.

## Value

The updated EX domain as data frame.

## Details

As this function conducts rather aggressive imputations, the message
output is not optional, i.e., cannot be suppressed using the global
'silent' option, but is issued in all cases.
