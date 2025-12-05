# Impute last EXENDTC per subject and treatment to cutoff date when absent.

In some instances, particularly when analyzing non-cleaned SDTM data
from ongoing clinical studies with multiple-dose administrations, the
last administration epoch in EX may have an empty EXENDTC field. Often,
the underlying reason is that the respective subjects are still on
treatment. This function replaces the missing EXENDTC with the global
data cut-off date, `cut_off_date`.

## Usage

``` r
impute_exendtc_to_cutoff(ex, cut_off_date = NA, silent = NULL)
```

## Arguments

- ex:

  The EX domain as data frame.

- cut_off_date:

  The cut-off date.

- silent:

  Suppress messages. Defaults to nif_option setting if NULL.

## Value

The updated EX domain as data frame.
