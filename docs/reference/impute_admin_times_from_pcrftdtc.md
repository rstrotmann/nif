# Derive administration time from PCRFTDTC

Derive administration time from PCRFTDTC

## Usage

``` r
impute_admin_times_from_pcrftdtc(obj, pc, analyte, pctestcd, silent = NULL)
```

## Arguments

- obj:

  A data frame.

- pc:

  The corresponding PC domain as data frame.

- analyte:

  The analyte as character.

- pctestcd:

  The PCTESTCD corresponding to the analyte as character.

- silent:

  Suppress messages, defaults to nif_option setting, if NULL.

## Value

A data frame.
