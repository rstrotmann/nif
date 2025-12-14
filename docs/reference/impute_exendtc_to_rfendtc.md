# Impute very last EXENDTC for a subject and EXTRT to RFENDTC, if absent

In EX for multiple-dose studies, the EXENDTC field for the very last
administration epoch may be missing. This is occasionally found when
SDTM data are generated before full cleaning of the clinical data, e.g.,
in the case of interim analyses of clinical study data. In some of these
cases, the DM domain is however already completed with the RFENDTC
field. This is the reference end date-time field that specifies the
date-time of the last treatment administration. This function completes
EXENDTC for the very last administration time based on the RFENDTC, if
provided in DM.

## Usage

``` r
impute_exendtc_to_rfendtc(ex, dm, cut_off_date = NULL, silent = NULL)
```

## Arguments

- ex:

  The EX domain as data frame.

- dm:

  The DM domain as data frame.

- cut_off_date:

  The cut-off date as POSIXct.

- silent:

  Suppress messages, defaults to nif_options setting if NULL.

## Value

The updated EX domain as data frame.
