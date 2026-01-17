# Convert date fields to POSIX format

Convert date-time code (DTC) variables from the ISO 8601
(https://w.wiki/8Bzr) format used in SDTM (i.e., something like
"2001-01-02T09:59" where date and time are separated by "T") to standard
POSIXct format. The names of the variables to be converted need to be
provided by `fields`.

## Usage

``` r
standardize_date_format(obj, fields = NULL)
```

## Arguments

- obj:

  A data frame.

- fields:

  Date variable names as character.

## Value

A data frame
