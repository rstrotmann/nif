# Make ntime lookup table from TPTNUM field

Make ntime lookup table from TPTNUM field

## Usage

``` r
make_ntime_from_tptnum(obj, domain = NULL)
```

## Arguments

- obj:

  A data frame containing time point text descriptions.

- domain:

  The domain code as character (default: "PC" for pharmacokinetic). Used
  to determine the column name containing time point descriptions.

## Value

A data frame with a column representing the unique values of the xxTPT
variable and a NTIME column with the time in hours.
