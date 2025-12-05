# Convert all DTC fields from ISO 8601 into POSIXct

Change all columns in the input data frame that end with 'DTC' to
standard POSIXct format.

## Usage

``` r
lubrify_dates(obj, col = NULL)
```

## Arguments

- obj:

  A data frame.

- col:

  Columns to convert. Defaults to all columns ending in "DTC", if NULL.

## Value

A data frame.
