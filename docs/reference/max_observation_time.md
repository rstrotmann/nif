# Maximal observation time

This function returns the time in hours of the last observation relative
to the first observation within the data set.

## Usage

``` r
max_observation_time(obj, analyte = NULL)
```

## Arguments

- obj:

  The NIF object

- analyte:

  The analyte as character. If `NULL` (default), all analytes are
  selected.

## Value

A scalar representing the time in hours.

## Examples

``` r
max_observation_time(examplinib_fe_nif)
#> [1] 480
max_observation_time(examplinib_poc_nif)
#> [1] 220.85
max_observation_time(examplinib_poc_min_nif)
#> [1] 228.417
max_observation_time(examplinib_poc_min_nif, analyte = "CMT4")
#> [1] NA
```
