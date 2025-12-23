# Maximal administration time

This function returns the time in hours of the last administration
within the data set.

## Usage

``` r
max_admin_time(obj, analyte = NULL)
```

## Arguments

- obj:

  The NIF object

- analyte:

  The analyte or analytes to filter for.

## Value

A scalar representing the time in hours.

## Examples

``` r
max_admin_time(examplinib_fe_nif)
#> [1] 312
max_admin_time(examplinib_poc_nif)
#> [1] 2400
max_admin_time(examplinib_poc_min_nif)
#> [1] 2376
max_admin_time(examplinib_poc_min_nif, analyte = "CMT1")
#> [1] 2376
```
