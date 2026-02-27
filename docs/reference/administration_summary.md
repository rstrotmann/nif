# Administration summary

Summary statistics on the number of administrations per subject, by
parent. This may useful to characterized the population exposure to a
treatment.

## Usage

``` r
administration_summary(obj)
```

## Arguments

- obj:

  A NIF object.

## Value

A data frame.

## Examples

``` r
administration_summary(examplinib_poc_nif)
#>   PARENT min max mean median
#> 1 RS2023  55  97 73.2   72.5
```
