# Compartments used in a nif object

Compartments used in a nif object

## Usage

``` r
compartments(obj)
```

## Arguments

- obj:

  A NIF object

## Value

A data frame

## Examples

``` r
compartments(examplinib_poc_nif)
#>      ANALYTE CMT
#> 1     RS2023   2
#> 2 RS2023487A   3
compartments(examplinib_poc_min_nif)
#>   ANALYTE CMT
#> 1    CMT2   2
#> 2    CMT3   3
```
