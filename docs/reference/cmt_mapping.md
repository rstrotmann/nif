# Analyte - compartment mapping

**\[deprecated\]**

## Usage

``` r
cmt_mapping(obj)
```

## Arguments

- obj:

  A NIF object

## Value

A data frame

## Examples

``` r
cmt_mapping(examplinib_poc_nif)
#> Warning: `cmt_mapping()` was deprecated in nif 0.57.11.
#> ℹ Please use `compartments()` instead.
#>      ANALYTE CMT
#> 1     RS2023   2
#> 2 RS2023487A   3
cmt_mapping(examplinib_poc_min_nif)
#>   ANALYTE CMT
#> 1    CMT2   2
#> 2    CMT3   3
```
