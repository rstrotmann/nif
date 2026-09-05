# Number of administrations per subject

This function returns the number of administrations per `ID` and
`PARENT`.

## Usage

``` r
n_administrations(obj)
```

## Arguments

- obj:

  A NIF object.

## Value

A data frame.

## Examples

``` r
head(n_administrations(examplinib_poc_nif))
#>   ID           USUBJID PARENT  N
#> 1  1 20230000221010001 RS2023 81
#> 2  2 20230000221010002 RS2023 92
#> 3  3 20230000221010003 RS2023 76
#> 4  4 20230000221010005 RS2023 58
#> 5  5 20230000221010006 RS2023 70
#> 6  6 20230000221010007 RS2023 70
head(n_administrations(examplinib_poc_min_nif))
#>   ID PARENT  N
#> 1  1   CMT1 81
#> 2  2   CMT1 92
#> 3  3   CMT1 76
#> 4  4   CMT1 58
#> 5  5   CMT1 70
#> 6  6   CMT1 70
```
