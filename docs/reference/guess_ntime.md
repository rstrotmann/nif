# Guess NTIME from PCTPT

Guess NTIME from PCTPT

## Usage

``` r
guess_ntime(sdtm)
```

## Arguments

- sdtm:

  A sdtm object.

## Value

A data frame.

## Examples

``` r
guess_ntime(examplinib_poc)
#>             PCTPT NTIME
#> 1         PREDOSE   0.0
#> 2  POSTDOSE 0.5 H   0.5
#> 3    POSTDOSE 1 H   1.0
#> 4  POSTDOSE 1.5 H   1.5
#> 5    POSTDOSE 2 H   2.0
#> 6    POSTDOSE 3 H   3.0
#> 7    POSTDOSE 4 H   4.0
#> 8    POSTDOSE 6 H   6.0
#> 9    POSTDOSE 8 H   8.0
#> 10  POSTDOSE 10 H  10.0
#> 11  POSTDOSE 12 H  12.0
```
