# Number of DTC entries with missing time information

Number of DTC entries with missing time information

## Usage

``` r
sdtm_missing_times(sdtm, fields)
```

## Arguments

- sdtm:

  A stm object.

- fields:

  The fields to test, as character.

## Value

A data frame.

## Examples

``` r
sdtm_missing_times(examplinib_poc, c("EXSTDTC", "EXENDTC"))
#>     field missing total percent_missing
#> 1 EXSTDTC       0   468               0
#> 2 EXENDTC       0   468               0
```
