# Observations per dose level

The total number of observations, per dose level and analyte.

## Usage

``` r
obs_per_dose_level(obj, analyte = NULL, group = NULL)
```

## Arguments

- obj:

  A NIF object.

- analyte:

  The analyte as character. If `NULL` (default), all analytes are
  selected.

- group:

  An (optional) grouping variable as character. Defaults to 'NULL'.

## Value

A data frame.

## Examples

``` r
obs_per_dose_level(examplinib_poc_nif)
#>    DL    ANALYTE   N
#> 1 500     RS2023 672
#> 2 500 RS2023487A 672
obs_per_dose_level(examplinib_sad_nif)
#>     DL ANALYTE   N
#> 1    5  RS2023  51
#> 2   10  RS2023  51
#> 3   20  RS2023  51
#> 4   50  RS2023  51
#> 5  100  RS2023 102
#> 6  200  RS2023  51
#> 7  500  RS2023 306
#> 8  800  RS2023 102
#> 9 1000  RS2023  51
obs_per_dose_level(examplinib_poc_nif, group = "SEX", analyte = "RS2023")
#>    DL ANALYTE SEX   N
#> 1 500  RS2023   1 294
#> 2 500  RS2023   0 378
```
