# Subjects per dose level

The number of subjects that have observations, per dose level and
analyte.

## Usage

``` r
subs_per_dose_level(obj, analyte = NULL, group = NULL)
```

## Arguments

- obj:

  A NIF object.

- analyte:

  The analyte as character. If `NULL` (default), all analytes are
  selected.

- group:

  An (optional) grouping variable as character. Defaults to

## Value

A data frame.

## Examples

``` r
subs_per_dose_level(examplinib_poc_nif)
#>    DL    ANALYTE  N
#> 1 500     RS2023 80
#> 2 500 RS2023487A 80
subs_per_dose_level(examplinib_sad_nif)
#>     DL ANALYTE  N
#> 1    5  RS2023  3
#> 2   10  RS2023  3
#> 3   20  RS2023  3
#> 4   50  RS2023  3
#> 5  100  RS2023  6
#> 6  200  RS2023  3
#> 7  500  RS2023 18
#> 8  800  RS2023  6
#> 9 1000  RS2023  3
subs_per_dose_level(examplinib_poc_nif, group = "SEX", analyte = "RS2023")
#>    DL ANALYTE SEX  N
#> 1 500  RS2023   1 33
#> 2 500  RS2023   0 47
```
