# Dose levels within a NIF object

This function summarizes the doses in the individual first
administration by subject and drug, and the number of subjects treated
at this dose level. Subsequent dose modifications are ignored.

## Usage

``` r
dose_levels(obj, cmt = 1, group = NULL)
```

## Arguments

- obj:

  A rich or minimal NIF object.

- cmt:

  The compartment (CMT) as numeric.

- group:

  Further fields to be included (and to be grouped by) in the output.

## Value

A data frame.

## Examples

``` r
dose_levels(examplinib_fe_nif)
#>   RS2023  N
#> 1    500 20
dose_levels(examplinib_fe_nif, group = "SEX")
#>   SEX RS2023  N
#> 1   0    500 13
#> 2   1    500  7
dose_levels(examplinib_fe_nif, group = c("SEX", "FASTED"))
#>   SEX FASTED RS2023  N
#> 1   0      0    500 13
#> 2   0      1    500 13
#> 3   1      0    500  7
#> 4   1      1    500  7
dose_levels(examplinib_sad_min_nif)
#>   CMT1  N
#> 1    5  3
#> 2   10  3
#> 3   20  3
#> 4   50  3
#> 5  100  6
#> 6  200  3
#> 7  500 18
#> 8  800  6
#> 9 1000  3
dose_levels(nif())
#> NULL
```
