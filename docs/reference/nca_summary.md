# PK parameter summary statistics by dose

PK parameter summary statistics by dose

## Usage

``` r
nca_summary(
  nca,
  parameters = c("auclast", "cmax", "tmax", "half.life", "aucinf.obs", "AUCLST", "CMAX",
    "TMAX", "LAMZHL", "AUCIFP"),
  group = NULL
)
```

## Arguments

- nca:

  The NCA results as provided by `nca`, as data frame.

- parameters:

  The NCA parameters to be tabulated as character,

- group:

  The grouping variable, defaults to DOSE.

## Value

A data frame

## Examples

``` r
nca_summary(nca(examplinib_sad_nif, analyte = "RS2023"))
#> ℹ Parent set to RS2023
#> # A tibble: 5 × 8
#>   PPTESTCD   geomean  geocv   median      iqr    min      max     n
#>   <chr>        <dbl>  <dbl>    <dbl>    <dbl>  <dbl>    <dbl> <int>
#> 1 aucinf.obs 5305.   374.   10404.   15341.   172.   41514.      48
#> 2 auclast    5257.   374.   10331.   15184.   170.   41065.      48
#> 3 cmax       1702.   375.    3694.    5459.    56.2  13911.      48
#> 4 half.life     7.39   9.83     7.32     1.04   6.13     8.98    48
#> 5 tmax          1.03   9.94     1        0      1        1.5     48
```
