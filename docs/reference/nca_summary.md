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
#> 1 aucinf.obs 5215.   417.   10797.   17015.   128.   38216.      48
#> 2 auclast    5170.   417.   10711.   16888.   127.   37870.      48
#> 3 cmax       1665.   412.    3674.    5595.    42.4  11777.      48
#> 4 half.life     7.33  10.7      7.31     1.17   5.30     9.00    48
#> 5 tmax          1.02   8.20     1        0      1        1.5     48
```
