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
#> # A tibble: 45 × 9
#> # Groups:   DOSE [9]
#>     DOSE PPTESTCD   geomean geocv median    iqr    min    max     n
#>    <dbl> <chr>        <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <int>
#>  1     5 aucinf.obs  138.    7.30 137.   10.1   128.   148.       3
#>  2     5 auclast     136.    7.19 136.    9.81  127.   147.       3
#>  3     5 cmax         44.8   7.15  43.6   3.07   42.4   48.6      3
#>  4     5 half.life     7.66  5.38   7.71  0.408   7.24   8.05     3
#>  5     5 tmax          1     0      1     0       1      1        3
#>  6    10 aucinf.obs  232.   11.6  237.   26.3   205.   258.       3
#>  7    10 auclast     230.   11.4  235.   25.8   204.   255.       3
#>  8    10 cmax         75.4   7.04  76.4   5.20   69.9   80.3      3
#>  9    10 half.life     7.54 10.2    7.40  0.768   6.89   8.42     3
#> 10    10 tmax          1     0      1     0       1      1        3
#> # ℹ 35 more rows
```
