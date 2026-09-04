# PK parameter summary statistics table by grouping variables

PK parameter summary statistics table by grouping variables

## Usage

``` r
nca_summary_table(
  nca,
  parameters = c("auclast", "cmax", "tmax", "half.life", "aucinf.obs", "AUCLST", "CMAX",
    "TMAX", "LAMZHL", "AUCIFP"),
  digits = 2,
  group = NULL
)
```

## Arguments

- nca:

  The NCA results as provided by `nca`, as data frame.

- parameters:

  The NCA parameters to be tabulated as character.

- digits:

  The number of significant digits to be displayed.

- group:

  The grouping variable, defaults to DOSE.

## Value

A data frame

## Examples

``` r
nca_summary_table(nca(examplinib_sad_nif, analyte = "RS2023"))
#> ℹ Parent set to RS2023
#> # A tibble: 1 × 6
#>       n aucinf.obs    auclast       cmax          half.life tmax      
#>   <int> <chr>         <chr>         <chr>         <chr>     <chr>     
#> 1    48 5305.17 (374) 5257.01 (374) 1701.72 (375) 7.39 (10) 1 (1; 1.5)
```
