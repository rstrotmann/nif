# PK parameter summary statistics table by dose

PK parameter summary statistics table by dose

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

  The NCA parameters to be tabulated as character,

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
#> # A tibble: 9 × 7
#> # Groups:   DOSE [9]
#>    DOSE     n aucinf.obs    auclast       cmax         half.life tmax      
#>   <dbl> <int> <chr>         <chr>         <chr>        <chr>     <chr>     
#> 1     5     3 137.59 (7)    136.41 (7)    44.78 (7)    7.66 (5)  1 (1; 1)  
#> 2    10     3 232.2 (12)    230.38 (11)   75.41 (7)    7.54 (10) 1 (1; 1)  
#> 3    20     3 536.89 (12)   532.46 (12)   169.04 (9)   7.11 (7)  1 (1; 1)  
#> 4    50     3 1704.76 (36)  1687.87 (36)  513.5 (37)   7.86 (16) 1 (1; 1)  
#> 5   100     6 2849.3 (43)   2822.32 (43)  905.74 (30)  7.4 (12)  1 (1; 1.5)
#> 6   200     3 6429.53 (14)  6368.67 (14)  2055.28 (13) 7.58 (13) 1 (1; 1)  
#> 7   500    18 15072.48 (32) 14942.75 (32) 4907.44 (29) 7.21 (12) 1 (1; 1)  
#> 8   800     6 23439.89 (26) 23245.64 (26) 7194.56 (17) 7.07 (11) 1 (1; 1.5)
#> 9  1000     3 30447.31 (28) 30182.23 (28) 9750.35 (26) 7.33 (5)  1 (1; 1)  
```
