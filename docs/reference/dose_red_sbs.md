# Subjects with dose reduction

Subjects with dose reduction

## Usage

``` r
dose_red_sbs(obj, analyte = NULL)
```

## Arguments

- obj:

  A NIF object object.

- analyte:

  The treatment of interest as character. Automatically selects the
  treatment if NULL but fails if there are multiple treatments.

## Value

A data frame with the ID and, if available, the USUBJID of subjects with
dose reductions.

## Examples

``` r
dose_red_sbs(examplinib_poc_nif)
#> # A tibble: 30 × 2
#>       ID USUBJID          
#>    <dbl> <chr>            
#>  1    29 20230000221030016
#>  2    34 20230000221040006
#>  3    67 20230000221070001
#>  4    79 20230000221080005
#>  5    76 20230000221080002
#>  6    13 20230000221020012
#>  7    35 20230000221040007
#>  8     5 20230000221010005
#>  9    77 20230000221080003
#> 10    32 20230000221040004
#> # ℹ 20 more rows
dose_red_sbs(examplinib_poc_nif, "RS2023")
#> # A tibble: 30 × 2
#>       ID USUBJID          
#>    <dbl> <chr>            
#>  1    29 20230000221030016
#>  2    34 20230000221040006
#>  3    67 20230000221070001
#>  4    79 20230000221080005
#>  5    76 20230000221080002
#>  6    13 20230000221020012
#>  7    35 20230000221040007
#>  8     5 20230000221010005
#>  9    77 20230000221080003
#> 10    32 20230000221040004
#> # ℹ 20 more rows
```
