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
#> # A tibble: 25 × 2
#>       ID USUBJID          
#>    <dbl> <chr>            
#>  1    62 20230000221060015
#>  2    66 20230000221070005
#>  3    44 20230000221050006
#>  4     3 20230000221010005
#>  5    17 20230000221030004
#>  6    76 20230000221070016
#>  7    77 20230000221070018
#>  8    10 20230000221020007
#>  9    22 20230000221030010
#> 10    46 20230000221050008
#> # ℹ 15 more rows
dose_red_sbs(examplinib_poc_nif, "RS2023")
#> # A tibble: 25 × 2
#>       ID USUBJID          
#>    <dbl> <chr>            
#>  1    62 20230000221060015
#>  2    66 20230000221070005
#>  3    44 20230000221050006
#>  4     3 20230000221010005
#>  5    17 20230000221030004
#>  6    76 20230000221070016
#>  7    77 20230000221070018
#>  8    10 20230000221020007
#>  9    22 20230000221030010
#> 10    46 20230000221050008
#> # ℹ 15 more rows
```
