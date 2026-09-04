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
#> # A tibble: 22 × 2
#>       ID USUBJID          
#>    <dbl> <chr>            
#>  1    36 20230000221040002
#>  2    13 20230000221020006
#>  3    73 20230000221070009
#>  4    48 20230000221050007
#>  5    75 20230000221080001
#>  6    11 20230000221020002
#>  7    53 20230000221050012
#>  8    47 20230000221050005
#>  9    18 20230000221020012
#> 10    14 20230000221020007
#> # ℹ 12 more rows
dose_red_sbs(examplinib_poc_nif, "RS2023")
#> # A tibble: 22 × 2
#>       ID USUBJID          
#>    <dbl> <chr>            
#>  1    36 20230000221040002
#>  2    13 20230000221020006
#>  3    73 20230000221070009
#>  4    48 20230000221050007
#>  5    75 20230000221080001
#>  6    11 20230000221020002
#>  7    53 20230000221050012
#>  8    47 20230000221050005
#>  9    18 20230000221020012
#> 10    14 20230000221020007
#> # ℹ 12 more rows
```
