# Index dosing intervals

This function adds a column 'DI' that indicates the dosing interval per
parent. All baseline observations before the first dosing interval get
assigned to the first dosing interval.

## Usage

``` r
index_dosing_interval(obj, parent = NULL)
```

## Arguments

- obj:

  The NIF object.

- parent:

  The treatments to filter for. Defaults to all parents, if NULL.

## Value

A NIF object with the DI column added.

## Examples

``` r
index_dosing_interval(examplinib_fe_nif)
#> ──────── NONMEM Input Format (NIF) data ────────
#> 680 observations from 20 subjects across 1 study 
#> 
#> # A tibble: 720 × 32
#>      REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT
#>    <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl>
#>  1     1     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  2     2     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  3     3     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  4     4     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  5     5     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  6     6     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  7     7     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  8     8     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>  9     9     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#> 10    10     1 2023000400 20230004001010002    53     1 WHITE   180.   73.1
#>      BMI DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV
#>    <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  22.5 2001-01-05 10:05:00   0     0     0     0       1   500     1   NA 
#>  2  22.5 2001-01-05 10:05:00   0     0     0     0       0     0     2    0 
#>  3  22.5 2001-01-05 10:35:00   0.5   0.5   0.5   0.5     0     0     2 4697.
#>  4  22.5 2001-01-05 11:05:00   1     1     1     1       0     0     2 6325.
#>  5  22.5 2001-01-05 11:35:00   1.5   1.5   1.5   1.5     0     0     2 6294.
#>  6  22.5 2001-01-05 12:05:00   2     2     2     2       0     0     2 5932.
#>  7  22.5 2001-01-05 13:05:00   3     3     3     3       0     0     2 4317.
#>  8  22.5 2001-01-05 14:05:00   4     4     4     4       0     0     2 2720.
#>  9  22.5 2001-01-05 16:05:00   6     6     6     6       0     0     2  939.
#> 10  22.5 2001-01-05 18:05:00   8     8     8     8       0     0     2  399.
#>    ANALYTE PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD
#>    <chr>   <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>   
#>  1 RS2023  RS2023     1 FALSE        500     1 AB      
#>  2 RS2023  RS2023     1 FALSE        500     0 AB      
#>  3 RS2023  RS2023     1 FALSE        500     0 AB      
#>  4 RS2023  RS2023     1 FALSE        500     0 AB      
#>  5 RS2023  RS2023     1 FALSE        500     0 AB      
#>  6 RS2023  RS2023     1 FALSE        500     0 AB      
#>  7 RS2023  RS2023     1 FALSE        500     0 AB      
#>  8 RS2023  RS2023     1 FALSE        500     0 AB      
#>  9 RS2023  RS2023     1 FALSE        500     0 AB      
#> 10 RS2023  RS2023     1 FALSE        500     0 AB      
#>    IMPUTATION                 EPOCH                  PERIOD TREATMENT FASTED
#>    <chr>                      <chr>                   <dbl> <chr>      <dbl>
#>  1 "time copied from EXSTDTC" OPEN LABEL TREATMENT 1      1 A              1
#>  2 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>  3 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>  4 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>  5 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>  6 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>  7 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>  8 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>  9 ""                         OPEN LABEL TREATMENT 1      1 A              1
#> 10 ""                         OPEN LABEL TREATMENT 1      1 A              1
#>       DI
#>    <int>
#>  1     1
#>  2     1
#>  3     1
#>  4     1
#>  5     1
#>  6     1
#>  7     1
#>  8     1
#>  9     1
#> 10     1
#> # ℹ 710 more rows
```
