# Add time after first dose column

Add time after first dose column

## Usage

``` r
add_tafd(nif)
```

## Arguments

- nif:

  A NIF object.

## Value

A NIF object.

## Examples

``` r
add_tafd(examplinib_poc_nif)
#> ──────── NONMEM Input Format (NIF) data ────────
#> 1344 observations from 80 subjects across 1 study 
#> 
#> # A tibble: 7,200 × 29
#>      REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT
#>  * <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl>
#>  1     1     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  2     2     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  3     3     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  4     4     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  5     5     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  6     6     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  7     7     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  8     8     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  9     9     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#> 10    10     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>      BMI DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV
#>  * <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  28.8 2001-01-07 09:42:00 0       0   0     0         1   500     1   NA 
#>  2  28.8 2001-01-07 09:42:00 0       0   0     0         0     0     2    0 
#>  3  28.8 2001-01-07 09:42:00 0       0   0     0         0     0     3    0 
#>  4  28.8 2001-01-07 10:34:00 0.867   0.5 0.867 0.867     0     0     2  615.
#>  5  28.8 2001-01-07 10:34:00 0.867   0.5 0.867 0.867     0     0     3  120.
#>  6  28.8 2001-01-07 11:02:00 1.33    1   1.33  1.33      0     0     2 1842.
#>  7  28.8 2001-01-07 11:02:00 1.33    1   1.33  1.33      0     0     3  604.
#>  8  28.8 2001-01-07 11:32:00 1.83    1.5 1.83  1.83      0     0     2 2563.
#>  9  28.8 2001-01-07 11:32:00 1.83    1.5 1.83  1.83      0     0     3 1375.
#> 10  28.8 2001-01-07 11:59:00 2.28    2   2.28  2.28      0     0     2 2995.
#>    ANALYTE    PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD  IMPUTATION BL_CREAT
#>  * <chr>      <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>     <chr>         <dbl>
#>  1 RS2023     RS2023     1 FALSE        500     1 TREATMENT ""             86.5
#>  2 RS2023     RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>  3 RS2023487A RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>  4 RS2023     RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>  5 RS2023487A RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>  6 RS2023     RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>  7 RS2023487A RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>  8 RS2023     RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>  9 RS2023487A RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#> 10 RS2023     RS2023     1 FALSE        500     0 TREATMENT ""             86.5
#>    BL_CRCL
#>  *   <dbl>
#>  1    78.7
#>  2    78.7
#>  3    78.7
#>  4    78.7
#>  5    78.7
#>  6    78.7
#>  7    78.7
#>  8    78.7
#>  9    78.7
#> 10    78.7
#> # ℹ 7,190 more rows
```
