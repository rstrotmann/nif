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
#>  1     1     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  2     2     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  3     3     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  4     4     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  5     5     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  6     6     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  7     7     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  8     8     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>  9     9     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#> 10    10     1 2023000400 20230004001010002    55     1 WHITE    173   90.3
#>      BMI DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV
#>    <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  30.2 2001-01-15 08:24:00   0     0     0     0       1   500     1   NA 
#>  2  30.2 2001-01-15 08:24:00   0     0     0     0       0     0     2    0 
#>  3  30.2 2001-01-15 08:54:00   0.5   0.5   0.5   0.5     0     0     2  624.
#>  4  30.2 2001-01-15 09:24:00   1     1     1     1       0     0     2 1740.
#>  5  30.2 2001-01-15 09:54:00   1.5   1.5   1.5   1.5     0     0     2 2670.
#>  6  30.2 2001-01-15 10:24:00   2     2     2     2       0     0     2 2843.
#>  7  30.2 2001-01-15 11:24:00   3     3     3     3       0     0     2 2492.
#>  8  30.2 2001-01-15 12:24:00   4     4     4     4       0     0     2 2159.
#>  9  30.2 2001-01-15 14:24:00   6     6     6     6       0     0     2 1301.
#> 10  30.2 2001-01-15 16:24:00   8     8     8     8       0     0     2  813.
#>    ANALYTE PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD
#>    <chr>   <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>   
#>  1 RS2023  RS2023     1 FALSE        500     1 BA      
#>  2 RS2023  RS2023     1 FALSE        500     0 BA      
#>  3 RS2023  RS2023     1 FALSE        500     0 BA      
#>  4 RS2023  RS2023     1 FALSE        500     0 BA      
#>  5 RS2023  RS2023     1 FALSE        500     0 BA      
#>  6 RS2023  RS2023     1 FALSE        500     0 BA      
#>  7 RS2023  RS2023     1 FALSE        500     0 BA      
#>  8 RS2023  RS2023     1 FALSE        500     0 BA      
#>  9 RS2023  RS2023     1 FALSE        500     0 BA      
#> 10 RS2023  RS2023     1 FALSE        500     0 BA      
#>    IMPUTATION                 EPOCH                  PERIOD TREATMENT FASTED
#>    <chr>                      <chr>                   <dbl> <chr>      <dbl>
#>  1 "time copied from EXSTDTC" OPEN LABEL TREATMENT 1      1 B              0
#>  2 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  3 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  4 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  5 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  6 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  7 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  8 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  9 ""                         OPEN LABEL TREATMENT 1      1 B              0
#> 10 ""                         OPEN LABEL TREATMENT 1      1 B              0
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
