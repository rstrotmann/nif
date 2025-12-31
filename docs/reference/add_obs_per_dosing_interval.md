# Add the number of observations per dosing interval

This function adds a variable, `OPDI`, to the NIF object that indicates
the number of observations per analyte and dosing interval. This field
can be helpful to identify dosing intervals across which rich sampling
was conducted.

## Usage

``` r
add_obs_per_dosing_interval(obj)
```

## Arguments

- obj:

  A NIF object.

## Value

Result as NIF object.

## Examples

``` r
add_obs_per_dosing_interval(examplinib_poc_nif)
#> # A tibble: 7,622 × 31
#> # Groups:   ID, USUBJID, ANALYTE, PARENT, DI [6,472]
#>      REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT
#>    <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl>
#>  1     1     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  2     2     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  3     3     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  4     4     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  5     5     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  6     6     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  7     7     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  8     8     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>  9     9     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#> 10    10     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4
#>      BMI DTC                  TIME NTIME  TAFD   TAD  EVID   AMT ANALYTE   
#>    <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>     
#>  1  26.6 2001-01-13 10:36:00  0      0    0     0        1   500 RS2023    
#>  2  26.6 2001-01-13 10:36:00  0      0    0     0        0     0 RS2023    
#>  3  26.6 2001-01-13 10:36:00  0      0    0     0        0     0 RS2023487A
#>  4  26.6 2001-01-13 11:36:00  1      0.5  1     1        0     0 RS2023    
#>  5  26.6 2001-01-13 11:36:00  1      0.5  1     1        0     0 RS2023487A
#>  6  26.6 2001-01-13 12:04:00  1.47   1    1.47  1.47     0     0 RS2023    
#>  7  26.6 2001-01-13 12:04:00  1.47   1    1.47  1.47     0     0 RS2023487A
#>  8  26.6 2001-01-13 12:36:00  2      1.5  2     2        0     0 RS2023    
#>  9  26.6 2001-01-13 12:36:00  2      1.5  2     2        0     0 RS2023487A
#> 10  26.6 2001-01-13 13:04:00  2.47   2    2.47  2.47     0     0 RS2023    
#>      CMT PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD  IMPUTATION    DI    DV
#>    <dbl> <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>     <chr>      <int> <dbl>
#>  1     1 RS2023     1 FALSE        500     1 TREATMENT ""             1   NA 
#>  2     2 RS2023     1 FALSE        500     0 TREATMENT ""             1    0 
#>  3     3 RS2023     1 FALSE        500     0 TREATMENT ""             1    0 
#>  4     2 RS2023     1 FALSE        500     0 TREATMENT ""             1  637.
#>  5     3 RS2023     1 FALSE        500     0 TREATMENT ""             1  135.
#>  6     2 RS2023     1 FALSE        500     0 TREATMENT ""             1 1845.
#>  7     3 RS2023     1 FALSE        500     0 TREATMENT ""             1  677.
#>  8     2 RS2023     1 FALSE        500     0 TREATMENT ""             1 2773.
#>  9     3 RS2023     1 FALSE        500     0 TREATMENT ""             1 1547.
#> 10     2 RS2023     1 FALSE        500     0 TREATMENT ""             1 2540.
#>    BL_CREAT BL_CRCL  OPDI
#>       <dbl>   <dbl> <int>
#>  1     72.8    107.    11
#>  2     72.8    107.    11
#>  3     72.8    107.    11
#>  4     72.8    107.    11
#>  5     72.8    107.    11
#>  6     72.8    107.    11
#>  7     72.8    107.    11
#>  8     72.8    107.    11
#>  9     72.8    107.    11
#> 10     72.8    107.    11
#> # ℹ 7,612 more rows
add_obs_per_dosing_interval(examplinib_poc_min_nif)
#> # A tibble: 6,578 × 13
#> # Groups:   ID, ANALYTE, PARENT, DI [5,620]
#>      REF    ID  TIME  EVID   AMT ANALYTE   CMT PARENT   MDV    DI  RATE    DV
#>    <int> <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <chr>  <dbl> <int> <dbl> <dbl>
#>  1     1     1  0        1   500 CMT1        1 CMT1       1     1     0 NA   
#>  2     2     1  0        0     0 CMT2        2 CMT1       0     1     0  0   
#>  3     3     1  0        0     0 CMT3        3 CMT1       0     1     0  0   
#>  4     4     1  2.17     0     0 CMT2        2 CMT1       0     1     0  3.38
#>  5     5     1  2.17     0     0 CMT3        3 CMT1       0     1     0  3.45
#>  6     6     1  4.63     0     0 CMT2        2 CMT1       0     1     0  1.48
#>  7     7     1  4.63     0     0 CMT3        3 CMT1       0     1     0 10.8 
#>  8     8     1 24        1   500 CMT1        1 CMT1       1     2     0 NA   
#>  9     9     1 48        1   500 CMT1        1 CMT1       1     3     0 NA   
#> 10    10     1 72        1   500 CMT1        1 CMT1       1     4     0 NA   
#>     OPDI
#>    <int>
#>  1     0
#>  2     3
#>  3     3
#>  4     3
#>  5     3
#>  6     3
#>  7     3
#>  8     0
#>  9     0
#> 10     0
#> # ℹ 6,568 more rows
```
