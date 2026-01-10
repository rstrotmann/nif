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
head(add_obs_per_dosing_interval(examplinib_poc_nif))
#> # A tibble: 6 × 31
#> # Groups:   ID, USUBJID, ANALYTE, PARENT, DI [2]
#>     REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT   BMI
#>   <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl> <dbl>
#> 1     1     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4  26.6
#> 2     2     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4  26.6
#> 3     3     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4  26.6
#> 4     4     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4  26.6
#> 5     5     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4  26.6
#> 6     6     1 2023000022 20230000221010001    58     1 WHITE   185.   91.4  26.6
#>   DTC                  TIME NTIME  TAFD   TAD  EVID   AMT ANALYTE      CMT
#>   <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>      <dbl>
#> 1 2001-01-13 10:36:00  0      0    0     0        1   500 RS2023         1
#> 2 2001-01-13 10:36:00  0      0    0     0        0     0 RS2023         2
#> 3 2001-01-13 10:36:00  0      0    0     0        0     0 RS2023487A     3
#> 4 2001-01-13 11:36:00  1      0.5  1     1        0     0 RS2023         2
#> 5 2001-01-13 11:36:00  1      0.5  1     1        0     0 RS2023487A     3
#> 6 2001-01-13 12:04:00  1.47   1    1.47  1.47     0     0 RS2023         2
#>   PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD  IMPUTATION    DI    DV BL_CREAT
#>   <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>     <chr>      <int> <dbl>    <dbl>
#> 1 RS2023     1 FALSE        500     1 TREATMENT ""             1   NA      72.8
#> 2 RS2023     1 FALSE        500     0 TREATMENT ""             1    0      72.8
#> 3 RS2023     1 FALSE        500     0 TREATMENT ""             1    0      72.8
#> 4 RS2023     1 FALSE        500     0 TREATMENT ""             1  637.     72.8
#> 5 RS2023     1 FALSE        500     0 TREATMENT ""             1  135.     72.8
#> 6 RS2023     1 FALSE        500     0 TREATMENT ""             1 1845.     72.8
#>   BL_CRCL  OPDI
#>     <dbl> <int>
#> 1    107.    11
#> 2    107.    11
#> 3    107.    11
#> 4    107.    11
#> 5    107.    11
#> 6    107.    11
head(add_obs_per_dosing_interval(examplinib_poc_min_nif))
#> # A tibble: 6 × 13
#> # Groups:   ID, ANALYTE, PARENT, DI [3]
#>     REF    ID  TIME  EVID   AMT ANALYTE   CMT PARENT   MDV    DI  RATE    DV
#>   <int> <dbl> <dbl> <dbl> <dbl> <chr>   <dbl> <chr>  <dbl> <int> <dbl> <dbl>
#> 1     1     1  0        1   500 CMT1        1 CMT1       1     1     0 NA   
#> 2     2     1  0        0     0 CMT2        2 CMT1       0     1     0  0   
#> 3     3     1  0        0     0 CMT3        3 CMT1       0     1     0  0   
#> 4     4     1  2.17     0     0 CMT2        2 CMT1       0     1     0  3.38
#> 5     5     1  2.17     0     0 CMT3        3 CMT1       0     1     0  3.45
#> 6     6     1  4.63     0     0 CMT2        2 CMT1       0     1     0  1.48
#>    OPDI
#>   <int>
#> 1     0
#> 2     3
#> 3     3
#> 4     3
#> 5     3
#> 6     3
```
