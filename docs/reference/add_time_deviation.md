# Calculate time deviations for observations

**\[experimental\]**

Calculate the time difference in hours between the actual time after
dose (TAD) and the scheduled time (NTIME) of observations.

If NTIME == 0, the data point is considered pre-dose, and instead of
TAD, the (negative) time difference to the next administration is
calculated!

## Usage

``` r
add_time_deviation(obj, silent = NULL)
```

## Arguments

- obj:

  A nif object.

- silent:

  Suppress messages.

## Value

A nif object with the 'TIME_DEV' field added

## Examples

``` r
library(dplyr)

examplinib_poc_nif |>
  add_time_deviation() |>
  head()
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT      BMI
#> 1   1  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 2   2  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 3   3  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 4   4  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 5   5  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 6   6  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT    ANALYTE CMT PARENT
#> 1 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    1 500     RS2023   1 RS2023
#> 2 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0     RS2023   2 RS2023
#> 3 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0 RS2023487A   3 RS2023
#> 4 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0     RS2023   2 RS2023
#> 5 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0 RS2023487A   3 RS2023
#> 6 2001-01-07 11:02:00 1.333   1.0 1.333 1.333    0   0     RS2023   2 RS2023
#>   TRTDY METABOLITE DOSE MDV  ACTARMCD IMPUTATION        DV BL_CREAT  BL_CRCL
#> 1     1      FALSE  500   1 TREATMENT                   NA 86.46559 78.66727
#> 2     1      FALSE  500   0 TREATMENT               0.0000 86.46559 78.66727
#> 3     1      FALSE  500   0 TREATMENT               0.0000 86.46559 78.66727
#> 4     1      FALSE  500   0 TREATMENT             615.0549 86.46559 78.66727
#> 5     1      FALSE  500   0 TREATMENT             120.1609 86.46559 78.66727
#> 6     1      FALSE  500   0 TREATMENT            1841.7238 86.46559 78.66727
#>   TIME_DEV
#> 1    0.000
#> 2  -24.000
#> 3  -24.000
#> 4    0.367
#> 5    0.367
#> 6    0.333
```
