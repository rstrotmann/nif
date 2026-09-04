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
#> 1   1  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 2   2  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 3   3  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 4   4  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 5   5  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 6   6  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT CMT        DV    ANALYTE
#> 1 2001-01-05 10:25:00 0.000   0.0 0.000 0.000    1 500   1        NA     RS2023
#> 2 2001-01-05 10:25:00 0.000   0.0 0.000 0.000    0   0   2    0.0000     RS2023
#> 3 2001-01-05 10:25:00 0.000   0.0 0.000 0.000    0   0   3    0.0000 RS2023487A
#> 4 2001-01-05 11:31:00 1.100   0.5 1.100 1.100    0   0   2  553.4686     RS2023
#> 5 2001-01-05 11:31:00 1.100   0.5 1.100 1.100    0   0   3  121.0349 RS2023487A
#> 6 2001-01-05 12:00:00 1.583   1.0 1.583 1.583    0   0   2 1484.4186     RS2023
#>   PARENT TRTDY METABOLITE DOSE MDV  ACTARMCD               IMPUTATION BL_CREAT
#> 1 RS2023     1      FALSE  500   1 TREATMENT time copied from EXSTDTC 58.84185
#> 2 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 3 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 4 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 5 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 6 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#>    BL_CRCL TIME_DEV
#> 1 165.5927    0.000
#> 2 165.5927  -24.000
#> 3 165.5927  -24.000
#> 4 165.5927    0.600
#> 5 165.5927    0.600
#> 6 165.5927    0.583
```
