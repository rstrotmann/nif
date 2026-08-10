# Add the number of observations per dosing interval

This function adds a variable, `OPDI`, to the NIF object that indicates
the number of observations per subject, parent and dosing interval. This
field can be helpful to identify dosing intervals across which rich
sampling was conducted.

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
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT      BMI
#> 1   1  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 2   2  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 3   3  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 4   4  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 5   5  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 6   6  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT CMT        DV    ANALYTE
#> 1 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    1 500   1        NA     RS2023
#> 2 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0   2    0.0000     RS2023
#> 3 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0   3    0.0000 RS2023487A
#> 4 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0   2  615.0549     RS2023
#> 5 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0   3  120.1609 RS2023487A
#> 6 2001-01-07 11:02:00 1.333   1.0 1.333 1.333    0   0   2 1841.7238     RS2023
#>   PARENT TRTDY METABOLITE DOSE MDV  ACTARMCD IMPUTATION DI BL_CREAT  BL_CRCL
#> 1 RS2023     1      FALSE  500   1 TREATMENT             1 86.46559 78.66727
#> 2 RS2023     1      FALSE  500   0 TREATMENT             1 86.46559 78.66727
#> 3 RS2023     1      FALSE  500   0 TREATMENT             1 86.46559 78.66727
#> 4 RS2023     1      FALSE  500   0 TREATMENT             1 86.46559 78.66727
#> 5 RS2023     1      FALSE  500   0 TREATMENT             1 86.46559 78.66727
#> 6 RS2023     1      FALSE  500   0 TREATMENT             1 86.46559 78.66727
#>   OPDI
#> 1   22
#> 2   22
#> 3   22
#> 4   22
#> 5   22
#> 6   22
head(add_obs_per_dosing_interval(examplinib_poc_min_nif))
#>   REF ID  TIME EVID AMT CMT       DV ANALYTE PARENT MDV DI RATE OPDI
#> 1   1  1 0.000    1 500   1       NA    CMT1   CMT1   1  1    0    6
#> 2   2  1 0.000    0   0   2 0.000000    CMT2   CMT1   0  1    0    6
#> 3   3  1 0.000    0   0   3 0.000000    CMT3   CMT1   0  1    0    6
#> 4   4  1 2.167    0   0   2 3.380265    CMT2   CMT1   0  1    0    6
#> 5   5  1 2.167    0   0   3 3.453869    CMT3   CMT1   0  1    0    6
#> 6   6  1 4.633    0   0   2 1.477397    CMT2   CMT1   0  1    0    6
```
