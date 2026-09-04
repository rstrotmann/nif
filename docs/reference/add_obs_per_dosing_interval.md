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
#>   PARENT TRTDY METABOLITE DOSE MDV  ACTARMCD               IMPUTATION DI
#> 1 RS2023     1      FALSE  500   1 TREATMENT time copied from EXSTDTC  1
#> 2 RS2023     1      FALSE  500   0 TREATMENT                           1
#> 3 RS2023     1      FALSE  500   0 TREATMENT                           1
#> 4 RS2023     1      FALSE  500   0 TREATMENT                           1
#> 5 RS2023     1      FALSE  500   0 TREATMENT                           1
#> 6 RS2023     1      FALSE  500   0 TREATMENT                           1
#>   BL_CREAT  BL_CRCL OPDI
#> 1 58.84185 165.5927   11
#> 2 58.84185 165.5927   11
#> 3 58.84185 165.5927   11
#> 4 58.84185 165.5927   11
#> 5 58.84185 165.5927   11
#> 6 58.84185 165.5927   11
head(add_obs_per_dosing_interval(examplinib_poc_min_nif))
#>   REF ID  TIME EVID AMT CMT       DV ANALYTE PARENT MDV DI RATE OPDI
#> 1   1  1 0.000    1 500   1       NA    CMT1   CMT1   1  1    0    0
#> 2   2  1 0.000    0   0   2 0.000000    CMT2   CMT1   0  1    0    3
#> 3   3  1 0.000    0   0   3 0.000000    CMT3   CMT1   0  1    0    3
#> 4   4  1 2.167    0   0   2 3.380265    CMT2   CMT1   0  1    0    3
#> 5   5  1 2.167    0   0   3 3.453869    CMT3   CMT1   0  1    0    3
#> 6   6  1 4.633    0   0   2 1.477397    CMT2   CMT1   0  1    0    3
```
