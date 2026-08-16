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
head(index_dosing_interval(examplinib_fe_nif))
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT      BMI
#> 1   1  1 2023000400 20230004001010002  53   1 WHITE  180.4   73.1 22.46179
#> 2   2  1 2023000400 20230004001010002  53   1 WHITE  180.4   73.1 22.46179
#> 3   3  1 2023000400 20230004001010002  53   1 WHITE  180.4   73.1 22.46179
#> 4   4  1 2023000400 20230004001010002  53   1 WHITE  180.4   73.1 22.46179
#> 5   5  1 2023000400 20230004001010002  53   1 WHITE  180.4   73.1 22.46179
#> 6   6  1 2023000400 20230004001010002  53   1 WHITE  180.4   73.1 22.46179
#>                   DTC TIME NTIME TAFD TAD EVID AMT CMT       DV ANALYTE PARENT
#> 1 2001-01-05 10:05:00  0.0   0.0  0.0 0.0    1 500   1       NA  RS2023 RS2023
#> 2 2001-01-05 10:05:00  0.0   0.0  0.0 0.0    0   0   2    0.000  RS2023 RS2023
#> 3 2001-01-05 10:35:00  0.5   0.5  0.5 0.5    0   0   2 4697.327  RS2023 RS2023
#> 4 2001-01-05 11:05:00  1.0   1.0  1.0 1.0    0   0   2 6325.101  RS2023 RS2023
#> 5 2001-01-05 11:35:00  1.5   1.5  1.5 1.5    0   0   2 6294.187  RS2023 RS2023
#> 6 2001-01-05 12:05:00  2.0   2.0  2.0 2.0    0   0   2 5932.464  RS2023 RS2023
#>   TRTDY METABOLITE DOSE MDV ACTARMCD IMPUTATION PERIOD FASTED DI TREATMENT
#> 1     1      FALSE  500   1       AB                 1      1  1         A
#> 2     1      FALSE  500   0       AB                 1      1  1         A
#> 3     1      FALSE  500   0       AB                 1      1  1         A
#> 4     1      FALSE  500   0       AB                 1      1  1         A
#> 5     1      FALSE  500   0       AB                 1      1  1         A
#> 6     1      FALSE  500   0       AB                 1      1  1         A
#>                    EPOCH
#> 1 OPEN LABEL TREATMENT 1
#> 2 OPEN LABEL TREATMENT 1
#> 3 OPEN LABEL TREATMENT 1
#> 4 OPEN LABEL TREATMENT 1
#> 5 OPEN LABEL TREATMENT 1
#> 6 OPEN LABEL TREATMENT 1
head(index_dosing_interval(examplinib_poc_nif))
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
head(index_dosing_interval(examplinib_poc_min_nif))
#>   REF ID  TIME EVID AMT CMT       DV ANALYTE PARENT MDV DI RATE
#> 1   1  1 0.000    1 500   1       NA    CMT1   CMT1   1  1    0
#> 2   2  1 0.000    0   0   2 0.000000    CMT2   CMT1   0  1    0
#> 3   3  1 0.000    0   0   3 0.000000    CMT3   CMT1   0  1    0
#> 4   4  1 2.167    0   0   2 3.380265    CMT2   CMT1   0  1    0
#> 5   5  1 2.167    0   0   3 3.453869    CMT3   CMT1   0  1    0
#> 6   6  1 4.633    0   0   2 1.477397    CMT2   CMT1   0  1    0
```
