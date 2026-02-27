# Index dosing intervals

This function adds a column 'DI' that indicates the dosing interval. All
baseline observations before the first dosing interval get assigned to
the first dosing interval.

## Usage

``` r
index_dosing_interval(obj)
```

## Arguments

- obj:

  The NIF object.

## Value

A new NIF object.

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
#>                   DTC TIME NTIME TAFD TAD EVID AMT ANALYTE CMT PARENT TRTDY
#> 1 2001-01-05 10:05:00  0.0   0.0  0.0 0.0    1 500  RS2023   1 RS2023     1
#> 2 2001-01-05 10:05:00  0.0   0.0  0.0 0.0    0   0  RS2023   2 RS2023     1
#> 3 2001-01-05 10:35:00  0.5   0.5  0.5 0.5    0   0  RS2023   2 RS2023     1
#> 4 2001-01-05 11:05:00  1.0   1.0  1.0 1.0    0   0  RS2023   2 RS2023     1
#> 5 2001-01-05 11:35:00  1.5   1.5  1.5 1.5    0   0  RS2023   2 RS2023     1
#> 6 2001-01-05 12:05:00  2.0   2.0  2.0 2.0    0   0  RS2023   2 RS2023     1
#>   METABOLITE DOSE MDV ACTARMCD IMPUTATION PERIOD FASTED DI TREATMENT       DV
#> 1      FALSE  500   1       AB                 1      1  1         A       NA
#> 2      FALSE  500   0       AB                 1      1  1         A    0.000
#> 3      FALSE  500   0       AB                 1      1  1         A 4697.327
#> 4      FALSE  500   0       AB                 1      1  1         A 6325.101
#> 5      FALSE  500   0       AB                 1      1  1         A 6294.187
#> 6      FALSE  500   0       AB                 1      1  1         A 5932.464
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
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT    ANALYTE CMT PARENT
#> 1 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    1 500     RS2023   1 RS2023
#> 2 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0     RS2023   2 RS2023
#> 3 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0 RS2023487A   3 RS2023
#> 4 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0     RS2023   2 RS2023
#> 5 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0 RS2023487A   3 RS2023
#> 6 2001-01-07 11:02:00 1.333   1.0 1.333 1.333    0   0     RS2023   2 RS2023
#>   TRTDY METABOLITE DOSE MDV  ACTARMCD IMPUTATION DI        DV BL_CREAT  BL_CRCL
#> 1     1      FALSE  500   1 TREATMENT             1        NA 86.46559 78.66727
#> 2     1      FALSE  500   0 TREATMENT             1    0.0000 86.46559 78.66727
#> 3     1      FALSE  500   0 TREATMENT             1    0.0000 86.46559 78.66727
#> 4     1      FALSE  500   0 TREATMENT             1  615.0549 86.46559 78.66727
#> 5     1      FALSE  500   0 TREATMENT             1  120.1609 86.46559 78.66727
#> 6     1      FALSE  500   0 TREATMENT             1 1841.7238 86.46559 78.66727
head(index_dosing_interval(examplinib_poc_min_nif))
#>   REF ID  TIME EVID AMT ANALYTE CMT PARENT MDV DI RATE       DV
#> 1   1  1 0.000    1 500    CMT1   1   CMT1   1  1    0       NA
#> 2   2  1 0.000    0   0    CMT2   2   CMT1   0  1    0 0.000000
#> 3   3  1 0.000    0   0    CMT3   3   CMT1   0  1    0 0.000000
#> 4   4  1 2.167    0   0    CMT2   2   CMT1   0  1    0 3.380265
#> 5   5  1 2.167    0   0    CMT3   3   CMT1   0  1    0 3.453869
#> 6   6  1 4.633    0   0    CMT2   2   CMT1   0  1    0 1.477397
```
