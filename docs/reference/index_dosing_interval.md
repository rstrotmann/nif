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
#>   TRTDY METABOLITE DOSE MDV ACTARMCD               IMPUTATION PERIOD FASTED DI
#> 1     1      FALSE  500   1       AB time copied from EXSTDTC      1      1  1
#> 2     1      FALSE  500   0       AB                               1      1  1
#> 3     1      FALSE  500   0       AB                               1      1  1
#> 4     1      FALSE  500   0       AB                               1      1  1
#> 5     1      FALSE  500   0       AB                               1      1  1
#> 6     1      FALSE  500   0       AB                               1      1  1
#>   TREATMENT                  EPOCH
#> 1         A OPEN LABEL TREATMENT 1
#> 2         A OPEN LABEL TREATMENT 1
#> 3         A OPEN LABEL TREATMENT 1
#> 4         A OPEN LABEL TREATMENT 1
#> 5         A OPEN LABEL TREATMENT 1
#> 6         A OPEN LABEL TREATMENT 1
head(index_dosing_interval(examplinib_poc_nif))
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
#>   BL_CREAT  BL_CRCL
#> 1 58.84185 165.5927
#> 2 58.84185 165.5927
#> 3 58.84185 165.5927
#> 4 58.84185 165.5927
#> 5 58.84185 165.5927
#> 6 58.84185 165.5927
head(index_dosing_interval(examplinib_poc_min_nif))
#>   REF ID  TIME EVID AMT CMT       DV ANALYTE PARENT MDV DI RATE
#> 1   1  1 0.000    1 500   1       NA    CMT1   CMT1   1  1    0
#> 2   2  1 0.000    0   0   2 0.000000    CMT2   CMT1   0  1    0
#> 3   3  1 0.000    0   0   3 0.000000    CMT3   CMT1   0  1    0
#> 4   4  1 2.167    0   0   2 3.380265    CMT2   CMT1   0  1    0
#> 5   5  1 2.167    0   0   3 3.453869    CMT3   CMT1   0  1    0
#> 6   6  1 4.633    0   0   2 1.477397    CMT2   CMT1   0  1    0
```
