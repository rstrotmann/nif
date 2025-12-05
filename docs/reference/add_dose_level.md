# Add dose level (`DL`) column

Dose level is defined as the starting dose. For data sets with single
drug administration, `DL`is a numerical value, for drug combinations, it
is a character value specifying the `PARENT` and dose level for the
individual components.

## Usage

``` r
add_dose_level(obj)
```

## Arguments

- obj:

  A NIF dataset.

## Value

A NIF dataset.

## Examples

``` r
head(add_dose_level(examplinib_sad_nif))
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 4   4  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 5   5  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 6   6  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT ANALYTE CMT PARENT TRTDY
#> 1 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    1   5  RS2023   1 RS2023     1
#> 2 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0  RS2023   2 RS2023     1
#> 3 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0  RS2023   2 RS2023     1
#> 4 2000-12-31 11:18:00  1.0   1.0  1.0 1.0    0   0  RS2023   2 RS2023     1
#> 5 2000-12-31 11:48:00  1.5   1.5  1.5 1.5    0   0  RS2023   2 RS2023     1
#> 6 2000-12-31 12:18:00  2.0   2.0  2.0 2.0    0   0  RS2023   2 RS2023     1
#>   METABOLITE DOSE MDV ACTARMCD IMPUTATION      DV BL_CREAT  BL_CRCL DL
#> 1      FALSE    5   1       C1                 NA  67.4825 115.5074  5
#> 2      FALSE    5   0       C1             0.0000  67.4825 115.5074  5
#> 3      FALSE    5   0       C1            40.7852  67.4825 115.5074  5
#> 4      FALSE    5   0       C1            48.5530  67.4825 115.5074  5
#> 5      FALSE    5   0       C1            44.0391  67.4825 115.5074  5
#> 6      FALSE    5   0       C1            34.0729  67.4825 115.5074  5
head(add_dose_level(examplinib_sad_min_nif))
#>   ID TIME AMT RATE EVID        DV CMT MDV DL
#> 1  1  0.0   5    0    1        NA   1   1  5
#> 2  1  0.0   0    0    0 0.0000000   2   0  5
#> 3  1  0.5   0    0    0 0.0024470   2   0  5
#> 4  1  1.0   0    0    0 0.0072445   2   0  5
#> 5  1  1.5   0    0    0 0.0157476   2   0  5
#> 6  1  2.0   0    0    0 0.0209229   2   0  5
```
