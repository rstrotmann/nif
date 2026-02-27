# Identify and index rich PK sampling intervals

Currently experimental. Don't use in production!

## Usage

``` r
index_rich_sampling_intervals(obj, analyte = NULL, min_n = 4)
```

## Arguments

- obj:

  The NIF object.

- analyte:

  The analyte as character. If `NA` (default), the most likely will be
  selected automatically.

- min_n:

  The minimum number of PK samples per analyte to qualify as rich
  sampling.

## Value

A new NIF object.

## Details

Adds the fields 'DI' (dosing interval per analyte), `RICHINT` (rich
sampling interval), and 'RICH_N' (index of the rich sampling interval by
analyte).

This function identifies rich sampling intervals by the number of
observations that follow an administration. A number of 'min_n' or more
observations before the next administration is interpreted as a rich
sampling interval and the corresponding observations are flagged with
'RICHINT' == TRUE. The index of the rich sampling intervals per subject
and analyte is reported in the 'RICH_N' field.

## Examples

``` r
head(index_rich_sampling_intervals(examplinib_poc_nif))
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
#>   TRTDY METABOLITE DOSE MDV  ACTARMCD IMPUTATION RICH_N DI        DV BL_CREAT
#> 1     1      FALSE  500   1 TREATMENT                 1  1        NA 86.46559
#> 2     1      FALSE  500   0 TREATMENT                 1  1    0.0000 86.46559
#> 3     1      FALSE  500   0 TREATMENT                 1  1    0.0000 86.46559
#> 4     1      FALSE  500   0 TREATMENT                 1  1  615.0549 86.46559
#> 5     1      FALSE  500   0 TREATMENT                 1  1  120.1609 86.46559
#> 6     1      FALSE  500   0 TREATMENT                 1  1 1841.7238 86.46559
#>    BL_CRCL OPDI
#> 1 78.66727   11
#> 2 78.66727   11
#> 3 78.66727   11
#> 4 78.66727   11
#> 5 78.66727   11
#> 6 78.66727   11
```
