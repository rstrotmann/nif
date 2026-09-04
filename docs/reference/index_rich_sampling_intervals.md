# Identify and index rich PK sampling intervals

Currently experimental. Don't use in production!

## Usage

``` r
index_rich_sampling_intervals(obj, min_n = 4)
```

## Arguments

- obj:

  The NIF object.

- min_n:

  The minimum number of PK samples per analyte to qualify as rich
  sampling.

## Value

A new NIF object.

## Details

Adds the fields 'DI' (dosing interval per analyte) and 'RICH_N' (index
of the rich sampling interval by analyte).

This function identifies rich sampling intervals by the number of
observations that follow an administration. A number of 'min_n' or more
observations before the next administration is interpreted as a rich
sampling interval. The index of the rich sampling intervals per subject
and analyte is reported in the 'RICH_N' field.

## Examples

``` r
head(index_rich_sampling_intervals(examplinib_poc_nif))
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
#>   PARENT TRTDY METABOLITE DOSE MDV  ACTARMCD               IMPUTATION RICH_N DI
#> 1 RS2023     1      FALSE  500   1 TREATMENT time copied from EXSTDTC      1  1
#> 2 RS2023     1      FALSE  500   0 TREATMENT                               1  1
#> 3 RS2023     1      FALSE  500   0 TREATMENT                               1  1
#> 4 RS2023     1      FALSE  500   0 TREATMENT                               1  1
#> 5 RS2023     1      FALSE  500   0 TREATMENT                               1  1
#> 6 RS2023     1      FALSE  500   0 TREATMENT                               1  1
#>   BL_CREAT  BL_CRCL OPDI
#> 1 58.84185 165.5927   11
#> 2 58.84185 165.5927   11
#> 3 58.84185 165.5927   11
#> 4 58.84185 165.5927   11
#> 5 58.84185 165.5927   11
#> 6 58.84185 165.5927   11
```
