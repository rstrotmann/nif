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
#> 1   1  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 2   2  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 3   3  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 4   4  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 5   5  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 6   6  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT    ANALYTE CMT PARENT
#> 1 2001-01-13 10:36:00 0.000   0.0 0.000 0.000    1 500     RS2023   1 RS2023
#> 2 2001-01-13 10:36:00 0.000   0.0 0.000 0.000    0   0     RS2023   2 RS2023
#> 3 2001-01-13 10:36:00 0.000   0.0 0.000 0.000    0   0 RS2023487A   3 RS2023
#> 4 2001-01-13 11:36:00 1.000   0.5 1.000 1.000    0   0     RS2023   2 RS2023
#> 5 2001-01-13 11:36:00 1.000   0.5 1.000 1.000    0   0 RS2023487A   3 RS2023
#> 6 2001-01-13 12:04:00 1.467   1.0 1.467 1.467    0   0     RS2023   2 RS2023
#>   TRTDY METABOLITE DOSE MDV  ACTARMCD IMPUTATION RICH_N DI        DV BL_CREAT
#> 1     1      FALSE  500   1 TREATMENT                 1  1        NA 72.78062
#> 2     1      FALSE  500   0 TREATMENT                 1  1    0.0000 72.78062
#> 3     1      FALSE  500   0 TREATMENT                 1  1    0.0000 72.78062
#> 4     1      FALSE  500   0 TREATMENT                 1  1  636.6833 72.78062
#> 5     1      FALSE  500   0 TREATMENT                 1  1  135.1259 72.78062
#> 6     1      FALSE  500   0 TREATMENT                 1  1 1844.8225 72.78062
#>    BL_CRCL OPDI
#> 1 107.4689   11
#> 2 107.4689   11
#> 3 107.4689   11
#> 4 107.4689   11
#> 5 107.4689   11
#> 6 107.4689   11
```
