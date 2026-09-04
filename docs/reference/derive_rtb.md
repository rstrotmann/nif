# Calculate ratio to baseline

Derive the individual baseline value (DVBL field) and the ratio of the
current value to baseline (DVRTB field) for an analyte. For analytes
with a baseline of zero, NA values are returned.

## Usage

``` r
derive_rtb(
  obj,
  analyte = NULL,
  baseline_filter = "TAFD <= 0",
  summary_function = median,
  default_baseline = NA_real_,
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- analyte:

  The analyte to derive the baseline for, as character. Defaults to all
  analytes if NULL.

- baseline_filter:

  The baseline condition as character, defaults to `TAFD <= 0`.

- summary_function:

  A function to reduce multiple baseline values, defaults to `median`.

- default_baseline:

  The default value if the baseline filter computes to NA.

- silent:

  Suppress messages, as logical.

## Value

A nif object with the DVBL and DVRTB fields added for the specified
analyte.

## Examples

``` r
head(derive_rtb(examplinib_sad_nif))
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 4   4  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 5   5  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 6   6  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT CMT      DV ANALYTE PARENT
#> 1 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    1   5   1      NA  RS2023 RS2023
#> 2 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0   2  0.0000  RS2023 RS2023
#> 3 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0   2 44.5174  RS2023 RS2023
#> 4 2000-12-31 11:18:00  1.0   1.0  1.0 1.0    0   0   2 56.2150  RS2023 RS2023
#> 5 2000-12-31 11:48:00  1.5   1.5  1.5 1.5    0   0   2 50.3743  RS2023 RS2023
#> 6 2000-12-31 12:18:00  2.0   2.0  2.0 2.0    0   0   2 40.7461  RS2023 RS2023
#>   TRTDY METABOLITE DOSE MDV ACTARMCD               IMPUTATION BL_CREAT  BL_CRCL
#> 1     1      FALSE    5   1       C1 time copied from EXSTDTC  88.9614 103.0815
#> 2     1      FALSE    5   0       C1                           88.9614 103.0815
#> 3     1      FALSE    5   0       C1                           88.9614 103.0815
#> 4     1      FALSE    5   0       C1                           88.9614 103.0815
#> 5     1      FALSE    5   0       C1                           88.9614 103.0815
#> 6     1      FALSE    5   0       C1                           88.9614 103.0815
#>   DVBL DVRTB
#> 1    0    NA
#> 2    0    NA
#> 3    0    NA
#> 4    0    NA
#> 5    0    NA
#> 6    0    NA
```
