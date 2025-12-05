# Add baseline and change from baseline fields

**\[deprecated\]**

## Usage

``` r
add_cfb(
  obj,
  baseline_filter = "TIME <= 0",
  summary_function = median,
  silent = NULL
)
```

## Arguments

- obj:

  A NIF object.

- baseline_filter:

  A filter term to identify the baseline condition.

- summary_function:

  The function to derive the baseline. This function is applied over the
  DV values identified by the 'baseline_filter' term. The default
  function is `median`. Alternatively, `mean`, `min` or `max` can be
  considered.

- silent:

  Suppress messages, defaults to nif_option setting if NULL.

## Value

A NIF object

## Details

Output fields:

- `DVBL` Baseline value for the dependent variable DV.

- `DVCFB` Change from baseline for the dependent variable DV.

The Baseline is calculated as the median (or the summary function
output) of the DV field for all time points identified by the
baseline_filter' term.

## Examples

``` r
head(add_cfb(examplinib_poc_nif))
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
#>   TRTDY METABOLITE DOSE MDV  ACTARMCD IMPUTATION        DV BL_CREAT  BL_CRCL
#> 1     1      FALSE  500   1 TREATMENT                   NA 72.78062 107.4689
#> 2     1      FALSE  500   0 TREATMENT               0.0000 72.78062 107.4689
#> 3     1      FALSE  500   0 TREATMENT               0.0000 72.78062 107.4689
#> 4     1      FALSE  500   0 TREATMENT             636.6833 72.78062 107.4689
#> 5     1      FALSE  500   0 TREATMENT             135.1259 72.78062 107.4689
#> 6     1      FALSE  500   0 TREATMENT            1844.8225 72.78062 107.4689
#>   DVBL     DVCFB
#> 1    0        NA
#> 2    0    0.0000
#> 3    0    0.0000
#> 4    0  636.6833
#> 5    0  135.1259
#> 6    0 1844.8225
head(add_cfb(examplinib_poc_min_nif))
#>   ID  TIME EVID AMT ANALYTE CMT MDV RATE       DV DVBL    DVCFB
#> 1  1 0.000    1 500    CMT1   1   1    0       NA   NA       NA
#> 2  1 0.000    0   0    CMT2   2   0    0 0.000000    0 0.000000
#> 3  1 0.000    0   0    CMT3   3   0    0 0.000000    0 0.000000
#> 4  1 2.167    0   0    CMT2   2   0    0 3.380265    0 3.380265
#> 5  1 2.167    0   0    CMT3   3   0    0 3.453869    0 3.453869
#> 6  1 4.633    0   0    CMT2   2   0    0 1.477397    0 1.477397
```
