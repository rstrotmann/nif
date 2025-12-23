# Add baseline renal function class

If baseline creatinine clearance (BL_CRCL) is not included in the input,
it will be calculated first.

## Usage

``` r
add_bl_renal(obj, method = egfr_cg)
```

## Arguments

- obj:

  A NIF object.

- method:

  The function to calculate eGFR (CrCL) from serum creatinine. Currently
  either: egfr_mdrd, egfr_cg or egfr_raynaud

## Value

A NIF object.

## Examples

``` r
head(add_bl_renal(examplinib_poc_nif), 5)
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT      BMI
#> 1   1  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 2   2  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 3   3  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 4   4  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 5   5  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#>                   DTC TIME NTIME TAFD TAD EVID AMT    ANALYTE CMT PARENT TRTDY
#> 1 2001-01-13 10:36:00    0   0.0    0   0    1 500     RS2023   1 RS2023     1
#> 2 2001-01-13 10:36:00    0   0.0    0   0    0   0     RS2023   2 RS2023     1
#> 3 2001-01-13 10:36:00    0   0.0    0   0    0   0 RS2023487A   3 RS2023     1
#> 4 2001-01-13 11:36:00    1   0.5    1   1    0   0     RS2023   2 RS2023     1
#> 5 2001-01-13 11:36:00    1   0.5    1   1    0   0 RS2023487A   3 RS2023     1
#>   METABOLITE DOSE MDV  ACTARMCD IMPUTATION       DV BL_CREAT  BL_CRCL BL_RENAL
#> 1      FALSE  500   1 TREATMENT                  NA 72.78062 107.4689   normal
#> 2      FALSE  500   0 TREATMENT              0.0000 72.78062 107.4689   normal
#> 3      FALSE  500   0 TREATMENT              0.0000 72.78062 107.4689   normal
#> 4      FALSE  500   0 TREATMENT            636.6833 72.78062 107.4689   normal
#> 5      FALSE  500   0 TREATMENT            135.1259 72.78062 107.4689   normal
```
