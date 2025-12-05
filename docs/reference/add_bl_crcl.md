# Add baseline creatinine clearance field.

Add baseline creatinine clearance field.

## Usage

``` r
add_bl_crcl(obj, method = egfr_cg)
```

## Arguments

- obj:

  A NIF object.

- method:

  The function to calculate eGFR (CrCL) from serum creatinine. Currently
  either: egfr_mdrd, egfr_cg or egfr_raynaud

## Value

A NIF object.

## See also

[`egfr_mdrd()`](egfr_mdrd.md)

[`egfr_cg()`](egfr_cg.md)

[`egfr_raynaud()`](egfr_raynaud.md)

## Examples

``` r
head(add_bl_crcl(examplinib_poc_nif))
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
```
