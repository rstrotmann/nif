# Add treatment day ('TRTDY') column

Add treatment day ('TRTDY') column

## Usage

``` r
add_trtdy(obj)
```

## Arguments

- obj:

  The NIF object as data frame.

## Value

The updated NIF object.

## Examples

``` r
head(add_trtdy(examplinib_poc_nif))
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
#>   PARENT TRTDY METABOLITE DOSE MDV  ACTARMCD               IMPUTATION BL_CREAT
#> 1 RS2023     1      FALSE  500   1 TREATMENT time copied from EXSTDTC 58.84185
#> 2 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 3 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 4 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 5 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 6 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#>    BL_CRCL
#> 1 165.5927
#> 2 165.5927
#> 3 165.5927
#> 4 165.5927
#> 5 165.5927
#> 6 165.5927
```
