# Recode RACE columns in nif object

For some purposes, e.g., NONMEM-based modeling, numerical values are
expected in the RACE field. This function recodes RACE based on the
following associations:

## Usage

``` r
recode_race(obj, coding_table = NULL, silent = NULL)
```

## Arguments

- obj:

  A nif object with RACE as character field.

- coding_table:

  A data frame with the columns RACE and RACEN. Uses default coding, if
  NULL.

- silent:

  Suppress messages, defaults to nif_option setting, if NULL.

## Value

A nif object with the original RACE replaced by the numerical race code.

## Examples

``` r
nif::race_coding
#> # A tibble: 8 × 3
#>   RACEN RACE                                      LABEL  
#>   <dbl> <chr>                                     <chr>  
#> 1     0 WHITE                                     White  
#> 2     1 ASIAN                                     Asian  
#> 3     2 BLACK OR AFRICAN AMERICAN                 Black  
#> 4     3 AMERICAN INDIAN OR ALASKA NATIVE          Native 
#> 5     4 NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER Pacific
#> 6     5 NOT REPORTED                              NR     
#> 7     6 UNKNOWN                                   Unknown
#> 8     7 OTHER                                     Other  
head(recode_race(examplinib_sad_nif))
#>   REF ID    STUDYID           USUBJID AGE SEX RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0    0  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0    0  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0    0  187.4     77 21.9256
#> 4   4  1 2023000001 20230000011010001  43   0    0  187.4     77 21.9256
#> 5   5  1 2023000001 20230000011010001  43   0    0  187.4     77 21.9256
#> 6   6  1 2023000001 20230000011010001  43   0    0  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT ANALYTE CMT PARENT TRTDY
#> 1 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    1   5  RS2023   1 RS2023     1
#> 2 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0  RS2023   2 RS2023     1
#> 3 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0  RS2023   2 RS2023     1
#> 4 2000-12-31 11:18:00  1.0   1.0  1.0 1.0    0   0  RS2023   2 RS2023     1
#> 5 2000-12-31 11:48:00  1.5   1.5  1.5 1.5    0   0  RS2023   2 RS2023     1
#> 6 2000-12-31 12:18:00  2.0   2.0  2.0 2.0    0   0  RS2023   2 RS2023     1
#>   METABOLITE DOSE MDV ACTARMCD IMPUTATION      DV BL_CREAT  BL_CRCL
#> 1      FALSE    5   1       C1                 NA  67.4825 115.5074
#> 2      FALSE    5   0       C1             0.0000  67.4825 115.5074
#> 3      FALSE    5   0       C1            40.7852  67.4825 115.5074
#> 4      FALSE    5   0       C1            48.5530  67.4825 115.5074
#> 5      FALSE    5   0       C1            44.0391  67.4825 115.5074
#> 6      FALSE    5   0       C1            34.0729  67.4825 115.5074
```
