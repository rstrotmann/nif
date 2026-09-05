# Add the number of observations per dosing interval

This function adds a variable, `OPDI`, to the NIF object that indicates
the number of observations per subject, parent and dosing interval. This
field can be helpful to identify dosing intervals across which rich
sampling was conducted.

## Usage

``` r
add_obs_per_dosing_interval(obj)
```

## Arguments

- obj:

  A NIF object.

## Value

Result as NIF object.

## Examples

``` r
add_obs_per_dosing_interval(examplinib_poc_nif)
#> ──────── NONMEM Input Format (NIF) data ────────
#> 1344 observations from 80 subjects across 1 study 
#> 
#> # A tibble: 7,494 × 31
#>      REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT
#>    <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl>
#>  1     1     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  2     2     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  3     3     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  4     4     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  5     5     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  6     6     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  7     7     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  8     8     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>  9     9     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#> 10    10     1 2023000022 20230000221010001    49     1 WHITE   180.   103.
#>      BMI DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV
#>    <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  31.5 2001-01-05 10:25:00  0      0    0     0        1   500     1   NA 
#>  2  31.5 2001-01-05 10:25:00  0      0    0     0        0     0     2    0 
#>  3  31.5 2001-01-05 10:25:00  0      0    0     0        0     0     3    0 
#>  4  31.5 2001-01-05 11:31:00  1.1    0.5  1.1   1.1      0     0     2  553.
#>  5  31.5 2001-01-05 11:31:00  1.1    0.5  1.1   1.1      0     0     3  121.
#>  6  31.5 2001-01-05 12:00:00  1.58   1    1.58  1.58     0     0     2 1484.
#>  7  31.5 2001-01-05 12:00:00  1.58   1    1.58  1.58     0     0     3  579.
#>  8  31.5 2001-01-05 12:32:00  2.12   1.5  2.12  2.12     0     0     2 2073.
#>  9  31.5 2001-01-05 12:32:00  2.12   1.5  2.12  2.12     0     0     3 1287.
#> 10  31.5 2001-01-05 13:00:00  2.58   2    2.58  2.58     0     0     2 2032.
#>    ANALYTE    PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD 
#>    <chr>      <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>    
#>  1 RS2023     RS2023     1 FALSE        500     1 TREATMENT
#>  2 RS2023     RS2023     1 FALSE        500     0 TREATMENT
#>  3 RS2023487A RS2023     1 FALSE        500     0 TREATMENT
#>  4 RS2023     RS2023     1 FALSE        500     0 TREATMENT
#>  5 RS2023487A RS2023     1 FALSE        500     0 TREATMENT
#>  6 RS2023     RS2023     1 FALSE        500     0 TREATMENT
#>  7 RS2023487A RS2023     1 FALSE        500     0 TREATMENT
#>  8 RS2023     RS2023     1 FALSE        500     0 TREATMENT
#>  9 RS2023487A RS2023     1 FALSE        500     0 TREATMENT
#> 10 RS2023     RS2023     1 FALSE        500     0 TREATMENT
#>    IMPUTATION                 BL_CREAT BL_CRCL    DI  OPDI
#>    <chr>                         <dbl>   <dbl> <int> <int>
#>  1 "time copied from EXSTDTC"     58.8    166.     1    11
#>  2 ""                             58.8    166.     1    11
#>  3 ""                             58.8    166.     1    11
#>  4 ""                             58.8    166.     1    11
#>  5 ""                             58.8    166.     1    11
#>  6 ""                             58.8    166.     1    11
#>  7 ""                             58.8    166.     1    11
#>  8 ""                             58.8    166.     1    11
#>  9 ""                             58.8    166.     1    11
#> 10 ""                             58.8    166.     1    11
#> # ℹ 7,484 more rows
add_obs_per_dosing_interval(examplinib_poc_min_nif)
#> ──────── NONMEM Input Format (NIF) data ────────
#> 1344 observations from 80 subjects  
#> 
#> # A tibble: 7,494 × 12
#>      REF    ID  TIME  EVID   AMT   CMT    DV ANALYTE PARENT   MDV    DI  OPDI
#>    <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>   <chr>  <dbl> <int> <int>
#>  1     1     1  0        1   500     1   NA  CMT1    CMT1       1     1     0
#>  2     2     1  0        0     0     2    0  CMT2    CMT1       0     1    11
#>  3     3     1  0        0     0     3    0  CMT3    CMT1       0     1    11
#>  4     4     1  1.1      0     0     2  553. CMT2    CMT1       0     1    11
#>  5     5     1  1.1      0     0     3  121. CMT3    CMT1       0     1    11
#>  6     6     1  1.58     0     0     2 1484. CMT2    CMT1       0     1    11
#>  7     7     1  1.58     0     0     3  579. CMT3    CMT1       0     1    11
#>  8     8     1  2.12     0     0     2 2073. CMT2    CMT1       0     1    11
#>  9     9     1  2.12     0     0     3 1287. CMT3    CMT1       0     1    11
#> 10    10     1  2.58     0     0     2 2032. CMT2    CMT1       0     1    11
#> # ℹ 7,484 more rows
```
