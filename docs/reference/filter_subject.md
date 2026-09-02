# Keep only selected USUBJID in the data set

Keep only selected USUBJID in the data set

## Usage

``` r
filter_subject(obj, usubjid)
```

## Arguments

- obj:

  The input, either a `nif` or `sdtm` object.

- usubjid:

  The USUBJID as character.

## Value

The filtered object.

## Examples

``` r
filter_subject(examplinib_poc, subjects(examplinib_poc)[1, "USUBJID"])
#> ──────── SDTM data set summary ──────── 
#> Study 2023000022
#> 
#> An open-label single-arm Phase 2 study of examplinib in patients
#> 
#> Data disposition:
#>   DOMAIN  SUBJECTS  OBSERVATIONS  
#>   dm      1         1             
#>   vs      1         2             
#>   ex      1         3             
#>   pc      1         44            
#>   lb      1         1             
#>   ts      0         0             
#>   pp      1         36
#> 
#> Arms:
#>   ACTARMCD   ACTARM                
#>   TREATMENT  Single Arm Treatment
#> 
#> Treatments:
#>   EXAMPLINIB
#> 
#> PK sample specimens:
#>   PLASMA
#> 
#> PK analytes:
#>   PCTEST      PCTESTCD    
#>   RS2023      RS2023      
#>   RS2023487A  RS2023487A
#> 
#> Hash: 7f166d3b06c5f65f1d3cc92cdbef0262
#> Last DTC: 2001-03-26 13:05:00
#> 
filter_subject(examplinib_poc_nif,
subjects(examplinib_poc_nif)[1, "USUBJID"])
#> ──────── NONMEM Input Format (NIF) data ────────
#> 44 observations from 1 subject across 1 study 
#> 
#> # A tibble: 114 × 29
#>      REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT
#>  * <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl>
#>  1     1     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  2     2     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  3     3     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  4     4     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  5     5     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  6     6     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  7     7     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  8     8     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>  9     9     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#> 10    10     1 2023000022 20230000221010001    81     0 WHITE   180.   93.9
#>      BMI DTC                  TIME NTIME  TAFD   TAD  EVID   AMT ANALYTE   
#>  * <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>     
#>  1  28.8 2001-01-07 09:42:00 0       0   0     0         1   500 RS2023    
#>  2  28.8 2001-01-07 09:42:00 0       0   0     0         0     0 RS2023    
#>  3  28.8 2001-01-07 09:42:00 0       0   0     0         0     0 RS2023487A
#>  4  28.8 2001-01-07 10:34:00 0.867   0.5 0.867 0.867     0     0 RS2023    
#>  5  28.8 2001-01-07 10:34:00 0.867   0.5 0.867 0.867     0     0 RS2023487A
#>  6  28.8 2001-01-07 11:02:00 1.33    1   1.33  1.33      0     0 RS2023    
#>  7  28.8 2001-01-07 11:02:00 1.33    1   1.33  1.33      0     0 RS2023487A
#>  8  28.8 2001-01-07 11:32:00 1.83    1.5 1.83  1.83      0     0 RS2023    
#>  9  28.8 2001-01-07 11:32:00 1.83    1.5 1.83  1.83      0     0 RS2023487A
#> 10  28.8 2001-01-07 11:59:00 2.28    2   2.28  2.28      0     0 RS2023    
#>      CMT PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD  IMPUTATION    DV BL_CREAT
#>  * <dbl> <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>     <chr>      <dbl>    <dbl>
#>  1     1 RS2023     1 FALSE        500     1 TREATMENT ""           NA      86.5
#>  2     2 RS2023     1 FALSE        500     0 TREATMENT ""            0      86.5
#>  3     3 RS2023     1 FALSE        500     0 TREATMENT ""            0      86.5
#>  4     2 RS2023     1 FALSE        500     0 TREATMENT ""          615.     86.5
#>  5     3 RS2023     1 FALSE        500     0 TREATMENT ""          120.     86.5
#>  6     2 RS2023     1 FALSE        500     0 TREATMENT ""         1842.     86.5
#>  7     3 RS2023     1 FALSE        500     0 TREATMENT ""          604.     86.5
#>  8     2 RS2023     1 FALSE        500     0 TREATMENT ""         2563.     86.5
#>  9     3 RS2023     1 FALSE        500     0 TREATMENT ""         1375.     86.5
#> 10     2 RS2023     1 FALSE        500     0 TREATMENT ""         2995.     86.5
#>    BL_CRCL
#>  *   <dbl>
#>  1    78.7
#>  2    78.7
#>  3    78.7
#>  4    78.7
#>  5    78.7
#>  6    78.7
#>  7    78.7
#>  8    78.7
#>  9    78.7
#> 10    78.7
#> # ℹ 104 more rows
```
