# Add baseline renal function class

If baseline creatinine clearance (BL_CRCL) is not included in the input,
it will be calculated first.

## Usage

``` r
add_bl_renal(obj, method = egfr_cg, molar = FALSE)
```

## Arguments

- obj:

  A NIF object.

- method:

  The function to calculate eGFR (CrCL) from serum creatinine.

- molar:

  Use molar concentrations. Currently either: egfr_mdrd, egfr_cg or
  egfr_raynaud

## Value

A NIF object.

## Examples

``` r
head(add_bl_renal(examplinib_poc_nif), 5)
#> ──────── NONMEM Input Format (NIF) data ────────
#> 4 observations from 1 subject across 1 study
#> 
#> # A tibble: 5 × 30
#>     REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT   BMI
#>   <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl> <dbl>
#> 1     1     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 2     2     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 3     3     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 4     4     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 5     5     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#>   DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV ANALYTE   
#>   <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>     
#> 1 2001-01-05 10:25:00   0     0     0     0       1   500     1   NA  RS2023    
#> 2 2001-01-05 10:25:00   0     0     0     0       0     0     2    0  RS2023    
#> 3 2001-01-05 10:25:00   0     0     0     0       0     0     3    0  RS2023487A
#> 4 2001-01-05 11:31:00   1.1   0.5   1.1   1.1     0     0     2  553. RS2023    
#> 5 2001-01-05 11:31:00   1.1   0.5   1.1   1.1     0     0     3  121. RS2023487A
#>   PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD  IMPUTATION                
#>   <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>     <chr>                     
#> 1 RS2023     1 FALSE        500     1 TREATMENT "time copied from EXSTDTC"
#> 2 RS2023     1 FALSE        500     0 TREATMENT ""                        
#> 3 RS2023     1 FALSE        500     0 TREATMENT ""                        
#> 4 RS2023     1 FALSE        500     0 TREATMENT ""                        
#> 5 RS2023     1 FALSE        500     0 TREATMENT ""                        
#>   BL_CREAT BL_CRCL BL_RENAL
#>      <dbl>   <dbl> <fct>   
#> 1     58.8    166. normal  
#> 2     58.8    166. normal  
#> 3     58.8    166. normal  
#> 4     58.8    166. normal  
#> 5     58.8    166. normal  
```
