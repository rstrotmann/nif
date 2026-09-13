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
#> ──────── NONMEM Input Format (NIF) data ────────
#> 5 observations from 1 subject across 1 study
#> 
#> # A tibble: 6 × 29
#>     REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT   BMI
#>   <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl> <dbl>
#> 1     1     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 2     2     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 3     3     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 4     4     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 5     5     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#> 6     6     1 2023000022 20230000221010001    49     1 WHITE   180.   103.  31.5
#>   DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV ANALYTE   
#>   <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>     
#> 1 2001-01-05 10:25:00  0      0    0     0        1   500     1   NA  RS2023    
#> 2 2001-01-05 10:25:00  0      0    0     0        0     0     2    0  RS2023    
#> 3 2001-01-05 10:25:00  0      0    0     0        0     0     3    0  RS2023487A
#> 4 2001-01-05 11:31:00  1.1    0.5  1.1   1.1      0     0     2  553. RS2023    
#> 5 2001-01-05 11:31:00  1.1    0.5  1.1   1.1      0     0     3  121. RS2023487A
#> 6 2001-01-05 12:00:00  1.58   1    1.58  1.58     0     0     2 1484. RS2023    
#>   PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD  IMPUTATION                
#>   <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>     <chr>                     
#> 1 RS2023     1 FALSE        500     1 TREATMENT "time copied from EXSTDTC"
#> 2 RS2023     1 FALSE        500     0 TREATMENT ""                        
#> 3 RS2023     1 FALSE        500     0 TREATMENT ""                        
#> 4 RS2023     1 FALSE        500     0 TREATMENT ""                        
#> 5 RS2023     1 FALSE        500     0 TREATMENT ""                        
#> 6 RS2023     1 FALSE        500     0 TREATMENT ""                        
#>   BL_CREAT BL_CRCL
#>      <dbl>   <dbl>
#> 1     58.8    166.
#> 2     58.8    166.
#> 3     58.8    166.
#> 4     58.8    166.
#> 5     58.8    166.
#> 6     58.8    166.
```
