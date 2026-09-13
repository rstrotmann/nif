# Calculate change from baseline

Derive the individual baseline value (DVBL field) and the change from
baseline (DVCFB field) for an analyte.

## Usage

``` r
derive_cfb(
  obj,
  analyte = NULL,
  baseline_filter = "TIME <= 0",
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

A nif object with the DVBL and DVCFB fields added for the specified
analyte.

## Examples

``` r
head(derive_cfb(examplinib_sad_nif))
#> ──────── NONMEM Input Format (NIF) data ────────
#> 5 observations from 1 subject across 1 study
#> 
#> # A tibble: 6 × 31
#>     REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT   BMI
#>   <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl> <dbl>
#> 1     1     1 2023000001 20230000011010001    43     0 WHITE   187.     77  21.9
#> 2     2     1 2023000001 20230000011010001    43     0 WHITE   187.     77  21.9
#> 3     3     1 2023000001 20230000011010001    43     0 WHITE   187.     77  21.9
#> 4     4     1 2023000001 20230000011010001    43     0 WHITE   187.     77  21.9
#> 5     5     1 2023000001 20230000011010001    43     0 WHITE   187.     77  21.9
#> 6     6     1 2023000001 20230000011010001    43     0 WHITE   187.     77  21.9
#>   DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV ANALYTE
#>   <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>  
#> 1 2000-12-31 10:18:00   0     0     0     0       1     5     1  NA   RS2023 
#> 2 2000-12-31 10:18:00   0     0     0     0       0     0     2   0   RS2023 
#> 3 2000-12-31 10:48:00   0.5   0.5   0.5   0.5     0     0     2  44.5 RS2023 
#> 4 2000-12-31 11:18:00   1     1     1     1       0     0     2  56.2 RS2023 
#> 5 2000-12-31 11:48:00   1.5   1.5   1.5   1.5     0     0     2  50.4 RS2023 
#> 6 2000-12-31 12:18:00   2     2     2     2       0     0     2  40.7 RS2023 
#>   PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD IMPUTATION                
#>   <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>    <chr>                     
#> 1 RS2023     1 FALSE          5     1 C1       "time copied from EXSTDTC"
#> 2 RS2023     1 FALSE          5     0 C1       ""                        
#> 3 RS2023     1 FALSE          5     0 C1       ""                        
#> 4 RS2023     1 FALSE          5     0 C1       ""                        
#> 5 RS2023     1 FALSE          5     0 C1       ""                        
#> 6 RS2023     1 FALSE          5     0 C1       ""                        
#>   BL_CREAT BL_CRCL  DVBL DVCFB
#>      <dbl>   <dbl> <dbl> <dbl>
#> 1     89.0    103.     0  NA  
#> 2     89.0    103.     0   0  
#> 3     89.0    103.     0  44.5
#> 4     89.0    103.     0  56.2
#> 5     89.0    103.     0  50.4
#> 6     89.0    103.     0  40.7
```
