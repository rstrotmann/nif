# Subset nif to rows with DTC before the last individual or global observation

Subset nif to rows with DTC before the last individual or global
observation

## Usage

``` r
limit(obj, individual = TRUE, keep_no_obs_sbs = FALSE)
```

## Arguments

- obj:

  A nif object.

- individual:

  Apply by ID, as logical.

- keep_no_obs_sbs:

  Retain subjects without observations.

## Value

A nif object.

## Examples

``` r
limit(examplinib_poc_nif)
#> ----- NONMEM Input Format (NIF) data -----
#> 1344 observations from 80 subjects across 1 study 
#> Analytes: RS2023 and RS2023487A 
#> 46 males (57.5%), 34 females (42.5%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL 
#> 
#> Hash: 204faa818c1d8276d5fd76dd38f835ca
#> 
#> Data (selected columns):
#>   ID   NTIME   TIME    TAD     ANALYTE      EVID   CMT   AMT   DOSE   DV         
#>   1    0       0       0       RS2023       1      1     500   500    NA         
#>   1    0       0       0       RS2023       0      2     0     500    0          
#>   1    0       0       0       RS2023487A   0      3     0     500    0          
#>   1    0.5     0.867   0.867   RS2023       0      2     0     500    615.055    
#>   1    0.5     0.867   0.867   RS2023487A   0      3     0     500    120.161    
#>   1    1       1.333   1.333   RS2023       0      2     0     500    1841.724   
#>   1    1       1.333   1.333   RS2023487A   0      3     0     500    604.224    
#>   1    1.5     1.833   1.833   RS2023       0      2     0     500    2563.4     
#>   1    1.5     1.833   1.833   RS2023487A   0      3     0     500    1374.883   
#>   1    2       2.283   2.283   RS2023       0      2     0     500    2995.306    
#> 2022 more rows
```
