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
#> 47 males (58.8%), 33 females (41.2%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL 
#> 
#> Hash: b8fca46bbaf36cbd42356014cae17bba
#> 
#> Data (selected columns):
#>   ID   NTIME   TIME    TAD     ANALYTE      EVID   CMT   AMT   DOSE   DV         
#>   1    0       0       0       RS2023       1      1     500   500    NA         
#>   1    0       0       0       RS2023       0      2     0     500    0          
#>   1    0       0       0       RS2023487A   0      3     0     500    0          
#>   1    0.5     1       1       RS2023       0      2     0     500    636.683    
#>   1    0.5     1       1       RS2023487A   0      3     0     500    135.126    
#>   1    1       1.467   1.467   RS2023       0      2     0     500    1844.823   
#>   1    1       1.467   1.467   RS2023487A   0      3     0     500    677.321    
#>   1    1.5     2       2       RS2023       0      2     0     500    2773.308   
#>   1    1.5     2       2       RS2023487A   0      3     0     500    1547.253   
#>   1    2       2.467   2.467   RS2023       0      2     0     500    2539.54     
#> 2024 more rows
```
