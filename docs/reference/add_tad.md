# Add time-after-dose (TAD) field

This function adds a time-after-dose (TAD) field to a NIF object. TAD
represents the time elapsed since the most recent administration of the
parent compound. For observations before the first dose, TAD will be
negative.

## Usage

``` r
add_tad(nif)
```

## Arguments

- nif:

  A NIF object.

## Value

A NIF object with an added TAD column.

## Examples

``` r
# Add TAD to a NIF object
add_tad(examplinib_poc_nif)
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
#> Hash: 7f4823f1ab95211e7cbf45cf78297804
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
#> 7612 more rows
```
