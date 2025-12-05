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
#> -------- SDTM data set summary -------- 
#> Study 2023000022 
#> 
#> Data disposition
#>   DOMAIN   SUBJECTS   OBSERVATIONS   
#>   dm       1          1              
#>   vs       1          2              
#>   ex       1          7              
#>   pc       1          44             
#>   lb       1          1              
#>   pp       1          18             
#> 
#> Arms (DM):
#>   ACTARMCD    ACTARM                 
#>   TREATMENT   Single Arm Treatment   
#> 
#> Treatments (EX):
#>   EXAMPLINIB
#> 
#> PK sample specimens (PC):
#>   PLASMA
#> 
#> PK analytes (PC):
#>   PCTEST       PCTESTCD     
#>   RS2023       RS2023       
#>   RS2023487A   RS2023487A    
#> 
#> Hash: 3a6ef69913398b9696c57980b2aa0058
#> Last DTC: 2001-03-14 10:09:00
filter_subject(examplinib_poc_nif, subjects(examplinib_poc_nif)[1, "USUBJID"])
#> ----- NONMEM Input Format (NIF) data -----
#> 44 observations from 1 subject across 1 study 
#> Analytes: RS2023 and RS2023487A 
#> 0 males (0%), 1 females (100%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL 
#> 
#> Hash: 3f41890ea1484c28b5397c380bee0a29
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
#> 125 more rows
```
