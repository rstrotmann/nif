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
#> An open-label single-arm Phase 2 study of examplinib in patients
#> 
#> Data disposition
#>   DOMAIN   SUBJECTS   OBSERVATIONS   
#>   dm       1          1              
#>   vs       1          2              
#>   ex       1          3              
#>   pc       1          44             
#>   lb       1          1              
#>   ts       0          0              
#>   pp       1          36              
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
#> Hash: 93dc2ba6e83c56fb5b03f2007f5493df
#> Last DTC: 2001-03-26 13:05:00
filter_subject(examplinib_poc_nif,
subjects(examplinib_poc_nif)[1, "USUBJID"])
#> ----- NONMEM Input Format (NIF) data -----
#> 44 observations from 1 subject across 1 study 
#> Analytes: RS2023 and RS2023487A 
#> 1 male (100%), 0 female (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL 
#> 
#> Hash: 41db6eb28a6c92fda185ad92d25887e9
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
#> 104 more rows
```
