# SDTM summary

This function returns a named list of properties of the SDTM object:

## Usage

``` r
# S3 method for class 'sdtm'
summary(object, ...)
```

## Arguments

- object:

  A SDTM object.

- ...:

  Further parameters.

## Value

A sdtm_summary object.

## Details

- `study` The study identifier as character.

- `subjects` The USUBJIDs as character.

- `domains` The included domains by their SDTM code and the number of
  respective unique USUBJID, as data frame,

- `treatments` The unique `EXTRT` as character.

- `arms` The unique `ACTAMCD` and `ACTARM` as data frame.

- `doses` The unique `EXTRT` and `EXDOSE` as data frame.

- `specimens` The unique `PCSPEC` as character.

- `analytes` The unique `PCTEST` and `PCTESTCD` as data frame.

- `pc_timepoints` The unique `PCTPT` and `PCTPTNUM` as data frame.

- `analyte_mapping` The analyte mapping as data frame.

- `metabolite_mapping` The metabolite mapping as data frame.

- `time_mapping` The time mapping as data frame.

## Examples

``` r
summary(examplinib_poc)
#> -------- SDTM data set summary -------- 
#> Study 2023000022 
#> 
#> An open-label single-arm Phase 2 study of examplinib in patients
#> 
#> Data disposition
#>   DOMAIN   SUBJECTS   OBSERVATIONS   
#>   dm       103        103            
#>   vs       103        206            
#>   ex       80         468            
#>   pc       80         1344           
#>   lb       103        103            
#>   ts       0          0              
#>   pp       13         432             
#> 
#> Arms (DM):
#>   ACTARMCD    ACTARM                 
#>   SCRNFAIL    Screen Faillure        
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
#> Hash: 6e3a2050709bc485b93bd185bb28c988
#> Last DTC: 2001-07-14 10:53:00
```
