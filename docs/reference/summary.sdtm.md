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

- `specimems` The unique `PCSPEC` as character.

- `analytes` The unique `PCTEST` and `PCTESTCD` as data frame.

- `pc_timepoints` The unique `PCTPT` and `PCTPTNUM` as data frame.

- `analyte_mapping` The analyte mapping as data frame (see
  [`add_analyte_mapping()`](add_analyte_mapping.md)).

- `metabolite_mapping` The metabolite mapping as data frame (see
  [`add_metabolite_mapping()`](add_metabolite_mapping.md)).

- `time_mapping` The time mapping as data frame (see
  [`add_time_mapping()`](add_time_mapping.md)).

## Examples

``` r
summary(examplinib_poc)
#> -------- SDTM data set summary -------- 
#> Study 2023000022 
#> 
#> Data disposition
#>   DOMAIN   SUBJECTS   OBSERVATIONS   
#>   dm       103        103            
#>   vs       103        206            
#>   ex       80         468            
#>   pc       80         1344           
#>   lb       103        103            
#>   pp       13         432            
#> 
#> Arms (DM):
#>   ACTARMCD    ACTARM                 
#>   TREATMENT   Single Arm Treatment   
#>   SCRNFAIL    Screen Faillure        
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
#> Hash: a839c3b7866a6c04a3007b1d7b877345
#> Last DTC: 2001-07-14 10:53:00
```
