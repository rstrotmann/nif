# Baseline details for specific subjects

Baseline details for specific subjects

## Usage

``` r
# S3 method for class 'sdtm'
subject_info(obj, id)
```

## Arguments

- obj:

  The object, either an SDTM or NIF object.

- id:

  The ID or USUBJID as numeric or character.

## Examples

``` r
subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
#>          [,1]             
#> SITEID   102              
#> SUBJID   1020001          
#> ACTARM   Screen Failure   
#> ACTARMCD SCRNFAIL         
#> RFICDTC  2000-12-21T10:30 
#> RFSTDTC  NA               
#> RFXSTDTC NA               
#> STUDYID  2023000400       
#> USUBJID  20230004001020001
#> SEX      F                
#> AGE      42               
#> AGEU     YEARS            
#> COUNTRY  DEU              
#> DOMAIN   DM               
#> ARM      Screen Failure   
#> ARMCD    SCRNFAIL         
#> RACE     WHITE            
#> ETHNIC                    
#> RFENDTC  NA               
```
