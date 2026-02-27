# Baseline details for specific subjects

Baseline details for specific subjects

## Usage

``` r
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
subject_info(examplinib_poc_nif, 1)
#>          [,1]              
#> USUBJID  20230000221010001 
#> ID       1                 
#> SEX      0                 
#> AGE      81                
#> RACE     WHITE             
#> WEIGHT   93.9              
#> HEIGHT   180.5             
#> BMI      28.8211416425595  
#> ACTARMCD TREATMENT         
#> BL_CREAT 86.4655906117736  
#> BL_CRCL  78.6672665801518  
#> ANALYTE  RS2023, RS2023487A
#> IMP      RS2023            
head(subject_info(examplinib_poc_nif, 1)$administrations)
#>   ANALYTE TIME TRTDY
#> 1  RS2023    0     1
#> 2  RS2023   24     2
#> 3  RS2023   48     3
#> 4  RS2023   72     4
#> 5  RS2023   96     5
#> 6  RS2023  120     6
```
