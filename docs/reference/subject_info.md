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
#> SUBJID    1020001            
#> USUBJID   20230004001020001  
#> SITEID    102                
#> COUNTRY   DEU                
#> ARM       Screen Failure     
#> ARMCD     SCRNFAIL           
#> ACTARM    Screen Failure     
#> ACTARMCD  SCRNFAIL           
#> RFSTDTC   NA                 
#> RFENDTC   NA                 
#> SEX       F                  
#> AGE       42                 
#> RACE      WHITE              
#> ETHNIC                       
subject_info(examplinib_poc_nif, 1)
#>          [,1]              
#> USUBJID  20230000221010001 
#> ID       1                 
#> SEX      1                 
#> AGE      49                
#> RACE     WHITE             
#> WEIGHT   102.6             
#> HEIGHT   180.4             
#> BMI      31.5263936755473  
#> ACTARMCD TREATMENT         
#> BL_CREAT 58.8418509311415  
#> BL_CRCL  165.592675040125  
#> ANALYTE  RS2023, RS2023487A
#> IMP      RS2023            
head(subject_info(examplinib_poc_nif, 1)$administrations)
#>   ANALYTE TIME TRTDY
#> 1  RS2023    0     1
#> 2  RS2023   24     2
#> 3  RS2023   48     3
#> 4  RS2023   72     4
#> 5  RS2023   96     5
#> 6  RS2023  144     7
```
