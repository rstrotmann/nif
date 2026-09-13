# Baseline details for specific subjects

Baseline details for specific subjects

## Usage

``` r
subject_info(obj, id, ...)
```

## Arguments

- obj:

  The object, either an SDTM or NIF object.

- id:

  The ID or USUBJID as numeric or character.

- ...:

  Further arguments.

## Examples

``` r
subject_info(examplinib_fe, subjects(examplinib_fe)[1, "USUBJID"])
#> ──────── Subject information ────────
#> SUBJID    1020001            
#> USUBJID   20230004001020001  
#> ARM       Screen Failure     
#> ARMCD     SCRNFAIL           
#> ACTARM    Screen Failure     
#> ACTARMCD  SCRNFAIL           
#> SITEID    102                
#> COUNTRY   DEU                
#> RFSTDTC   NA                 
#> RFENDTC   NA                 
#> SEX       F                  
#> AGE       42                 
#> RACE      WHITE              
#> ETHNIC                       
subject_info(examplinib_poc_nif, 1)
#> ──────── Subject information ────────
#> ID        1                  
#> USUBJID   20230000221010001  
#> ACTARMCD  TREATMENT          
#> SEX       1                  
#> AGE       49                 
#> RACE      WHITE              
#> WEIGHT    102.6              
#> HEIGHT    180.4              
#> BMI       31.5               
#> BL_CREAT  58.8               
#> BL_CRCL   165.6              
```
