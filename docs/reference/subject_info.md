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
#> SUBJID    1040001            
#> USUBJID   20230004001040001  
#> ARM       Fed - Fasted       
#> ARMCD     BA                 
#> ACTARM    Fed - Fasted       
#> ACTARMCD  BA                 
#> SITEID    104                
#> COUNTRY   DEU                
#> RFSTDTC   2001-01-02T09:10   
#> RFENDTC   2001-01-15T09:10   
#> SEX       M                  
#> AGE       22                 
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
