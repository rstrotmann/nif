# Baseline details for specific subjects

Baseline details for specific subjects

## Usage

``` r
# S3 method for class 'sdtm'
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
subject_info(examplinib_fe, subjects(examplinib_fe)[1:3, "USUBJID"])
#> ──────── Subject information ────────
#> SUBJID    1040001            1020001            1040002            
#> USUBJID   20230004001040001  20230004001020001  20230004001040002  
#> ARM       Fed - Fasted       Fed - Fasted       Fasted - Fed       
#> ARMCD     BA                 BA                 AB                 
#> ACTARM    Fed - Fasted       Fed - Fasted       Fasted - Fed       
#> ACTARMCD  BA                 BA                 AB                 
#> SITEID    104                102                104                
#> COUNTRY   DEU                DEU                DEU                
#> RFSTDTC   2001-01-02T09:10   2000-12-31T10:22   2001-01-02T10:26   
#> RFENDTC   2001-01-15T09:10   2001-01-13T10:22   2001-01-15T10:26   
#> SEX       M                  F                  M                  
#> AGE       22                 38                 34                 
#> RACE      WHITE              WHITE              WHITE              
#> ETHNIC                                                             
```
