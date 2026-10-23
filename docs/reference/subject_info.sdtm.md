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
#> SUBJID    1020001            
#> USUBJID   20230004001020001  
#> ARM       Fasted - Fed       
#> ARMCD     AB                 
#> ACTARM    Fasted - Fed       
#> ACTARMCD  AB                 
#> SITEID    102                
#> COUNTRY   DEU                
#> RFSTDTC   2001-01-01T09:49   
#> RFENDTC   2001-01-14T09:49   
#> SEX       M                  
#> AGE       23                 
#> RACE      WHITE              
#> ETHNIC                       
subject_info(examplinib_fe, subjects(examplinib_fe)[1:3, "USUBJID"])
#> ──────── Subject information ────────
#> SUBJID    1020001            1040001            1040002            
#> USUBJID   20230004001020001  20230004001040001  20230004001040002  
#> ARM       Fasted - Fed       Fed - Fasted       Fed - Fasted       
#> ARMCD     AB                 BA                 BA                 
#> ACTARM    Fasted - Fed       Fed - Fasted       Fed - Fasted       
#> ACTARMCD  AB                 BA                 BA                 
#> SITEID    102                104                104                
#> COUNTRY   DEU                DEU                DEU                
#> RFSTDTC   2001-01-01T09:49   2001-01-03T11:25   2001-01-07T10:17   
#> RFENDTC   2001-01-14T09:49   2001-01-16T11:25   2001-01-20T10:17   
#> SEX       M                  F                  F                  
#> AGE       23                 47                 53                 
#> RACE      WHITE              WHITE              WHITE              
#> ETHNIC                                                             
```
