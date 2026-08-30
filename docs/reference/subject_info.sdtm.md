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
subject_info(examplinib_fe, subjects(examplinib_fe)[1:3, "USUBJID"])
#> SUBJID    1020001            1040001            1050001                    
#> USUBJID   20230004001020001  20230004001040001  20230004001050001          
#> SITEID    102                104                105                        
#> COUNTRY   DEU                DEU                DEU                        
#> ARM       Screen Failure     Fed - Fasted       Fasted - Fed               
#> ARMCD     SCRNFAIL           BA                 AB                         
#> ACTARM    Screen Failure     Fed - Fasted       Fasted - Fed               
#> ACTARMCD  SCRNFAIL           BA                 AB                         
#> RFSTDTC   NA                 2001-01-02T09:47   2001-01-05T10:05           
#> RFENDTC   NA                 2001-01-15T09:47   2001-01-18T10:05           
#> SEX       F                  M                  M                          
#> AGE       42                 28                 34                         
#> RACE      WHITE              WHITE              BLACK OR AFRICAN AMERICAN  
#> ETHNIC                                                                     
```
