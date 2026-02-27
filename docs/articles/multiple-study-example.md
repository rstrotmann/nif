# More than one study in a NIF dataset

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(nif)
```

``` r
nif <- nif() %>%
  add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023") %>%
  add_observation(examplinib_sad, "pc", "RS2023", cmt = 2) %>%
  add_baseline(examplinib_sad, "lb", "CREAT") %>%
  add_administration(examplinib_fe, "EXAMPLINIB", analyte = "RS2023") %>%
  add_observation(examplinib_fe, "pc", "RS2023", cmt = 2) %>%
  add_baseline(examplinib_fe, "lb", "CREAT") %>%
  add_administration(examplinib_poc, "EXAMPLINIB", analyte = "RS2023") %>%
  add_observation(examplinib_poc, "pc", "RS2023", cmt = 2) %>%
  add_baseline(examplinib_poc, "lb", "CREAT") %>%
  add_bl_crcl() %>%
  add_bl_renal()
#> ℹ Imputation model 'imputation_rules_standard' applied to administration of EXAMPLINIB
#> ℹ A global cut-off-date of 2001-02-23 11:31:00 was automatically assigned!
#> ℹ Imputation model 'imputation_rules_standard' applied to RS2023 observations
#> baseline_filter for BL_CREAT set to LBBLFL == 'Y'
#> ℹ Imputation model 'imputation_rules_standard' applied to administration of EXAMPLINIB
#> ℹ A global cut-off-date of 2001-03-03 10:28:00 was automatically assigned!
#> ℹ Imputation model 'imputation_rules_standard' applied to RS2023 observations
#> Warning in add_observation(., examplinib_fe, "pc", "RS2023", cmt = 2):
#> Compartment 2 is already assigned!
#> baseline_filter for BL_CREAT set to LBBLFL == 'Y'
#> ℹ Imputation model 'imputation_rules_standard' applied to administration of EXAMPLINIB
#> ℹ A global cut-off-date of 2001-07-18 08:24:00 was automatically assigned!
#> ℹ Imputation model 'imputation_rules_standard' applied to RS2023 observations
#> Warning in add_observation(., examplinib_poc, "pc", "RS2023", cmt = 2):
#> Compartment 2 is already assigned!
#> baseline_filter for BL_CREAT set to LBBLFL == 'Y'

nif %>%
  summary()
#> ----- NONMEM Input Format (NIF) data summary -----
#> Data from 148 subjects across 3 studies:
#>   STUDYID      N    
#>   2023000001   48   
#>   2023000022   80   
#>   2023000400   20    
#> 
#> Sex distribution:
#>   SEX      N     percent   
#>   male     107   72.3      
#>   female   41    27.7       
#> 
#> Renal impairment class:
#>   CLASS      N    percent   
#>   normal     89   60.1      
#>   mild       48   32.4      
#>   moderate   10   6.8       
#>   severe     1    0.7        
#> 
#> Treatments:
#>   RS2023
#> 
#> Analytes:
#>   RS2023
#> 
#> Subjects per dose level:
#>   RS2023   N     
#>   5        3     
#>   10       3     
#>   20       3     
#>   50       3     
#>   100      6     
#>   200      3     
#>   500      118   
#>   800      6     
#>   1000     3      
#> 
#> 2168 observations:
#>   CMT   ANALYTE   N      
#>   2     RS2023    2168    
#> 
#> Observations by NTIME:
#>   NTIME   RS2023   
#>   0       248      
#>   0.5     112      
#>   1       112      
#>   1.5     248      
#>   2       112      
#>   3       112      
#>   4       248      
#>   6       112      
#>   8       112      
#>   10      112       
#>   (7 more rows)
#> 
#> Subjects with dose reductions
#>   RS2023   
#>   30        
#> 
#> Treatment duration overview:
#>   PARENT   min   max   mean   median   
#>   RS2023   1     97    40.2   58.5      
#> 
#> Hash: 25977a4c8479217861e2a2a88c8231ba
#> Last DTC: 2001-07-18 08:24:00
```
