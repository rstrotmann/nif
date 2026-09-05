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
#> ! Missing fields: PCLLOQ and PCSTRESC. LLOQ imputation cannot be done.
#> ℹ baseline_filter for BL_CREAT set to LBBLFL == 'Y'
#> ℹ Imputation model 'imputation_rules_standard' applied to administration of EXAMPLINIB
#> ℹ A global cut-off-date of 2001-03-03 10:28:00 was automatically assigned!
#> ℹ Imputation model 'imputation_rules_standard' applied to RS2023 observations
#> ! Compartment 2 is already assigned. Is this intended?
#> ! Missing fields: PCLLOQ and PCSTRESC. LLOQ imputation cannot be done.
#> ℹ baseline_filter for BL_CREAT set to LBBLFL == 'Y'
#> ℹ Imputation model 'imputation_rules_standard' applied to administration of EXAMPLINIB
#> ℹ A global cut-off-date of 2001-08-15 09:01:00 was automatically assigned!
#> ℹ Imputation model 'imputation_rules_standard' applied to RS2023 observations
#> ! Compartment 2 is already assigned. Is this intended?
#> ! Missing fields: PCLLOQ and PCSTRESC. LLOQ imputation cannot be done.
#> ℹ baseline_filter for BL_CREAT set to LBBLFL == 'Y'

nif %>%
  summary()
#> ──────── NONMEM Input Format (NIF) data summary ────────
#> Data from 148 subjects across 3studies:
#>   STUDYID     N   
#>   2023000001  48  
#>   2023000022  80  
#>   2023000400  20
#> 
#> Sex distribution:
#>   SEX     N    percent  
#>   male    112  75.7     
#>   female  36   24.3
#> 
#> Renal impairment class:
#>   CLASS     N   percent  
#>   normal    81  54.7     
#>   mild      56  37.8     
#>   moderate  11  7.4      
#>   severe    0   0
#> 
#> Treatments: RS2023
#> 
#> Analytes: RS2023
#> 
#> Subjects per dose level:
#>   DL           n    
#>   5-RS2023     3    
#>   10-RS2023    3    
#>   20-RS2023    3    
#>   50-RS2023    3    
#>   100-RS2023   6    
#>   200-RS2023   3    
#>   500-RS2023   118  
#>   800-RS2023   6    
#>   1000-RS2023  3
#> 
#> 2168 observations:
#>   CMT  ANALYTE  n     
#>   2    RS2023   2168
#> 
#> Observations by NTIME:
#>   NTIME  RS2023  
#>   0      248     
#>   0.5    112     
#>   1      112     
#>   1.5    248     
#>   2      112     
#>   3      112     
#>   4      248     
#>   6      112     
#>   8      112     
#>   10     112     
#>   (7 more rows)
#> 
#> Subjects with dose reductions:
#>   treatment  n   
#>   RS2023     22
#> 
#> Treatment duration overview:
#>   PARENT  min  max  mean  median  
#>   RS2023  1    101  42.1  61
#> 
#> NIF version: 0.66.1
#> Creation date: 2026-09-05
#> Hash: b97d100759eb983a807af91c99c0221c
#> Last DTC: 2001-08-15 09:01:00
```
