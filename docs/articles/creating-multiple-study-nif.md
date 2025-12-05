# creating-multiple-study-nif

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
nif <- new_nif() %>% 
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
#> baseline_filter for BL_CREAT set to LBBLFL == 'Y'
#> Warning in add_observation(., examplinib_fe, "pc", "RS2023", cmt = 2):
#> Compartment 2 is already assigned!
#> baseline_filter for BL_CREAT set to LBBLFL == 'Y'
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
#>   male     108   73        
#>   female   40    27        
#> 
#> Renal impairment class:
#>   CLASS      N    percent   
#>   normal     89   60.1      
#>   mild       44   29.7      
#>   moderate   15   10.1      
#>   severe     0    0         
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
#> Subjects with dose reductions
#>   RS2023   
#>   2        
#> 
#> Treatment duration overview:
#>   PARENT   min   max   mean   median   
#>   RS2023   1     99    43     58.5
```
