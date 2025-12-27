# Recode RACE columns in nif object

For some purposes, e.g., NONMEM-based modeling, numerical values are
expected in the RACE field. This function recodes RACE based on the
following associations:

## Usage

``` r
recode_race(obj, coding_table = NULL, silent = NULL)
```

## Arguments

- obj:

  A nif object with RACE as character field.

- coding_table:

  A data frame with the columns RACE and RACEN. Uses default coding, if
  NULL.

- silent:

  Suppress messages, defaults to nif_option setting, if NULL.

## Value

A nif object with the original RACE replaced by the numerical race code.

## Examples

``` r
nif::race_coding
#> # A tibble: 8 × 3
#>   RACEN RACE                                      LABEL  
#>   <dbl> <chr>                                     <chr>  
#> 1     0 WHITE                                     White  
#> 2     1 ASIAN                                     Asian  
#> 3     2 BLACK OR AFRICAN AMERICAN                 Black  
#> 4     3 AMERICAN INDIAN OR ALASKA NATIVE          Native 
#> 5     4 NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER Pacific
#> 6     5 NOT REPORTED                              NR     
#> 7     6 UNKNOWN                                   Unknown
#> 8     7 OTHER                                     Other  
recode_race(examplinib_sad_nif)
#> ----- NONMEM Input Format (NIF) data -----
#> 816 observations from 48 subjects across 1 study 
#> Analytes: RS2023 
#> 48 males (100%), 0 females (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL 
#> 
#> Hash: d1aff48457b2d945efda66883fbc9da9
#> 
#> Data (selected columns):
#>   ID   NTIME   TIME   TAD   ANALYTE   EVID   CMT   AMT   DOSE   DV       
#>   1    0       0      0     RS2023    1      1     5     5      NA       
#>   1    0       0      0     RS2023    0      2     0     5      0        
#>   1    0.5     0.5    0.5   RS2023    0      2     0     5      40.785   
#>   1    1       1      1     RS2023    0      2     0     5      48.553   
#>   1    1.5     1.5    1.5   RS2023    0      2     0     5      44.039   
#>   1    2       2      2     RS2023    0      2     0     5      34.073   
#>   1    3       3      3     RS2023    0      2     0     5      19.399   
#>   1    4       4      4     RS2023    0      2     0     5      10.533   
#>   1    6       6      6     RS2023    0      2     0     5      2.953    
#>   1    8       8      8     RS2023    0      2     0     5      1.034     
#> 854 more rows
```
