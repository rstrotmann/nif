# Attach baseline covariate from SDTM domain

Baseline covariates, as specified by the 'testcd' field, can come from
any SDTM domain. By default, the baseline value is identified by
`xxBLFL == "Y"` in the respective SDTM domain. Alternatively, a custom
observation filter can be defined. The name of the baseline covariate is
the 'testcd', prefixed with 'BL\_'.

## Usage

``` r
add_baseline(
  nif,
  sdtm,
  domain,
  testcd,
  name = NULL,
  DV_field = NULL,
  TESTCD_field = NULL,
  observation_filter = "TRUE",
  baseline_filter = NULL,
  coding_table = NULL,
  summary_function = mean,
  silent = NULL
)
```

## Arguments

- nif:

  A nif object.

- sdtm:

  A sdtm object.

- domain:

  The domain as character.

- testcd:

  The covariate variable name as character.

- name:

  The column label, as character.

- DV_field:

  The name of the DV field as character.

- TESTCD_field:

  The name of the TESTCD field. defaults to xxTESTCD with xx the domain
  name, as character.

- observation_filter:

  A filter term for the `domain`, as character. Note: if the filter term
  includes date comparisons, make sure to represent the date as,
  datetime object e.g.,
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html).

- baseline_filter:

  A filter term to identify the baseline condition. within the `domain`.
  Defaults to either "xxBLFL == 'Y'" or "xxLOBXFL == 'Y'" (with xx the
  domain code), whichever is found first in the domain data.

- coding_table:

  A recoding table as data frame, or NULL. If present, the table needs
  to have a field that matches a column in the domain, and a field 'DV'
  that provides the re-coded value.

- summary_function:

  The summary function to summarize multiple baseline values. Defaults
  to `mean`.

- silent:

  Suppress messages, defaults to nif_option setting if NULL.

## Value

A nif object.

## Examples

``` r
add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT")
#> baseline_filter for BL_WEIGHT set to VSBLFL == 'Y'
#> ----- NONMEM Input Format (NIF) data -----
#> 816 observations from 48 subjects across 1 study 
#> Analytes: RS2023 
#> 48 males (100%), 0 females (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL, BL_WEIGHT 
#> 
#> Hash: 6b8f5ccdcf4a531bb0ca624e6e8a5dc5
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
add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT",
  baseline_filter = "VSBLFL == 'Y'"
)
#> ----- NONMEM Input Format (NIF) data -----
#> 816 observations from 48 subjects across 1 study 
#> Analytes: RS2023 
#> 48 males (100%), 0 females (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL, BL_WEIGHT 
#> 
#> Hash: 6b8f5ccdcf4a531bb0ca624e6e8a5dc5
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
