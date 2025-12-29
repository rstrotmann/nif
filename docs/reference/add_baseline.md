# Add a baseline covariate

Add a column to a [nif](nif.md) object that represents the baseline
value for a subject-level covariate.

## Usage

``` r
add_baseline(
  nif,
  sdtm,
  domain,
  testcd,
  name = NULL,
  dv_field = NULL,
  testcd_field = NULL,
  observation_filter = "TRUE",
  cat = NULL,
  scat = NULL,
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

- dv_field:

  The name of the DV field as character.

- testcd_field:

  The name of the TESTCD field. defaults to xxTESTCD with xx the domain
  name, as character.

- observation_filter:

  A filter term for the `domain`, as character. Note: if the filter term
  includes date comparisons, make sure to represent the date as,
  datetime object e.g.,
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html).

- cat:

  xxCAT filter to apply, as character.

- scat:

  xxSCAT filter to apply, as character.

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

## Details

The source of the baseline covariate is specified by the `domain` and
`testcd` arguments. The baseline condition is defined by the
`baseline_filter` argument. If none is provided, the baseline filter
defaults to `xxBLFL == "Y"` where 'xx' is the domain code. In addition,
a custom `observation_filter` can be defined to further specify the
observation. This may be necessary, when observations defined by the
`testcd` alone are ambiguous, e.g., when for pharmacokinetic baseline
observations, both BLOOD and URINE observations are included in the PC
domain data.

The name of the baseline column defaults to the 'testcd', prefixed with
'BL\_', e.g., BL_WEIGHT. A specific name can be defined by the `name`
argument. Note that baseline WEIGHT, HEIGHT and BMI (if applicable) are
automatically inclucded during the generation of a nif object.

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
