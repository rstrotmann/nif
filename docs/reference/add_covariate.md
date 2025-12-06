# Attach time-varying covariate

A time-varying covariate is added as a new field with daily time
granularity and carried forward for missing entries. The name of the
covariate can be specified by 'covariate'. By default, it is set to the
'testcd' (without any prefix).

## Usage

``` r
add_covariate(
  nif,
  sdtm,
  domain,
  testcd,
  covariate = NULL,
  DTC_field = NULL,
  DV_field = NULL,
  TESTCD_field = NULL,
  observation_filter = "TRUE",
  cat = NULL,
  scat = NULL,
  duplicate_function = mean,
  silent = NULL
)
```

## Arguments

- nif:

  A nif object.

- sdtm:

  The corresponding sdtm object.

- domain:

  The domain as character.

- testcd:

  The xxTESTCD with xx the domain name, as character.

- covariate:

  The name of the covariate, defaults to the testcd if NULL.

- DTC_field:

  The field to use as the date-time code for the observation. Defaults
  to the two-character domain name followed by 'DTC', if NULL.

- DV_field:

  The name of the DV field as character.

- TESTCD_field:

  The name of the TESTCD field. defaults to xxTESTCD (with xx the domain
  code), as character.

- observation_filter:

  A filter term for the `domain`, as character.

- cat:

  xxCAT filter to apply, as character.

- scat:

  xxSCAT filter to apply, as character.

- duplicate_function:

  The function to apply if multiple covariate values are found by day.

- silent:

  Suppress messages, defaults to nif_option setting if NULL.

## Value

A nif object with a new column added that contains the time-varying
covariate values. The name of this column is determined by the
`covariate` parameter (or defaults to the value of `testcd` if not
specified). The covariate values are matched to the nif object by
USUBJID and date. For each subject, missing covariate values are filled
using the last observed value (carrying forward).

## See also

[`add_baseline()`](add_baseline.md)

## Examples

``` r
add_covariate(examplinib_poc_nif, examplinib_poc, "vs", "WEIGHT",
  covariate = "wt")
#> ----- NONMEM Input Format (NIF) data -----
#> 1344 observations from 80 subjects across 1 study 
#> Analytes: RS2023 and RS2023487A 
#> 47 males (58.8%), 33 females (41.2%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL, wt 
#> 
#> Hash: cb42ffa8a0c3b6ec888fbc69c431e581
#> 
#> Data (selected columns):
#>   ID   NTIME   TIME    TAD     ANALYTE      EVID   CMT   AMT   DOSE   DV         
#>   1    0       0       0       RS2023       1      1     500   500    NA         
#>   1    0       0       0       RS2023       0      2     0     500    0          
#>   1    0       0       0       RS2023487A   0      3     0     500    0          
#>   1    0.5     1       1       RS2023       0      2     0     500    636.683    
#>   1    0.5     1       1       RS2023487A   0      3     0     500    135.126    
#>   1    1       1.467   1.467   RS2023       0      2     0     500    1844.823   
#>   1    1       1.467   1.467   RS2023487A   0      3     0     500    677.321    
#>   1    1.5     2       2       RS2023       0      2     0     500    2773.308   
#>   1    1.5     2       2       RS2023487A   0      3     0     500    1547.253   
#>   1    2       2.467   2.467   RS2023       0      2     0     500    2539.54    
#> 7612 more rows
```
