# Append administration events

Drug administration data is taken from the EX domain of the sdtm object.
The 'extrt' field specifies the drug name as represented in 'EX',
however, a different 'analyte' name can be assigned to match with that
of the pharmacokinetic observations for the parent drug in plasma.

## Usage

``` r
add_administration(
  nif,
  sdtm,
  extrt,
  analyte = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = NULL,
  debug = FALSE,
  silent = NULL
)
```

## Arguments

- nif:

  A nif object.

- sdtm:

  A sdtm object.

- extrt:

  The EXTRT for the administration, as character.

- analyte:

  The name of the analyte as character.

- cmt:

  The compartment for the administration as numeric.

- subject_filter:

  The filtering to apply to the DM domain, as string,

- cut_off_date:

  The data cut-off date as Posix date-time.

- keep:

  Columns to keep after cleanup, as character.

- debug:

  Include debug fields, as logical.

- silent:

  Suppress messages, defaults to nif_option standard when NULL.

## Value

A nif object.

## Details

For an overview on the representation of administration events in NONMEM
Input Format compliant data sets, see Bauer, R.J. (2019), NONMEM
Tutorial Part I: Description of Commands and Options, With Simple
Examples of Population Analysis. CPT Pharmacometrics Syst. Pharmacol.,
8: 525-537. <https://doi.org/10.1002/psp4.12404>

## Examples

``` r
add_administration(new_nif(), examplinib_sad, "EXAMPLINIB")
#> ----- NONMEM Input Format (NIF) data -----
#> 0 observations from 48 subjects across 1 study 
#> Analytes: EXAMPLINIB 
#> 48 males (100%), 0 females (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV 
#> 
#> Hash: cfcb899c225389f5de3fb2a91b381eaa
#> 
#> Data (selected columns):
#>   ID   NTIME   TIME   TAD   ANALYTE      EVID   CMT   AMT   DOSE   DV   
#>   1    0       0      0     EXAMPLINIB   1      1     5     5      NA   
#>   2    0       0      0     EXAMPLINIB   1      1     5     5      NA   
#>   3    0       0      0     EXAMPLINIB   1      1     5     5      NA   
#>   4    0       0      0     EXAMPLINIB   1      1     10    10     NA   
#>   5    0       0      0     EXAMPLINIB   1      1     10    10     NA   
#>   6    0       0      0     EXAMPLINIB   1      1     10    10     NA   
#>   7    0       0      0     EXAMPLINIB   1      1     20    20     NA   
#>   8    0       0      0     EXAMPLINIB   1      1     20    20     NA   
#>   9    0       0      0     EXAMPLINIB   1      1     20    20     NA   
#>   10   0       0      0     EXAMPLINIB   1      1     50    50     NA    
#> 38 more rows
```
