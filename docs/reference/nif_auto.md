# Auto-generate nif from sdtm object

**\[experimental\]**

## Usage

``` r
nif_auto(
  sdtm,
  ...,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  observation_filter = "TRUE",
  baseline_filter = NULL,
  duplicates = "resolve",
  duplicate_function = mean,
  keep = NULL,
  silent = NULL
)
```

## Arguments

- sdtm:

  A sdtm object. Needs at least the 'DM' and 'VS' domains, and the
  domain the observations come from.

- ...:

  Formulae to define the relationships between PCTESTCD and EXTRT.

- subject_filter:

  The filtering to apply to the DM domain.

- observation_filter:

  The filtering to apply to the observation source data.

- baseline_filter:

  A filter term to identify the baseline condition, as character.

- duplicates:

  Selection how to deal with duplicate observations with respect to the
  USUBJID, ANALYTE and DTC fields:

  - 'stop': Stop execution and produce error message

  - 'ignore': Include duplicates in the data set

  - 'identify': Return a list of duplicate entries

  - 'resolve': Resolve duplicates, applying the `duplicate_function` to
    the duplicate entries.

- duplicate_function:

  Function to resolve duplicate values, defaults to `mean`.

- keep:

  Columns to keep, as character.

- silent:

  Suppress messages, as logical. Defaults to nif_option setting if NULL.

## Value

A nif object.

## Details

If no mapping is provided, `nif_auto()` will try to find matching
treatment names in EX and PCTESTCD in PC to create a basic nif object
for pharmacokinetic observations. It will fail if no EXTRT and PCTESTCD
with identical names are found.

If one or more mappings are provided as additional parameters,
`nif_auto()` will create respective observations. Mappings must be
provided in the form:

`TESTCD ~ EXTRT`

where TESTCD can be any xxTESTCD from any domain xx included in the sdtm
object. See [`testcd()`](testcd.md) for an overview on all analytes in
all domains. Formulae can also specify multiple observations to be
associated with one treatment, e.g.,

`RS2023 + RS2023487A ~ EXAMPLINIB`.

Multiple mappings can be given, separated by commas. The analyte name in
the nif object is automatically set to the respective TESTCD.

**IMPORTANT**: If multiple analytes are specified for a treatment, it is
implicitly assumed that the first analyte corresponds to the parent
analyte for the treatment, e.g., in the case above, 'RS2023' is assumed
to be the analyte for the 'EXAMPLINIB' treatment parent compound.

Note that compartments (CMT) are automatically assigned. If more control
over compartments, analyte names and parent assignment is desired,
stepwise creation of a nif object using
[`add_administration()`](add_administration.md) and
[`add_observation()`](add_observation.md) is recommended.

## See also

[`testcd()`](testcd.md)

[`add_administration()`](add_administration.md)

[`add_observation()`](add_observation.md)

## Examples

``` r
nif_auto(examplinib_sad, RS2023 ~ EXAMPLINIB, silent = TRUE)
#> ----- NONMEM Input Format (NIF) data -----
#> 816 observations from 48 subjects across 1 study 
#> Analytes: RS2023 
#> 48 males (100%), 0 females (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL, BL_RENAL 
#> 
#> Hash: e4a70fe21b38f3c32213db69ab84f713
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
nif_auto(examplinib_sad, RS2023 + RS2023487A ~ EXAMPLINIB, silent = TRUE)
#> ----- NONMEM Input Format (NIF) data -----
#> 1632 observations from 48 subjects across 1 study 
#> Analytes: RS2023 and RS2023487A 
#> 48 males (100%), 0 females (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL, BL_RENAL 
#> 
#> Hash: f126ff9d6121ebf975bd32288e0ab095
#> 
#> Data (selected columns):
#>   ID   NTIME   TIME   TAD   ANALYTE      EVID   CMT   AMT   DOSE   DV       
#>   1    0       0      0     RS2023       1      1     5     5      NA       
#>   1    0       0      0     RS2023       0      2     0     5      0        
#>   1    0       0      0     RS2023487A   0      3     0     5      0        
#>   1    0.5     0.5    0.5   RS2023       0      2     0     5      40.785   
#>   1    0.5     0.5    0.5   RS2023487A   0      3     0     5      13.823   
#>   1    1       1      1     RS2023       0      2     0     5      48.553   
#>   1    1       1      1     RS2023487A   0      3     0     5      31.645   
#>   1    1.5     1.5    1.5   RS2023       0      2     0     5      44.039   
#>   1    1.5     1.5    1.5   RS2023487A   0      3     0     5      41.021   
#>   1    2       2      2     RS2023       0      2     0     5      34.073    
#> 1670 more rows
nif_auto(examplinib_sad, RS2023 + WEIGHT ~ EXAMPLINIB, silent = TRUE)
#> ✖ Analyte WEIGHT not found in PCTESTCD. Administrations times for WEIGHT cannot be derived from PCRFDTC and will be taken from EXSTDTC/EXENDTC!
#> ----- NONMEM Input Format (NIF) data -----
#> 0 observations from 48 subjects across 1 study 
#> Analytes: WEIGHT 
#> 48 males (100%), 0 females (0%)
#> 
#> Columns:
#>   REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, HEIGHT, WEIGHT, BMI, DTC, TIME,
#>   NTIME, TAFD, TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE,
#>   MDV, ACTARMCD, IMPUTATION, DV, BL_CREAT, BL_CRCL, BL_RENAL 
#> 
#> Hash: 6340df690d0a998d02c33959708be7b7
#> 
#> Data (selected columns):
#>   ID   NTIME   TIME   TAD   ANALYTE   EVID   CMT   AMT   DOSE   DV   
#>   1    0       0      0     WEIGHT    1      1     5     5      NA   
#>   2    0       0      0     WEIGHT    1      1     5     5      NA   
#>   3    0       0      0     WEIGHT    1      1     5     5      NA   
#>   4    0       0      0     WEIGHT    1      1     10    10     NA   
#>   5    0       0      0     WEIGHT    1      1     10    10     NA   
#>   6    0       0      0     WEIGHT    1      1     10    10     NA   
#>   7    0       0      0     WEIGHT    1      1     20    20     NA   
#>   8    0       0      0     WEIGHT    1      1     20    20     NA   
#>   9    0       0      0     WEIGHT    1      1     20    20     NA   
#>   10   0       0      0     WEIGHT    1      1     50    50     NA    
#> 38 more rows
```
