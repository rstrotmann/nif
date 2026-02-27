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
  include_renal = TRUE,
  include_hepatic = TRUE,
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

- include_renal:

  Add baseline creatinine, CRCL and renal function category, if
  possible.

- include_hepatic:

  Add baseline hepatic function category per ODWG classification, if
  possible.

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
nif_auto(examplinib_sad, RS2023 ~ EXAMPLINIB, silent = TRUE) |> head()
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 4   4  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 5   5  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 6   6  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT ANALYTE CMT PARENT TRTDY
#> 1 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    1   5  RS2023   1 RS2023     1
#> 2 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0  RS2023   2 RS2023    NA
#> 3 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0  RS2023   2 RS2023    NA
#> 4 2000-12-31 11:18:00  1.0   1.0  1.0 1.0    0   0  RS2023   2 RS2023    NA
#> 5 2000-12-31 11:48:00  1.5   1.5  1.5 1.5    0   0  RS2023   2 RS2023    NA
#> 6 2000-12-31 12:18:00  2.0   2.0  2.0 2.0    0   0  RS2023   2 RS2023    NA
#>   METABOLITE DOSE MDV ACTARMCD                 IMPUTATION      DV BL_CREAT
#> 1      FALSE    5   1       C1 time imputed from PCRFTDTC      NA  67.4825
#> 2      FALSE    5   0       C1                             0.0000  67.4825
#> 3      FALSE    5   0       C1                            40.7852  67.4825
#> 4      FALSE    5   0       C1                            48.5530  67.4825
#> 5      FALSE    5   0       C1                            44.0391  67.4825
#> 6      FALSE    5   0       C1                            34.0729  67.4825
#>    BL_CRCL BL_RENAL
#> 1 135.8911   normal
#> 2 135.8911   normal
#> 3 135.8911   normal
#> 4 135.8911   normal
#> 5 135.8911   normal
#> 6 135.8911   normal
nif_auto(examplinib_sad, RS2023 + RS2023487A ~ EXAMPLINIB, silent = TRUE) |>
head()
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 4   4  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 5   5  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 6   6  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT    ANALYTE CMT PARENT TRTDY
#> 1 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    1   5     RS2023   1 RS2023     1
#> 2 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0     RS2023   2 RS2023    NA
#> 3 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0 RS2023487A   3 RS2023    NA
#> 4 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0     RS2023   2 RS2023    NA
#> 5 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0 RS2023487A   3 RS2023    NA
#> 6 2000-12-31 11:18:00  1.0   1.0  1.0 1.0    0   0     RS2023   2 RS2023    NA
#>   METABOLITE DOSE MDV ACTARMCD                 IMPUTATION      DV BL_CREAT
#> 1      FALSE    5   1       C1 time imputed from PCRFTDTC      NA  67.4825
#> 2      FALSE    5   0       C1                             0.0000  67.4825
#> 3       TRUE    5   0       C1                             0.0000  67.4825
#> 4      FALSE    5   0       C1                            40.7852  67.4825
#> 5       TRUE    5   0       C1                            13.8228  67.4825
#> 6      FALSE    5   0       C1                            48.5530  67.4825
#>    BL_CRCL BL_RENAL
#> 1 135.8911   normal
#> 2 135.8911   normal
#> 3 135.8911   normal
#> 4 135.8911   normal
#> 5 135.8911   normal
#> 6 135.8911   normal
```
