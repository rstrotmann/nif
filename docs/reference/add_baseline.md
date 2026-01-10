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
  factor = 1,
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

- factor:

  A multiplier for the baseline value, defaults to 1.

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
automatically included during the generation of a nif object.

## Examples

``` r
head(add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT"))
#> baseline_filter for BL_WEIGHT set to VSBLFL == 'Y'
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 4   4  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 5   5  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 6   6  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT ANALYTE CMT PARENT TRTDY
#> 1 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    1   5  RS2023   1 RS2023     1
#> 2 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0  RS2023   2 RS2023     1
#> 3 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0  RS2023   2 RS2023     1
#> 4 2000-12-31 11:18:00  1.0   1.0  1.0 1.0    0   0  RS2023   2 RS2023     1
#> 5 2000-12-31 11:48:00  1.5   1.5  1.5 1.5    0   0  RS2023   2 RS2023     1
#> 6 2000-12-31 12:18:00  2.0   2.0  2.0 2.0    0   0  RS2023   2 RS2023     1
#>   METABOLITE DOSE MDV ACTARMCD IMPUTATION      DV BL_CREAT  BL_CRCL BL_WEIGHT
#> 1      FALSE    5   1       C1                 NA  67.4825 115.5074        77
#> 2      FALSE    5   0       C1             0.0000  67.4825 115.5074        77
#> 3      FALSE    5   0       C1            40.7852  67.4825 115.5074        77
#> 4      FALSE    5   0       C1            48.5530  67.4825 115.5074        77
#> 5      FALSE    5   0       C1            44.0391  67.4825 115.5074        77
#> 6      FALSE    5   0       C1            34.0729  67.4825 115.5074        77
add_baseline(examplinib_sad_nif, examplinib_sad, "vs", "WEIGHT",
  baseline_filter = "VSBLFL == 'Y'"
) |>
head()
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 4   4  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 5   5  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 6   6  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT ANALYTE CMT PARENT TRTDY
#> 1 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    1   5  RS2023   1 RS2023     1
#> 2 2000-12-31 10:18:00  0.0   0.0  0.0 0.0    0   0  RS2023   2 RS2023     1
#> 3 2000-12-31 10:48:00  0.5   0.5  0.5 0.5    0   0  RS2023   2 RS2023     1
#> 4 2000-12-31 11:18:00  1.0   1.0  1.0 1.0    0   0  RS2023   2 RS2023     1
#> 5 2000-12-31 11:48:00  1.5   1.5  1.5 1.5    0   0  RS2023   2 RS2023     1
#> 6 2000-12-31 12:18:00  2.0   2.0  2.0 2.0    0   0  RS2023   2 RS2023     1
#>   METABOLITE DOSE MDV ACTARMCD IMPUTATION      DV BL_CREAT  BL_CRCL BL_WEIGHT
#> 1      FALSE    5   1       C1                 NA  67.4825 115.5074        77
#> 2      FALSE    5   0       C1             0.0000  67.4825 115.5074        77
#> 3      FALSE    5   0       C1            40.7852  67.4825 115.5074        77
#> 4      FALSE    5   0       C1            48.5530  67.4825 115.5074        77
#> 5      FALSE    5   0       C1            44.0391  67.4825 115.5074        77
#> 6      FALSE    5   0       C1            34.0729  67.4825 115.5074        77
```
