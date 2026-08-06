# Add time-varying covariate

Add a column to a [nif](nif.md) object representing a time-varying
covariate. In contrast to observations (see
[`add_observation()`](add_observation.md)), covariates are not captured
as rows of observation events but are attached as a separate column to
the observations in a nif object. The values reflect the status of the
covariate at the time of the existing observations and are carried
forward as needed.

## Usage

``` r
add_covariate(
  nif,
  sdtm,
  domain,
  testcd,
  covariate = NULL,
  dtc_field = NULL,
  dv_field = NULL,
  testcd_field = NULL,
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

- dtc_field:

  The field to use as the date-time code for the observation. Defaults
  to the two-character domain name followed by 'DTC', if NULL.

- dv_field:

  The name of the DV field as character.

- testcd_field:

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

## Details

Covariate data may come from any domain, and like for observations,
their source is defined by the `domain` and `testcd` arguments.
Covariate observations can be further specified with the `cat` and
`scat` arguments that refer to the 'xxCAT' and 'xxSCAT' fields of the
source domain, and the `observation_filter` argument. This may be
necessary, when observations defined by the `testcd` alone are
ambiguous.

In general, the covariate value and the respective observation time
stamp are taken from the 'xxSTRESN' and 'xxDTC' fields of the source
(where xx refers to the domain code). Other fields can be specified by
the `dv_field` and `dtc_field` arguments.

The name of the covariate columns can be specified by the `covariate`
argument. By default, it is set to the 'testcd' (without any prefix).

## See also

[`add_baseline()`](add_baseline.md)

## Examples

``` r
add_covariate(examplinib_poc_nif, examplinib_poc, "vs", "WEIGHT",
  covariate = "wt"
) |>
head()
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT      BMI
#> 1   1  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 2   2  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 3   3  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 4   4  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 5   5  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#> 6   6  1 2023000022 20230000221010001  81   0 WHITE  180.5   93.9 28.82114
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT CMT        DV    ANALYTE
#> 1 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    1 500   1        NA     RS2023
#> 2 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0   2    0.0000     RS2023
#> 3 2001-01-07 09:42:00 0.000   0.0 0.000 0.000    0   0   3    0.0000 RS2023487A
#> 4 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0   2  615.0549     RS2023
#> 5 2001-01-07 10:34:00 0.867   0.5 0.867 0.867    0   0   3  120.1609 RS2023487A
#> 6 2001-01-07 11:02:00 1.333   1.0 1.333 1.333    0   0   2 1841.7238     RS2023
#>   PARENT TRTDY METABOLITE DOSE MDV  ACTARMCD IMPUTATION BL_CREAT  BL_CRCL   wt
#> 1 RS2023     1      FALSE  500   1 TREATMENT            86.46559 78.66727 93.9
#> 2 RS2023     1      FALSE  500   0 TREATMENT            86.46559 78.66727 93.9
#> 3 RS2023     1      FALSE  500   0 TREATMENT            86.46559 78.66727 93.9
#> 4 RS2023     1      FALSE  500   0 TREATMENT            86.46559 78.66727 93.9
#> 5 RS2023     1      FALSE  500   0 TREATMENT            86.46559 78.66727 93.9
#> 6 RS2023     1      FALSE  500   0 TREATMENT            86.46559 78.66727 93.9
```
