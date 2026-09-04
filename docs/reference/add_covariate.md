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
#> 1   1  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 2   2  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 3   3  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 4   4  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 5   5  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#> 6   6  1 2023000022 20230000221010001  49   1 WHITE  180.4  102.6 31.52639
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT CMT        DV    ANALYTE
#> 1 2001-01-05 10:25:00 0.000   0.0 0.000 0.000    1 500   1        NA     RS2023
#> 2 2001-01-05 10:25:00 0.000   0.0 0.000 0.000    0   0   2    0.0000     RS2023
#> 3 2001-01-05 10:25:00 0.000   0.0 0.000 0.000    0   0   3    0.0000 RS2023487A
#> 4 2001-01-05 11:31:00 1.100   0.5 1.100 1.100    0   0   2  553.4686     RS2023
#> 5 2001-01-05 11:31:00 1.100   0.5 1.100 1.100    0   0   3  121.0349 RS2023487A
#> 6 2001-01-05 12:00:00 1.583   1.0 1.583 1.583    0   0   2 1484.4186     RS2023
#>   PARENT TRTDY METABOLITE DOSE MDV  ACTARMCD               IMPUTATION BL_CREAT
#> 1 RS2023     1      FALSE  500   1 TREATMENT time copied from EXSTDTC 58.84185
#> 2 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 3 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 4 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 5 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#> 6 RS2023     1      FALSE  500   0 TREATMENT                          58.84185
#>    BL_CRCL    wt
#> 1 165.5927 102.6
#> 2 165.5927 102.6
#> 3 165.5927 102.6
#> 4 165.5927 102.6
#> 5 165.5927 102.6
#> 6 165.5927 102.6
```
