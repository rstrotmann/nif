# Correlate observations

**\[experimental\]**

## Usage

``` r
correlate_obs(
  obj,
  indep_analyte,
  dep_analyte,
  window = 10/60,
  time_field = "TIME",
  duplicate_function = mean
)
```

## Arguments

- obj:

  A nif object.

- indep_analyte:

  The independent analyte as character.

- dep_analyte:

  The dependent analyte(s) as character.

- window:

  The allowed time window between the independent and dependent analyte
  observations in hours.

- time_field:

  Time variable to use for the correlation of observations, can be 'DTC'
  or 'TIME' (default).

- duplicate_function:

  A function to resolve duplicate values for observations of the
  dependent analyte. Defaults to `mean`.

## Value

A data frame.

## Details

Identify observations of the dependent variables that are timely
correlated to the independent variable, i.e., are within a specified
time window to the observations of the independent variable. If multiple
dependent observations of a given analyte fall into the window, they are
summarized using the 'duplicate_function'. The output data frame is a
wide table based on the independent observations, with the (summarized)
dependent observations and the respective summarized observation times
for the dependent observations as additional columns.

## Examples

``` r
head(correlate_obs(examplinib_poc_nif, "RS2023", "RS2023487A"), 3)
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT      BMI
#> 1   2  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 2   4  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#> 3   6  1 2023000022 20230000221010001  58   1 WHITE  185.3   91.4 26.61922
#>                   DTC  TIME NTIME  TAFD   TAD EVID AMT ANALYTE CMT PARENT TRTDY
#> 1 2001-01-13 10:36:00 0.000   0.0 0.000 0.000    0   0  RS2023   2 RS2023     1
#> 2 2001-01-13 11:36:00 1.000   0.5 1.000 1.000    0   0  RS2023   2 RS2023     1
#> 3 2001-01-13 12:04:00 1.467   1.0 1.467 1.467    0   0  RS2023   2 RS2023     1
#>   METABOLITE DOSE MDV  ACTARMCD IMPUTATION        DV BL_CREAT  BL_CRCL
#> 1      FALSE  500   0 TREATMENT               0.0000 72.78062 107.4689
#> 2      FALSE  500   0 TREATMENT             636.6833 72.78062 107.4689
#> 3      FALSE  500   0 TREATMENT            1844.8225 72.78062 107.4689
#>      RS2023 RS2023487A RS2023487A_TIME
#> 1    0.0000     0.0000           0.000
#> 2  636.6833   135.1259           1.000
#> 3 1844.8225   677.3207           1.467
```
