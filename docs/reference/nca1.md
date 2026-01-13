# Non-compartmental analysis of NIF data

**\[experimental\]**

## Usage

``` r
nca1(
  nif,
  analyte = NULL,
  parent = NULL,
  keep = NULL,
  group = NULL,
  time = "TIME",
  duplicates = "stop",
  duplicate_function = mean,
  silent = NULL
)
```

## Arguments

- nif:

  A nif object.

- analyte:

  The analyte as character.

- parent:

  The parent as character.

- keep:

  Columns to keep, as character.

- group:

  Columns to group by, as character.

- time:

  The time field as character.

- duplicates:

  Selection how to deal with duplicate observations with respect to the
  USUBJID, ANALYTE and DTC fields:

  - 'stop': Stop execution and produce error message

  - 'identify': Return a list of duplicate entries

  - 'resolve': Resolve duplicates, applying the `duplicate_function` to
    the duplicate entries.

- duplicate_function:

  Function to resolve duplicate values, defaults to `mean`.

- silent:

  Suppress messages.

## Value

A data frame.

## Details

This function is a wrapper around the NCA functions provided by the
[PKNCA](https://CRAN.R-project.org/package=PKNCA) package.

NA values are set to zero! Negative concentrations are set to zero!

## Examples

``` r
head(nca1(examplinib_sad_nif, time = "TAD"))
#> ! No analyte specified for NCA!
#> Selected RS2023 as the most likely analyte!
#>   ID DI start end  PPTESTCD      PPORRES exclude DOSE
#> 1  1  1     0  24   auclast 135.83589639    <NA>    5
#> 2  1  1     0 Inf      cmax  48.55300000    <NA>    5
#> 3  1  1     0 Inf      tmax   1.00000000    <NA>    5
#> 4  1  1     0 Inf     tlast  96.00000000    <NA>    5
#> 5  1  1     0 Inf clast.obs   0.00020000    <NA>    5
#> 6  1  1     0 Inf  lambda.z   0.08606345    <NA>    5
head(nca1(examplinib_fe_nif, time = "TIME", group = "FASTED"))
#> ! No analyte specified for NCA!
#> Selected RS2023 as the most likely analyte!
#> ! 5 negative concentrations set to zero!
#> ℹ NCA: Group by FASTED
#>   FASTED ID DI start end  PPTESTCD      PPORRES exclude DOSE
#> 1      1  1  1     0  24   auclast 2.504642e+04    <NA>  500
#> 2      1  1  1     0 Inf      cmax 6.325101e+03    <NA>  500
#> 3      1  1  1     0 Inf      tmax 1.000000e+00    <NA>  500
#> 4      1  1  1     0 Inf     tlast 1.680000e+02    <NA>  500
#> 5      1  1  1     0 Inf clast.obs 9.704197e-05    <NA>  500
#> 6      1  1  1     0 Inf  lambda.z 8.895551e-02    <NA>  500
```
