# Non-compartmental analysis of NIF data

**\[experimental\]**

## Usage

``` r
nca(
  nif,
  analyte = NULL,
  parent = NULL,
  keep = NULL,
  group = NULL,
  time = "TIME",
  duplicates = "stop",
  duplicate_function = mean,
  silent = NULL,
  na_action = "exclude"
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
  ID, ANALYTE and the selected time fields:

  - 'stop': Stop execution and produce error message

  - 'identify': Return a list of duplicate entries

  - 'resolve': Resolve duplicates, applying the `duplicate_function` to
    the duplicate entries.

- duplicate_function:

  Function to resolve duplicate values, defaults to `mean`.

- silent:

  Suppress messages.

- na_action:

  How to handle missing observation `DV`:

  - `'exclude'`: Drop missing concentrations (default).

  - `'zero'`: Replace missing concentrations with 0.

  - `'error'`: Stop if any observation `DV` is missing. Dose rows are
    not changed. Missing concentrations at the start of an interval are
    still imputed as 0 by PKNCA.

## Value

A data frame.

## Details

This function is a wrapper around the NCA functions provided by the
[PKNCA](https://CRAN.R-project.org/package=PKNCA) package.

Missing observation concentrations are excluded by default. Use
`na_action = "zero"` to treat them as 0, or `"error"` to stop. Negative
concentrations are set to zero with a warning. PKNCA still imputes a
missing concentration at the start of each interval
(`impute = "start_conc0"`).

## Examples

``` r
head(nca(examplinib_sad_nif, time = "TAD"))
#> Error in nca(examplinib_sad_nif, time = "TAD"): `time` must be TIME or NTIME!
head(nca(examplinib_fe_nif, time = "TIME", group = "FASTED"))
#> ! No analyte specified for NCA!
#> Selected RS2023 as the most likely analyte!
#> ℹ Parent set to RS2023
#> ℹ NCA: Group by FASTED
#>   FASTED ID DI start end  PPTESTCD      PPORRES exclude DOSE
#> 1      0  1  1     0  24   auclast 2.379511e+04    <NA>  500
#> 2      0  1  1     0 Inf      cmax 3.589220e+03    <NA>  500
#> 3      0  1  1     0 Inf      tmax 2.000000e+00    <NA>  500
#> 4      0  1  1     0 Inf     tlast 1.680000e+02    <NA>  500
#> 5      0  1  1     0 Inf clast.obs 5.281776e-05    <NA>  500
#> 6      0  1  1     0 Inf  lambda.z 9.272462e-02    <NA>  500
```
