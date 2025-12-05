# Correlate two observations by their actual observation time

**\[experimental\]**

## Usage

``` r
correlate_analytes(
  obj,
  indep_analyte,
  dep_analyte,
  window = 10/60,
  duplicate_function = mean
)
```

## Arguments

- obj:

  A nif object.

- indep_analyte:

  The independent analyte as character.

- dep_analyte:

  The dependent analyte as character.

- window:

  The allowed time window between the independent and dependent analyte
  observations in hours.

- duplicate_function:

  A function to resolve duplicate values for observations of the
  dependent analyte. Defaults to `mean`.

## Value

A data frame.
