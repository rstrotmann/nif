# Non-compartmental analysis with analyte-based grouping

This function performs NCA analysis using the PKNCA package for multiple
analytes in a NIF object. It uses analyte-specific grouping in the
concentration and dose formulas.

## Usage

``` r
nca2(
  obj,
  analytes = NULL,
  parameters = NULL,
  keep = NULL,
  average_duplicates = TRUE
)
```

## Arguments

- obj:

  A NIF object containing concentration-time data

- analytes:

  Optional vector of analytes to analyze. If NULL, all analytes will be
  analyzed.

- parameters:

  Optional vector of PK parameters to calculate. If NULL, default
  parameters will be used.

- keep:

  Optional vector of additional columns to keep in the output.

- average_duplicates:

  Boolean to indicate whether duplicate entries should be averaged.

## Value

A data frame containing NCA results
