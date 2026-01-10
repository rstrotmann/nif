# Non-compartmental analysis

This function is a wrapper around the NCA functions provided by the
[PKNCA](https://CRAN.R-project.org/package=PKNCA) package.

## Usage

``` r
nca(
  obj,
  analyte = NULL,
  parent = NULL,
  keep = "DOSE",
  group = NULL,
  nominal_time = FALSE,
  average_duplicates = TRUE,
  silent = NULL
)
```

## Arguments

- obj:

  The source NIF object.

- analyte:

  The analyte. If none specified and multiple analytes are in the
  dataset, defaults to the first analyte.

- parent:

  The parent compound to derive the administration information from. By
  default, equal to the analyte.

- keep:

  A vector of fields to retain on the subject level in the output.

- group:

  The grouping variable as string.

- nominal_time:

  A boolean to indicate whether nominal time rather than actual time
  should be used for NCA.

- average_duplicates:

  Boolean to indicate whether duplicate entries should be averaged.

- silent:

  Suppress messages, defaults to nif_option setting, if NULL.

## Value

A data frame.
