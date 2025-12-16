# Remove duplicate rows from a data frame

This function removes duplicate rows from a data frame based on
specified fields, applying a function to handle duplicate values in the
dependent variable.

## Usage

``` r
resolve_duplicates_old(
  df,
  fields = "TIME",
  duplicate_function = mean,
  dependent_variable = "DV",
  na.rm = TRUE
)
```

## Arguments

- df:

  A data frame to remove duplicates from

- fields:

  A character vector of field names to check for duplicates. If NULL,
  defaults to c("USUBJID", "TIME", "ANALYTE") for NIF data.

- duplicate_function:

  A function to apply to duplicate values. Default is mean. The function
  should take a vector and return a single value.

- dependent_variable:

  The name of the field to apply the duplicate_function to. Defaults to
  "DV".

- na.rm:

  Logical indicating whether to remove NA values when applying the
  duplicate_function. Defaults to TRUE.

## Value

A data frame with duplicate rows removed
