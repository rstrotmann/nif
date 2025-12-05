# Convert nif object to wide data frame by NTIME

**\[experimental\]**

## Usage

``` r
pivot_analytes(
  obj,
  analyte = NULL,
  keep = NULL,
  duplicates = "stop",
  duplicate_function = mean,
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- analyte:

  Analytes to include, defaults to all, if NULL.

- keep:

  Fields to keep in pivoted output. Defaults to standard subject- level
  covariates, if NULL.

- duplicates:

  Selection how to deal with duplicate observations with respect to the
  ID, ANALYTE, NTIME and TRTDY fields:

  - 'stop': Stop execution and produce error message

  - 'ignore': Include duplicates in the data set

  - 'identify': Return a list of duplicate entries

  - 'resolve': Resolve duplicates, applying the `duplicate_function` to
    the duplicate entries.

- duplicate_function:

  Function to resolve duplicate values, defaults to `mean`.

- silent:

  Suppress messages.

## Value

A data frame.
