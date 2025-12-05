# Find duplicate rows in a data frame

This function identifies duplicate rows in a data frame based on
specified fields. It returns a data frame containing the duplicate rows
and their counts.

## Usage

``` r
find_duplicates(
  df,
  fields = NULL,
  count_only = FALSE,
  return_all_cols = TRUE,
  additional_cols = NULL
)
```

## Arguments

- df:

  A data frame to check for duplicates

- fields:

  A character vector of field names to check for duplicates. If NULL,
  defaults to c("USUBJID", "TIME", "ANALYTE") for NIF data.

- count_only:

  Logical indicating whether to return only the count of duplicates
  (default: FALSE)

- return_all_cols:

  Logical indicating whether to return all columns from the original
  data frame (default: TRUE)

- additional_cols:

  Character vector of additional columns to include in the output when
  return_all_cols is FALSE

## Value

A data frame containing the duplicate rows and their counts, or just the
count if count_only is TRUE
