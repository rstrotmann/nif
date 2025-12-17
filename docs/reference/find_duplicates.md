# Find duplicate rows in a data frame

This function identifies duplicate rows in a data frame based on
specified fields. It returns a data frame containing the duplicate rows
and their counts. The 'ID' field must always be present.

## Usage

``` r
find_duplicates(df, fields = NULL, count_only = FALSE)
```

## Arguments

- df:

  A data frame to check for duplicates

- fields:

  A character vector of field names to check for duplicates. If NULL,
  defaults to c("ID", "TIME", "ANALYTE") for NIF data.

- count_only:

  Logical indicating whether to return only the count of duplicates
  (default: FALSE)

## Value

A data frame containing the duplicate rows and their counts, or just the
count if count_only is TRUE
