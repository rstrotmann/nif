# Resolve duplicate rows by averaging DV and setting conflicting fields to NA

This function identifies duplicate rows based on specified identifier
fields and resolves them by:

- Averaging the DV field across duplicates

- Keeping other fields as-is if they have the same value across
  duplicates

- Setting fields to NA if they have multiple different values within
  duplicates

## Usage

``` r
resolve_duplicates(
  df,
  fields = "TIME",
  dependent_variable = "DV",
  duplicate_function = mean,
  na.rm = TRUE
)
```

## Arguments

- df:

  A data frame to resolve duplicates from

- fields:

  A character vector of field names to identify duplicates. These fields
  are used to group rows that are considered duplicates.

- dependent_variable:

  The name of the field to apply the duplicate_function to. Defaults to
  "DV".

- duplicate_function:

  A function to apply to duplicate values. Default is mean. The function
  should take a vector and return a single value.

- na.rm:

  Logical indicating whether to remove NA values when applying the
  duplicate_function. Defaults to TRUE.

## Value

A data frame with duplicate rows resolved. The DV field contains the
average of duplicate values, and other fields are kept as-is if
consistent or set to NA if inconsistent within duplicate groups.
