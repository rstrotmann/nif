# Identify baseline columns in a data frame

Identifies columns that are constant (baseline) for each ID value. A
baseline column is one where all rows with the same ID have the same
value.

## Usage

``` r
identify_baseline_columns(df, id_col = "ID")
```

## Arguments

- df:

  A data frame.

- id_col:

  Character string specifying the ID column name. Defaults to "ID".

## Value

A character vector of column names that are baseline columns (constant
per ID). Returns an empty character vector if no baseline columns are
found.

## Examples

``` r
# Create example data frame
df <- tibble::tribble(
  ~ID, ~SEX, ~AGE, ~TIME, ~DV,
  1,   "M",  25,   0,     10,
  1,   "M",  25,   1,     12,
  1,   "M",  25,   2,     15,
  2,   "F",  30,   0,     8,
  2,   "F",  30,   1,     9,
  2,   "F",  30,   2,     11
)
identify_baseline_columns(df, id_col = "ID")
#> [1] "SEX" "AGE"
# Returns: c("SEX", "AGE")
```
