# Coalescing join

Source: https://www.r-bloggers.com/2023/05/
replace-missing-value-from-other-columns-using-coalesce-join-in-dplyr/

## Usage

``` r
coalesce_join(
  x,
  y,
  by = NULL,
  keep = c("left", "right"),
  suffix = c(".x", ".y"),
  join = c("full_join", "left_join", "right_join", "inner_join")
)
```

## Arguments

- x:

  Left, as data frame.

- y:

  Right, as data frame.

- by:

  The 'by' argument of the join functions.

- keep:

  'left' means keep value from left table if values exist in both
  tables.

- suffix:

  Same as the suffix argument in dplyr joins.

- join:

  Choose a join type from the list. The default is full_join.

## Value

A data frame.
