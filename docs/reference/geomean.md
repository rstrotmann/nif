# Geometric mean

Geometric mean

## Usage

``` r
geomean(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Logical. Drop missing values before calculation. Defaults to `FALSE`.

## Value

A numeric scalar. Zeros yield `0`. Negatives yield `NaN`. An empty
vector, or all-`NA` input with `na.rm = TRUE`, returns `NA_real_`.
