# Geometric coefficient of variation, in percent

Geometric coefficient of variation, in percent

## Usage

``` r
geocv(x, na.rm = FALSE)
```

## Arguments

- x:

  A numeric vector.

- na.rm:

  Logical. Drop missing values before calculation. Defaults to `FALSE`.

## Value

A numeric scalar (percent). Fewer than two non-missing values returns
`NA_real_`. Zeros and negatives yield `NaN`.
