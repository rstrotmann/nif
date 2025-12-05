# Positive value or zero if negative

Positive value or zero if negative

## Usage

``` r
positive_or_zero(x)
```

## Arguments

- x:

  Numeric.

## Value

Numeric.

## Examples

``` r
positive_or_zero(2)
#> [1] 2
positive_or_zero(-2)
#> [1] 0
positive_or_zero(c(2, 1, 0, -1, -2))
#> [1] 2 1 0 0 0
```
