# Make BINTIME field

Make BINTIME field

## Usage

``` r
add_bintime(obj, method = "fisher", silent = NULL)
```

## Arguments

- obj:

  A nif object.

- method:

  Univariate class intervals method, can be one of jenks, kmeans,
  pretty, quantile, hclust, sd, bclust or fisher. See
  classInt::classInterval() for details.

- silent:

  Suppress messages.

## Value

A nif object with the BINTIME, BIN_LEFT and BIN_RIGHT fields added.
