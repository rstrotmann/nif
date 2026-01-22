# Make BINTIME field

\#' @description **\[experimental\]**

## Usage

``` r
add_bintime(obj, method = "fisher")
```

## Arguments

- obj:

  A nif object.

- method:

  Univariate class intervals method, can be one of jenks, kmeans,
  pretty, quantile, hclust, sd, bclust or fisher. See
  classInt::classInterval() for details. Default is fisher.

## Value

A nif object with the BINTIME, BIN_LEFT and BIN_RIGHT fields added.
