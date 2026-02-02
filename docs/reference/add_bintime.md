# Make BINTIME field

**\[experimental\]**

## Usage

``` r
add_bintime(obj, method = "fisher", time = "TAFD")
```

## Arguments

- obj:

  A nif object.

- method:

  Univariate class intervals method, can be one of jenks, kmeans,
  pretty, quantile, hclust, sd, bclust or fisher. See
  classInt::classInterval() for details. Default is fisher.

- time:

  The time field to use.

## Value

A nif object with the BINTIME, BIN_LEFT and BIN_RIGHT fields added.
