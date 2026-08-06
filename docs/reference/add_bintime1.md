# Make BINTIME field (shared global bins)

**\[experimental\]**

Optimized variant of add_bintime() that always derives a single set of
time bins from the full data set (shared / global binning). No per-group
recursion.

## Usage

``` r
add_bintime1(
  obj,
  method = "fisher",
  time = "TAFD",
  n = NULL,
  group = NULL,
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- method:

  Univariate class intervals method, can be one of jenks, kmeans,
  pretty, quantile, hclust, sd, bclust or fisher. See
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
  for details. Default is fisher.

- time:

  The time field to use.

- n:

  Number of bins passed to
  [`classInt::classIntervals()`](https://r-spatial.github.io/classInt/reference/classIntervals.html).
  If `NULL` (default), classInt chooses the number of classes.

- group:

  Grouping variables as character.

- silent:

  Suppress messages.

## Value

A nif object with the BINTIME, BIN_LEFT and BIN_RIGHT fields added.
