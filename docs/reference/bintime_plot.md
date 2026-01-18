# BINTIME plot

BINTIME plot

## Usage

``` r
bintime_plot(
  obj,
  analyte,
  method = "fisher",
  points = FALSE,
  caption = TRUE,
  size = 1.5,
  alpha = 1
)
```

## Arguments

- obj:

  A nif object.

- analyte:

  The analyte as character.

- method:

  Univariate class intervals method, can be one of jenks, kmeans,
  pretty, quantile, hclust, sd, bclust or fisher. See
  classInt::classInterval() for details.

- points:

  Plot original data points as logical.

- caption:

  Show caption as logical.

- size:

  The point size.

- alpha:

  The alpha parameter for the data points.

## Value

A ggplot2 object.
