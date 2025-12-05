# ggplot stat for treatment administrations

ggplot stat for treatment administrations

## Usage

``` r
stat_admin(
  mapping = NULL,
  data = NULL,
  geom = "vline",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  color = "grey",
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  If specified and `inherit.aes = TRUE` (the default), it is combined
  with the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  The data to be displayed in this layer. There are three options: If
  `NULL`, the default, the data is inherited from the plot data as
  specified in the call to
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).
  A `data.frame`, or other object, will override the plot data. A
  `function` will be called with a single argument, the plot data. The
  return value must be a `data.frame`, and will be used as the layer
  data.

- geom:

  The geometric object to use to display the data. Defaults to "vline".

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function. Defaults to "identity".

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behavior from the default
  plot specification.

- color:

  The color of the vertical lines. Defaults to "grey".

- ...:

  Additional parameters passed to the layer.

## Value

A ggplot layer object.
