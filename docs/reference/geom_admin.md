# Administration lines for a selected analyte

Draws vertical lines at administration times (`EVID == 1`) for the
analyte named in the `admin` aesthetic. The plot or layer data must be a
nif object (with `EVID` and `ANALYTE` columns).

## Usage

``` r
geom_admin(
  mapping = NULL,
  data = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  color = "grey",
  linewidth = 0.5,
  linetype = 1,
  alpha = NA,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  Must include `admin`, a character string naming the analyte whose
  administrations should be plotted (e.g. `aes(admin = "RS2023")`). `x`
  is typically inherited from the plot (e.g. `TIME`).

- data:

  A nif object. If `NULL`, the default, the plot data is used and must
  be a nif object.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends?

- inherit.aes:

  If `FALSE`, overrides the default aesthetics rather than combining
  with them.

- color:

  The color of the vertical lines. Defaults to `"grey"`.

- linewidth:

  The width of the lines. Defaults to `0.5`.

- linetype:

  The type of the lines. Defaults to `1` (solid).

- alpha:

  The transparency of the lines. Defaults to `NA` (opaque).

- ...:

  Additional parameters passed to the layer.

## Value

A ggplot layer object.

## Examples

``` r
library(dplyr)
library(ggplot2)

examplinib_sad_nif |>
  filter(ID == 1) |>
  ggplot(aes(x = TIME, y = DV)) +
  geom_admin(aes(admin = "RS2023")) +
  geom_point(na.rm = TRUE)
```
