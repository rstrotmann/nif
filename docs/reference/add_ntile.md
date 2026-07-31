# Add quantiles for a subject-level covariate

Assigns n-tile bins for a subject-level numeric column. Bins are
computed from the distinct non-missing values across subjects (via
[`dplyr::ntile()`](https://dplyr.tidyverse.org/reference/ntile.html)),
then mapped back so identical values always share the same bin. Each
subject gets the same n-tile across all their rows. The input column
must have exactly one distinct value per subject, including `NA` (e.g.,
age, weight, baseline values). If there are fewer distinct values than
`n`, fewer than `n` bins are used.

## Usage

``` r
add_ntile(nif, input_col, n = 4, ntile_name = NULL, silent = NULL)
```

## Arguments

- nif:

  A nif object

- input_col:

  The column name to calculate n-tiles for (must have one distinct entry
  per subject)

- n:

  The number of quantiles (n-tiles) to generate (default = 4)

- ntile_name:

  Custom name for the output column. If NULL, uses `x_NTILE` format
  where x is the name of the input column

- silent:

  Suppress messages.

## Value

A nif object with a new column containing the n-tile values (1 to at
most `n`), named either `x_NTILE` (default) or the custom name specified
in `ntile_name`. Subjects with a missing input value receive `NA`.

## See also

[`dplyr::ntile()`](https://dplyr.tidyverse.org/reference/ntile.html)
used on distinct subject values

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

examplinib_sad_nif |>
  add_ntile("WEIGHT") |>
  plot(dose_norm = TRUE, facet = "WEIGHT_NTILE")


examplinib_poc_nif |>
  add_ntile("WEIGHT", n = 5) |>
  distinct(ID, WEIGHT, WEIGHT_NTILE) |>
  ggplot(aes(x = WEIGHT_NTILE, y = WEIGHT)) +
  geom_point() +
  labs(title = "WEIGHT by n-tile") +
  theme_bw()
```
