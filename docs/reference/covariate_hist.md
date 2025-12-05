# Generic covariate distribution histogram

Generic covariate distribution histogram

## Usage

``` r
covariate_hist(
  obj,
  cov,
  nbins = 11,
  group = NULL,
  alpha = 0.5,
  density = TRUE,
  title = NULL
)
```

## Arguments

- obj:

  The NIF object.

- cov:

  The covariate field of the NIF object to analyze, as character.

- nbins:

  The number of bins to be used if no binwidth is specified. Defaults to
  11.

- group:

  The field(s) to group by, as character.

- alpha:

  The alpha as numeric.

- density:

  Plot density instead of N on the y axis, as logical.

- title:

  The title as character.

## Value

A ggplot object.

## Examples

``` r
covariate_hist(examplinib_sad_nif, "AGE")

covariate_hist(examplinib_sad_nif, "BL_CRCL")
```
