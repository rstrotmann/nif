# Plot domain object

Plot domain object

## Usage

``` r
# S3 method for class 'domain'
plot(
  x,
  testcd = NULL,
  points = TRUE,
  lines = FALSE,
  legend = TRUE,
  color = NULL,
  ...
)
```

## Arguments

- x:

  A domain object.

- testcd:

  Testcd field to filter for, defaults to all if NULL.

- points:

  Plot points, as logical.

- lines:

  Plot lines, as logical.

- legend:

  Plot legend, as logical.

- color:

  Field to color points and lines by.

- ...:

  Further parameters.

## Value

A ggplot2 object.

## Examples

``` r
plot(domain(examplinib_sad, "lb"))
```
