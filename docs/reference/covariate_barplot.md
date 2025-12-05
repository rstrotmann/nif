# Generic covariate barplot

Generic covariate barplot

## Usage

``` r
covariate_barplot(obj, cov, group = NULL, title = NULL)
```

## Arguments

- obj:

  A nif object.

- cov:

  The (categorical) baseline covariate as character.

- group:

  A grouping variable, as character.

- title:

  The plot title as character. Defaults to cov, if NULL.

## Value

A ggplot object.

## Examples

``` r
covariate_barplot(examplinib_poc_nif, "SEX")
```
