# Maximal time in nif object

Maximal time in nif object

## Usage

``` r
max_time(obj, time_field = "TIME", analyte = NULL, only_observations = TRUE)
```

## Arguments

- obj:

  A nif object.

- time_field:

  The field to use as the time metric, as character.

- analyte:

  The analyte to filter for, as character.

- only_observations:

  Maximal observation time as logical.

## Value

Numeric.

## Examples

``` r
max_time(examplinib_poc_nif)
#> [1] 228.717
max_time(examplinib_poc_nif, analyte = "RS2023")
#> [1] 228.717
max_time(examplinib_poc_nif, only_observations = TRUE)
#> [1] 228.717
```
