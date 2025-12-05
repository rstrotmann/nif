# Convert ISO 8601-formatted duration to hours

Convert ISO 8601-formatted duration to hours

## Usage

``` r
pt_to_hours(iso)
```

## Arguments

- iso:

  The duration as ISO 8601-formatted string.

## Value

Duration in hours.

## Examples

``` r
pt_to_hours(c("PT1H15M", "PT1.5H", "-PT4H30M"))
#> [1]  1.25  1.50 -4.50
```
