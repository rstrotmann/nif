# Test whether string represents ISO 8601-formatted date-time

The expected format is "dddd-dd-ddTdd:dd" with "d" a digit. This
function tests whether the above is part of the input, i.e., date-time
formats that also include seconds information are also recognized.

## Usage

``` r
is_iso_date_time(x)
```

## Arguments

- x:

  The input as character.

## Value

Boolean.

## Examples

``` r
is_iso_date_time("2023-09-27T15:04")
#> [1] TRUE
is_iso_date_time("2023-09-27T15:04:00")
#> [1] TRUE
is_iso_date_time(c("2023-03-21T11:55", "2023-07-18"))
#> [1]  TRUE FALSE
```
