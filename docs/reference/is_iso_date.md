# Test whether string represents ISO 8601-formatted date

The expected format is "dddd-dd-dd" with "d" a digit. This function
tests whether the above is part of the input, i.e., ISO 8601-formatted
date-time objects like "dddd-dd-ddTdd:dd" are also recognized. Dates
with missing day of month are not accepted.

## Usage

``` r
is_iso_date(x)
```

## Arguments

- x:

  The input as character.

## Value

Boolean.

## Examples

``` r
is_iso_date("2023-09-27")
#> [1] TRUE
is_iso_date(c("2023-03-21T11:55", "2023-07-18"))
#> [1] TRUE TRUE
```
