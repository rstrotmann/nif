# Check if a string matches ISO 8601 date format

This function checks whether a character string complies with the ISO
8601 standard for date representation (without time components). Unlike
the more general `is_iso8601_format()` function or the
[`is_iso8601_datetime()`](is_iso8601_datetime.md) function, this
specifically validates date-only formats.

## Usage

``` r
is_iso8601_date(x, allow_reduced_precision = TRUE)
```

## Arguments

- x:

  A character string or vector of strings to check.

- allow_reduced_precision:

  Logical, whether to allow reduced precision formats (year-month or
  year only). Default is TRUE.

## Value

Logical value indicating whether the string is in ISO 8601 date format.

## Details

Valid formats include:

- Extended format with separators: "2023-10-15"

- Basic format without separators: "20231015"

- Reduced precision (year-month): "2023-10" or "202310"

- Reduced precision (year only): "2023"

## Examples

``` r
is_iso8601_date("2023-10-15") # TRUE
#> [1] TRUE
is_iso8601_date("20231015") # TRUE
#> [1] TRUE
is_iso8601_date("2023-10") # TRUE (with default allow_reduced_precision=TRUE)
#> [1] TRUE
is_iso8601_date("2023") # TRUE (with default allow_reduced_precision=TRUE)
#> [1] TRUE
is_iso8601_date("2023-10", FALSE) # FALSE (with allow_reduced_precision=FALSE)
#> [1] FALSE
is_iso8601_date("2023/10/15") # FALSE (not ISO 8601 format)
#> [1] FALSE
is_iso8601_date("2023-10-15T14:30:00") # FALSE (has time component)
#> [1] FALSE
```
