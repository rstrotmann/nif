# Check if a string matches ISO 8601 date-time format

This function checks whether a character string complies with the ISO
8601 standard for date-time representation (combined date and time).
Unlike the more general `is_iso8601_format()` function, this
specifically checks for the presence of both date and time components.

## Usage

``` r
is_iso8601_datetime(x, strict = FALSE)
```

## Arguments

- x:

  A character string or vector of strings to check.

- strict:

  Logical, whether to strictly enforce ISO 8601 specification. Default
  is FALSE, which allows some common variations like space instead of
  'T' separator.

## Value

Logical value indicating whether the string is in ISO 8601 date-time
format.

## Details

Valid formats include:

- Extended format with separators: "2023-10-15T14:30:00"

- Basic format without separators: "20231015T143000"

- With timezone information: "2023-10-15T14:30:00Z" or
  "2023-10-15T14:30:00+02:00"

- With fractional seconds: "2023-10-15T14:30:00.123"

- Using space instead of T separator: "2023-10-15 14:30:00" (non-strict
  mode only)

## Examples

``` r
is_iso8601_datetime("2023-10-15T14:30:00") # TRUE
#> [1] TRUE
is_iso8601_datetime("2023-10-15 14:30:00") # TRUE (with default strict=FALSE)
#> [1] TRUE
is_iso8601_datetime("2023-10-15 14:30:00", TRUE) # FALSE (with strict=TRUE)
#> [1] FALSE
is_iso8601_datetime("2023-10-15T14:30:00Z") # TRUE
#> [1] TRUE
is_iso8601_datetime("2023-10-15T14:30:00+02:00") # TRUE
#> [1] TRUE
is_iso8601_datetime("20231015T143000") # TRUE
#> [1] TRUE
is_iso8601_datetime("2023-10-15") # FALSE (no time component)
#> [1] FALSE
is_iso8601_datetime("14:30:00") # FALSE (no date component)
#> [1] FALSE
```
