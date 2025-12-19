# Set or get global options

Set global behavior options for all `nif` package functions. Currently
supported:

- `silent` as logical: Suppress messages.

- `watermark` as character: Watermark text on all figures.

- `pinboard` as character: Pinboard path for sharing of nif/sdtm
  objects.

- `debug` as logical: Print debug information.

- `show_hash` as logical: Include dataset hash in figures.

- `abbreviation_threshold` as numeric: Allowed line count in message
  output without abbreviation. Set to Inf to always show all lines.

- `abbrevation_maxlines` as numeric: Line number to that lists in
  message output will be abbreviated if the line count threshold is
  exceeded.

## Usage

``` r
nif_option(...)
```

## Arguments

- ...:

  Options as named values, or nothing.

## Value

The global options as list, if ... is empty, or nothing.

## Examples

``` r
nif_option(silent = TRUE)
```
