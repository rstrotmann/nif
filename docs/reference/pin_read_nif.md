# Read nif object from pinboard

**\[deprecated\]** This function is deprecated. Please use pb_read_nif()
instead!

## Usage

``` r
pin_read_nif(name, board = NULL)
```

## Arguments

- name:

  The pin name, as character.

- board:

  The path to the board object. Defaults to the 'pinboard' value of
  nif_option, or the value of the 'NIF_PINBOARD' key in the .Renviron
  setting if the 'nif_option' is not set.

## Value

A nif object.

## Details

This function is deprecated, please use
[`pb_read_nif()`](pb_read_nif.md) instead.
