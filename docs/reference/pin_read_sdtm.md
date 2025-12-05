# Read sdtm object from pinboard

**\[deprecated\]**

## Usage

``` r
pin_read_sdtm(name, board = NULL)
```

## Arguments

- name:

  The pin name, as character.

- board:

  The path to the board object. Defaults to the 'pinboard' value of
  nif_option, or the value of the 'NIF_PINBOARD' key in the .Renviron
  setting if the 'nif_option' is not set.

## Value

A sdtm object.

## Details

This function is deprecated, please use
[`pb_read_sdtm()`](pb_read_sdtm.md) instead.
