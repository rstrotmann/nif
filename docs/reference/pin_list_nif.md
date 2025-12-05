# List nif objects in pinboard

**\[deprecated\]**

## Usage

``` r
pin_list_nif(board = NULL)
```

## Arguments

- board:

  The path to the board object. Defaults to the 'pinboard' value of
  nif_option, or the value of the 'NIF_PINBOARD' key in the .Renviron
  setting if the 'nif_option' is not set.

## Value

A data frame.

## Details

This function is deprecated, please use
[`pb_list_nif()`](pb_list_nif.md) instead.
