# Generic pin_write function

**\[deprecated\]**

This function is deprecated, please use [`pb_write()`](pb_write.md)
instead!

## Usage

``` r
pin_write(
  obj,
  name = NULL,
  board = NULL,
  title = NULL,
  dco = NULL,
  force = FALSE,
  silent = NULL
)
```

## Arguments

- obj:

  The object to pin.

- name:

  Name for the pin object, as character. Defaults to 'xx_sdtm' or
  'xx_nif' with xx the study name.

- board:

  The path to the board object. Defaults to the 'pinboard' value of
  nif_option, or the value of the 'NIF_PINBOARD' key in the .Renviron
  setting if the 'nif_option' is not set.

- title:

  Title for the pin object, as character. Defaults to the study name(s).

- dco:

  Data cut off as character.

- force:

  Force-write, as logical.

- silent:

  Suppress messages. Defaults to nif_option setting if NULL.

## Value

Nothing.
