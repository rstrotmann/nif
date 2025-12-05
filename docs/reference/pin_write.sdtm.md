# Write to pin board

**\[experimental\]**

## Usage

``` r
# S3 method for class 'sdtm'
pin_write(
  obj,
  name = NULL,
  board = NULL,
  title = NULL,
  dco = NULL,
  silent = NULL
)
```

## Arguments

- obj:

  sdtm object.

- name:

  Name for the sdtm pin object, as character. Defaults to 'xx_sdtm' with
  xx the study name.

- board:

  The path to the board object. Defaults to the 'pinboard' value of
  nif_option, or the value of the 'NIF_PINBOARD' key in the .Renviron
  setting if the 'nif_option' is not set.

- title:

  Title for the sdtm pin object, as character. Defaults to the study
  name.

- dco:

  Data cut off as character.

- silent:

  Suppress messages. Defaults to nif_option setting if NULL.

## Value

Nothing.
