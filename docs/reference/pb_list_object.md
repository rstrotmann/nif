# List contents of pinboard

**\[experimental\]**

## Usage

``` r
pb_list_object(board = NULL, object_type)
```

## Arguments

- board:

  The path to the board object. Defaults to the 'pinboard' value of
  nif_option, or the value of the 'NIF_PINBOARD' key in the .Renviron
  setting if the 'nif_option' is not set.

- object_type:

  The object to filter for (sdtm or nif)

## Value

The board folder, defaults to the respective nif_option setting.
