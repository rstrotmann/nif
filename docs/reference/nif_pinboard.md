# Get or set pinboard path

This function returns the currently active pinboard path, i.e., either
the respective nif_option setting or the folder set in the .Renviron
variable NIF_PINBOARD. If the 'path' argument is provided, the
nif_option 'pinboard' will be set to that folder. Use an empty string
("") to reset the pinboard path.

## Usage

``` r
nif_pinboard(path = NULL)
```

## Arguments

- path:

  Pinboard path as character.

## Value

Currently active pinboard path as character.

## Details

Overall, the following pinboard paths are used for all pinboard
functions (in the order of their priority):

- The 'board' parameter in the individual function call (highest)

- The 'pinboard' setting in nif_options

- The .Renviron variable NIF_PINBOARD (lowest)

You may use the convenience function
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)
to edit the .Renviron file and include a user-level setting for the
default pinboard path (e.g., `NIF_PINBOARD=\path\to\pinboard\folder`).

## See also

[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html)

[`nif_option()`](nif_option.md)
