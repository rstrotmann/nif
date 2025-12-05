# Import nif file

Import nif file

## Usage

``` r
import_nif(
  filename,
  ...,
  format = NULL,
  delimiter = ",",
  no_numeric = c("USUBJID", "STUDYID"),
  silent = NULL
)
```

## Arguments

- filename:

  Filename as character.

- ...:

  Renaming terms as function.

- format:

  The input data format, can be 'csv' or 'fixed_width', or NULL
  (default) to automatically determine the format.

- delimiter:

  Delimiter character.

- no_numeric:

  Fields that will not be converted to numeric.

- silent:

  Suppress message output, as logical.

## Value

A nif object.
