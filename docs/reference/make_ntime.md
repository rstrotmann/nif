# Make nominal time

Return NTIME lookup table or NULL if the xxELTM field is not included in
the input data frame.

## Usage

``` r
make_ntime(obj, domain = NULL, include_day = FALSE, silent = NULL)
```

## Arguments

- obj:

  The input as data table.

- domain:

  The domain name as character.

- include_day:

  include_day Include time component of treatment day, as logical.

- silent:

  Suppress messages, as logical. Defaults to nif_option setting if NULL.

## Value

A data frame.
