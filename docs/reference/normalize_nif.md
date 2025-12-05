# Normalize nif object

Order nif object, index and fill missing fields, and reduce to essential
columns.

## Usage

``` r
normalize_nif(obj, cleanup = TRUE, keep = NULL)
```

## Arguments

- obj:

  A nif object.

- cleanup:

  Remove non-essential fields, as logical.

- keep:

  Fields to explicitly keep, as character.

## Value

A nif object.
