# Ensure that analytes are present in nif object

Ensure that analytes are present in nif object

## Usage

``` r
validate_analyte(
  nif,
  analyte,
  allow_multiple = TRUE,
  allow_null = FALSE,
  allow_empty = FALSE
)
```

## Arguments

- nif:

  A nif object.

- analyte:

  Required analyte(s) as character.

- allow_multiple:

  Allow vector of the specified type, as logical.

- allow_null:

  Allow NULL values, as logical.

- allow_empty:

  Allow empty parameter, as logical.

## Value

Nothing or stop.
