# Validate numeric parameter

Validate numeric parameter

## Usage

``` r
validate_numeric_param(
  param,
  param_name,
  allow_null = FALSE,
  allow_empty = FALSE,
  allow_multiple = FALSE,
  allow_na = FALSE
)
```

## Arguments

- param:

  The parameter to be tested.

- param_name:

  The parameter name as character.

- allow_null:

  Allow NULL values, as logical.

- allow_empty:

  Allow empty parameter, as logical.

- allow_multiple:

  Allow vector of the specified type, as logical.

- allow_na:

  Allow NA values, as logical.

## Value

Nothing or stop.
