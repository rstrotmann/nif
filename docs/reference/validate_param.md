# Generic function parameter validation

Generic function parameter validation

## Usage

``` r
validate_param(
  type = c("string", "logical", "numeric"),
  param,
  param_name,
  allow_null = FALSE,
  allow_empty = FALSE,
  allow_multiple = FALSE,
  allow_na = FALSE
)
```

## Arguments

- type:

  Parameter type (string, logical or numeric)

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

  Allow NA value, as logical.

## Value

Nothing or stop.
