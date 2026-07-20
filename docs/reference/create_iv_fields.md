# Check and prepare ex for iv administrations

Check and prepare ex for iv administrations

## Usage

``` r
create_iv_fields(admin, iv_admin = NULL, duration = NULL, silent = NULL)
```

## Arguments

- admin:

  The un-expanded EX domain

- iv_admin:

  Administration is iv, as boolean. If NULL, will be tested
  automatically based on the EXROUTE field, if available.

- duration:

  The duration of the iv administration as numeric. If NULL, will be
  determined automatically based on the EXDUR field, if available.

- silent:

  Suppress messages.

## Value

The updated EX domain.
