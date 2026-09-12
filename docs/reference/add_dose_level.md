# Add dose level column

Dose level is the starting dose regimen for each ID: administrations
whose consecutive times fall within 12 hours (the same window as
[`index_regimen()`](index_regimen.md)). Later dose or regimen changes
are ignored.

## Usage

``` r
add_dose_level(obj, silent = NULL)
```

## Arguments

- obj:

  A NIF dataset.

- silent:

  Suppress messages.

## Value

A NIF dataset.
