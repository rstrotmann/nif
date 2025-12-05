# Get the USUBJID of subject

Get the USUBJID of subject

## Usage

``` r
usubjid(obj, id, silent = NULL)
```

## Arguments

- obj:

  A NIF object.

- id:

  A subject ID in numeric form.

- silent:

  Suppress messages, defaults to nif_option setting.

## Value

The USUBJID as character.

## Examples

``` r
usubjid(examplinib_fe_nif, 1, silent = FALSE)
#> [1] "20230004001010002"
```
