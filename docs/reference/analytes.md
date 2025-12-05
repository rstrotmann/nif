# Analytes within a NIF or SDTM object

Analytes within a NIF or SDTM object

## Usage

``` r
analytes(obj)
```

## Arguments

- obj:

  A nif or sdtm object.

## Value

Character.

## Examples

``` r
analytes(examplinib_fe_nif)
#> [1] "RS2023"
analytes(examplinib_poc_nif)
#> [1] "RS2023"     "RS2023487A"
analytes(examplinib_poc_min_nif)
#> [1] "CMT2" "CMT3"
```
