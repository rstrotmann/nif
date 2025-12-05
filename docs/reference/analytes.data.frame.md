# Analytes within a NIF object

All analytes found in the NIF object. If the field 'ANALYTE' is not
present, The analyte title is derived from the compartment.

## Usage

``` r
# S3 method for class 'data.frame'
analytes(obj)
```

## Arguments

- obj:

  A NIF object

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
