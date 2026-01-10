# Unique subjects within a NIF object

Unique subjects within a NIF object

## Usage

``` r
# S3 method for class 'nif'
subjects(obj)
```

## Arguments

- obj:

  A NIF object.

## Value

A data frame of all ID - USUBJID pairs in the data set.

## Examples

``` r
head(subjects(examplinib_fe_nif))
#>   ID           USUBJID
#> 1  1 20230004001010002
#> 2  2 20230004001010003
#> 3  3 20230004001010004
#> 4  4 20230004001010006
#> 5  5 20230004001010007
#> 6  6 20230004001020002
```
