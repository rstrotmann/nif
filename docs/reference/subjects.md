# Unique subjects within a data set

Unique subjects within a data set

## Usage

``` r
subjects(obj)
```

## Arguments

- obj:

  The data set, either a `nif` or a `sdtm` object.

## Value

A data frame.

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
head(subjects(examplinib_poc))
#>             USUBJID
#> 1 20230000221020001
#> 2 20230000221060001
#> 3 20230000221030001
#> 4 20230000221030002
#> 5 20230000221030003
#> 6 20230000221050001
```
