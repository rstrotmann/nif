# Check whether a domain is present in an SDTM object

Check whether a domain is present in an SDTM object

## Usage

``` r
has_domain(obj, name)
```

## Arguments

- obj:

  The sdtm object.

- name:

  The domain name(s) to check as a character string or vector.

## Value

Logical indicating whether all specified domains exist in the SDTM
object.

## Examples

``` r
# Check if DM domain exists
has_domain(examplinib_fe, "dm")
#> [1] TRUE

# Check if a non-existent domain exists
has_domain(examplinib_fe, "xyz")
#> [1] FALSE

# Check if multiple domains exist
has_domain(examplinib_fe, c("dm", "vs"))
#> [1] TRUE
```
