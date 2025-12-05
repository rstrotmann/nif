# Extract TESTCD fields by domain from a sdtm object

Extract TESTCD fields by domain from a sdtm object

## Usage

``` r
testcd(obj, domain = NULL)
```

## Arguments

- obj:

  A sdtm object.

- domain:

  Domains to select, as character. Defaults to all domains, if NULL.

## Value

A data frame with columns DOMAIN and TESTCD. Returns an empty data frame
if no TESTCD columns are found.
