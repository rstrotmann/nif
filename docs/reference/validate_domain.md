# Check whether fields in domain are compliant with SDTM

Check whether fields in domain are compliant with SDTM

## Usage

``` r
validate_domain(domain, silent = NULL)
```

## Arguments

- domain:

  The SDTM domain as data frame.

- silent:

  Suppress optional messages, as logical. Defaults to global nif_options
  if NULL.

## Value

Invisibly returns TRUE if validation passes, or stops with an error if
required columns are missing.

## Examples

``` r
validate_domain(domain(examplinib_sad, "dm"))
```
