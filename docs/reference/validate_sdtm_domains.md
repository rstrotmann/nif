# Check whether domains of sdtm object are compliant with SDTM standard

Check whether domains of sdtm object are compliant with SDTM standard

## Usage

``` r
validate_sdtm_domains(sdtm, silent = NULL)
```

## Arguments

- sdtm:

  SDTM object.

- silent:

  Suppress optional messages, as logical. Defaults to global nif_options
  if NULL.

## Value

Invisibly returns TRUE if validation passes, or stops with an error if
required columns are missing.
