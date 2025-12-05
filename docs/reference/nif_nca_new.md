# Perform Non-Compartmental Analysis (NCA) for each subject and analyte

This function performs NCA analysis using the pknca package for each
subject and analyte in a NIF object. It identifies administrations using
the EVID field and calculates PK parameters for each dosing interval.

## Usage

``` r
nif_nca_new(obj, analytes = NULL, parameters = NULL, keep = NULL)
```

## Arguments

- obj:

  A NIF object containing concentration-time data

- analytes:

  Optional vector of analytes to analyze. If NULL, all analytes will be
  analyzed.

- parameters:

  Optional vector of PK parameters to calculate. If NULL, default
  parameters will be used.

- keep:

  Optional vector of additional columns to keep in the output.

## Value

A long data frame containing NCA results with at least the fields
USUBJID, PPTESTCD, ANALYTE, PPSTRESN

## Examples

``` r
# nca_results <- nif_nca_new(nif_object)
```
