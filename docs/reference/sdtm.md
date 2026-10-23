# sdtm object constructor

Create an `sdtm` object from a named list of domain data frames. Domain
names are stored in lowercase. The object is that named list (class
`c("sdtm", "list")`); extract a domain with `obj$dm` or
[`domain()`](domain.md).

## Usage

``` r
sdtm(sdtm_data, source = "")
```

## Arguments

- sdtm_data:

  The SDTM domains as a named list of data frames.

- source:

  Source information as character.

## Value

A sdtm object.
