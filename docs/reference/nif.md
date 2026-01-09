# nif class constructor

Create an empty nif object or a nif object from sdtm data or from a data
table.

## Usage

``` r
nif(obj = NULL, ..., silent = NULL)
```

## Arguments

- obj:

  A data frame containing the actual NIF data or a sdtm object.

- ...:

  Further arguments.

- silent:

  suppress messages.

## Value

A nif object.

## Details

If no obj argument is provided, an empty, minimal nif object will be
generated. If a data frame is provided for obj, it is converted into a
nif object. Minimally expected fields are ID, TIME, AMT, CMT, EVID and
DV. If ID is missing but USUBJID is available, ID will be derived.

If the input is a sdtm object, a pharmacokinetic nif object is
automatically generated. Analyte mapping formulae can be supplied as the
... argument. For details, see [`nif_auto()`](nif_auto.md).

## See also

[`nif_auto()`](nif_auto.md)

## Examples

``` r
nif()
#> [1] REF  ID   TIME EVID AMT  CMT  DV  
#> <0 rows> (or 0-length row.names)
```
