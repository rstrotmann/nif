# Attach a parent-metabolite mapping to a SDTM object

In case multiple analytes are measured for a specific administered drug,
some functions need that information to correlate plasma concentrations
with administrations.

## Usage

``` r
add_metabolite_mapping(obj, pctestcd_parent, pctestcd_metabolite)
```

## Arguments

- obj:

  The SDTM object.

- pctestcd_parent:

  The PCTESTCD of the parent compound.

- pctestcd_metabolite:

  The PCTESTCD of the metabolite.

## Value

The SDTM object.

## See also

[`nif_auto()`](nif_auto.md)
