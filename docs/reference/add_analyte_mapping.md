# Attach a treatment-analyte mapping to SDTM object

In some studies, multiple drugs are co-administered, and there may be
analyte data related to different parent drugs. In order to
appropriately correlate observations with administrations, the nif_auto
function needs to know which analyte (`PCTESTCD`) belongs to which drug
(`EXTRT`). Multiple mappings can be provided.

## Usage

``` r
add_analyte_mapping(obj, extrt, pctestcd, analyte = NULL)
```

## Arguments

- obj:

  A SDTM object.

- extrt:

  The treatment as defined in EX.

- pctestcd:

  The analyte as character (as in 'PCTESTCD' for PK observations

- analyte:

  The analyte name to be used in the nif object, as character.

## Value

A SDTM object.
