# Generate NCA table from the SDTM.PP domain

Generate NCA table from the SDTM.PP domain

## Usage

``` r
nca_from_pp(
  obj,
  sdtm_data,
  analyte = NULL,
  ppcat = NULL,
  ppscat = NULL,
  keep = NULL,
  group = NULL,
  observation_filter = "TRUE",
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- sdtm_data:

  A SDTM data object containing a PP domain.

- analyte:

  The analyte as character. If NULL, will be guessed from the nif
  object.

- ppcat:

  The value for PPCAT (Test category) to filter the PP domain for. If
  NULL, no filtering is done.

- ppscat:

  The value for PPSCAT (Test subcategory) to filter the PP domain for.
  If NULL, no filtering is done.

- keep:

  Columns to keep from the input nif object, as character.

- group:

  Grouping variable from the pp domain, as character.

- observation_filter:

  Observation filter term as character. Must be valid R code that can be
  evaluated on the PP domain.

- silent:

  Suppress message output.

## Value

A data frame containing the filtered and joined PP domain data.
