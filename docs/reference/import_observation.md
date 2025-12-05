# Add observation from non-SDTM-formatted data table

**\[experimental\]**

## Usage

``` r
import_observation(
  nif,
  raw,
  analyte,
  parent = NULL,
  cmt = NULL,
  observation_filter = "TRUE",
  USUBJID_field = "USUBJID",
  DTC_field = NULL,
  NTIME_field = NULL,
  DV_field = NULL,
  keep = NULL,
  debug = FALSE,
  silent = NULL
)
```

## Arguments

- nif:

  A nif object.

- raw:

  The raw observation data frame.

- analyte:

  The analyte name as character.

- parent:

  The parent as character.

- cmt:

  The compartment for the analyte as numeric.

- observation_filter:

  Filter term, as character.

- USUBJID_field:

  The field specifying the USUBJID, as character.

- DTC_field:

  The field specifying the DTC, as character.

- NTIME_field:

  The field specifying the NTIME, as character.

- DV_field:

  The field specifying the dependent variable, as character.

- keep:

  Columns to keep, as character.

- debug:

  Keep debug information.

- silent:

  Suppress messages, defaults to nif_option setting, if NULL.

## Value

A nif object.
