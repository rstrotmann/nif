# Make AE observation

This function adds AE events as observations with the time of the AE
onset as the observation time point and the severity grade as the
dependent variable if the field AETOXGR is included. If AETOXGR is not
included in the AE domain, a coding table must be provided to translate
any other field into a numeric DV column, e.g.,
`coding_table = data.frame( AESEV = c("MILD", "MODERATE", "SEVERE"), DV = c(1, 2, 3)`

## Usage

``` r
make_ae(
  sdtm,
  ae_term,
  ae_field = "AEDECOD",
  analyte = NULL,
  parent = "",
  cmt = NA,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  observation_filter = "TRUE",
  coding_table = NULL,
  keep = NULL
)
```

## Arguments

- sdtm:

  A sdtm object.

- ae_term:

  The AE term as character.

- ae_field:

  The field in which the AE term is specified in. Can be:

  - AEDECOD: Dictionary-derived term

  - AELLT: Lowest-level term

  - AEHLT: High-level term

  - AEHLGT: High-level group term

  - AESOC: System organ class

  - AEBODSYS: Body system or organ class

  or any other field from the 'AE' domain

- analyte:

  The name for the AE observation, defaults to 'AE_xx' with xx the
  ae_term.

- parent:

  The parent compound as character.

- cmt:

  The compartment as numeric.

- subject_filter:

  A subject filter term.

- observation_filter:

  An observation filter term.

- coding_table:

  A DV coding table as data frame. The coding table translates columns
  included in the AE domain into a DV field.

- keep:

  Columns to keep, as character.

## Value

A nif object.
