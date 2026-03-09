# Consolidate multiplicate observations in a nif object

**\[experimental\]**

## Usage

``` r
gather_duplicates(
  obj,
  id_field = "NTIME",
  duplicate_function = mean,
  na_rm = TRUE,
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- id_field:

  Character vector of additional field(s) used (together with ID, CMT,
  AMT, EVID, and ANALYTE) to define duplicate groups. Defaults to
  "NTIME".

- duplicate_function:

  A function applied to numeric columns within each duplicate group
  (e.g., `mean`, `median`, `sum`). Defaults to `mean`.

- na_rm:

  Logical. Remove NA values before applying `duplicate_function`?
  Defaults to TRUE.

- silent:

  Logical or NULL. Suppress informational messages? NULL uses the
  package default.

## Value

A nif object with multiplicate observations consolidated.

## Details

Collapses replicate measurements (e.g., triplicate ECG readings) that
share the same subject, analyte, compartment, and nominal time point
into a single row by applying `duplicate_function` to the dependent
variable and time-related fields.

Baseline covariates are preserved safely: they are separated before
aggregation and re-joined by ID afterward, so they are never set to NA
due to row-level inconsistencies.

Duplicate groups are defined by ID, CMT, AMT, EVID, ANALYTE, and
`id_field`. The aggregation function is applied to DV, TIME, NTIME, TAD,
and TAFD (except those used as grouping fields). Rows with MDV == 1 are
excluded before processing.
