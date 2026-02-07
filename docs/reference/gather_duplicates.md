# Consolidate multiplicate observations

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

  The field(s) over which to identify duplicates (in addition to ID
  ANALYTE and CMT which are automatically considered).

- duplicate_function:

  A function by which to consolidate the multiplicates

- na_rm:

  Remove NA values.

- silent:

  Suppress messages.

## Value

A nif object.

## Details

Identify multiplicate observations over ID, CMT and 'fields' and
integrate the DV value for them using the 'duplicate_function'. A common
application is the averaging of triplicate ECG parameter observations by
NTIME.

Observations with MDV == 1 are excluded from the process.
