# Sort nif object and add REF field

The input data format expected by NONMEM requires all rows ordered by ID
and TIME, and indexed sequentially on a subject level with a REF field.
Re-indexing may be required if a NIF object is extended, e.g., by
merging in further data.

## Usage

``` r
limit(obj, individual = TRUE, keep_no_obs_sbs = FALSE)
```

## Arguments

- obj:

  A nif object.

- individual:

  Apply by ID, as logical.

- keep_no_obs_sbs:

  Retain subjects without observations.

## Value

A nif object.

## Details

Subset nif to rows with DTC before the last individual or global
observation
