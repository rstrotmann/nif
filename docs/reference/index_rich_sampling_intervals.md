# Identify and index rich PK sampling intervals

Currently experimental. Don't use in production!

## Usage

``` r
index_rich_sampling_intervals(obj, analyte = NULL, min_n = 4)
```

## Arguments

- obj:

  The NIF object.

- analyte:

  The analyte as character. If `NA` (default), the most likely will be
  selected automatically.

- min_n:

  The minimum number of PK samples per analyte to qualify as rich
  sampling.

## Value

A new NIF object.

## Details

Adds the fields `DI` (dosing interval per analyte), `RICHINT` (rich
sampling interval), and `RICH_N` (index of the rich sampling interval by
analyte).

This function identifies rich sampling intervals by the number of
observations that follow an administration. A number of `min_n` or more
observations before the next administration is interpreted as a rich
sampling interval and the corresponding observations are flagged with
`RICHINT` == TRUE. The index of the rich sampling intervals per subject
and analyte is reported in the `RICH_N` field.
