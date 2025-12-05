# Add baseline and change from baseline fields

**\[deprecated\]**

## Usage

``` r
legacy_add_cfb(
  obj,
  baseline_filter = "TIME <= 0",
  summary_function = median,
  silent = NULL
)
```

## Arguments

- obj:

  A NIF object.

- baseline_filter:

  A filter term to identify the baseline condition.

- summary_function:

  The function to derive the baseline. This function is applied over the
  DV values identified by the 'baseline_filter' term. The default
  function is `median`. Alternatively, `mean`, `min` or `max` can be
  considered.

- silent:

  Suppress messages, defaults to nif_option setting if NULL.

## Value

A NIF object

## Details

FOR DELETION

Output fields:

- `DVBL` Baseline value for the dependent variable DV.

- `DVCFB` Change from baseline for the dependent variable DV.

The Baseline is calculated as the median (or the summary function
output) of the DV field for all time points identified by the
baseline_filter' term.
