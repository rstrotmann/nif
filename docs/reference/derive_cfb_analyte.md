# Derive a new analyte with change from baseline from an existing analyte

Derive a new analyte with change from baseline from an existing analyte

## Usage

``` r
derive_cfb_analyte(
  obj,
  source_analyte,
  analyte = NULL,
  baseline_filter = "TIME <= 0",
  summary_function = median,
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- source_analyte:

  The original analyte.

- analyte:

  The name of the derived analyte. Defaults to "CFB_xx" with xx the
  original analyte name.

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

A nif object.
