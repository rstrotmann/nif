# Add baseline creatinine

Add baseline creatinine

## Usage

``` r
add_bl_creat(obj, sdtm, baseline_filter = NULL, molar = NULL, silent = NULL)
```

## Arguments

- obj:

  A nif data set.

- sdtm:

  Source sdtm data object.

- baseline_filter:

  The filter term to identify baseline conditions.

- molar:

  Use molar units.

- silent:

  Suppress messages.

## Value

A nif object with the BL_CREAT column added, if possible.
