# Identify subjects with rich sampling

Identify subjects with rich sampling

## Usage

``` r
rich_sampling_sbs(obj, analyte = NA, max_time = NA, n = 4)
```

## Arguments

- obj:

  The NIF dataset.

- analyte:

  The analyte. If the analyte is NA, the most likely will be selected.

- max_time:

  The end of the target interval across which the number of samples is
  determined. If NA, the full treatment interval is selected.

- n:

  The sample number cut-off.

## Value

A list of IDs in numeric format.

## Examples

``` r
rich_sampling_sbs(examplinib_poc_nif, n = 6)
#>  [1]  1 10 11 24 25 35 36 45 56 57 58 75
```
