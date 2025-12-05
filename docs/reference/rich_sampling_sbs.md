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
#>  [1]  1  4  5 15 28 29 40 50 51 52 63 64
```
