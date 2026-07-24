# Impute missing baseline values

Fill individual missing baseline values, and replace missing baseline
values on subject level with the population center.

## Usage

``` r
impute_missing_baseline(
  nif,
  baseline_fields = NULL,
  summary_function = median,
  silent = NULL
)
```

## Arguments

- nif:

  The input nif object.

- baseline_fields:

  Baseline fields to impute, as character. Will be set to default values
  (WEIGHT, HEIGHT, BMI, other fields starting with BL\_), if NULL.

- summary_function:

  A function to determine the population center value of each baseline.
  Defaults to median.

- silent:

  Suppress messages.

## Value

The nif object with missing baseline values imputed to the respective
population center value.
