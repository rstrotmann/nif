# Filter out any rows containing DTC rows with incomplete data format as per ISO 8601.

NA values and empty strings are preserved.

## Usage

``` r
filter_correct_date_format(obj, verbose = TRUE, silent = NULL)
```

## Arguments

- obj:

  The SDTM domain as data frame.

- verbose:

  Boolean to indicate whether to include details.

- silent:

  Suppress messages, defaults to nif_option settin, if NULL.

## Value

The filtered SDTM domain as data frame.
