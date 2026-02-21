# Load ADaM data sets from disk

Load ADaM data sets from disk

## Usage

``` r
read_adam(data_path, dataset = NULL, format = "sas", delim = ",", ...)
```

## Arguments

- data_path:

  The path to read from.

- dataset:

  The data set names to load, defaults to all, if NULL.

- format:

  The data format, can be one of "sas", "xpt" or "csv."

- delim:

  Delimiter in csv files.

- ...:

  Further arguments to
  [`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html),
  [`haven::read_xpt()`](https://haven.tidyverse.org/reference/read_xpt.html)
  or
  [`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html).

## Value

An adam object.
