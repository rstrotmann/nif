# Render data frame object to string

This function renders a data.frame into a character vector, similar to
its representation when printed without line numbers

## Usage

``` r
df_preprint(
  df,
  indent = 0,
  n = NULL,
  header = TRUE,
  header_sep = FALSE,
  color = FALSE,
  show_none = FALSE,
  na_string = "NA",
  abbr_lines = NULL,
  abbr_threshold = NULL
)
```

## Arguments

- df:

  The data frame to be rendered.

- indent:

  Indentation level, as numeric.

- n:

  The number of lines to be included, or all if NULL.

- header:

  Boolean to indicate whether the header row is to be included.

- header_sep:

  Show separation line after header, as logical.

- color:

  Print headers in grey as logical.

- show_none:

  Show empty data frame as 'none', as logical.

- na_string:

  String to use for NA values. Defaults to "NA".

- abbr_lines:

  The row number to which long data frames are abbreviated in the ouput
  if the threshold is exceeded. Defaults to nif_option settings if NULL.

- abbr_threshold:

  The row number threshold beyond which long data frames are
  abbreviated. Defaults to nif_option settings if NULL.

## Value

Character vector.
