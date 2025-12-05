# Flag time window violations

**\[experimental\]**

.BEFORE and .AFTER are inclusive!

## Usage

``` r
add_time_window_flag(
  obj,
  window,
  analyte = NULL,
  use_minutes = TRUE,
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- window:

  Time window definition as data frame.

- analyte:

  The analyte to apply the time window to.

- use_minutes:

  time window is given in minutes rather than hours, defaults to TRUE.

- silent:

  Suppress messages, as logical.

## Value

A nif object with TIME_DEV, EXCL, EXCL_REASON fields added.
