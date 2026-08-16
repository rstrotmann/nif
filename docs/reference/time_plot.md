# Plot time metrics against each other for observations

Plot time metrics against each other for observations

## Usage

``` r
time_plot(
  obj,
  xtime = "TIME",
  ytime = "TAD",
  analyte = NULL,
  max_time = NULL,
  color = "CHECK",
  ...
)
```

## Arguments

- obj:

  A nif object.

- xtime:

  The x axis time metric as character.

- ytime:

  The y axis time metric as character.

- analyte:

  The analyte as character. Defaults to all, if NULL.

- max_time:

  The maximum x axis time as numeric.

- color:

  The field to color by, as character. Defaults to CHECK.

- ...:

  Further graphical parameters.

## Value

A ggplot object.
