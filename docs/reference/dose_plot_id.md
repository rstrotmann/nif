# Plot dose time course data from individual subject

This function plots AMT over TIME for an individual subject, id. Id can
be either the ID or the USUBJID. Administration time points are
indicated with vertical lines.

## Usage

``` r
dose_plot_id(
  obj,
  id,
  y_scale = "lin",
  max_dose = NA,
  time_field = "TIME",
  point_size = 2,
  max_time = NA,
  analyte = NULL,
  ...
)
```

## Arguments

- obj:

  The NIF object.

- id:

  The subject ID to be plotted.

- y_scale:

  Y-scale. Use 'scale="log"' for a logarithmic y scale. Default is
  "lin".

- max_dose:

  The upper limit of the dose scale.

- time_field:

  The field to use as the time metric, as character.

- point_size:

  The point size as numeric.

- max_time:

  The right limit of the time scale.

- analyte:

  The analyte of interest.

- ...:

  Further graphical parameters.

## Value

A ggplot object.

## Examples

``` r
dose_plot_id(examplinib_poc_nif, 18)

dose_plot_id(examplinib_poc_nif, dose_red_sbs(examplinib_poc_nif)[1, 1])

dose_plot_id(examplinib_poc_min_nif, 18)
```
