# Plot DV time course data from individual subject

This function plots DV over TIME for an individual subject, id. Id can
be either the ID or the USUBJID. Administration time points are
indicated with vertical lines.

## Usage

``` r
nif_plot_id(
  obj,
  id,
  analyte = NULL,
  cmt = NULL,
  time_field = "TIME",
  max_time = NA,
  lines = TRUE,
  point_size = 2,
  log = FALSE,
  imp = NULL,
  ...
)
```

## Arguments

- obj:

  The NIF object

- id:

  The subject ID to be plotted

- analyte:

  The analytes to be displayed. Defaults to NULL (all).

- cmt:

  The compartment to plot as numeric.

- time_field:

  The field to use as the time metric, as character.

- max_time:

  The right limit of the time scale

- lines:

  Plot lines as logical.

- point_size:

  Point size as numeric.

- log:

  Logarithmic y scale.

- imp:

  The IMP for which administrations are to be indicated by vertical
  lines. Defaults to NULL.

- ...:

  Further graphical parameters.

## Value

A ggplot2 object.

## Examples

``` r
nif_plot_id(examplinib_poc_nif, 1)

nif_plot_id(examplinib_poc_min_nif, 1, log = TRUE)

nif_plot_id(examplinib_poc_nif, 1, log = TRUE)

nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023")

nif_plot_id(examplinib_poc_nif, 1, analyte="RS2023", tad = TRUE)

nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")

nif_plot_id(examplinib_poc_nif, "20230000221010001", analyte="RS2023")

nif_plot_id(examplinib_poc_nif, 8, analyte="RS2023", imp="RS2023")

nif_plot_id(examplinib_poc_nif, 8, analyte=c("RS2023", "RS2023487A"))

nif_plot_id(examplinib_poc_min_nif, 1, analyte="CMT3")

nif_plot_id(examplinib_poc_min_nif, 1, tad=TRUE)
```
