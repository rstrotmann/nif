# Plot analyte over time by discretized time after first dose (QTIME)

**\[experimental\]**

## Usage

``` r
qtime_plot(
  obj,
  analyte,
  breaks,
  points = FALSE,
  caption = TRUE,
  size = 1.5,
  alpha = 1
)
```

## Arguments

- obj:

  A nif object.

- analyte:

  The analyte as character.

- breaks:

  The breaks for TAFD, as numeric.

- points:

  Plot original data points as logical.

- caption:

  Show caption as logical.

- size:

  The point size.

- alpha:

  The alpha parameter for the data points.

## Value

A ggplot object

## Details

First, individual DV values are summarized by analyte over the QTIME
periods, then those mean values are summarized over all subjects. The
mean and its 90% CI shown in the figure refers to the latter summary.
