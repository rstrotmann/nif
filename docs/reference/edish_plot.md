# Drug-induced serious hepatotoxicity (eDISH) plot

Refer to the FDA guidance on DILI,
https://www.fda.gov/media/116737/download and Watkins 2011,
doi.org/10.2165/11586600-000000000-00000.

## Usage

``` r
edish_plot(
  nif,
  sdtm,
  enzyme = "ALT",
  observation_filter = NULL,
  baseline_filter = "LBBLFL == 'Y'",
  show_labels = FALSE,
  shading = TRUE,
  title = "eDISH plot",
  size = 3,
  alpha = 0.5,
  silent = NULL,
  ...
)
```

## Arguments

- nif:

  A nif object.

- sdtm:

  A sdtm object.

- enzyme:

  The transaminase enzyme to be plotted on the x axis as character, can
  be 'AST' or 'ALT' (default).

- observation_filter:

  A filter term as character.

- baseline_filter:

  A filter term to identify baseline conditions.

- show_labels:

  Show ID labels per point.

- shading:

  Highlight Hy's law area, as logical.

- title:

  The plot title as character.

- size:

  The point size as numeric.

- alpha:

  The alpha value as numeric.

- silent:

  Suppress messages.

- ...:

  Further graphical parameters.

## Value

A ggplot object.
