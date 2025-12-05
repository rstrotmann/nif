# Drug-induced serious hepatotoxicity (eDISH) plot

Refer to the [FDA guidance on Drug-induced liver
injury](https://www.fda.gov/media/116737/download) and [Watkins
2011](https://doi.org/10.2165/11586600-000000000-00000).

## Usage

``` r
edish_plot(
  nif,
  sdtm,
  enzyme = "ALT",
  observation_filter = "LBSPEC != 'URINE'",
  show_labels = FALSE,
  autoscale = TRUE,
  shading = TRUE,
  nominal_time = TRUE,
  ntime_method = NULL,
  time = NULL,
  parent = NULL,
  title = "eDISH plot: All time points",
  size = 3,
  alpha = 0.5,
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

- show_labels:

  Show ID labels per point.

- autoscale:

  Use automatic axis scaling, as logical. Defaults to 0.01-1000 for
  ALT/AST and 0.01-100 for bili.

- shading:

  Highlight Hy's law area, as logical.

- nominal_time:

  Use NTIME as logical.

- ntime_method:

  the field to derive the nominal time from. Allowed values are "TPT"
  and "ELTM".Defaults to xxTPT where xx is the domain name, if NULL.

- time:

  time/nominal time filter as numeric.

- parent:

  The parent compound as character.

- title:

  The plot title as character.

- size:

  The point size as numeric.

- alpha:

  The alpha value as numeric.

- ...:

  Further graphical parameters.

## Value

A ggplot object.
