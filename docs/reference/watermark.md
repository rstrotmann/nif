# Add a watermark annotation layer for a ggplot2 object

Add a watermark annotation layer for a ggplot2 object

## Usage

``` r
watermark(
  watermark_text = NULL,
  cex = 1.5,
  fontface = "bold",
  color = "lightgrey",
  alpha = 0.1,
  x = 0.5,
  y = 1,
  rotation = 0
)
```

## Arguments

- watermark_text:

  The watermark as character. If NULL, uses the value from
  nif_option("watermark").

- cex:

  The text size multiplier as numeric. Must be positive.

- fontface:

  Font face ("bold" by default). Must be one of "plain", "bold",
  "italic", "bold.italic".

- color:

  The color of the watermark text (default: "lightgrey").

- alpha:

  The transparency of the watermark (default: 0.1).

- x:

  The x position of the watermark (default: 0.5, centered).

- y:

  The y position of the watermark (default: 1, top).

- rotation:

  The rotation angle in degrees (default: 0).

## Value

A ggplot2 annotation layer with the watermark, or NULL if watermark_text
is empty.
