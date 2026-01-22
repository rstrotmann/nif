# Add baseline hepatic function class

Based on the NCI ODWG criteria with TB the total (direct and indirect)
serum bilirubin, and AST aspartate aminotransferase.

## Usage

``` r
add_bl_odwg(
  obj,
  sdtm,
  observation_filter = NULL,
  baseline_filter = NULL,
  summary_function = mean,
  silent = NULL
)
```

## Arguments

- obj:

  A nif object.

- sdtm:

  The corresponding sdtm object.

- observation_filter:

  The filter term for the observation source data, as character.

- baseline_filter:

  A filter term to identify the baseline condition, as character.

- summary_function:

  The summary function to summarize multiple baseline values. Defaults
  to `mean`

- silent:

  Suppress messages.

## Value

A nif object.

## Details

- normal: TB & AST \<= upper limit of normal (ULN)

- mild hepatic dysfunction: TB \> ULN to 1.5 x ULN or AST \> ULN

- moderate hepatic dysfunction: TB \>1.5–3 x ULN, any AST

- severe hepatic dysfunction: TB \>3 - 10 x ULN, any AST
