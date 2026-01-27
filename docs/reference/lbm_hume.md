# Lean body mass (Hume formula)

Source: Hume R. Prediction of lean body mass from height and weight. J
Clin Pathol. 1966 Jul;19(4):389-91, <doi:10.1136/jcp.19.4.389>.

## Usage

``` r
lbm_hume(weight, height, sex)
```

## Arguments

- weight:

  Body weight in kg, as numeric.

- height:

  Body height in cm, as numeric.

- sex:

  Sex encoded as number (male is 0) or character (male is "M").

## Value

Lean body mass in kg, as numeric. Returns NA for invalid inputs.

## See also

[`lbm_boer()`](lbm_boer.md)

[`lbm_peters()`](lbm_peters.md)
