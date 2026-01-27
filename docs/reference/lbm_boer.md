# Lean body mass (Boer formula)

Source: Caruso D, et al., Lean Body Weight-Tailored Iodinated Contrast
Injection in Obese Patient: Boer versus James Formula. Biomed Res Int.
2018 Aug 13;2018:8521893, <doi:10.1155/2018/8521893>.

## Usage

``` r
lbm_boer(weight, height, sex)
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

## Details

The Boer formula is one of the most commonly used formulas for
estimating lean body mass. It provides separate equations for males and
females based on height and weight measurements.

## See also

[`lbm_hume()`](lbm_hume.md)

[`lbm_peters()`](lbm_peters.md)
