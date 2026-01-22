# Lean body mass (Peters formula)

Source: Peters AM, Snelling HL, Glass DM, Bird NJ. Estimation of lean
body mass in children. Br J Anaesth. 2011 May;106(5):719-23,
doi.org/10.1093/bja/aer057. PMID: 21498495.

## Usage

``` r
lbm_peters(weight, height, sex)
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

[`lbm_hume()`](lbm_hume.md)
