# Glomerular filtration rate estimation from serum creatinine (Cockcroft-Gault)

Reference: Cockcroft and Gault, Nephron 1976,
<https://doi.org/10.1159/000180580>.

## Usage

``` r
egfr_cg(crea, age, sex, race = "", weight = NA, molar = FALSE)
```

## Arguments

- crea:

  Serum creatinine in mg/dl.

- age:

  Age in years.

- sex:

  Sex encoded as number (female is 1) or character (female is "F").

- race:

  Race as per CDISC nomenclature. Black race is identified as the
  occurrence of 'black' in the value. For campatibility onla, not used
  in this formula.

- weight:

  Body weight in kg.

- molar:

  Switch to select whether the creatinine value is in mg/dl (default) or
  umol/l units.

## Value

Estimated GFR in ml/min (as body size is accounted for by the weight in
the input).

## See also

[egfr_raynaud](egfr_raynaud.md)

[`egfr_mdrd()`](egfr_mdrd.md)
