# Glomerular filtration rate estimation from serum creatinine (Raynaud method)

Source: Raynaud M, et al., Race-free estimated glomerular filtration
rate equation in kidney transplant recipients: development and
validation study. BMJ. 2023 May 31;381:e073654,
<https://doi.org/10.1136/bmj-2022-073654>.

## Usage

``` r
egfr_raynaud(crea, age, sex, race = "", weight = NA, molar = FALSE)
```

## Arguments

- crea:

  Serum creatinine in mg/dl or umol/l (if molar=TRUE).

- age:

  Age in years.

- sex:

  Sex encoded as number (male is 0) or character (male is "M").

- race:

  Race. Dummy variable for compatibility, race is not used by this
  method.

- weight:

  Body weight. Not used in this formula but included for compatibility.

- molar:

  Switch to select whether the creatinine value is in mg/dl (default) or
  umol/l units.

## Value

Estimated GFR in ml/min/1.73 m^2.

## Details

\$\$ eGFR = e^{4.43 - 0.82 \* ln(crea) - 0.012 \* crea^2 - 0.006 \*
age + 0.18 \* male} \$\$

## See also

[`egfr_mdrd()`](egfr_mdrd.md)

[`egfr_cg()`](egfr_cg.md)
