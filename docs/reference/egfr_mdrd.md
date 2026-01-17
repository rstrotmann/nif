# Glomerular filtration rate estimation from serum creatinine (MDRD)

Source: doi.org/10.7326/0003-4819-130-6-199903160-00002.

## Usage

``` r
egfr_mdrd(crea, age, sex, race = "", weight = NA, molar = FALSE)
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
  occurrence of 'black' in the value.

- weight:

  Body weight. Not used in this formula but included for compatibility.

- molar:

  Switch to select whether the creatinine value is in mg/dl (default) or
  umol/l units.

## Value

Estimated GFR in ml/min/1.73 m^2.

## Details

\$\$ eGFR = 175 \* crea^{1.154} \* age^{0.203} \* 0.742 (female) \*
1.212 (black) \$\$

## See also

[`egfr_raynaud()`](egfr_raynaud.md)

[`egfr_cg()`](egfr_cg.md)
