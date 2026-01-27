# Serum creatinine estimation from eGFR (MDRD)

Inverse of the function published in
<doi:10.7326/0003-4819-130-6-199903160-00002>.

## Usage

``` r
crea_mdrd(egfr, age, sex, race = "")
```

## Arguments

- egfr:

  EGFR in ml/min/1.73 m^2.

- age:

  Age in years.

- sex:

  Sex encoded as number (female is 1) or character (female is "F").

- race:

  Race as per CDISC nomenclature. Black race is identified as the
  occurrence of 'black' in the value.

## Value

Serum creatinine in mg/dl.

## Details

To convert crea from mg/dl to umol/l, multiply by 88.4.

## See also

[`crea_raynaud()`](crea_raynaud.md)
