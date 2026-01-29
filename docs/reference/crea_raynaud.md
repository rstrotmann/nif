# Serum creatinine estimation from eGFR (Raynaud method)

Inverse of the function published in
[doi:10.1136/bmj-2022-073654](https://doi.org/10.1136/bmj-2022-073654) .

## Usage

``` r
crea_raynaud(egfr, age, sex, race = "")
```

## Arguments

- egfr:

  EGFR in ml/min/1.73 m^2.

- age:

  Age in years.

- sex:

  Sex encoded as number (male is 0) or character (male is "M").

- race:

  Race. Dummy variable for compatibility, race is not used by this
  method.

## Value

Serum creatinine in mg/dl.

## Details

To convert crea from mg/dl to umol/l, multiply by 88.4.

## See also

[`crea_mdrd()`](crea_mdrd.md)
