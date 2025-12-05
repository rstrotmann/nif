# Calculate SLD for SDTM.TR domain

**\[experimental\]**

## Usage

``` r
derive_sld(
  sdtm_obj,
  testcd = "DIAMETER",
  group = c("TRMETHOD", "TRGRPID"),
  observation_filter = "TRGRPID == 'TARGET'"
)
```

## Arguments

- sdtm_obj:

  A SDTM object.

- testcd:

  The TRTESTCD to select for SLD calculation, as character.

- group:

  Grouping variables, as character.

- observation_filter:

  A filter term, as character.

## Value

A SDTM object.

## Details

Calculate the sum of longest diameters (SLD) and number of lesions by
subject and time point, and add as rows. If a SDTM.TR domain is not
included in the input SDTM object, the SDTM object is returned as-is.

literature: https://www.lexjansen.com/phuse/2018/ds/DS03.pdf
