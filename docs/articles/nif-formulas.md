# Data definition using formulas

``` r
library(dplyr)
library(nif)

nif_option(silent = TRUE)
```

## INTRODUCTION

The ‘nif’ package is mean to facilitate the creation of NONMEM Input
Format (NIF)- compliant data files for the analysis of longitudinal
(time-varying) clinical data. For a gentle introduction, see
`vignette("nif-tutorial")`.

The overarching concept of the ‘nif’ package is to gradually build a
data set, sequentially adding administrations, observations, baseline
data and covariates. A typical workflow may look like this:

``` r
sdtm <- examplinib_sad

nif <- nif() %>%
  add_administration(sdtm, "EXAMPLINIB") %>%
  add_observation(sdtm, "pc", "RS2023") %>%
  add_observation(sdtm, "pc", "RS2023487A") %>%
  add_baseline(sdtm, "lb", "CREAT")
```

The only time-varying data in this NIF data set are now the two
pharmacokinetic analytes, ‘RS2023’ and ‘RS2023487A’. Real clinical data
typically include many time-varying observations, and for PK/PD
analyses, and we might be interested in adding further observation
events to the nif data set.

We can get an overview on all observation events included in the SDTM
data like so:

``` r
testcd(sdtm)
#>    DOMAIN     TESTCD
#> 1      VS     HEIGHT
#> 2      VS     WEIGHT
#> 3      PC     RS2023
#> 4      PC RS2023487A
#> 5      LB      CREAT
#> 6      PP     AUCLST
#> 7      PP       CMAX
#> 8      PP       TMAX
#> 9      PP       TLST
#> 10     PP       LAMZ
#> 11     PP      R2ADJ
#> 12     PP    LAMZNPT
#> 13     PP     LAMZHL
#> 14     PP     AUCIFP
```

Likewise, we can get an overview on the treatments included in the SDTM
data with:

``` r
treatments(sdtm)
#> [1] "EXAMPLINIB"
```

## NIF FORMULAE

While the code for building a NIF data set that is shown above follows a
clear logic, sequentially addition data components, it may be sometimes
preferable to use more compact code. To this end, the ‘nif’ package
allows the definition of the data sources for an analysis data set using
*formulae*:

``` r
nif <- nif_auto(sdtm, RS2023 + RS2023487A ~ EXAMPLINIB)
```

The formulae provided as arguments to
[`nif_auto()`](../reference/nif_auto.md) follow a consistent notation.
Observations are on the left hand side, and treatments (drug
administrations) are on the right hand side of the formula:

`analyte1 + analyte2 ~ treatment`

The number of analytes on the left hand side is not limited, but the
right hand side of the formula must specify a single administration that
the observations relate to.

We have specified two pharmacokinetic observations (‘RS2023’ and
‘RS2023487A’). However observations may be of any kind, e.g., from the
clinical lab (LB), vital sign (VS), ECG (EG), or any other SDTM domain.

Note that the above code includes only one formula, but if there are
multiple drugs administered with associated observations, multiple
formulae can be provided as arguments to
[`nif_auto()`](../reference/nif_auto.md).

Inspecting the resulting nif object, we can see that
[`nif_auto()`](../reference/nif_auto.md) has also added baseline serum
creatinine data and derived the baseline creatinine clearance and renal
function category from it:

``` r
head(nif, 3)
#>   REF ID    STUDYID           USUBJID AGE SEX  RACE HEIGHT WEIGHT     BMI
#> 1   1  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 2   2  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#> 3   3  1 2023000001 20230000011010001  43   0 WHITE  187.4     77 21.9256
#>                   DTC TIME NTIME TAFD TAD EVID AMT    ANALYTE CMT PARENT TRTDY
#> 1 2000-12-31 10:18:00    0     0    0   0    1   5     RS2023   1 RS2023     1
#> 2 2000-12-31 10:18:00    0     0    0   0    0   0     RS2023   2 RS2023     1
#> 3 2000-12-31 10:18:00    0     0    0   0    0   0 RS2023487A   3 RS2023     1
#>   METABOLITE DOSE MDV ACTARMCD IMPUTATION DV BL_CREAT BL_CRCL BL_RENAL
#> 1      FALSE    5   1       C1            NA  67.4825 1.53723   severe
#> 2      FALSE    5   0       C1             0  67.4825 1.53723   severe
#> 3       TRUE    5   0       C1             0  67.4825 1.53723   severe
```

[`nif_auto()`](../reference/nif_auto.md) will further attempt to infer
the baseline hepatic function category as per the NCI ODWG
classification from baseline bilirubin and AST data. In this example
SDTM data set, these parameters are not part of the LB domain, though.
