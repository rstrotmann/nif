# Add administration events

Add rows to a [nif](nif.md) object that represent drug administration
events (EVID of 1) This is usually the first step in the stepwise
creation of NIF data tables.

## Usage

``` r
add_administration(
  nif,
  sdtm,
  extrt,
  analyte = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = NULL,
  debug = FALSE,
  silent = NULL
)
```

## Arguments

- nif:

  A nif object.

- sdtm:

  A sdtm object.

- extrt:

  The EXTRT for the administration, as character.

- analyte:

  The name of the analyte as character.

- cmt:

  The compartment for the administration as numeric.

- subject_filter:

  The filtering to apply to the DM domain, as string,

- cut_off_date:

  The data cut-off date as Posix date-time or character.

- keep:

  Columns to keep after cleanup, as character.

- debug:

  Include debug fields, as logical.

- silent:

  Suppress messages, defaults to nif_option standard when NULL.

## Value

A nif object.

## Details

Drug administration data is taken from the EX domain of the source
[sdtm](sdtm.md) object. The `extrt` argument specifies the drug name as
represented in EX. By default, this will also be the value of the
'ANALYTE' column for the respective rows in the resulting [nif](nif.md)
object. Alternatively, a custom `analyte` name can be explicitly
provided, e.g., to match with the 'ANALYTE' name of the corresponding
pharmacokinetic observations.

For administrations, a model compartment of 1 is selected by default and
will be the corresponding value of the 'CMT' column. A different
compartment can be explicitly specified by the `cmt` argument.

For an overview on the representation of administration events in NONMEM
Input Format compliant data sets, see: Bauer, R.J. (2019), NONMEM
Tutorial Part I: Description of Commands and Options, With Simple
Examples of Population Analysis. CPT Pharmacometrics Syst. Pharmacol.,
8: 525-537. doi.org/10.1002/psp4.12404
[doi:10.1086/723805](https://doi.org/10.1086/723805) .

To add observation events to the [nif](nif.md) object, see
[`add_observation()`](add_observation.md).

## Examples

``` r
add_administration(nif(), examplinib_sad, "EXAMPLINIB") |>
head()
#> ℹ A global cut-off-date of 2001-02-23 11:31:00 was automatically assigned!
#> ℹ Analyte EXAMPLINIB not found in PCTESTCD
#> Administrations times for EXAMPLINIB cannot be derived from PCRFDTC and will be
#> taken from EXSTDTC/EXENDTC!
#>   REF ID    STUDYID           USUBJID AGE SEX                      RACE HEIGHT
#> 1   1  1 2023000001 20230000011010001  43   0                     WHITE  187.4
#> 2   2  2 2023000001 20230000011010002  49   0                     WHITE  189.6
#> 3   3  3 2023000001 20230000011010003  46   0 BLACK OR AFRICAN AMERICAN  174.6
#> 4   4  4 2023000001 20230000011010005  23   0                     WHITE  168.2
#> 5   5  5 2023000001 20230000011010006  47   0 BLACK OR AFRICAN AMERICAN  174.1
#> 6   6  6 2023000001 20230000011010007  31   0                     WHITE  172.5
#>   WEIGHT      BMI                 DTC TIME NTIME TAFD TAD EVID AMT    ANALYTE
#> 1   77.0 21.92560 2000-12-31 10:18:00    0     0    0   0    1   5 EXAMPLINIB
#> 2   72.8 20.25138 2000-12-29 10:30:00    0     0    0   0    1   5 EXAMPLINIB
#> 3   80.0 26.24228 2000-12-29 09:22:00    0     0    0   0    1   5 EXAMPLINIB
#> 4   78.8 27.85314 2001-01-02 09:22:00    0     0    0   0    1  10 EXAMPLINIB
#> 5   89.5 29.52742 2001-01-03 12:24:00    0     0    0   0    1  10 EXAMPLINIB
#> 6   90.0 30.24575 2001-01-02 10:00:00    0     0    0   0    1  10 EXAMPLINIB
#>   CMT     PARENT TRTDY METABOLITE DOSE MDV ACTARMCD IMPUTATION DV
#> 1   1 EXAMPLINIB     1      FALSE    5   1       C1            NA
#> 2   1 EXAMPLINIB     1      FALSE    5   1       C1            NA
#> 3   1 EXAMPLINIB     1      FALSE    5   1       C1            NA
#> 4   1 EXAMPLINIB     1      FALSE   10   1       C2            NA
#> 5   1 EXAMPLINIB     1      FALSE   10   1       C2            NA
#> 6   1 EXAMPLINIB     1      FALSE   10   1       C2            NA
```
