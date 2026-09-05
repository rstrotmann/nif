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
  pctestcd = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = NULL,
  debug = FALSE,
  imputation = imputation_rules_standard,
  duration = NULL,
  iv_admin = NULL,
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

- pctestcd:

  The PCTESTCD of the pharmacokinetic analyte corresponding to the
  administered drug. This is needed when administration times are
  imputed from the PCRFTDTC field from the PC domain.

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

- imputation:

  The imputation rule set.

- duration:

  The duration of the iv administration as numeric. If NULL, will be
  determined automatically based on the EXDUR field, if available.

- iv_admin:

  Administration is iv, as boolean. If NULL, will be tested
  automatically based on the EXROUTE field, if available.

- silent:

  Suppress messages, defaults to nif_option standard, if NULL.

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
8: 525-537 [doi:10.1002/psp4.12404](https://doi.org/10.1002/psp4.12404)
.

To add observation events to the [nif](nif.md) object, see
[`add_observation()`](add_observation.md).

## Examples

``` r
add_administration(nif(), examplinib_sad, "EXAMPLINIB")
#> ℹ Imputation model 'imputation_rules_standard' applied to administration of EXAMPLINIB
#> ℹ A global cut-off-date of 2001-02-23 11:31:00 was automatically assigned!
#> ──────── NONMEM Input Format (NIF) data ────────
#> 0 observations from 48 subjects across 1 study 
#> 
#> # A tibble: 48 × 27
#>      REF    ID STUDYID    USUBJID             AGE   SEX
#>    <int> <dbl> <chr>      <chr>             <dbl> <dbl>
#>  1     1     1 2023000001 20230000011010001    43     0
#>  2     2     2 2023000001 20230000011010002    49     0
#>  3     3     3 2023000001 20230000011010003    46     0
#>  4     4     4 2023000001 20230000011010005    23     0
#>  5     5     5 2023000001 20230000011010006    47     0
#>  6     6     6 2023000001 20230000011010007    31     0
#>  7     7     7 2023000001 20230000011010008    53     0
#>  8     8     8 2023000001 20230000011010010    40     0
#>  9     9     9 2023000001 20230000011010012    47     0
#> 10    10    10 2023000001 20230000011010013    40     0
#>    RACE                      HEIGHT WEIGHT   BMI DTC                  TIME NTIME
#>    <fct>                      <dbl>  <dbl> <dbl> <dttm>              <dbl> <dbl>
#>  1 WHITE                       187.   77    21.9 2000-12-31 10:18:00     0     0
#>  2 WHITE                       190.   72.8  20.3 2000-12-29 10:30:00     0     0
#>  3 BLACK OR AFRICAN AMERICAN   175.   80    26.2 2000-12-29 09:22:00     0     0
#>  4 WHITE                       168.   78.8  27.9 2001-01-02 09:22:00     0     0
#>  5 BLACK OR AFRICAN AMERICAN   174.   89.5  29.5 2001-01-03 12:24:00     0     0
#>  6 WHITE                       172.   90    30.2 2001-01-02 10:00:00     0     0
#>  7 WHITE                       175.   81.5  26.7 2001-01-01 10:34:00     0     0
#>  8 BLACK OR AFRICAN AMERICAN   183.   77.6  23.2 2001-01-03 09:29:00     0     0
#>  9 WHITE                       190    76.3  21.1 2001-01-07 11:26:00     0     0
#> 10 WHITE                       173.   77.9  25.9 2001-01-13 09:30:00     0     0
#>     TAFD   TAD  EVID   AMT   CMT    DV ANALYTE    PARENT     TRTDY METABOLITE
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>      <chr>      <dbl> <lgl>     
#>  1     0     0     1     5     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  2     0     0     1     5     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  3     0     0     1     5     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  4     0     0     1    10     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  5     0     0     1    10     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  6     0     0     1    10     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  7     0     0     1    20     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  8     0     0     1    20     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>  9     0     0     1    20     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#> 10     0     0     1    50     1    NA EXAMPLINIB EXAMPLINIB     1 FALSE     
#>     DOSE   MDV ACTARMCD IMPUTATION              
#>    <dbl> <dbl> <chr>    <chr>                   
#>  1     5     1 C1       time copied from EXSTDTC
#>  2     5     1 C1       time copied from EXSTDTC
#>  3     5     1 C1       time copied from EXSTDTC
#>  4    10     1 C2       time copied from EXSTDTC
#>  5    10     1 C2       time copied from EXSTDTC
#>  6    10     1 C2       time copied from EXSTDTC
#>  7    20     1 C3       time copied from EXSTDTC
#>  8    20     1 C3       time copied from EXSTDTC
#>  9    20     1 C3       time copied from EXSTDTC
#> 10    50     1 C4       time copied from EXSTDTC
#> # ℹ 38 more rows
```
