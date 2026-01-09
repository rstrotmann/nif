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
8: 525-537. <https://doi.org/10.1002/psp4.12404>.

To add observation events to the [nif](nif.md) object, see
[`add_observation()`](add_observation.md).

## Examples

``` r
add_administration(nif(), examplinib_sad, "EXAMPLINIB")
#> ℹ A global cut-off-date of 2001-02-23 11:31:00 was automatically assigned!
#> ℹ Analyte EXAMPLINIB not found in PCTESTCD
#> Administrations times for EXAMPLINIB cannot be derived from PCRFDTC and will be
#> taken from EXSTDTC/EXENDTC!
#>    REF ID    STUDYID           USUBJID AGE SEX                      RACE HEIGHT
#> 1    1  1 2023000001 20230000011010001  43   0                     WHITE  187.4
#> 2    2  2 2023000001 20230000011010002  49   0                     WHITE  189.6
#> 3    3  3 2023000001 20230000011010003  46   0 BLACK OR AFRICAN AMERICAN  174.6
#> 4    4  4 2023000001 20230000011010005  23   0                     WHITE  168.2
#> 5    5  5 2023000001 20230000011010006  47   0 BLACK OR AFRICAN AMERICAN  174.1
#> 6    6  6 2023000001 20230000011010007  31   0                     WHITE  172.5
#> 7    7  7 2023000001 20230000011010008  53   0                     WHITE  174.6
#> 8    8  8 2023000001 20230000011010010  40   0 BLACK OR AFRICAN AMERICAN  182.8
#> 9    9  9 2023000001 20230000011010012  47   0                     WHITE  190.0
#> 10  10 10 2023000001 20230000011010013  40   0                     WHITE  173.4
#> 11  11 11 2023000001 20230000011010018  27   0                     ASIAN  175.5
#> 12  12 12 2023000001 20230000011010019  31   0                     WHITE  182.8
#> 13  13 13 2023000001 20230000011010020  52   0                     WHITE  177.5
#> 14  14 14 2023000001 20230000011010021  39   0                     WHITE  182.7
#> 15  15 15 2023000001 20230000011010022  48   0                     WHITE  173.0
#> 16  16 16 2023000001 20230000011010023  19   0                     WHITE  192.3
#> 17  17 17 2023000001 20230000011010025  41   0                     WHITE  178.0
#> 18  18 18 2023000001 20230000011010026  27   0 BLACK OR AFRICAN AMERICAN  188.8
#> 19  19 19 2023000001 20230000011010027  27   0                     WHITE  181.1
#> 20  20 20 2023000001 20230000011010028  41   0                     WHITE  194.3
#> 21  21 21 2023000001 20230000011010029  33   0                     WHITE  168.7
#> 22  22 22 2023000001 20230000011010030  48   0                     WHITE  178.7
#> 23  23 23 2023000001 20230000011010032  55   0                     WHITE  174.4
#> 24  24 24 2023000001 20230000011010033  42   0 BLACK OR AFRICAN AMERICAN  176.7
#> 25  25 25 2023000001 20230000011010034  42   0                     WHITE  160.9
#> 26  26 26 2023000001 20230000011010035  46   0 BLACK OR AFRICAN AMERICAN  188.9
#> 27  27 27 2023000001 20230000011010036  37   0 BLACK OR AFRICAN AMERICAN  187.3
#> 28  28 28 2023000001 20230000011010040  26   0                     WHITE  186.5
#> 29  29 29 2023000001 20230000011010041  46   0                     WHITE  184.4
#> 30  30 30 2023000001 20230000011010042  34   0                     WHITE  182.7
#> 31  31 31 2023000001 20230000011010043  49   0                     WHITE  172.8
#> 32  32 32 2023000001 20230000011010044  51   0                     WHITE  179.2
#> 33  33 33 2023000001 20230000011010045  54   0                     WHITE  172.9
#> 34  34 34 2023000001 20230000011010046  52   0                     WHITE  180.0
#> 35  35 35 2023000001 20230000011010047  28   0 BLACK OR AFRICAN AMERICAN  171.4
#> 36  36 36 2023000001 20230000011010048  26   0                     WHITE  178.5
#> 37  37 37 2023000001 20230000011010049  47   0                     WHITE  177.1
#> 38  38 38 2023000001 20230000011010050  24   0                     WHITE  164.4
#> 39  39 39 2023000001 20230000011010052  54   0                     WHITE  174.9
#> 40  40 40 2023000001 20230000011010053  36   0                     WHITE  180.0
#> 41  41 41 2023000001 20230000011010054  19   0                     WHITE  181.5
#> 42  42 42 2023000001 20230000011010055  41   0                     ASIAN  180.1
#> 43  43 43 2023000001 20230000011010058  55   0                     WHITE  184.1
#> 44  44 44 2023000001 20230000011010059  43   0                     WHITE  165.9
#> 45  45 45 2023000001 20230000011010060  50   0                     WHITE  173.9
#> 46  46 46 2023000001 20230000011010061  27   0                     WHITE  189.9
#> 47  47 47 2023000001 20230000011010062  50   0                     WHITE  183.9
#> 48  48 48 2023000001 20230000011010063  47   0                     WHITE  179.2
#>    WEIGHT      BMI                 DTC TIME NTIME TAFD TAD EVID  AMT    ANALYTE
#> 1    77.0 21.92560 2000-12-31 10:18:00    0     0    0   0    1    5 EXAMPLINIB
#> 2    72.8 20.25138 2000-12-29 10:30:00    0     0    0   0    1    5 EXAMPLINIB
#> 3    80.0 26.24228 2000-12-29 09:22:00    0     0    0   0    1    5 EXAMPLINIB
#> 4    78.8 27.85314 2001-01-02 09:22:00    0     0    0   0    1   10 EXAMPLINIB
#> 5    89.5 29.52742 2001-01-03 12:24:00    0     0    0   0    1   10 EXAMPLINIB
#> 6    90.0 30.24575 2001-01-02 10:00:00    0     0    0   0    1   10 EXAMPLINIB
#> 7    81.5 26.73432 2001-01-01 10:34:00    0     0    0   0    1   20 EXAMPLINIB
#> 8    77.6 23.22252 2001-01-03 09:29:00    0     0    0   0    1   20 EXAMPLINIB
#> 9    76.3 21.13573 2001-01-07 11:26:00    0     0    0   0    1   20 EXAMPLINIB
#> 10   77.9 25.90832 2001-01-13 09:30:00    0     0    0   0    1   50 EXAMPLINIB
#> 11   54.9 17.82453 2001-01-18 12:32:00    0     0    0   0    1   50 EXAMPLINIB
#> 12   70.5 21.09778 2001-01-19 09:31:00    0     0    0   0    1   50 EXAMPLINIB
#> 13   62.3 19.77385 2001-01-19 11:19:00    0     0    0   0    1  100 EXAMPLINIB
#> 14   97.8 29.29960 2001-01-19 09:57:00    0     0    0   0    1  100 EXAMPLINIB
#> 15   77.4 25.86120 2001-01-16 09:50:00    0     0    0   0    1  100 EXAMPLINIB
#> 16   88.0 23.79710 2001-01-19 13:01:00    0     0    0   0    1  100 EXAMPLINIB
#> 17   73.3 23.13471 2001-01-20 10:12:00    0     0    0   0    1  100 EXAMPLINIB
#> 18   65.4 18.34737 2001-01-21 08:48:00    0     0    0   0    1  100 EXAMPLINIB
#> 19   78.0 23.78251 2001-01-23 07:59:00    0     0    0   0    1  200 EXAMPLINIB
#> 20   72.3 19.15105 2001-01-25 08:51:00    0     0    0   0    1  200 EXAMPLINIB
#> 21   80.9 28.42617 2001-01-26 10:21:00    0     0    0   0    1  200 EXAMPLINIB
#> 22   69.9 21.88911 2001-01-25 09:29:00    0     0    0   0    1  500 EXAMPLINIB
#> 23   83.6 27.48611 2001-01-26 09:48:00    0     0    0   0    1  500 EXAMPLINIB
#> 24   59.8 19.15262 2001-01-28 09:34:00    0     0    0   0    1  500 EXAMPLINIB
#> 25   72.6 28.04300 2001-01-30 10:10:00    0     0    0   0    1  500 EXAMPLINIB
#> 26   87.0 24.38121 2001-01-29 09:18:00    0     0    0   0    1  500 EXAMPLINIB
#> 27   88.5 25.22712 2001-01-29 11:49:00    0     0    0   0    1  500 EXAMPLINIB
#> 28   83.9 24.12150 2001-01-30 10:55:00    0     0    0   0    1  800 EXAMPLINIB
#> 29   83.2 24.46817 2001-01-30 10:47:00    0     0    0   0    1  800 EXAMPLINIB
#> 30   78.6 23.54754 2001-02-04 10:23:00    0     0    0   0    1  800 EXAMPLINIB
#> 31   66.0 22.10327 2001-02-01 08:48:00    0     0    0   0    1  800 EXAMPLINIB
#> 32   74.2 23.10617 2001-02-04 11:38:00    0     0    0   0    1  800 EXAMPLINIB
#> 33   82.8 27.69749 2001-02-05 10:28:00    0     0    0   0    1  800 EXAMPLINIB
#> 34   93.4 28.82716 2001-02-05 09:36:00    0     0    0   0    1 1000 EXAMPLINIB
#> 35   73.2 24.91664 2001-02-05 09:39:00    0     0    0   0    1 1000 EXAMPLINIB
#> 36   64.1 20.11785 2001-02-09 11:40:00    0     0    0   0    1 1000 EXAMPLINIB
#> 37   71.3 22.73277 2001-02-08 09:46:00    0     0    0   0    1  500 EXAMPLINIB
#> 38   75.6 27.97166 2001-02-16 08:10:00    0     0    0   0    1  500 EXAMPLINIB
#> 39   84.2 27.52533 2001-02-15 07:16:00    0     0    0   0    1  500 EXAMPLINIB
#> 40   65.9 20.33951 2001-02-19 09:16:00    0     0    0   0    1  500 EXAMPLINIB
#> 41   77.4 23.49566 2001-02-19 09:29:00    0     0    0   0    1  500 EXAMPLINIB
#> 42   85.2 26.26710 2001-02-15 09:10:00    0     0    0   0    1  500 EXAMPLINIB
#> 43   66.0 19.47316 2001-02-20 10:41:00    0     0    0   0    1  500 EXAMPLINIB
#> 44   70.2 25.50612 2001-02-22 09:47:00    0     0    0   0    1  500 EXAMPLINIB
#> 45   72.3 23.90777 2001-02-19 10:01:00    0     0    0   0    1  500 EXAMPLINIB
#> 46   95.2 26.39897 2001-02-21 08:44:00    0     0    0   0    1  500 EXAMPLINIB
#> 47   65.5 19.36769 2001-02-22 10:25:00    0     0    0   0    1  500 EXAMPLINIB
#> 48   83.5 26.00222 2001-02-23 11:31:00    0     0    0   0    1  500 EXAMPLINIB
#>    CMT     PARENT TRTDY METABOLITE DOSE MDV ACTARMCD IMPUTATION SRC_DOMAIN
#> 1    1 EXAMPLINIB     1      FALSE    5   1       C1                    EX
#> 2    1 EXAMPLINIB     1      FALSE    5   1       C1                    EX
#> 3    1 EXAMPLINIB     1      FALSE    5   1       C1                    EX
#> 4    1 EXAMPLINIB     1      FALSE   10   1       C2                    EX
#> 5    1 EXAMPLINIB     1      FALSE   10   1       C2                    EX
#> 6    1 EXAMPLINIB     1      FALSE   10   1       C2                    EX
#> 7    1 EXAMPLINIB     1      FALSE   20   1       C3                    EX
#> 8    1 EXAMPLINIB     1      FALSE   20   1       C3                    EX
#> 9    1 EXAMPLINIB     1      FALSE   20   1       C3                    EX
#> 10   1 EXAMPLINIB     1      FALSE   50   1       C4                    EX
#> 11   1 EXAMPLINIB     1      FALSE   50   1       C4                    EX
#> 12   1 EXAMPLINIB     1      FALSE   50   1       C4                    EX
#> 13   1 EXAMPLINIB     1      FALSE  100   1       C5                    EX
#> 14   1 EXAMPLINIB     1      FALSE  100   1       C5                    EX
#> 15   1 EXAMPLINIB     1      FALSE  100   1       C5                    EX
#> 16   1 EXAMPLINIB     1      FALSE  100   1       C5                    EX
#> 17   1 EXAMPLINIB     1      FALSE  100   1       C5                    EX
#> 18   1 EXAMPLINIB     1      FALSE  100   1       C5                    EX
#> 19   1 EXAMPLINIB     1      FALSE  200   1       C6                    EX
#> 20   1 EXAMPLINIB     1      FALSE  200   1       C6                    EX
#> 21   1 EXAMPLINIB     1      FALSE  200   1       C6                    EX
#> 22   1 EXAMPLINIB     1      FALSE  500   1       C7                    EX
#> 23   1 EXAMPLINIB     1      FALSE  500   1       C7                    EX
#> 24   1 EXAMPLINIB     1      FALSE  500   1       C7                    EX
#> 25   1 EXAMPLINIB     1      FALSE  500   1       C7                    EX
#> 26   1 EXAMPLINIB     1      FALSE  500   1       C7                    EX
#> 27   1 EXAMPLINIB     1      FALSE  500   1       C7                    EX
#> 28   1 EXAMPLINIB     1      FALSE  800   1       C8                    EX
#> 29   1 EXAMPLINIB     1      FALSE  800   1       C8                    EX
#> 30   1 EXAMPLINIB     1      FALSE  800   1       C8                    EX
#> 31   1 EXAMPLINIB     1      FALSE  800   1       C8                    EX
#> 32   1 EXAMPLINIB     1      FALSE  800   1       C8                    EX
#> 33   1 EXAMPLINIB     1      FALSE  800   1       C8                    EX
#> 34   1 EXAMPLINIB     1      FALSE 1000   1       C9                    EX
#> 35   1 EXAMPLINIB     1      FALSE 1000   1       C9                    EX
#> 36   1 EXAMPLINIB     1      FALSE 1000   1       C9                    EX
#> 37   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 38   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 39   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 40   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 41   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 42   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 43   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 44   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 45   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 46   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 47   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#> 48   1 EXAMPLINIB     1      FALSE  500   1      C10                    EX
#>    SRC_SEQ DV
#> 1        1 NA
#> 2        1 NA
#> 3        1 NA
#> 4        1 NA
#> 5        1 NA
#> 6        1 NA
#> 7        1 NA
#> 8        1 NA
#> 9        1 NA
#> 10       1 NA
#> 11       1 NA
#> 12       1 NA
#> 13       1 NA
#> 14       1 NA
#> 15       1 NA
#> 16       1 NA
#> 17       1 NA
#> 18       1 NA
#> 19       1 NA
#> 20       1 NA
#> 21       1 NA
#> 22       1 NA
#> 23       1 NA
#> 24       1 NA
#> 25       1 NA
#> 26       1 NA
#> 27       1 NA
#> 28       1 NA
#> 29       1 NA
#> 30       1 NA
#> 31       1 NA
#> 32       1 NA
#> 33       1 NA
#> 34       1 NA
#> 35       1 NA
#> 36       1 NA
#> 37       1 NA
#> 38       1 NA
#> 39       1 NA
#> 40       1 NA
#> 41       1 NA
#> 42       1 NA
#> 43       1 NA
#> 44       1 NA
#> 45       1 NA
#> 46       1 NA
#> 47       1 NA
#> 48       1 NA
```
