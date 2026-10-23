# Pharmaverse sample nif object

A nif object based on the CDISCPILOT01 data set exported from the
'pharmaversesdtm' package including administration events for XAN and
PLACEBO, and observations for XAN, ALT and HGB.

## Usage

``` r
cdiscpilot01_nif
```

## Format

nif object with 38475 rows and 29 columns:

- REF:

  row number

- ID:

  subject ID

- STUDYID:

  study ID

- USUBJID:

  unique subject ID

- AGE:

  age in years

- SEX:

  sex (0: male, 1, female)

- RACE:

  race

- WEIGHT:

  baseline body weight

- DTC:

  date time code for event

- TIME:

  time in hours

- NTIME:

  nominal time for event in hours

- TAFD:

  time after individual first dose

- TAD:

  time after last dose

- EVID:

  event ID (0: observation, 1: administration)

- AMT:

  amount in mg

- ANALYTE:

  analyte name

- CMT:

  analyte compartment

- PARENT:

  parent analyte

- TRTDY:

  treatment day

- METABOLITE:

  metabolite (0: no, 1: yes)

- DOSE:

  dose in mg

- MDV:

  missing dependent variable

- ACTARMCD:

  actual arm code

- IMPUTATION:

  imputation note

- DV:

  dependent variable

- BL_CREAT:

  baseline creatinine in umol/l

- BL_CRCL:

  baseline creatinine clearance in ml/min

- BL_RENAL:

  baseline renal function category

- DL:

  dose level in mg

## Source

Created from <https://github.com/pharmaverse/pharmaversesdtm>

## Details

    ----- NONMEM Input Format (NIF) data -----
    8988 observations from 254 subjects across 1 study
    Analytes: ALT, HGB, CFB_HGB, XAN and PLACEBO
    111 males (43.7

    Columns:
      REF, ID, STUDYID, USUBJID, AGE, SEX, RACE, WEIGHT, DTC, TIME, NTIME, TAFD,
      TAD, EVID, AMT, ANALYTE, CMT, PARENT, TRTDY, METABOLITE, DOSE, MDV, ACTARMCD,
      IMPUTATION, DV, BL_CREAT, BL_CRCL, BL_RENAL, DL

    Hash: f36650cc3aa5ecdad143a87e418b1439

    Data (selected columns):
      ID   NTIME   TIME      TAD       ANALYTE   EVID   CMT   AMT   DOSE   DV
      1    -192    0         -153.25   ALT       0      3     0     0      27
      1    -192    0         -153.25   HGB       0      4     0     0      8.875
      1    -192    0         -153.25   CFB_HGB   0      5     0     0      0
      1    NA      152.75    -0.5      XAN       0      2     0     0      0
      1    0       153.25    0         PLACEBO   1      1     0     0      NA
      1    0.083   153.333   0.083     XAN       0      2     0     0      NA
      1    0.5     153.75    0.5       XAN       0      2     0     0      NA
      1    NA      154.25    1         XAN       0      2     0     0      NA
      1    NA      154.75    1.5       XAN       0      2     0     0      NA
      1    NA      155.25    2         XAN       0      2     0     0      NA
    38465 more rows
