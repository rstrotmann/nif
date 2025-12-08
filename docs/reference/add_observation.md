# Append observation events

Observations can be pharmacokinetic observations (i.e., from the PC
domain), or any other class of observation from any other SDTM domain.
The 'testcd' specifies the value of the respective **TESTCD** field
(e.g., 'PCTESTCD', 'VSTESTCD' or 'LBTESTCD') that defines the
observation. Observation events can be attached to an administered drug
by specifying the 'parent' field. This is required for, e.g., the
time-after-dose ('TAD') and time-after-first-dose ('TAFD') time
calculation.

## Usage

``` r
add_observation(
  nif,
  sdtm,
  domain,
  testcd,
  analyte = NULL,
  parent = NULL,
  metabolite = FALSE,
  cmt = NULL,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  observation_filter = "TRUE",
  cat = NULL,
  scat = NULL,
  TESTCD_field = NULL,
  DTC_field = NULL,
  DV_field = NULL,
  coding_table = NULL,
  factor = 1,
  NTIME_lookup = NULL,
  ntime_method = "TPT",
  keep = NULL,
  debug = FALSE,
  include_day_in_ntime = FALSE,
  silent = NULL,
  duplicates = "stop",
  duplicate_function = mean,
  omit_not_done = TRUE,
  na.rm = TRUE,
  na_to_zero = FALSE
)
```

## Arguments

- nif:

  A nif object.

- sdtm:

  A sdtm object. Needs at least the 'DM' and 'VS' domains, and the
  domain the observations come from.

- domain:

  The domain as character.

- testcd:

  The observation variable, as character.

- analyte:

  The name for the analyte. Defaults to the 'testcd', if NULL.

- parent:

  The name of the parent analyte for the observation as character.
  Defaults to the respective treatment administered before the
  observation, if NULL.

- metabolite:

  Observation is a metabolite, as logical.

- cmt:

  The compartment for the observation as numeric.

- subject_filter:

  The filtering to apply to the DM domain.

- observation_filter:

  The filtering to apply to the observation source data.

- cat:

  xxCAT filter to apply, as character.

- scat:

  xxSCAT filter to apply, as character.

- TESTCD_field:

  The xxTESTCD field. Defaults to the two-character domain name followed
  by 'TESTCD', if NULL.

- DTC_field:

  The field to use as the date-time code for the observation. Defaults
  to the two-character domain name followed by 'DTC', if NULL.

- DV_field:

  the field to use as the dependent variable. Defaults to the
  two-character domain name followed by 'STRESN', if NULL.

- coding_table:

  Coding table to translate a categorical values into numerical values,
  as data frame. The data frame must have at least one column that
  matches a column in the domain, and a numerical 'DV' column that
  provides the recoding result.

- factor:

  Multiplier for the DV field, as numeric.

- NTIME_lookup:

  A data frame with two columns, a column that defines the custom
  nominal time information in the target domain (e.g., 'PCELTM'), and
  'NTIME'. This data frame is left_joined into the observation data
  frame to provide the NTIME field.

- ntime_method:

  the field to derive the nominal time from. Allowed values are 'TPT',
  'TPTNUM', 'ELTM' and 'DY'. Defaults to xxTPT where xx is the domain
  name.

- keep:

  Columns to keep, as character.

- debug:

  Include debug fields, as logical.

- include_day_in_ntime:

  as logical.

- silent:

  Suppress messages, as logical. Defaults to nif_option setting if NULL.

- duplicates:

  Selection how to deal with duplicate observations with respect to the
  USUBJID, ANALYTE and DTC fields:

  - 'stop': Stop execution and produce error message

  - 'ignore': Include duplicates in the data set

  - 'identify': Return a list of duplicate entries

  - 'resolve': Resolve duplicates, applying the `duplicate_function` to
    the duplicate entries.

- duplicate_function:

  Function to resolve duplicate values, defaults to `mean`.

- omit_not_done:

  Delete rows where xxSTAT is "NOT DONE, as logical.

- na.rm:

  Logical indicating whether to remove NA values when applying the
  duplicate_function. Defaults to TRUE.

- na_to_zero:

  Set all NA values of DV to 0, as logical.

## Value

A nif object.

## Details

Observations can be further specified with the 'observation_filter'
term. The filter term can refer to any field of the respective SDTM
domain.

A PK/PD model compartment can be specified with 'cmt' or will be
automatically assigned if `cmt = NULL`.

For an overview on the representation of observation events in NONMEM
Input Format compliant data sets, see Bauer, R.J. (2019), NONMEM
Tutorial Part I: Description of Commands and Options, With Simple
Examples of Population Analysis. CPT Pharmacometrics Syst. Pharmacol.,
8: 525-537. <https://doi.org/10.1002/psp4.12404>

## See also

[`add_administration()`](add_administration.md)
