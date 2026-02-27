# Add observation events to nif

Add rows to a [nif](nif.md) object that represent observation events,
i.e., with EVID values of 0. This is usually the second step in the
creation of NIF data tables, after
[`add_administration()`](add_administration.md).

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
  testcd_field = NULL,
  dtc_field = NULL,
  dv_field = NULL,
  coding_table = NULL,
  factor = 1,
  ntime_lookup = NULL,
  ntime_method = "TPT",
  keep = NULL,
  debug = FALSE,
  include_day_in_ntime = FALSE,
  silent = NULL,
  duplicates = "stop",
  duplicate_function = mean,
  duplicate_identifier = "DTC",
  omit_not_done = TRUE,
  na_rm = TRUE,
  na_to_zero = FALSE,
  imputation = imputation_rules_standard
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

- testcd_field:

  The xxTESTCD field. Defaults to the two-character domain name followed
  by 'TESTCD', if NULL.

- dtc_field:

  The field to use as the date-time code for the observation. Defaults
  to the two-character domain name followed by 'DTC', if NULL.

- dv_field:

  the field to use as the dependent variable. Defaults to the
  two-character domain name followed by 'STRESN', if NULL.

- coding_table:

  Coding table to translate a categorical values into numerical values,
  as data frame. The data frame must have at least one column that
  matches a column in the domain, and a numerical 'DV' column that
  provides the recoding result.

- factor:

  Multiplier for the DV field, as numeric.

- ntime_lookup:

  A data frame with two columns, a column that defines the custom
  nominal time information in the target domain (e.g., 'PCELTM'), and
  'NTIME'. This data frame is left_joined into the observation data
  frame to provide the NTIME field.

- ntime_method:

  the field to derive the nominal time from. Allowed values are 'TPT',
  'TPTNUM', 'ELTM', 'VISITDY' and 'DY'. Defaults to xxTPT where xx is
  the domain name.

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

- duplicate_identifier:

  Fields by which duplicates are identified (after addition of the
  observations to the nif object), defaults to "DTC". Consider also
  "NTIME", or any other custom field.

- omit_not_done:

  Delete rows where xxSTAT is "NOT DONE, as logical.

- na_rm:

  Logical indicating whether to remove NA values when applying the
  duplicate_function. Defaults to TRUE.

- na_to_zero:

  Set all NA values of DV to 0, as logical.

- imputation:

  The imputation rule set.

## Value

A nif object.

## Details

Observations can be pharmacokinetic observations (i.e., from the PC
domain), or any other type of observation from any other SDTM domain.
The `domain` and `testcd` arguments specify the source of the
observation events.

In general, the dependent variable and the observation time stamp are
taken from the 'xxSTRESN' and 'xxDTC' fields of the source domain (where
xx refers to the domain code). Differing fields can be specified by the
`dv_field` and `dtc_field` arguments

Observation events can be attached to an administered drug by the
`parent` argument. Specifying the respective parent ANALYTE code links
these observations to the respective drug administration event. This is
required for the calculation of time-after-dose ('TAD') and
time-after-first-dose ('TAFD') for observations. If no `parent` is
specified, the most likely parent is automatically selected. This
usually works well for studies where only one treatment is administered.
In other cases, it may be beneficial to explicitly define a parent for
all observations.

Observations can be further specified with `cat` and `scat` arguments
that refer to the 'xxCAT' and 'xxSCAT' fields of the source domain (xx),
and the `observation_filter` argument. This may be necessary, when
observations defined by the `testcd` alone are ambiguous, e.g., when for
pharmacokinetic observations, both BLOOD and URINE observations are
included in the PC domain data.

A model compartment can be specified by the `cmt` argument, or will be
automatically assigned otherwise.

### Nominal time

While the actual time fields (i.e., TIME, TAFD, TAD) for the observation
events are automatically derived from the date-time stamp (the xxDTC
field), the nominal time of the observation is often not unambiguously
represented in the source SDTM data. Different methods are provided to
derive the nominal time (NTIME) field and can be selected using the
`ntime_method` argument.

- 'TPT' attempts to extract the nominal time from the 'xxTPT' field of
  the SDTM data

- 'TPTNUM' interprets the 'xxTPTNUM' field as the numerical
  representation of the nominal observation time in hours. Note that
  depending on the SDTM generation method, this may or may not be
  correct

- 'ELTM' uses the xxELTM (elapsed time) column, if available

- 'VISITDY' uses the VISIDY column to derive the nominal time. Note that
  the (planned) visit day may differ from the actual day of the
  observation. As this column has not time information, the granularity
  of the observation is full days.

- 'DY' uses the 'xxDY' columns of the original SDTM data. As this column
  has not time information, the granularity of the observation is full
  days.

Alternatively, a lookup table data frame can be provided by the
`ntime_lookup` argument that associates any column in the source SDTM
data with the respective NTIME.

### Recoding

For some observation types, the source SDTM data may not include a
dependent variable in numerical form (i.e., for AE observations when
AETOXGR is provided in text form). In such cases, a `coding_table` can
be provided that links the values of an arbitrary columns to the
expected 'DV' value.

### Duplicate observations

SDTM data may contain duplicate observations, most often because of
incomplete data cleaning. `add_observation()` provides methods to deal
with duplicates, see the description of the `duplicates` argument.

### Further information

For an overview on the representation of observation events in NONMEM
Input Format compliant data sets, see Bauer, R.J. (2019), NONMEM
Tutorial Part I: Description of Commands and Options, With Simple
Examples of Population Analysis. CPT Pharmacometrics Syst. Pharmacol.,
8: 525-537. [doi:10.1002/psp4.12404](https://doi.org/10.1002/psp4.12404)
.

To add administration events to the [nif](nif.md) object, see
[`add_administration()`](add_administration.md).

## See also

[`add_administration()`](add_administration.md)
