# Compile observation data frame

Create a data frame of observations from a SDTM domain specified by the
`domain` argument where the dependent variable comes from the `dv_field`
argument and the timing information from the `dtc_field` argument.

The 'TIME' in the output is `NA` throughout and needs to be calculated
based on administration time point information provided separately.

If the 'ntime_lookup' parameter is provided, 'NTIME' can be derived from
a field contained in the input data set, e.g., 'PCELTM' (see the code
examples). Otherwise, 'NTIME' will be `NA`.

## Usage

``` r
make_observation(
  sdtm,
  domain,
  testcd,
  analyte = NULL,
  parent = NULL,
  metabolite = FALSE,
  cmt = NA,
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
  include_day_in_ntime = FALSE,
  omit_not_done = TRUE,
  imputation = imputation_rules_standard,
  silent = NULL,
  na_to_zero = FALSE
)
```

## Arguments

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

- include_day_in_ntime:

  as logical.

- omit_not_done:

  Delete rows where xxSTAT is "NOT DONE, as logical.

- imputation:

  The imputation rule set.

- silent:

  Suppress messages, as logical. Defaults to nif_option setting if NULL.

- na_to_zero:

  Set all NA values of DV to 0, as logical.

## Value

A data frame.
