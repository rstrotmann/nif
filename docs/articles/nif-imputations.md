# Imputations and assumptions

## INTRODUCTION

Dealing with missing data and resolving ambiguity in the input is an
important aspect of creating analysis data sets. Typical examples
include the imputation of treatment administration times that are not
explicitly captured in the input, substitution of missing demographic
data with best guesses from the overall population, or making
assumptions about pharmacokinetic observations that are be below the
detection limit of the method.

In most cases, there are different options how to resolve such data
inconsistencies, and in fact, analysts use individual imputation
strategies based on the scientific question, certain conventions or even
personal preferences. The nif package accounts for these different
approaches by giving the data programmer the choice of different
pre-defined imputation rule sets, or the option to define custom
imputation rules.

This vignette outlines the inner workings of the two functions that are
central to the dataset generation pipeline, i.e.,
[`add_administration()`](../reference/add_administration.md) and
`add_obervation()`, including the ways how data imputations can be
adjusted.

## BASELINE AND DEMOGRAPHIC PARAMETERS

The subjects’ age is derived from the ‘AGE’ field in the ‘DM’ domain of
the input SDTM data. If this columns is missing, age is derived as the
difference between ‘RFSTDTC’ and ‘BRTHDTC’.

Besides age, the subjects’ height and body weight are included, if
possible, as standard fields in the nif object. Both values are derived
from the ‘VS’ domain of the SDTM input, where the baseline time point is
identified as either `VISIT == "SCREENING"` or `VSBLFL' == "Y"`. If
multiple measurements fulfilling these condition are found for a given
subject, their mean value is used.

## TREATMENT ADMINISTRATIONS

In general, drug administration events are added to a nif object in the
following way:

``` r
library(dplyr)
library(nif)

my_nif <- nif() |> 
  add_administration(examplinib_sad, extrt = "EXAMPLINIB", analyte = "RS2023")
```

Note that [`add_administration()`](../reference/add_administration.md)
has an optional argument, `imputation`, that defines a set of imputation
rules, essentially a list of functions that are applied at different
stages. The default is ’imputation_rules_standard\`. More on this
further below.

### Subject filtering

By default, the subjects included by
[`add_administration()`](../reference/add_administration.md) exclude
screening failures, as well as subjects not treated (see the default
‘subject_filter’ string of `!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')`.
Other exclusion filters can be used, if needed.

### Time imputations

Depending on the study type (single vs. multiple administrations), ‘EX’
may define administration episodes spanning multiple administrations,
i.e., from ‘EXSTDTC’ to ‘EXENDTC’. A typical example is shown below
(some columns omitted for clarity):

    #>      STUDYID           USUBJID EXDOSE          EXSTDTC          EXENDTC
    #> 1 2023000001 20230000011010001      5 2000-12-31T10:18 2000-12-31T10:18

In general, [`add_administration()`](../reference/add_administration.md)
expands administration episodes to individual rows for each
administration event.

Administration episodes in ‘EX’ do not include time information for
individual treatment administrations but only for the first and last
ones (i.e., as reflected in ‘EXSTDTC’ and ‘EXENDTC’). In addition, the
time parts of ‘EXSDTDTC’ or ‘EXENDTC’ may be missing. This if often the
case when preliminary and not fully cleaned SDTM data are used to
generate an analysis data set Since precise time information for
administration events is usually essential for modeling analyses, a
series of time imputations are performed by `add_admininstration()`.

The following section describes the steps performed by the default
imputation rule set, ‘imputation_rules_standard’, and the alternative
pre-defined rule set, ‘imputation_rules_1’. It is possible and
encouraged to write custom rule sets, however the details go beyond the
scope of this vignette.

In either case, a first set of data imputations are performed on the
non-expanded EX domain, with the aim to ensure that each episode has
valid ‘EXSTDTC’ and ‘EXENDTC’ fields that can be then expanded to
individual rows. A second set of imputations are performed after
expansion. The focus there is to ensure that all administration events
include the most precise time information, either derived from the
available data sources, or imputed in a consistent way.

#### Default imputation rule set

To be completed.

#### Alternative imputation rule set

To be completed.

## OBSERVATIONS

To be completed.

#### 1. Missing last EXENDTC

If the last administration episode for a subject (and a treatment) has
an empty ‘EXENDTC’, it is replaced with the date/time provided by
‘DM.RFENDTC’, if available, i.e., the subject’s reference end date. See
the documentation for the (internal) function
`impute_exendtc_to_rfendtc()` for further details.

#### 2. Ongoing treatment

If after the above imputation attempt, the last administration episode
still has no ‘EXENDTC’ entry, it is replaced with `cut_off_date`. This
situation is often found in interim analyses where some subjects are
still on treatment. The `cut_off_date` parameter can be specified in the
call to [`add_administration()`](../reference/add_administration.md),
or, if not specified, is set to the last administration event found in
the whole dataset (refer to the documentation of
`impute_exendtc_to_cutoff()` for details).

#### 3. Missing EXENDTC in other administration episodes

If in an unclean data set, ‘EXENDTC’ is missing in episodes that are not
the last episode for a given subject and treatment, it is replaced with
the day before the subsequent administration episode start (‘EXSTDTC’).
It should be understood that this reflects a rather strong assumption,
i.e., that the treatment was continued into the next administration
episode. Consider this a last-resort imputation that should be avoided
by prior data cleaning, if ever possible. This imputation, if conducted,
therefore issues a warning that cannot be suppressed with
`silent = TRUE` (see the documentation to `impute_missing_exendtc()` for
details).

#### 4. Expansion of treatment administration episodes

All administration episodes, i.e., the intervals between ‘EXSTDTC’ and
‘EXENDTC’ for a given row in EX, are expanded into a sequence of rows
with one administration day per row. The administration times for all
rows except for the last are taken from the time information in EXSTDTD,
whereas the time for the last administration event in the respective
episode is taken from the time information in EXENDTC.

#### 5. Impute administration time from PCRFTDTC

For administration days for which PK sampling events are recorded in PC,
the administration time is taken from PC.PCRFTDTC, if this field is
available. Time information derived during expansion (see 4.) is
overwritten during this process. See the documentation to
`impute_admin_times_from_pcrftdtc()` for details.

#### 6. Carry forward administration time

Finally, for all administration events per subject and treatment,
further missing time information is carried forward from the last
available time information.
