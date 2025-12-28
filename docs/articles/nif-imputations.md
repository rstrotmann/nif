# Imputations and assumptions

This vignette outlines the inner workings of the two main functions
provided by the `nif` package to create nif objects, i.e.,
[`add_administration()`](../reference/add_administration.md) and
`add_obervation()`. It is meant to summarize what assumptions are made
and how missing information is imputed.

## Baseline parameters

The subjects’ age is derived from the ‘AGE’ field in the ‘DM’ domain
(such domain/field pairs are referred to in the following as, e.g.,
*DM.AGE*). If ‘DM.AGE’ is missing, age is derived as the difference
between ‘DM.RFSTDTC’ and ‘DM.BRTHDTC’.

Besides age, the subjects’ height and body weight are included as
standard fields in the nif object. Both values are derived from the ‘VS’
domain, where the baseline time point is identified as either ‘VISIT’ =
“SCREENING” or ‘VSBLFL’ = “Y”. If multiple measurements fulfilling these
condition are found for a given subject, the mean value is used.

## Administrations

In general, drug administration events are added to a nif object in the
following way:

``` r
library(dplyr)
library(nif)

my_nif <- nif() %>%
  add_administration(examplinib_sad, extrt = "EXAMPLINIB", analyte = "RS2023")
```

The [`add_administration()`](../reference/add_administration.md)
function uses the drug administration data from ‘EX’ with ‘EX.EXTRT’
filtered for the indicated treatment. If an ‘analyte’ is specified, this
analyte name is used instead of the ‘extrt’.

### Subject filtering

By default, the subjects included by
[`add_administration()`](../reference/add_administration.md) exclude
screening failures, as well as subjects not treated (see the default
‘subject_filter’ string of `!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')`.
Other exclusion filters can be used, if needed.

### Time imputations

Depending on the study type (single vs. multiple administrations), ‘EX’
may define administration episodes spanning multiple administrations,
i.e., from ‘EX.EXSTDTC’ to ‘EX.EXENDTC’. A typical example is shown
below (some columns omitted for clarity):

    STUDYID     USUBJID           EXDOSE  EXDOSFRQ  EXSTDTC           EXENDTC
    2023000400  0230004001010002  500     ONCE      2023-08-18T08:06  2023-08-21T08:43

[`add_administration()`](../reference/add_administration.md) expands
administration episodes to individual rows for each day.

Administration episodes do not include time information for individual
days. In addition, the time part of ‘EXSDTDTC’ or ‘EXENDTC’ may be
missing. This if often the case when uncleaned data is analyzed. Since
precise time information for administration events is essential for all
downstream analyses, a series of time imputations are performed by
`add_admininstration()`.

A first set of imputations (see 1. through 3. below) is performed on the
non-expanded EX domain with the aim to ensure that each episode has
‘EXSTDTC’ and ‘EXENDTC’ fields. Imputations 5. and 6. are performed
after expansion of the administration episodes:

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
