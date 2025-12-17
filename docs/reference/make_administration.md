# Compile administration data frame

Compile administration data frame

## Usage

``` r
make_administration(
  sdtm,
  extrt,
  analyte = NULL,
  cmt = 1,
  subject_filter = "!ACTARMCD %in% c('SCRNFAIL', 'NOTTRT')",
  cut_off_date = NULL,
  keep = "",
  silent = NULL
)
```

## Arguments

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

  The data cut-off date as Posix date-time.

- keep:

  Columns to keep after cleanup, as character.

- silent:

  Suppress messages, defaults to nif_option standard, if NULL.

## Value

A data frame.

## Details

A discussion on EC vs EX is provided
[here](https://www.cdisc.org/kb/ecrf/exposure-collected#:~:text=In%20the%20SDTMIG%2C%20the%20Exposure,data%20collected%20on%20the%20CRF.)

## Time imputations and filtering

The following time imputations and filters are applied in the given
order:

### 1. [`impute_exendtc_to_rfendtc()`](impute_exendtc_to_rfendtc.md)

If EXENDTC is missing in the last administration episode for a given
subject, it is replaced with DM.RFENDTC, if available.

### 2. [`filter_EXENDTC_after_EXSTDTC()`](filter_EXENDTC_after_EXSTDTC.md)

Administration episodes in which EXSTDTC is after EXENDT are deleted
from the data set.

### 3. [`impute_exendtc_to_cutoff()`](impute_exendtc_to_cutoff.md)

If in the last administration episode per subject and treatment, EXENDTC
is missing, for example because the treatment is still ongoing at the
time of the SDTM generation, EXENDTC is replaced with the cut-off date.

### 4. [`impute_missing_exendtc()`](impute_missing_exendtc.md)

If in any further episode, EXENDTC is missing, it is replaced with the
day before the subsequent administration episode start (EXSTDTC). It
should be understood that this reflects a rather strong assumption,
i.e., that the treatment was continued into the next administration
episode. This imputation therefore issues a warning that cannot be
suppressed.

### 5. Expand administration episodes

All administration episodes, i.e., the intervals between EXSTDTC and
EXENDTC for a given row in EX, are expanded into a sequence of rows with
one administration day per row. The administration times for all rows
except for the last are taken from the time information in EXSTDTD,
whereas the time for the last administration event in the respective
episode is taken from the time information in EXENDTC.

**Development note:** In the present version of the function, once-daily
(QD) dosing is assumed. Multiple-daily dosings are not supported. In
future versions, the dosing frequency provided in `EXDOSFRQ` may be
taken into account to adequately handle multiple daily administrations.

### 6. [`impute_admin_times_from_pcrftdtc()`](impute_admin_times_from_pcrftdtc.md)

For administration days for which PK sampling events are recorded in PC,
the administration time is taken from PC.PCRFTDTC, if this field is
available.

**Development note:** This may be updated in future versions of the
function to work with multiple-daily administrations.

### 7. Carry forward time

For all administration events per subject and treatment, missing time
information is finally carried forward from available time information.

## See also

[`add_administration()`](add_administration.md)
