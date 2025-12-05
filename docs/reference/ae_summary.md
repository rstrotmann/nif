# AE summary

AE summary

## Usage

``` r
ae_summary(
  sdtm_data,
  level = "AESOC",
  show_cd = FALSE,
  group = NULL,
  order_by_subj = FALSE,
  ae_filter = "TRUE"
)
```

## Arguments

- sdtm_data:

  An sdtm object.

- level:

  The level to summarize by, as character. Can be one or multiple of:

  - 'AETERM': Reported term

  - 'AELLT': Lowest level term

  - 'AEDECOD': Dictionary-derived term

  - 'AEHLT': High level term

  - 'AEBODSYS': Body system or organ class

  - 'AESOC': System organ class

- show_cd:

  Show AE term code, as logical.

- group:

  Additional grouping variable, as character.

- order_by_subj:

  Order by number of subject, instead of by number of event, as logical.

- ae_filter:

  A filter term as character.

## Value

A data frame.

## Details

To filter for specific toxicity grades only, use the ae_filter argument,
e.g., `ae_filter = "AETOXGR == 3"`.
