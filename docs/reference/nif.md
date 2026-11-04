# nif class constructor

Create an empty nif object or a nif object from sdtm data or from a data
table.

## Usage

``` r
nif(
  obj = NULL,
  ...,
  imputation_rules = "imputation_rules_standard",
  silent = NULL
)
```

## Arguments

- obj:

  A data frame, a sdtm object, or an existing nif object.

- ...:

  Further arguments.

- imputation_rules:

  Default imputation rule set for
  [`add_administration()`](add_administration.md) and
  [`add_observation()`](add_observation.md) when those functions are
  called with `imputation = NULL`. A list, or the name of a package rule
  set as character (e.g. `"imputation_rules_standard"`). Used only when
  creating a new nif. Ignored if `obj` is already a nif object.

- silent:

  suppress messages.

## Value

A nif object.

## Details

If no obj argument is provided, an empty, minimal nif object will be
generated. If a data frame is provided for obj, it is converted into a
nif object. Minimally expected fields are ID, TIME, AMT, CMT, EVID and
DV. If ID is missing but USUBJID is available, ID will be derived.

If the input is a sdtm object, a pharmacokinetic nif object is
automatically generated. Analyte mapping formulas can be supplied as the
... argument. For details, see [`nif_auto()`](nif_auto.md).

If a new nif object is created (no `obj`, a data frame, or a sdtm
object), it stores `imputation_rules` (default
[`imputation_rules_standard()`](imputation_rules_standard.md)). If `obj`
is already a nif, that stored set is kept: `imputation_rules` is
ignored. The wrap still re-sorts rows and refreshes `REF`.

## Row order and records at the same time

Rows are ordered by `ID`, `TIME`, `DTC` and `EVID`, in this order. If a
sample (`EVID = 0`) and a dose (`EVID = 1`) have the same `ID`, `TIME`
and `DTC`, the **sample is placed before the dose**. By convention, such
a sample is a trough: it relates to the previous dose, and its time
after dose (`TAD`) is calculated accordingly. The order matters for the
analysis software: NONMEM does not re-sort records but processes them in
file order, so a sample listed before a dose is evaluated before that
dose is given. The row order and `TAD` therefore always agree, and `REF`
follows the row order. Records with other `EVID` values are ordered
numerically after samples and doses.

## See also

[`nif_auto()`](nif_auto.md)

## Examples

``` r
nif(silent = TRUE)
#> ──────── NONMEM Input Format (NIF) data ────────
#> 0 observations from 0 subjects
#> 
#> # A tibble: 0 × 7
#> # ℹ 7 variables: REF <int>, ID <int>, TIME <int>, AMT <int>, CMT <int>, EVID <int>, DV <int>
```
