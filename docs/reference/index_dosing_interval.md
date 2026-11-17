# Index dosing intervals

Add the column 'DI' that numbers the dosing intervals per subject and
parent.

## Usage

``` r
index_dosing_interval(obj, parent = NULL)
```

## Arguments

- obj:

  A nif object.

- parent:

  The parents whose administrations are indexed, as character. If NULL
  (default), all parents are indexed. Parents that are not listed get
  `DI = NA`.

## Value

A nif object with the `DI` column added. An existing `DI` column is
replaced.

## Details

Each administration of a parent starts a new interval, numbered 1, 2,
... in time order. An interval covers the time from its administration
up to and including the time of the next administration. Two
administrations at the same time count as one.

- An observation belongs to the most recent administration before it.

- An observation at exactly the time of an administration is a trough:
  it belongs to the *previous* administration, the same convention as
  for `TAD`. The administration itself starts the new interval.

- Observations at or before the first administration are assigned to the
  first interval.

- A parent without any administration gets `DI = NA`.

`DI` is calculated from the times only. It does not depend on the order
of the rows, and the row order is not changed. In a nif object, an
observation at the time of an administration is ordered before the
administration.

Example for administrations at 0 and 12 h:

|      |             |     |
|------|-------------|-----|
| TIME | Record      | DI  |
| 0    | observation | 1   |
| 0    | dose        | 1   |
| 6    | observation | 1   |
| 12   | observation | 1   |
| 12   | dose        | 2   |
| 24   | observation | 2   |

## Examples

``` r
index_dosing_interval(examplinib_fe_nif)
#> ──────── NONMEM Input Format (NIF) data ────────
#> 660 observations from 20 subjects across 1 study
#> 
#> # A tibble: 700 × 32
#>      REF    ID STUDYID    USUBJID             AGE   SEX RACE  HEIGHT WEIGHT
#>    <int> <dbl> <chr>      <chr>             <dbl> <dbl> <fct>  <dbl>  <dbl>
#>  1     1     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  2     2     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  3     3     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  4     4     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  5     5     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  6     6     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  7     7     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  8     8     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>  9     9     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#> 10    10     1 2023000400 20230004001010002    43     1 WHITE   184.   73.3
#>      BMI DTC                  TIME NTIME  TAFD   TAD  EVID   AMT   CMT    DV
#>    <dbl> <dttm>              <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21.7 2001-01-12 09:47:00   0     0     0     0       0     0     2    0 
#>  2  21.7 2001-01-12 09:47:00   0     0     0     0       1   500     1   NA 
#>  3  21.7 2001-01-12 10:17:00   0.5   0.5   0.5   0.5     0     0     2  707.
#>  4  21.7 2001-01-12 10:47:00   1     1     1     1       0     0     2 2229.
#>  5  21.7 2001-01-12 11:17:00   1.5   1.5   1.5   1.5     0     0     2 3420.
#>  6  21.7 2001-01-12 11:47:00   2     2     2     2       0     0     2 3589.
#>  7  21.7 2001-01-12 12:47:00   3     3     3     3       0     0     2 3319.
#>  8  21.7 2001-01-12 13:47:00   4     4     4     4       0     0     2 2890.
#>  9  21.7 2001-01-12 15:47:00   6     6     6     6       0     0     2 1695.
#> 10  21.7 2001-01-12 17:47:00   8     8     8     8       0     0     2 1254.
#>    ANALYTE PARENT TRTDY METABOLITE  DOSE   MDV ACTARMCD
#>    <chr>   <chr>  <dbl> <lgl>      <dbl> <dbl> <chr>   
#>  1 RS2023  RS2023     1 FALSE        500     0 BA      
#>  2 RS2023  RS2023     1 FALSE        500     1 BA      
#>  3 RS2023  RS2023     1 FALSE        500     0 BA      
#>  4 RS2023  RS2023     1 FALSE        500     0 BA      
#>  5 RS2023  RS2023     1 FALSE        500     0 BA      
#>  6 RS2023  RS2023     1 FALSE        500     0 BA      
#>  7 RS2023  RS2023     1 FALSE        500     0 BA      
#>  8 RS2023  RS2023     1 FALSE        500     0 BA      
#>  9 RS2023  RS2023     1 FALSE        500     0 BA      
#> 10 RS2023  RS2023     1 FALSE        500     0 BA      
#>    IMPUTATION                 EPOCH                  PERIOD TREATMENT FASTED
#>    <chr>                      <chr>                   <dbl> <chr>      <dbl>
#>  1 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  2 "time copied from EXSTDTC" OPEN LABEL TREATMENT 1      1 B              0
#>  3 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  4 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  5 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  6 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  7 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  8 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>  9 ""                         OPEN LABEL TREATMENT 1      1 B              0
#> 10 ""                         OPEN LABEL TREATMENT 1      1 B              0
#>       DI
#>    <int>
#>  1     1
#>  2     1
#>  3     1
#>  4     1
#>  5     1
#>  6     1
#>  7     1
#>  8     1
#>  9     1
#> 10     1
#> # ℹ 690 more rows
```
