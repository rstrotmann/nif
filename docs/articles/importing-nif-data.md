# Importing NIF data sets

## INTRODUCTION

- NIF data can be imported
- nif objects by default contain more information than minimally
  required by the NONMEM specification
- ANALYTE, PARENT, DOSE and TAFD fields are automatically generated
- other fields may be derived by renaming terms

The code in this vignette depends on the following packages:

``` r

library(dplyr)
library(nif)
```

For this demonstration, we use a minimal nif dataset that contains data
from 2 subjects with single administrations into compartment 1, and
observations up to 6 hours from compartment 2:

    ID,TIME,AMT,EVID,DV,CMT,MDV
    1,0,5,1,.,1,1
    1,0,0,0,0,2,0
    1,0.5,0,0,44.5174,2,0
    1,1,0,0,56.215,2,0
    1,1.5,0,0,50.3743,2,0
    1,2,0,0,40.7461,2,0
    1,3,0,0,26.22,2,0
    1,4,0,0,14.9048,2,0
    1,6,0,0,4.7158,2,0
    2,0,5,1,.,1,1
    2,0,0,0,0,2,0
    2,0.5,0,0,50.6929,2,0
    2,1,0,0,58.5075,2,0
    2,1.5,0,0,56.1302,2,0
    2,2,0,0,49.8835,2,0
    2,3,0,0,30.8974,2,0
    2,4,0,0,19.4844,2,0
    2,6,0,0,6.4006,2,0

The dataset contains the minimally expected columns, ID, TIME, AMT,
RATE, EVID, DV, CMT, MDV, and is provided as a csv file
(`csv_file_name`).

## IMPORTING NIF DATA

The external data can be imported as a nif object using
[`import_nif()`](../reference/import_nif.md):

``` r

nif <- import_nif(csv_file_name, format = "csv")
nif
#> ──────── NONMEM Input Format (NIF) data ────────
#> 16 observations from 2 subjects  
#> 
#> # A tibble: 18 × 8
#>      REF    ID  TIME  EVID   AMT   CMT    DV   MDV
#>  * <int> <int> <dbl> <int> <int> <int> <dbl> <int>
#>  1     1     1   0       1     5     1 NA        1
#>  2     2     1   0       0     0     2  0        0
#>  3     3     1   0.5     0     0     2 44.5      0
#>  4     4     1   1       0     0     2 56.2      0
#>  5     5     1   1.5     0     0     2 50.4      0
#>  6     6     1   2       0     0     2 40.7      0
#>  7     7     1   3       0     0     2 26.2      0
#>  8     8     1   4       0     0     2 14.9      0
#>  9     9     1   6       0     0     2  4.72     0
#> 10    10     2   0       1     5     1 NA        1
#> 11    11     2   0       0     0     2  0        0
#> 12    12     2   0.5     0     0     2 50.7      0
#> 13    13     2   1       0     0     2 58.5      0
#> 14    14     2   1.5     0     0     2 56.1      0
#> 15    15     2   2       0     0     2 49.9      0
#> 16    16     2   3       0     0     2 30.9      0
#> 17    17     2   4       0     0     2 19.5      0
#> 18    18     2   6       0     0     2  6.40     0
```

The resulting object is a regular `nif` object. It can be summarized or
plotted or otherwise explored (see `vignette("nif-tutorial")` for more
information):

``` r

summary(nif)
#> ──────── NONMEM Input Format (NIF) data summary ────────
#> Data from 2 subjects across 0studies:
#>   N  
#>   2
#> 
#> Sex distribution:
#>   SEX     N  percent  
#>   male    0  0        
#>   female  0  0        
#>   NA      2  100
#> 
#> Treatments: CMT1
#> 
#> Analytes: CMT2
#> 
#> Subjects per dose level:
#>   DL      n  
#>   5-CMT1  2
#> 
#> 16 observations:
#>   CMT  n   
#>   2    16
#> 
#> Subjects with dose reductions:
#>   treatment  n  
#>   CMT1       0
#> 
#> Treatment duration overview:
#>   PARENT  min  max  mean  median  
#>   CMT1    1    1    1     1
#> 
#> NIF version: 0.66.1
#> Creation date: 2026-09-05
#> Hash: be76f670814bbd674b60e36f0fcf0875
plot(nif, log = T, points = T)
```

![](importing-nif-data_files/figure-html/unnamed-chunk-5-1.png)

### Transformations

In some cases, the data to be imported include columns that should be
renamed or transformed to match the desired naming conventions, or to
otherwise support downstream analyses.

For this demonstration, let’s assume that the ‘TIME’ field in the csv
file is also the nominal time, and we want to include it in the nif
object as ‘NTIME’. in addition, the values in DV should be transformed
from ng/ml to micromolar concentrations (assuming a MW of 400 g/mol).
[`import_nif()`](../reference/import_nif.md) allows these
transformations to be executed during the data import. Transformation
terms need to be provided in the form of
`new_field ~ transformation_term`:

``` r

nif <- import_nif(
  csv_file_name,
  format = "csv",
  NTIME ~ TIME,
  DV ~ DV/400
)

head(nif)
#>   REF ID TIME NTIME EVID AMT CMT        DV MDV
#> 1   1  1  0.0   0.0    1   5   1        NA   1
#> 2   2  1  0.0   0.0    0   0   2 0.0000000   0
#> 3   3  1  0.5   0.5    0   0   2 0.1112935   0
#> 4   4  1  1.0   1.0    0   0   2 0.1405375   0
#> 5   5  1  1.5   1.5    0   0   2 0.1259357   0
#> 6   6  1  2.0   2.0    0   0   2 0.1018652   0
```
