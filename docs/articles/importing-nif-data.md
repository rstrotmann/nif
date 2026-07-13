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

    ID,TIME,AMT,RATE,EVID,DV,CMT,MDV
    1,0,5,0,1,.,1,1
    1,0,0,0,0,0,2,0
    1,0.5,0,0,0,0.0024,2,0
    1,1,0,0,0,0.0072,2,0
    1,1.5,0,0,0,0.0157,2,0
    1,2,0,0,0,0.0209,2,0
    1,3,0,0,0,0.0148,2,0
    1,4,0,0,0,0.0133,2,0
    1,6,0,0,0,0.0055,2,0
    2,0,5,0,1,.,1,1
    2,0,0,0,0,0,2,0
    2,0.5,0,0,0,0.0036,2,0
    2,1,0,0,0,0.0128,2,0
    2,1.5,0,0,0,0.0228,2,0
    2,2,0,0,0,0.0272,2,0
    2,3,0,0,0,0.0256,2,0
    2,4,0,0,0,0.0197,2,0
    2,6,0,0,0,0.0117,2,0

The dataset contains the minimally expected columns, ID, TIME, AMT,
RATE, EVID, DV, CMT, MDV, and is provided as a csv file
(`csv_file_name`).

## IMPORTING NIF DATA

The external data can be imported as a nif object using
[`import_nif()`](../reference/import_nif.md):

``` r

nif <- import_nif(csv_file_name, format = "csv")
head(nif)
#>   ID TIME AMT RATE EVID     DV CMT MDV
#> 1  1  0.0   5    0    1     NA   1   1
#> 2  1  0.0   0    0    0 0.0000   2   0
#> 3  1  0.5   0    0    0 0.0024   2   0
#> 4  1  1.0   0    0    0 0.0072   2   0
#> 5  1  1.5   0    0    0 0.0157   2   0
#> 6  1  2.0   0    0    0 0.0209   2   0
```

The resulting object behaves as a regular `nif` object, for example, it
can be summarized or plotted or otherwise explored (see
`vignette("nif-tutorial")` for more information):

``` r

summary(nif)
#>        ID           TIME          AMT              RATE        EVID       
#>  Min.   :1.0   Min.   :0.0   Min.   :0.0000   Min.   :0   Min.   :0.0000  
#>  1st Qu.:1.0   1st Qu.:0.5   1st Qu.:0.0000   1st Qu.:0   1st Qu.:0.0000  
#>  Median :1.5   Median :1.5   Median :0.0000   Median :0   Median :0.0000  
#>  Mean   :1.5   Mean   :2.0   Mean   :0.5556   Mean   :0   Mean   :0.1111  
#>  3rd Qu.:2.0   3rd Qu.:3.0   3rd Qu.:0.0000   3rd Qu.:0   3rd Qu.:0.0000  
#>  Max.   :2.0   Max.   :6.0   Max.   :5.0000   Max.   :0   Max.   :1.0000  
#>                                                                           
#>        DV                CMT             MDV        
#>  Min.   :0.000000   Min.   :1.000   Min.   :0.0000  
#>  1st Qu.:0.005025   1st Qu.:2.000   1st Qu.:0.0000  
#>  Median :0.013050   Median :2.000   Median :0.0000  
#>  Mean   :0.012700   Mean   :1.889   Mean   :0.1111  
#>  3rd Qu.:0.020000   3rd Qu.:2.000   3rd Qu.:0.0000  
#>  Max.   :0.027200   Max.   :2.000   Max.   :1.0000  
#>  NAs    :2
plot(nif, log = T, points = T)
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
#> Warning in axis(side = side, at = at, labels = labels, ...): "points" is not a
#> graphical parameter
#> Warning in plot.xy(xy.coords(x, y), type = type, ...): "points" is not a
#> graphical parameter
#> Warning in plot.window(...): "points" is not a graphical parameter
#> Warning in plot.xy(xy, type, ...): "points" is not a graphical parameter
#> Warning in title(...): "points" is not a graphical parameter
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
#>   ID TIME AMT RATE EVID        DV CMT MDV NTIME
#> 1  1  0.0   5    0    1        NA   1   1   0.0
#> 2  1  0.0   0    0    0 0.000e+00   2   0   0.0
#> 3  1  0.5   0    0    0 6.000e-06   2   0   0.5
#> 4  1  1.0   0    0    0 1.800e-05   2   0   1.0
#> 5  1  1.5   0    0    0 3.925e-05   2   0   1.5
#> 6  1  2.0   0    0    0 5.225e-05   2   0   2.0
```
