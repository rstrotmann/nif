# Non-compartmental analysis

This function is a wrapper around the NCA functions provided by the
[PKNCA](https://CRAN.R-project.org/package=PKNCA) package.

## Usage

``` r
nca(
  obj,
  analyte = NULL,
  parent = NULL,
  keep = "DOSE",
  group = NULL,
  nominal_time = FALSE,
  average_duplicates = TRUE,
  silent = NULL
)
```

## Arguments

- obj:

  The source NIF object.

- analyte:

  The analyte. If none specified and multiple analytes are in the
  dataset, defaults to the first analyte.

- parent:

  The parent compound to derive the administration information from. By
  default, equal to the analyte.

- keep:

  A vector of fields to retain on the subject level in the output.

- group:

  The grouping variable as string.

- nominal_time:

  A boolean to indicate whether nominal time rather than actual time
  should be used for NCA.

- average_duplicates:

  Boolean to indicate whether duplicate entries should be averaged.

- silent:

  Suppress messages, defaults to nif_option setting, if NULL.

## Value

A data frame.

## Examples

``` r
nca(examplinib_fe_nif)
#> NCA: No analyte specified. Selected RS2023 as the most likely.
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> 
#>    ID start end PPTESTCD   PPORRES exclude DOSE
#> 1   1     0 312  auclast 19749.312    <NA>  500
#> 2   1     0 312     cmax  5605.797    <NA>  500
#> 3   1     0 312     tmax     1.000    <NA>  500
#> 4   2     0 312  auclast       NaN    <NA>  500
#> 5   2     0 312     cmax  2168.668    <NA>  500
#> 6   2     0 312     tmax     2.000    <NA>  500
#> 7   3     0 312  auclast 21508.110    <NA>  500
#> 8   3     0 312     cmax  6439.872    <NA>  500
#> 9   3     0 312     tmax     1.000    <NA>  500
#> 10  4     0 312  auclast 24912.778    <NA>  500
#> 11  4     0 312     cmax  6043.145    <NA>  500
#> 12  4     0 312     tmax     1.500    <NA>  500
#> 13  5     0 312  auclast 32624.194    <NA>  500
#> 14  5     0 312     cmax  5283.736    <NA>  500
#> 15  5     0 312     tmax     2.000    <NA>  500
#> 16  6     0 312  auclast 18956.413    <NA>  500
#> 17  6     0 312     cmax  3236.202    <NA>  500
#> 18  6     0 312     tmax     2.000    <NA>  500
#> 19  7     0 312  auclast 15400.895    <NA>  500
#> 20  7     0 312     cmax  2479.996    <NA>  500
#> 21  7     0 312     tmax     1.500    <NA>  500
#> 22  8     0 312  auclast       NaN    <NA>  500
#> 23  8     0 312     cmax  6994.537    <NA>  500
#> 24  8     0 312     tmax     1.500    <NA>  500
#> 25  9     0 312  auclast 40878.720    <NA>  500
#> 26  9     0 312     cmax  5496.735    <NA>  500
#> 27  9     0 312     tmax     3.000    <NA>  500
#> 28 10     0 312  auclast 21157.387    <NA>  500
#> 29 10     0 312     cmax  3262.716    <NA>  500
#> 30 10     0 312     tmax     2.000    <NA>  500
#> 31 11     0 312  auclast       NaN    <NA>  500
#> 32 11     0 312     cmax  2301.752    <NA>  500
#> 33 11     0 312     tmax     2.000    <NA>  500
#> 34 12     0 312  auclast       NaN    <NA>  500
#> 35 12     0 312     cmax  4611.494    <NA>  500
#> 36 12     0 312     tmax     1.000    <NA>  500
#> 37 13     0 312  auclast 17484.105    <NA>  500
#> 38 13     0 312     cmax  3521.432    <NA>  500
#> 39 13     0 312     tmax     2.000    <NA>  500
#> 40 14     0 312  auclast       NaN    <NA>  500
#> 41 14     0 312     cmax  4004.570    <NA>  500
#> 42 14     0 312     tmax     1.000    <NA>  500
#> 43 15     0 312  auclast       NaN    <NA>  500
#> 44 15     0 312     cmax  4926.105    <NA>  500
#> 45 15     0 312     tmax     2.000    <NA>  500
#> 46 16     0 312  auclast 14013.842    <NA>  500
#> 47 16     0 312     cmax  4537.822    <NA>  500
#> 48 16     0 312     tmax     1.000    <NA>  500
#> 49 17     0 312  auclast       NaN    <NA>  500
#> 50 17     0 312     cmax  4572.924    <NA>  500
#> 51 17     0 312     tmax     1.000    <NA>  500
#> 52 18     0 312  auclast 19796.209    <NA>  500
#> 53 18     0 312     cmax  5212.429    <NA>  500
#> 54 18     0 312     tmax     1.000    <NA>  500
#> 55 19     0 312  auclast 17956.496    <NA>  500
#> 56 19     0 312     cmax  2815.318    <NA>  500
#> 57 19     0 312     tmax     1.500    <NA>  500
#> 58 20     0 312  auclast 13780.457    <NA>  500
#> 59 20     0 312     cmax  4126.813    <NA>  500
#> 60 20     0 312     tmax     1.000    <NA>  500
nca(examplinib_fe_nif, group = c("FASTED", "SEX"), analyte = "RS2023")
#> NCA: Group byFASTED+SEX
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: Negative concentrations found
#> Warning: NaNs produced
#> Warning: Negative concentrations found
#> 
#>     FASTED SEX ID start end            PPTESTCD      PPORRES exclude DOSE
#> 1        1   1  1     0  24             auclast 1.952250e+04    <NA>  500
#> 2        1   1  1     0 Inf                cmax 5.605797e+03    <NA>  500
#> 3        1   1  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 4        1   1  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 5        1   1  1     0 Inf           clast.obs 4.150914e-05    <NA>  500
#> 6        1   1  1     0 Inf            lambda.z 9.115619e-02    <NA>  500
#> 7        1   1  1     0 Inf           r.squared 9.999821e-01    <NA>  500
#> 8        1   1  1     0 Inf       adj.r.squared 9.999776e-01    <NA>  500
#> 9        1   1  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 10       1   1  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 11       1   1  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 12       1   1  1     0 Inf          clast.pred 4.129542e-05    <NA>  500
#> 13       1   1  1     0 Inf           half.life 7.603951e+00    <NA>  500
#> 14       1   1  1     0 Inf          span.ratio 1.893752e+01    <NA>  500
#> 15       1   1  1     0 Inf          aucinf.obs 1.974931e+04    <NA>  500
#> 16       0   1  1   312 336             auclast 2.692168e+04    <NA>  500
#> 17       0   1  1   312 Inf                cmax 4.207640e+03    <NA>  500
#> 18       0   1  1   312 Inf                tmax 2.000000e+00    <NA>  500
#> 19       0   1  1   312 Inf               tlast 1.680000e+02    <NA>  500
#> 20       0   1  1   312 Inf           clast.obs 7.955180e-05    <NA>  500
#> 21       0   1  1   312 Inf            lambda.z 9.108573e-02    <NA>  500
#> 22       0   1  1   312 Inf           r.squared 9.999968e-01    <NA>  500
#> 23       0   1  1   312 Inf       adj.r.squared 9.999958e-01    <NA>  500
#> 24       0   1  1   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 25       0   1  1   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 26       0   1  1   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 27       0   1  1   312 Inf          clast.pred 7.996105e-05    <NA>  500
#> 28       0   1  1   312 Inf           half.life 7.609833e+00    <NA>  500
#> 29       0   1  1   312 Inf          span.ratio 1.576907e+01    <NA>  500
#> 30       0   1  1   312 Inf          aucinf.obs 2.747138e+04    <NA>  500
#> 31       0   1  2     0  24             auclast 1.296987e+04    <NA>  500
#> 32       0   1  2     0 Inf                cmax 2.168668e+03    <NA>  500
#> 33       0   1  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 34       0   1  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 35       0   1  2     0 Inf           clast.obs 1.751290e-05    <NA>  500
#> 36       0   1  2     0 Inf            lambda.z 9.392942e-02    <NA>  500
#> 37       0   1  2     0 Inf           r.squared 9.999678e-01    <NA>  500
#> 38       0   1  2     0 Inf       adj.r.squared 9.999570e-01    <NA>  500
#> 39       0   1  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 40       0   1  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 41       0   1  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 42       0   1  2     0 Inf          clast.pred 1.735746e-05    <NA>  500
#> 43       0   1  2     0 Inf           half.life 7.379447e+00    <NA>  500
#> 44       0   1  2     0 Inf          span.ratio 1.626138e+01    <NA>  500
#> 45       0   1  2     0 Inf          aucinf.obs 1.318205e+04    <NA>  500
#> 46       1   1  2   312 336             auclast 8.499976e+03    <NA>  500
#> 47       1   1  2   312 Inf                cmax 2.783724e+03    <NA>  500
#> 48       1   1  2   312 Inf                tmax 1.000000e+00    <NA>  500
#> 49       1   1  2   312 Inf               tlast 1.680000e+02    <NA>  500
#> 50       1   1  2   312 Inf           clast.obs 8.307843e-06    <NA>  500
#> 51       1   1  2   312 Inf            lambda.z 9.363952e-02    <NA>  500
#> 52       1   1  2   312 Inf           r.squared 9.999818e-01    <NA>  500
#> 53       1   1  2   312 Inf       adj.r.squared 9.999772e-01    <NA>  500
#> 54       1   1  2   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 55       1   1  2   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 56       1   1  2   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 57       1   1  2   312 Inf          clast.pred 8.131810e-06    <NA>  500
#> 58       1   1  2   312 Inf           half.life 7.402293e+00    <NA>  500
#> 59       1   1  2   312 Inf          span.ratio 1.945343e+01    <NA>  500
#> 60       1   1  2   312 Inf          aucinf.obs 8.562265e+03    <NA>  500
#> 61       1   1  3     0  24             auclast 2.127273e+04    <NA>  500
#> 62       1   1  3     0 Inf                cmax 6.439872e+03    <NA>  500
#> 63       1   1  3     0 Inf                tmax 1.000000e+00    <NA>  500
#> 64       1   1  3     0 Inf               tlast 1.680000e+02    <NA>  500
#> 65       1   1  3     0 Inf           clast.obs 7.390936e-06    <NA>  500
#> 66       1   1  3     0 Inf            lambda.z 1.047673e-01    <NA>  500
#> 67       1   1  3     0 Inf           r.squared 9.999660e-01    <NA>  500
#> 68       1   1  3     0 Inf       adj.r.squared 9.999592e-01    <NA>  500
#> 69       1   1  3     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 70       1   1  3     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 71       1   1  3     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 72       1   1  3     0 Inf          clast.pred 7.106165e-06    <NA>  500
#> 73       1   1  3     0 Inf           half.life 6.616064e+00    <NA>  500
#> 74       1   1  3     0 Inf          span.ratio 2.357897e+01    <NA>  500
#> 75       1   1  3     0 Inf          aucinf.obs 2.150811e+04    <NA>  500
#> 76       0   1  3   312 336             auclast 3.058559e+04    <NA>  500
#> 77       0   1  3   312 Inf                cmax 4.767907e+03    <NA>  500
#> 78       0   1  3   312 Inf                tmax 2.000000e+00    <NA>  500
#> 79       0   1  3   312 Inf               tlast 1.680000e+02    <NA>  500
#> 80       0   1  3   312 Inf           clast.obs 1.462743e-05    <NA>  500
#> 81       0   1  3   312 Inf            lambda.z 1.045455e-01    <NA>  500
#> 82       0   1  3   312 Inf           r.squared 9.999830e-01    <NA>  500
#> 83       0   1  3   312 Inf       adj.r.squared 9.999773e-01    <NA>  500
#> 84       0   1  3   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 85       0   1  3   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 86       0   1  3   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 87       0   1  3   312 Inf          clast.pred 1.458709e-05    <NA>  500
#> 88       0   1  3   312 Inf           half.life 6.630102e+00    <NA>  500
#> 89       0   1  3   312 Inf          span.ratio 1.809927e+01    <NA>  500
#> 90       0   1  3   312 Inf          aucinf.obs 3.116406e+04    <NA>  500
#> 91       1   1  4     0  24             auclast 2.443897e+04    <NA>  500
#> 92       1   1  4     0 Inf                cmax 6.043145e+03    <NA>  500
#> 93       1   1  4     0 Inf                tmax 1.500000e+00    <NA>  500
#> 94       1   1  4     0 Inf               tlast 1.680000e+02    <NA>  500
#> 95       1   1  4     0 Inf           clast.obs 1.984748e-04    <NA>  500
#> 96       1   1  4     0 Inf            lambda.z 8.484051e-02    <NA>  500
#> 97       1   1  4     0 Inf           r.squared 9.999722e-01    <NA>  500
#> 98       1   1  4     0 Inf       adj.r.squared 9.999652e-01    <NA>  500
#> 99       1   1  4     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 100      1   1  4     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 101      1   1  4     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 102      1   1  4     0 Inf          clast.pred 1.988844e-04    <NA>  500
#> 103      1   1  4     0 Inf           half.life 8.170002e+00    <NA>  500
#> 104      1   1  4     0 Inf          span.ratio 1.762545e+01    <NA>  500
#> 105      1   1  4     0 Inf          aucinf.obs 2.491278e+04    <NA>  500
#> 106      0   1  4   312 336             auclast 3.817864e+04    <NA>  500
#> 107      0   1  4   312 Inf                cmax 5.333942e+03    <NA>  500
#> 108      0   1  4   312 Inf                tmax 2.000000e+00    <NA>  500
#> 109      0   1  4   312 Inf               tlast 1.680000e+02    <NA>  500
#> 110      0   1  4   312 Inf           clast.obs 3.870134e-04    <NA>  500
#> 111      0   1  4   312 Inf            lambda.z 8.522916e-02    <NA>  500
#> 112      0   1  4   312 Inf           r.squared 9.999328e-01    <NA>  500
#> 113      0   1  4   312 Inf       adj.r.squared 9.999104e-01    <NA>  500
#> 114      0   1  4   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 115      0   1  4   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 116      0   1  4   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 117      0   1  4   312 Inf          clast.pred 4.005540e-04    <NA>  500
#> 118      0   1  4   312 Inf           half.life 8.132747e+00    <NA>  500
#> 119      0   1  4   312 Inf          span.ratio 1.475516e+01    <NA>  500
#> 120      0   1  4   312 Inf          aucinf.obs 3.934480e+04    <NA>  500
#> 121      0   0  5     0  24             auclast 3.202317e+04    <NA>  500
#> 122      0   0  5     0 Inf                cmax 5.283736e+03    <NA>  500
#> 123      0   0  5     0 Inf                tmax 2.000000e+00    <NA>  500
#> 124      0   0  5     0 Inf               tlast 1.680000e+02    <NA>  500
#> 125      0   0  5     0 Inf           clast.obs 3.525085e-05    <NA>  500
#> 126      0   0  5     0 Inf            lambda.z 9.840913e-02    <NA>  500
#> 127      0   0  5     0 Inf           r.squared 9.999789e-01    <NA>  500
#> 128      0   0  5     0 Inf       adj.r.squared 9.999718e-01    <NA>  500
#> 129      0   0  5     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 130      0   0  5     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 131      0   0  5     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 132      0   0  5     0 Inf          clast.pred 3.514853e-05    <NA>  500
#> 133      0   0  5     0 Inf           half.life 7.043525e+00    <NA>  500
#> 134      0   0  5     0 Inf          span.ratio 1.703692e+01    <NA>  500
#> 135      0   0  5     0 Inf          aucinf.obs 3.262419e+04    <NA>  500
#> 136      1   0  5   312 336             auclast 2.316675e+04    <NA>  500
#> 137      1   0  5   312 Inf                cmax 7.048011e+03    <NA>  500
#> 138      1   0  5   312 Inf                tmax 1.000000e+00    <NA>  500
#> 139      1   0  5   312 Inf               tlast 1.680000e+02    <NA>  500
#> 140      1   0  5   312 Inf           clast.obs 1.856992e-05    <NA>  500
#> 141      1   0  5   312 Inf            lambda.z 9.916635e-02    <NA>  500
#> 142      1   0  5   312 Inf           r.squared 9.999721e-01    <NA>  500
#> 143      1   0  5   312 Inf       adj.r.squared 9.999665e-01    <NA>  500
#> 144      1   0  5   312 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 145      1   0  5   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 146      1   0  5   312 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 147      1   0  5   312 Inf          clast.pred 1.843892e-05    <NA>  500
#> 148      1   0  5   312 Inf           half.life 6.989742e+00    <NA>  500
#> 149      1   0  5   312 Inf          span.ratio 2.231842e+01    <NA>  500
#> 150      1   0  5   312 Inf          aucinf.obs 2.345301e+04    <NA>  500
#> 151      0   1  6     0  24             auclast 1.866451e+04    <NA>  500
#> 152      0   1  6     0 Inf                cmax 3.236202e+03    <NA>  500
#> 153      0   1  6     0 Inf                tmax 2.000000e+00    <NA>  500
#> 154      0   1  6     0 Inf               tlast 1.680000e+02    <NA>  500
#> 155      0   1  6     0 Inf           clast.obs 8.552855e-05    <NA>  500
#> 156      0   1  6     0 Inf            lambda.z 8.536023e-02    <NA>  500
#> 157      0   1  6     0 Inf           r.squared 9.999375e-01    <NA>  500
#> 158      0   1  6     0 Inf       adj.r.squared 9.999167e-01    <NA>  500
#> 159      0   1  6     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 160      0   1  6     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 161      0   1  6     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 162      0   1  6     0 Inf          clast.pred 8.550542e-05    <NA>  500
#> 163      0   1  6     0 Inf           half.life 8.120259e+00    <NA>  500
#> 164      0   1  6     0 Inf          span.ratio 1.477785e+01    <NA>  500
#> 165      0   1  6     0 Inf          aucinf.obs 1.895641e+04    <NA>  500
#> 166      1   1  6   312 336             auclast 1.373147e+04    <NA>  500
#> 167      1   1  6   312 Inf                cmax 4.724722e+03    <NA>  500
#> 168      1   1  6   312 Inf                tmax 1.000000e+00    <NA>  500
#> 169      1   1  6   312 Inf               tlast 1.680000e+02    <NA>  500
#> 170      1   1  6   312 Inf           clast.obs 4.544319e-05    <NA>  500
#> 171      1   1  6   312 Inf            lambda.z 8.501096e-02    <NA>  500
#> 172      1   1  6   312 Inf           r.squared 9.999110e-01    <NA>  500
#> 173      1   1  6   312 Inf       adj.r.squared 9.998888e-01    <NA>  500
#> 174      1   1  6   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 175      1   1  6   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 176      1   1  6   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 177      1   1  6   312 Inf          clast.pred 4.775939e-05    <NA>  500
#> 178      1   1  6   312 Inf           half.life 8.153621e+00    <NA>  500
#> 179      1   1  6   312 Inf          span.ratio 1.766086e+01    <NA>  500
#> 180      1   1  6   312 Inf          aucinf.obs 1.384603e+04    <NA>  500
#> 181      0   1  7     0  24             auclast 1.516648e+04    <NA>  500
#> 182      0   1  7     0 Inf                cmax 2.479996e+03    <NA>  500
#> 183      0   1  7     0 Inf                tmax 1.500000e+00    <NA>  500
#> 184      0   1  7     0 Inf               tlast 1.680000e+02    <NA>  500
#> 185      0   1  7     0 Inf           clast.obs 5.028866e-06    <NA>  500
#> 186      0   1  7     0 Inf            lambda.z 1.033940e-01    <NA>  500
#> 187      0   1  7     0 Inf           r.squared 9.999443e-01    <NA>  500
#> 188      0   1  7     0 Inf       adj.r.squared 9.999258e-01    <NA>  500
#> 189      0   1  7     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 190      0   1  7     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 191      0   1  7     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 192      0   1  7     0 Inf          clast.pred 5.064049e-06    <NA>  500
#> 193      0   1  7     0 Inf           half.life 6.703939e+00    <NA>  500
#> 194      0   1  7     0 Inf          span.ratio 1.789992e+01    <NA>  500
#> 195      0   1  7     0 Inf          aucinf.obs 1.540089e+04    <NA>  500
#> 196      1   1  7   312 336             auclast 1.137142e+04    <NA>  500
#> 197      1   1  7   312 Inf                cmax 3.963992e+03    <NA>  500
#> 198      1   1  7   312 Inf                tmax 1.000000e+00    <NA>  500
#> 199      1   1  7   312 Inf               tlast 1.680000e+02    <NA>  500
#> 200      1   1  7   312 Inf           clast.obs 2.704002e-06    <NA>  500
#> 201      1   1  7   312 Inf            lambda.z 1.027174e-01    <NA>  500
#> 202      1   1  7   312 Inf           r.squared 9.999537e-01    <NA>  500
#> 203      1   1  7   312 Inf       adj.r.squared 9.999422e-01    <NA>  500
#> 204      1   1  7   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 205      1   1  7   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 206      1   1  7   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 207      1   1  7   312 Inf          clast.pred 2.636875e-06    <NA>  500
#> 208      1   1  7   312 Inf           half.life 6.748101e+00    <NA>  500
#> 209      1   1  7   312 Inf          span.ratio 2.133934e+01    <NA>  500
#> 210      1   1  7   312 Inf          aucinf.obs 1.144124e+04    <NA>  500
#> 211      1   0  8     0  24             auclast 2.721779e+04    <NA>  500
#> 212      1   0  8     0 Inf                cmax 6.994537e+03    <NA>  500
#> 213      1   0  8     0 Inf                tmax 1.500000e+00    <NA>  500
#> 214      1   0  8     0 Inf               tlast 1.680000e+02    <NA>  500
#> 215      1   0  8     0 Inf           clast.obs 7.224614e-05    <NA>  500
#> 216      1   0  8     0 Inf            lambda.z 9.219047e-02    <NA>  500
#> 217      1   0  8     0 Inf           r.squared 9.999442e-01    <NA>  500
#> 218      1   0  8     0 Inf       adj.r.squared 9.999330e-01    <NA>  500
#> 219      1   0  8     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 220      1   0  8     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 221      1   0  8     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 222      1   0  8     0 Inf          clast.pred 7.169306e-05    <NA>  500
#> 223      1   0  8     0 Inf           half.life 7.518643e+00    <NA>  500
#> 224      1   0  8     0 Inf          span.ratio 2.074843e+01    <NA>  500
#> 225      1   0  8     0 Inf          aucinf.obs 2.765162e+04    <NA>  500
#> 226      0   0  8   312 336             auclast 3.788023e+04    <NA>  500
#> 227      0   0  8   312 Inf                cmax 5.483093e+03    <NA>  500
#> 228      0   0  8   312 Inf                tmax 2.000000e+00    <NA>  500
#> 229      0   0  8   312 Inf               tlast 1.680000e+02    <NA>  500
#> 230      0   0  8   312 Inf           clast.obs 1.471476e-04    <NA>  500
#> 231      0   0  8   312 Inf            lambda.z 9.125122e-02    <NA>  500
#> 232      0   0  8   312 Inf           r.squared 9.999876e-01    <NA>  500
#> 233      0   0  8   312 Inf       adj.r.squared 9.999835e-01    <NA>  500
#> 234      0   0  8   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 235      0   0  8   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 236      0   0  8   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 237      0   0  8   312 Inf          clast.pred 1.451554e-04    <NA>  500
#> 238      0   0  8   312 Inf           half.life 7.596032e+00    <NA>  500
#> 239      0   0  8   312 Inf          span.ratio 1.579772e+01    <NA>  500
#> 240      0   0  8   312 Inf          aucinf.obs 3.886029e+04    <NA>  500
#> 241      0   0  9     0  24             auclast 3.965673e+04    <NA>  500
#> 242      0   0  9     0 Inf                cmax 5.496735e+03    <NA>  500
#> 243      0   0  9     0 Inf                tmax 3.000000e+00    <NA>  500
#> 244      0   0  9     0 Inf               tlast 1.680000e+02    <NA>  500
#> 245      0   0  9     0 Inf           clast.obs 2.767436e-04    <NA>  500
#> 246      0   0  9     0 Inf            lambda.z 8.765911e-02    <NA>  500
#> 247      0   0  9     0 Inf           r.squared 9.999889e-01    <NA>  500
#> 248      0   0  9     0 Inf       adj.r.squared 9.999852e-01    <NA>  500
#> 249      0   0  9     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 250      0   0  9     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 251      0   0  9     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 252      0   0  9     0 Inf          clast.pred 2.815239e-04    <NA>  500
#> 253      0   0  9     0 Inf           half.life 7.907304e+00    <NA>  500
#> 254      0   0  9     0 Inf          span.ratio 1.517584e+01    <NA>  500
#> 255      0   0  9     0 Inf          aucinf.obs 4.087872e+04    <NA>  500
#> 256      1   0  9   312 336             auclast 2.903697e+04    <NA>  500
#> 257      1   0  9   312 Inf                cmax 7.358267e+03    <NA>  500
#> 258      1   0  9   312 Inf                tmax 1.500000e+00    <NA>  500
#> 259      1   0  9   312 Inf               tlast 1.680000e+02    <NA>  500
#> 260      1   0  9   312 Inf           clast.obs 1.519074e-04    <NA>  500
#> 261      1   0  9   312 Inf            lambda.z 8.781736e-02    <NA>  500
#> 262      1   0  9   312 Inf           r.squared 9.999437e-01    <NA>  500
#> 263      1   0  9   312 Inf       adj.r.squared 9.999297e-01    <NA>  500
#> 264      1   0  9   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 265      1   0  9   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 266      1   0  9   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 267      1   0  9   312 Inf          clast.pred 1.437517e-04    <NA>  500
#> 268      1   0  9   312 Inf           half.life 7.893054e+00    <NA>  500
#> 269      1   0  9   312 Inf          span.ratio 1.824389e+01    <NA>  500
#> 270      1   0  9   312 Inf          aucinf.obs 2.955187e+04    <NA>  500
#> 271      0   0 10     0  24             auclast 2.072545e+04    <NA>  500
#> 272      0   0 10     0 Inf                cmax 3.262716e+03    <NA>  500
#> 273      0   0 10     0 Inf                tmax 2.000000e+00    <NA>  500
#> 274      0   0 10     0 Inf               tlast 1.680000e+02    <NA>  500
#> 275      0   0 10     0 Inf           clast.obs 1.266630e-04    <NA>  500
#> 276      0   0 10     0 Inf            lambda.z 8.516665e-02    <NA>  500
#> 277      0   0 10     0 Inf           r.squared 9.999193e-01    <NA>  500
#> 278      0   0 10     0 Inf       adj.r.squared 9.998923e-01    <NA>  500
#> 279      0   0 10     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 280      0   0 10     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 281      0   0 10     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 282      0   0 10     0 Inf          clast.pred 1.218694e-04    <NA>  500
#> 283      0   0 10     0 Inf           half.life 8.138716e+00    <NA>  500
#> 284      0   0 10     0 Inf          span.ratio 1.474434e+01    <NA>  500
#> 285      0   0 10     0 Inf          aucinf.obs 2.115739e+04    <NA>  500
#> 286      1   0 10   312 336             auclast 1.400201e+04    <NA>  500
#> 287      1   0 10   312 Inf                cmax 4.273190e+03    <NA>  500
#> 288      1   0 10   312 Inf                tmax 1.000000e+00    <NA>  500
#> 289      1   0 10   312 Inf               tlast 1.680000e+02    <NA>  500
#> 290      1   0 10   312 Inf           clast.obs 6.121180e-05    <NA>  500
#> 291      1   0 10   312 Inf            lambda.z 8.504572e-02    <NA>  500
#> 292      1   0 10   312 Inf           r.squared 9.999877e-01    <NA>  500
#> 293      1   0 10   312 Inf       adj.r.squared 9.999847e-01    <NA>  500
#> 294      1   0 10   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 295      1   0 10   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 296      1   0 10   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 297      1   0 10   312 Inf          clast.pred 6.124034e-05    <NA>  500
#> 298      1   0 10   312 Inf           half.life 8.150289e+00    <NA>  500
#> 299      1   0 10   312 Inf          span.ratio 1.766809e+01    <NA>  500
#> 300      1   0 10   312 Inf          aucinf.obs 1.415292e+04    <NA>  500
#> 301      0   0 11     0  24             auclast 1.412949e+04    <NA>  500
#> 302      0   0 11     0 Inf                cmax 2.301752e+03    <NA>  500
#> 303      0   0 11     0 Inf                tmax 2.000000e+00    <NA>  500
#> 304      0   0 11     0 Inf               tlast 1.680000e+02    <NA>  500
#> 305      0   0 11     0 Inf           clast.obs 9.495220e-06    <NA>  500
#> 306      0   0 11     0 Inf            lambda.z 9.872524e-02    <NA>  500
#> 307      0   0 11     0 Inf           r.squared 9.999859e-01    <NA>  500
#> 308      0   0 11     0 Inf       adj.r.squared 9.999813e-01    <NA>  500
#> 309      0   0 11     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 310      0   0 11     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 311      0   0 11     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 312      0   0 11     0 Inf          clast.pred 9.693159e-06    <NA>  500
#> 313      0   0 11     0 Inf           half.life 7.020973e+00    <NA>  500
#> 314      0   0 11     0 Inf          span.ratio 1.709165e+01    <NA>  500
#> 315      0   0 11     0 Inf          aucinf.obs 1.436188e+04    <NA>  500
#> 316      1   0 11   312 336             auclast 9.913804e+03    <NA>  500
#> 317      1   0 11   312 Inf                cmax 3.246430e+03    <NA>  500
#> 318      1   0 11   312 Inf                tmax 1.000000e+00    <NA>  500
#> 319      1   0 11   312 Inf               tlast 1.680000e+02    <NA>  500
#> 320      1   0 11   312 Inf           clast.obs 4.354529e-06    <NA>  500
#> 321      1   0 11   312 Inf            lambda.z 9.894250e-02    <NA>  500
#> 322      1   0 11   312 Inf           r.squared 9.999855e-01    <NA>  500
#> 323      1   0 11   312 Inf       adj.r.squared 9.999819e-01    <NA>  500
#> 324      1   0 11   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 325      1   0 11   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 326      1   0 11   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 327      1   0 11   312 Inf          clast.pred 4.369671e-06    <NA>  500
#> 328      1   0 11   312 Inf           half.life 7.005555e+00    <NA>  500
#> 329      1   0 11   312 Inf          span.ratio 2.055512e+01    <NA>  500
#> 330      1   0 11   312 Inf          aucinf.obs 9.982154e+03    <NA>  500
#> 331      1   0 12     0  24             auclast 1.750481e+04    <NA>  500
#> 332      1   0 12     0 Inf                cmax 4.611494e+03    <NA>  500
#> 333      1   0 12     0 Inf                tmax 1.000000e+00    <NA>  500
#> 334      1   0 12     0 Inf               tlast 1.680000e+02    <NA>  500
#> 335      1   0 12     0 Inf           clast.obs 2.251078e-05    <NA>  500
#> 336      1   0 12     0 Inf            lambda.z 9.469446e-02    <NA>  500
#> 337      1   0 12     0 Inf           r.squared 9.999719e-01    <NA>  500
#> 338      1   0 12     0 Inf       adj.r.squared 9.999648e-01    <NA>  500
#> 339      1   0 12     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 340      1   0 12     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 341      1   0 12     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 342      1   0 12     0 Inf          clast.pred 2.210769e-05    <NA>  500
#> 343      1   0 12     0 Inf           half.life 7.319828e+00    <NA>  500
#> 344      1   0 12     0 Inf          span.ratio 1.967259e+01    <NA>  500
#> 345      1   0 12     0 Inf          aucinf.obs 1.770308e+04    <NA>  500
#> 346      0   0 12   312 336             auclast 2.522414e+04    <NA>  500
#> 347      0   0 12   312 Inf                cmax 3.802789e+03    <NA>  500
#> 348      0   0 12   312 Inf                tmax 2.000000e+00    <NA>  500
#> 349      0   0 12   312 Inf               tlast 1.680000e+02    <NA>  500
#> 350      0   0 12   312 Inf           clast.obs 4.748016e-05    <NA>  500
#> 351      0   0 12   312 Inf            lambda.z 9.521710e-02    <NA>  500
#> 352      0   0 12   312 Inf           r.squared 9.999906e-01    <NA>  500
#> 353      0   0 12   312 Inf       adj.r.squared 9.999875e-01    <NA>  500
#> 354      0   0 12   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 355      0   0 12   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 356      0   0 12   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 357      0   0 12   312 Inf          clast.pred 4.682720e-05    <NA>  500
#> 358      0   0 12   312 Inf           half.life 7.279650e+00    <NA>  500
#> 359      0   0 12   312 Inf          span.ratio 1.648431e+01    <NA>  500
#> 360      0   0 12   312 Inf          aucinf.obs 2.584586e+04    <NA>  500
#> 361      0   0 13     0  24             auclast 1.729230e+04    <NA>  500
#> 362      0   0 13     0 Inf                cmax 3.521432e+03    <NA>  500
#> 363      0   0 13     0 Inf                tmax 2.000000e+00    <NA>  500
#> 364      0   0 13     0 Inf               tlast 1.680000e+02    <NA>  500
#> 365      0   0 13     0 Inf           clast.obs 2.320590e-05    <NA>  500
#> 366      0   0 13     0 Inf            lambda.z 9.318105e-02    <NA>  500
#> 367      0   0 13     0 Inf           r.squared 9.999748e-01    <NA>  500
#> 368      0   0 13     0 Inf       adj.r.squared 9.999663e-01    <NA>  500
#> 369      0   0 13     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 370      0   0 13     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 371      0   0 13     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 372      0   0 13     0 Inf          clast.pred 2.317447e-05    <NA>  500
#> 373      0   0 13     0 Inf           half.life 7.438714e+00    <NA>  500
#> 374      0   0 13     0 Inf          span.ratio 1.613182e+01    <NA>  500
#> 375      0   0 13     0 Inf          aucinf.obs 1.748411e+04    <NA>  500
#> 376      1   0 13   312 336             auclast 1.181238e+04    <NA>  500
#> 377      1   0 13   312 Inf                cmax 4.475683e+03    <NA>  500
#> 378      1   0 13   312 Inf                tmax 1.000000e+00    <NA>  500
#> 379      1   0 13   312 Inf               tlast 1.680000e+02    <NA>  500
#> 380      1   0 13   312 Inf           clast.obs 1.361345e-05    <NA>  500
#> 381      1   0 13   312 Inf            lambda.z 9.338642e-02    <NA>  500
#> 382      1   0 13   312 Inf           r.squared 9.999705e-01    <NA>  500
#> 383      1   0 13   312 Inf       adj.r.squared 9.999646e-01    <NA>  500
#> 384      1   0 13   312 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 385      1   0 13   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 386      1   0 13   312 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 387      1   0 13   312 Inf          clast.pred 1.340461e-05    <NA>  500
#> 388      1   0 13   312 Inf           half.life 7.422355e+00    <NA>  500
#> 389      1   0 13   312 Inf          span.ratio 2.101759e+01    <NA>  500
#> 390      1   0 13   312 Inf          aucinf.obs 1.190945e+04    <NA>  500
#> 391      1   0 14     0  24             auclast 1.126597e+04    <NA>  500
#> 392      1   0 14     0 Inf                cmax 4.004570e+03    <NA>  500
#> 393      1   0 14     0 Inf                tmax 1.000000e+00    <NA>  500
#> 394      1   0 14     0 Inf               tlast 1.680000e+02    <NA>  500
#> 395      1   0 14     0 Inf           clast.obs 2.505832e-05    <NA>  500
#> 396      1   0 14     0 Inf            lambda.z 8.872611e-02    <NA>  500
#> 397      1   0 14     0 Inf           r.squared 9.999870e-01    <NA>  500
#> 398      1   0 14     0 Inf       adj.r.squared 9.999837e-01    <NA>  500
#> 399      1   0 14     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 400      1   0 14     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 401      1   0 14     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 402      1   0 14     0 Inf          clast.pred 2.537010e-05    <NA>  500
#> 403      1   0 14     0 Inf           half.life 7.812212e+00    <NA>  500
#> 404      1   0 14     0 Inf          span.ratio 1.843268e+01    <NA>  500
#> 405      1   0 14     0 Inf          aucinf.obs 1.136659e+04    <NA>  500
#> 406      0   0 14   312 336             auclast 1.721037e+04    <NA>  500
#> 407      0   0 14   312 Inf                cmax 3.157314e+03    <NA>  500
#> 408      0   0 14   312 Inf                tmax 2.000000e+00    <NA>  500
#> 409      0   0 14   312 Inf               tlast 1.680000e+02    <NA>  500
#> 410      0   0 14   312 Inf           clast.obs 4.999301e-05    <NA>  500
#> 411      0   0 14   312 Inf            lambda.z 8.850608e-02    <NA>  500
#> 412      0   0 14   312 Inf           r.squared 9.999777e-01    <NA>  500
#> 413      0   0 14   312 Inf       adj.r.squared 9.999703e-01    <NA>  500
#> 414      0   0 14   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 415      0   0 14   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 416      0   0 14   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 417      0   0 14   312 Inf          clast.pred 5.122493e-05    <NA>  500
#> 418      0   0 14   312 Inf           half.life 7.831634e+00    <NA>  500
#> 419      0   0 14   312 Inf          span.ratio 1.532247e+01    <NA>  500
#> 420      0   0 14   312 Inf          aucinf.obs 1.746625e+04    <NA>  500
#> 421      0   0 15     0  24             auclast 2.874750e+04    <NA>  500
#> 422      0   0 15     0 Inf                cmax 4.926105e+03    <NA>  500
#> 423      0   0 15     0 Inf                tmax 2.000000e+00    <NA>  500
#> 424      0   0 15     0 Inf               tlast 1.680000e+02    <NA>  500
#> 425      0   0 15     0 Inf           clast.obs 2.789319e-05    <NA>  500
#> 426      0   0 15     0 Inf            lambda.z 9.788170e-02    <NA>  500
#> 427      0   0 15     0 Inf           r.squared 9.999226e-01    <NA>  500
#> 428      0   0 15     0 Inf       adj.r.squared 9.998968e-01    <NA>  500
#> 429      0   0 15     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 430      0   0 15     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 431      0   0 15     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 432      0   0 15     0 Inf          clast.pred 2.680777e-05    <NA>  500
#> 433      0   0 15     0 Inf           half.life 7.081479e+00    <NA>  500
#> 434      0   0 15     0 Inf          span.ratio 1.694561e+01    <NA>  500
#> 435      0   0 15     0 Inf          aucinf.obs 2.920060e+04    <NA>  500
#> 436      1   0 15   312 336             auclast 2.129608e+04    <NA>  500
#> 437      1   0 15   312 Inf                cmax 6.733367e+03    <NA>  500
#> 438      1   0 15   312 Inf                tmax 1.000000e+00    <NA>  500
#> 439      1   0 15   312 Inf               tlast 1.680000e+02    <NA>  500
#> 440      1   0 15   312 Inf           clast.obs 1.402076e-05    <NA>  500
#> 441      1   0 15   312 Inf            lambda.z 9.862580e-02    <NA>  500
#> 442      1   0 15   312 Inf           r.squared 9.999469e-01    <NA>  500
#> 443      1   0 15   312 Inf       adj.r.squared 9.999363e-01    <NA>  500
#> 444      1   0 15   312 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 445      1   0 15   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 446      1   0 15   312 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 447      1   0 15   312 Inf          clast.pred 1.357619e-05    <NA>  500
#> 448      1   0 15   312 Inf           half.life 7.028052e+00    <NA>  500
#> 449      1   0 15   312 Inf          span.ratio 2.219676e+01    <NA>  500
#> 450      1   0 15   312 Inf          aucinf.obs 2.149229e+04    <NA>  500
#> 451      1   0 16     0  24             auclast 1.388796e+04    <NA>  500
#> 452      1   0 16     0 Inf                cmax 4.537822e+03    <NA>  500
#> 453      1   0 16     0 Inf                tmax 1.000000e+00    <NA>  500
#> 454      1   0 16     0 Inf               tlast 1.680000e+02    <NA>  500
#> 455      1   0 16     0 Inf           clast.obs 2.099104e-05    <NA>  500
#> 456      1   0 16     0 Inf            lambda.z 9.182541e-02    <NA>  500
#> 457      1   0 16     0 Inf           r.squared 9.999899e-01    <NA>  500
#> 458      1   0 16     0 Inf       adj.r.squared 9.999873e-01    <NA>  500
#> 459      1   0 16     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 460      1   0 16     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 461      1   0 16     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 462      1   0 16     0 Inf          clast.pred 2.081782e-05    <NA>  500
#> 463      1   0 16     0 Inf           half.life 7.548534e+00    <NA>  500
#> 464      1   0 16     0 Inf          span.ratio 1.907655e+01    <NA>  500
#> 465      1   0 16     0 Inf          aucinf.obs 1.401384e+04    <NA>  500
#> 466      0   0 16   312 336             auclast 1.920053e+04    <NA>  500
#> 467      0   0 16   312 Inf                cmax 3.222528e+03    <NA>  500
#> 468      0   0 16   312 Inf                tmax 2.000000e+00    <NA>  500
#> 469      0   0 16   312 Inf               tlast 1.680000e+02    <NA>  500
#> 470      0   0 16   312 Inf           clast.obs 3.758053e-05    <NA>  500
#> 471      0   0 16   312 Inf            lambda.z 9.256004e-02    <NA>  500
#> 472      0   0 16   312 Inf           r.squared 9.999986e-01    <NA>  500
#> 473      0   0 16   312 Inf       adj.r.squared 9.999982e-01    <NA>  500
#> 474      0   0 16   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 475      0   0 16   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 476      0   0 16   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 477      0   0 16   312 Inf          clast.pred 3.779789e-05    <NA>  500
#> 478      0   0 16   312 Inf           half.life 7.488622e+00    <NA>  500
#> 479      0   0 16   312 Inf          span.ratio 1.602431e+01    <NA>  500
#> 480      0   0 16   312 Inf          aucinf.obs 1.952937e+04    <NA>  500
#> 481      1   0 17     0  24             auclast 1.469949e+04    <NA>  500
#> 482      1   0 17     0 Inf                cmax 4.572924e+03    <NA>  500
#> 483      1   0 17     0 Inf                tmax 1.000000e+00    <NA>  500
#> 484      1   0 17     0 Inf               tlast 1.680000e+02    <NA>  500
#> 485      1   0 17     0 Inf           clast.obs 1.582716e-05    <NA>  500
#> 486      1   0 17     0 Inf            lambda.z 9.426986e-02    <NA>  500
#> 487      1   0 17     0 Inf           r.squared 9.999544e-01    <NA>  500
#> 488      1   0 17     0 Inf       adj.r.squared 9.999430e-01    <NA>  500
#> 489      1   0 17     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 490      1   0 17     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 491      1   0 17     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 492      1   0 17     0 Inf          clast.pred 1.564340e-05    <NA>  500
#> 493      1   0 17     0 Inf           half.life 7.352797e+00    <NA>  500
#> 494      1   0 17     0 Inf          span.ratio 1.958438e+01    <NA>  500
#> 495      1   0 17     0 Inf          aucinf.obs 1.482847e+04    <NA>  500
#> 496      0   0 17   312 336             auclast 2.124316e+04    <NA>  500
#> 497      0   0 17   312 Inf                cmax 3.759509e+03    <NA>  500
#> 498      0   0 17   312 Inf                tmax 2.000000e+00    <NA>  500
#> 499      0   0 17   312 Inf               tlast 1.680000e+02    <NA>  500
#> 500      0   0 17   312 Inf           clast.obs 3.137722e-05    <NA>  500
#> 501      0   0 17   312 Inf            lambda.z 9.387799e-02    <NA>  500
#> 502      0   0 17   312 Inf           r.squared 9.999892e-01    <NA>  500
#> 503      0   0 17   312 Inf       adj.r.squared 9.999856e-01    <NA>  500
#> 504      0   0 17   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 505      0   0 17   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 506      0   0 17   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 507      0   0 17   312 Inf          clast.pred 3.140842e-05    <NA>  500
#> 508      0   0 17   312 Inf           half.life 7.383490e+00    <NA>  500
#> 509      0   0 17   312 Inf          span.ratio 1.625248e+01    <NA>  500
#> 510      0   0 17   312 Inf          aucinf.obs 2.157841e+04    <NA>  500
#> 511      1   0 18     0  24             auclast 1.963267e+04    <NA>  500
#> 512      1   0 18     0 Inf                cmax 5.212429e+03    <NA>  500
#> 513      1   0 18     0 Inf                tmax 1.000000e+00    <NA>  500
#> 514      1   0 18     0 Inf               tlast 1.680000e+02    <NA>  500
#> 515      1   0 18     0 Inf           clast.obs 2.234632e-06    <NA>  500
#> 516      1   0 18     0 Inf            lambda.z 1.106557e-01    <NA>  500
#> 517      1   0 18     0 Inf           r.squared 9.999557e-01    <NA>  500
#> 518      1   0 18     0 Inf       adj.r.squared 9.999446e-01    <NA>  500
#> 519      1   0 18     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 520      1   0 18     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 521      1   0 18     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 522      1   0 18     0 Inf          clast.pred 2.119822e-06    <NA>  500
#> 523      1   0 18     0 Inf           half.life 6.263998e+00    <NA>  500
#> 524      1   0 18     0 Inf          span.ratio 2.298851e+01    <NA>  500
#> 525      1   0 18     0 Inf          aucinf.obs 1.979621e+04    <NA>  500
#> 526      0   0 18   312 336             auclast 2.652888e+04    <NA>  500
#> 527      0   0 18   312 Inf                cmax 3.693272e+03    <NA>  500
#> 528      0   0 18   312 Inf                tmax 2.000000e+00    <NA>  500
#> 529      0   0 18   312 Inf               tlast 1.680000e+02    <NA>  500
#> 530      0   0 18   312 Inf           clast.obs 5.333505e-06    <NA>  500
#> 531      0   0 18   312 Inf            lambda.z 1.105909e-01    <NA>  500
#> 532      0   0 18   312 Inf           r.squared 9.999868e-01    <NA>  500
#> 533      0   0 18   312 Inf       adj.r.squared 9.999803e-01    <NA>  500
#> 534      0   0 18   312 Inf lambda.z.time.first 7.200000e+01    <NA>  500
#> 535      0   0 18   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 536      0   0 18   312 Inf   lambda.z.n.points 4.000000e+00    <NA>  500
#> 537      0   0 18   312 Inf          clast.pred 5.395323e-06    <NA>  500
#> 538      0   0 18   312 Inf           half.life 6.267668e+00    <NA>  500
#> 539      0   0 18   312 Inf          span.ratio 1.531670e+01    <NA>  500
#> 540      0   0 18   312 Inf          aucinf.obs 2.722821e+04    <NA>  500
#> 541      0   1 19     0  24             auclast 1.759191e+04    <NA>  500
#> 542      0   1 19     0 Inf                cmax 2.815318e+03    <NA>  500
#> 543      0   1 19     0 Inf                tmax 1.500000e+00    <NA>  500
#> 544      0   1 19     0 Inf               tlast 1.680000e+02    <NA>  500
#> 545      0   1 19     0 Inf           clast.obs 4.124970e-04    <NA>  500
#> 546      0   1 19     0 Inf            lambda.z 7.452964e-02    <NA>  500
#> 547      0   1 19     0 Inf           r.squared 9.999964e-01    <NA>  500
#> 548      0   1 19     0 Inf       adj.r.squared 9.999928e-01    <NA>  500
#> 549      0   1 19     0 Inf lambda.z.time.first 9.600000e+01    <NA>  500
#> 550      0   1 19     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 551      0   1 19     0 Inf   lambda.z.n.points 3.000000e+00    <NA>  500
#> 552      0   1 19     0 Inf          clast.pred 4.141164e-04    <NA>  500
#> 553      0   1 19     0 Inf           half.life 9.300289e+00    <NA>  500
#> 554      0   1 19     0 Inf          span.ratio 7.741695e+00    <NA>  500
#> 555      0   1 19     0 Inf          aucinf.obs 1.795650e+04    <NA>  500
#> 556      1   1 19   312 336             auclast 1.210305e+04    <NA>  500
#> 557      1   1 19   312 Inf                cmax 3.838087e+03    <NA>  500
#> 558      1   1 19   312 Inf                tmax 1.000000e+00    <NA>  500
#> 559      1   1 19   312 Inf               tlast 1.680000e+02    <NA>  500
#> 560      1   1 19   312 Inf           clast.obs 2.247090e-04    <NA>  500
#> 561      1   1 19   312 Inf            lambda.z 7.521028e-02    <NA>  500
#> 562      1   1 19   312 Inf           r.squared 9.999746e-01    <NA>  500
#> 563      1   1 19   312 Inf       adj.r.squared 9.999682e-01    <NA>  500
#> 564      1   1 19   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 565      1   1 19   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 566      1   1 19   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 567      1   1 19   312 Inf          clast.pred 2.218219e-04    <NA>  500
#> 568      1   1 19   312 Inf           half.life 9.216123e+00    <NA>  500
#> 569      1   1 19   312 Inf          span.ratio 1.562479e+01    <NA>  500
#> 570      1   1 19   312 Inf          aucinf.obs 1.225372e+04    <NA>  500
#> 571      1   0 20     0  24             auclast 1.365089e+04    <NA>  500
#> 572      1   0 20     0 Inf                cmax 4.126813e+03    <NA>  500
#> 573      1   0 20     0 Inf                tmax 1.000000e+00    <NA>  500
#> 574      1   0 20     0 Inf               tlast 1.680000e+02    <NA>  500
#> 575      1   0 20     0 Inf           clast.obs 3.322179e-06    <NA>  500
#> 576      1   0 20     0 Inf            lambda.z 1.064484e-01    <NA>  500
#> 577      1   0 20     0 Inf           r.squared 9.999136e-01    <NA>  500
#> 578      1   0 20     0 Inf       adj.r.squared 9.998963e-01    <NA>  500
#> 579      1   0 20     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 580      1   0 20     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 581      1   0 20     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 582      1   0 20     0 Inf          clast.pred 3.193431e-06    <NA>  500
#> 583      1   0 20     0 Inf           half.life 6.511579e+00    <NA>  500
#> 584      1   0 20     0 Inf          span.ratio 2.395732e+01    <NA>  500
#> 585      1   0 20     0 Inf          aucinf.obs 1.378046e+04    <NA>  500
#> 586      0   0 20   312 336             auclast 2.050570e+04    <NA>  500
#> 587      0   0 20   312 Inf                cmax 3.358273e+03    <NA>  500
#> 588      0   0 20   312 Inf                tmax 2.000000e+00    <NA>  500
#> 589      0   0 20   312 Inf               tlast 1.680000e+02    <NA>  500
#> 590      0   0 20   312 Inf           clast.obs 7.391557e-06    <NA>  500
#> 591      0   0 20   312 Inf            lambda.z 1.061880e-01    <NA>  500
#> 592      0   0 20   312 Inf           r.squared 9.999752e-01    <NA>  500
#> 593      0   0 20   312 Inf       adj.r.squared 9.999670e-01    <NA>  500
#> 594      0   0 20   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 595      0   0 20   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 596      0   0 20   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 597      0   0 20   312 Inf          clast.pred 7.244380e-06    <NA>  500
#> 598      0   0 20   312 Inf           half.life 6.527547e+00    <NA>  500
#> 599      0   0 20   312 Inf          span.ratio 1.838363e+01    <NA>  500
#> 600      0   0 20   312 Inf          aucinf.obs 2.087764e+04    <NA>  500
```
