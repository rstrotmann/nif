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
#> 
#>    ID start end PPTESTCD   PPORRES exclude DOSE
#> 1   1     0 312  auclast       NaN    <NA>  500
#> 2   1     0 312     cmax  6325.101    <NA>  500
#> 3   1     0 312     tmax     1.000    <NA>  500
#> 4   2     0 312  auclast 18252.638    <NA>  500
#> 5   2     0 312     cmax  2789.380    <NA>  500
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
#> 16  6     0 312  auclast 26174.681    <NA>  500
#> 17  6     0 312     cmax  4106.337    <NA>  500
#> 18  6     0 312     tmax     2.000    <NA>  500
#> 19  7     0 312  auclast       NaN    <NA>  500
#> 20  7     0 312     cmax  3525.703    <NA>  500
#> 21  7     0 312     tmax     3.000    <NA>  500
#> 22  8     0 312  auclast       NaN    <NA>  500
#> 23  8     0 312     cmax  6994.537    <NA>  500
#> 24  8     0 312     tmax     1.500    <NA>  500
#> 25  9     0 312  auclast 40878.720    <NA>  500
#> 26  9     0 312     cmax  5496.735    <NA>  500
#> 27  9     0 312     tmax     3.000    <NA>  500
#> 28 10     0 312  auclast 26751.872    <NA>  500
#> 29 10     0 312     cmax  3842.711    <NA>  500
#> 30 10     0 312     tmax     2.000    <NA>  500
#> 31 11     0 312  auclast 21929.126    <NA>  500
#> 32 11     0 312     cmax  3166.345    <NA>  500
#> 33 11     0 312     tmax     2.000    <NA>  500
#> 34 12     0 312  auclast 23243.084    <NA>  500
#> 35 12     0 312     cmax  5426.283    <NA>  500
#> 36 12     0 312     tmax     1.500    <NA>  500
#> 37 13     0 312  auclast       NaN    <NA>  500
#> 38 13     0 312     cmax  4815.076    <NA>  500
#> 39 13     0 312     tmax     2.000    <NA>  500
#> 40 14     0 312  auclast       NaN    <NA>  500
#> 41 14     0 312     cmax  4489.737    <NA>  500
#> 42 14     0 312     tmax     1.000    <NA>  500
#> 43 15     0 312  auclast 48328.416    <NA>  500
#> 44 15     0 312     cmax  6801.036    <NA>  500
#> 45 15     0 312     tmax     3.000    <NA>  500
#> 46 16     0 312  auclast 19915.700    <NA>  500
#> 47 16     0 312     cmax  5547.706    <NA>  500
#> 48 16     0 312     tmax     1.500    <NA>  500
#> 49 17     0 312  auclast 20075.151    <NA>  500
#> 50 17     0 312     cmax  5447.778    <NA>  500
#> 51 17     0 312     tmax     1.500    <NA>  500
#> 52 18     0 312  auclast 26635.716    <NA>  500
#> 53 18     0 312     cmax  6407.331    <NA>  500
#> 54 18     0 312     tmax     1.500    <NA>  500
#> 55 19     0 312  auclast 25325.634    <NA>  500
#> 56 19     0 312     cmax  3516.646    <NA>  500
#> 57 19     0 312     tmax     3.000    <NA>  500
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
#> 
#>     FASTED SEX ID start end            PPTESTCD      PPORRES exclude DOSE
#> 1        1   1  1     0  24             auclast 2.504642e+04    <NA>  500
#> 2        1   1  1     0 Inf                cmax 6.325101e+03    <NA>  500
#> 3        1   1  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 4        1   1  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 5        1   1  1     0 Inf           clast.obs 9.704197e-05    <NA>  500
#> 6        1   1  1     0 Inf            lambda.z 8.895551e-02    <NA>  500
#> 7        1   1  1     0 Inf           r.squared 9.999811e-01    <NA>  500
#> 8        1   1  1     0 Inf       adj.r.squared 9.999764e-01    <NA>  500
#> 9        1   1  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 10       1   1  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 11       1   1  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 12       1   1  1     0 Inf          clast.pred 9.649366e-05    <NA>  500
#> 13       1   1  1     0 Inf           half.life 7.792066e+00    <NA>  500
#> 14       1   1  1     0 Inf          span.ratio 1.848034e+01    <NA>  500
#> 15       1   1  1     0 Inf          aucinf.obs 2.544210e+04    <NA>  500
#> 16       0   1  1   312 336             auclast 3.431709e+04    <NA>  500
#> 17       0   1  1   312 Inf                cmax 4.956902e+03    <NA>  500
#> 18       0   1  1   312 Inf                tmax 3.000000e+00    <NA>  500
#> 19       0   1  1   312 Inf               tlast 1.680000e+02    <NA>  500
#> 20       0   1  1   312 Inf           clast.obs 1.845688e-04    <NA>  500
#> 21       0   1  1   312 Inf            lambda.z 8.884753e-02    <NA>  500
#> 22       0   1  1   312 Inf           r.squared 9.999968e-01    <NA>  500
#> 23       0   1  1   312 Inf       adj.r.squared 9.999958e-01    <NA>  500
#> 24       0   1  1   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 25       0   1  1   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 26       0   1  1   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 27       0   1  1   312 Inf          clast.pred 1.853255e-04    <NA>  500
#> 28       0   1  1   312 Inf           half.life 7.801535e+00    <NA>  500
#> 29       0   1  1   312 Inf          span.ratio 1.538159e+01    <NA>  500
#> 30       0   1  1   312 Inf          aucinf.obs 3.521725e+04    <NA>  500
#> 31       0   1  2     0  24             auclast 1.787013e+04    <NA>  500
#> 32       0   1  2     0 Inf                cmax 2.789380e+03    <NA>  500
#> 33       0   1  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 34       0   1  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 35       0   1  2     0 Inf           clast.obs 4.580101e-05    <NA>  500
#> 36       0   1  2     0 Inf            lambda.z 9.187969e-02    <NA>  500
#> 37       0   1  2     0 Inf           r.squared 9.999718e-01    <NA>  500
#> 38       0   1  2     0 Inf       adj.r.squared 9.999624e-01    <NA>  500
#> 39       0   1  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 40       0   1  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 41       0   1  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 42       0   1  2     0 Inf          clast.pred 4.548223e-05    <NA>  500
#> 43       0   1  2     0 Inf           half.life 7.544074e+00    <NA>  500
#> 44       0   1  2     0 Inf          span.ratio 1.590652e+01    <NA>  500
#> 45       0   1  2     0 Inf          aucinf.obs 1.825264e+04    <NA>  500
#> 46       1   1  2   312 336             auclast 1.178450e+04    <NA>  500
#> 47       1   1  2   312 Inf                cmax 3.431745e+03    <NA>  500
#> 48       1   1  2   312 Inf                tmax 1.000000e+00    <NA>  500
#> 49       1   1  2   312 Inf               tlast 1.680000e+02    <NA>  500
#> 50       1   1  2   312 Inf           clast.obs 2.193919e-05    <NA>  500
#> 51       1   1  2   312 Inf            lambda.z 9.165271e-02    <NA>  500
#> 52       1   1  2   312 Inf           r.squared 9.999813e-01    <NA>  500
#> 53       1   1  2   312 Inf       adj.r.squared 9.999766e-01    <NA>  500
#> 54       1   1  2   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 55       1   1  2   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 56       1   1  2   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 57       1   1  2   312 Inf          clast.pred 2.149044e-05    <NA>  500
#> 58       1   1  2   312 Inf           half.life 7.562757e+00    <NA>  500
#> 59       1   1  2   312 Inf          span.ratio 1.904068e+01    <NA>  500
#> 60       1   1  2   312 Inf          aucinf.obs 1.191081e+04    <NA>  500
#> 61       1   1  3     0  24             auclast 2.127273e+04    <NA>  500
#> 62       1   1  3     0 Inf                cmax 6.439872e+03    <NA>  500
#> 63       1   1  3     0 Inf                tmax 1.000000e+00    <NA>  500
#> 64       1   1  3     0 Inf               tlast 1.680000e+02    <NA>  500
#> 65       1   1  3     0 Inf           clast.obs 7.390935e-06    <NA>  500
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
#> 80       0   1  3   312 Inf           clast.obs 1.471343e-05    <NA>  500
#> 81       0   1  3   312 Inf            lambda.z 1.045051e-01    <NA>  500
#> 82       0   1  3   312 Inf           r.squared 9.999828e-01    <NA>  500
#> 83       0   1  3   312 Inf       adj.r.squared 9.999771e-01    <NA>  500
#> 84       0   1  3   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 85       0   1  3   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 86       0   1  3   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 87       0   1  3   312 Inf          clast.pred 1.464370e-05    <NA>  500
#> 88       0   1  3   312 Inf           half.life 6.632665e+00    <NA>  500
#> 89       0   1  3   312 Inf          span.ratio 1.809228e+01    <NA>  500
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
#> 125      0   0  5     0 Inf           clast.obs 3.524927e-05    <NA>  500
#> 126      0   0  5     0 Inf            lambda.z 9.840941e-02    <NA>  500
#> 127      0   0  5     0 Inf           r.squared 9.999789e-01    <NA>  500
#> 128      0   0  5     0 Inf       adj.r.squared 9.999719e-01    <NA>  500
#> 129      0   0  5     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 130      0   0  5     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 131      0   0  5     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 132      0   0  5     0 Inf          clast.pred 3.514760e-05    <NA>  500
#> 133      0   0  5     0 Inf           half.life 7.043505e+00    <NA>  500
#> 134      0   0  5     0 Inf          span.ratio 1.703697e+01    <NA>  500
#> 135      0   0  5     0 Inf          aucinf.obs 3.262419e+04    <NA>  500
#> 136      1   0  5   312 336             auclast 2.316675e+04    <NA>  500
#> 137      1   0  5   312 Inf                cmax 7.048011e+03    <NA>  500
#> 138      1   0  5   312 Inf                tmax 1.000000e+00    <NA>  500
#> 139      1   0  5   312 Inf               tlast 1.680000e+02    <NA>  500
#> 140      1   0  5   312 Inf           clast.obs 1.858451e-05    <NA>  500
#> 141      1   0  5   312 Inf            lambda.z 9.916307e-02    <NA>  500
#> 142      1   0  5   312 Inf           r.squared 9.999720e-01    <NA>  500
#> 143      1   0  5   312 Inf       adj.r.squared 9.999665e-01    <NA>  500
#> 144      1   0  5   312 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 145      1   0  5   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 146      1   0  5   312 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 147      1   0  5   312 Inf          clast.pred 1.844627e-05    <NA>  500
#> 148      1   0  5   312 Inf           half.life 6.989973e+00    <NA>  500
#> 149      1   0  5   312 Inf          span.ratio 2.231768e+01    <NA>  500
#> 150      1   0  5   312 Inf          aucinf.obs 2.345301e+04    <NA>  500
#> 151      0   1  6     0  24             auclast 2.563207e+04    <NA>  500
#> 152      0   1  6     0 Inf                cmax 4.106337e+03    <NA>  500
#> 153      0   1  6     0 Inf                tmax 2.000000e+00    <NA>  500
#> 154      0   1  6     0 Inf               tlast 1.680000e+02    <NA>  500
#> 155      0   1  6     0 Inf           clast.obs 2.177254e-04    <NA>  500
#> 156      0   1  6     0 Inf            lambda.z 8.351055e-02    <NA>  500
#> 157      0   1  6     0 Inf           r.squared 9.999362e-01    <NA>  500
#> 158      0   1  6     0 Inf       adj.r.squared 9.999149e-01    <NA>  500
#> 159      0   1  6     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 160      0   1  6     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 161      0   1  6     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 162      0   1  6     0 Inf          clast.pred 2.177732e-04    <NA>  500
#> 163      0   1  6     0 Inf           half.life 8.300115e+00    <NA>  500
#> 164      0   1  6     0 Inf          span.ratio 1.445763e+01    <NA>  500
#> 165      0   1  6     0 Inf          aucinf.obs 2.617468e+04    <NA>  500
#> 166      1   1  6   312 336             auclast 1.899695e+04    <NA>  500
#> 167      1   1  6   312 Inf                cmax 5.739598e+03    <NA>  500
#> 168      1   1  6   312 Inf                tmax 1.000000e+00    <NA>  500
#> 169      1   1  6   312 Inf               tlast 1.680000e+02    <NA>  500
#> 170      1   1  6   312 Inf           clast.obs 1.165751e-04    <NA>  500
#> 171      1   1  6   312 Inf            lambda.z 8.317778e-02    <NA>  500
#> 172      1   1  6   312 Inf           r.squared 9.999066e-01    <NA>  500
#> 173      1   1  6   312 Inf       adj.r.squared 9.998833e-01    <NA>  500
#> 174      1   1  6   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 175      1   1  6   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 176      1   1  6   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 177      1   1  6   312 Inf          clast.pred 1.225492e-04    <NA>  500
#> 178      1   1  6   312 Inf           half.life 8.333321e+00    <NA>  500
#> 179      1   1  6   312 Inf          span.ratio 1.728002e+01    <NA>  500
#> 180      1   1  6   312 Inf          aucinf.obs 1.922765e+04    <NA>  500
#> 181      0   1  7     0  24             auclast 2.479636e+04    <NA>  500
#> 182      0   1  7     0 Inf                cmax 3.525703e+03    <NA>  500
#> 183      0   1  7     0 Inf                tmax 3.000000e+00    <NA>  500
#> 184      0   1  7     0 Inf               tlast 1.680000e+02    <NA>  500
#> 185      0   1  7     0 Inf           clast.obs 2.398994e-05    <NA>  500
#> 186      0   1  7     0 Inf            lambda.z 9.960954e-02    <NA>  500
#> 187      0   1  7     0 Inf           r.squared 9.999575e-01    <NA>  500
#> 188      0   1  7     0 Inf       adj.r.squared 9.999433e-01    <NA>  500
#> 189      0   1  7     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 190      0   1  7     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 191      0   1  7     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 192      0   1  7     0 Inf          clast.pred 2.430465e-05    <NA>  500
#> 193      0   1  7     0 Inf           half.life 6.958642e+00    <NA>  500
#> 194      0   1  7     0 Inf          span.ratio 1.724474e+01    <NA>  500
#> 195      0   1  7     0 Inf          aucinf.obs 2.536877e+04    <NA>  500
#> 196      1   1  7   312 336             auclast 1.878799e+04    <NA>  500
#> 197      1   1  7   312 Inf                cmax 5.463950e+03    <NA>  500
#> 198      1   1  7   312 Inf                tmax 1.000000e+00    <NA>  500
#> 199      1   1  7   312 Inf               tlast 1.680000e+02    <NA>  500
#> 200      1   1  7   312 Inf           clast.obs 1.331033e-05    <NA>  500
#> 201      1   1  7   312 Inf            lambda.z 9.901730e-02    <NA>  500
#> 202      1   1  7   312 Inf           r.squared 9.999483e-01    <NA>  500
#> 203      1   1  7   312 Inf       adj.r.squared 9.999353e-01    <NA>  500
#> 204      1   1  7   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 205      1   1  7   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 206      1   1  7   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 207      1   1  7   312 Inf          clast.pred 1.297499e-05    <NA>  500
#> 208      1   1  7   312 Inf           half.life 7.000263e+00    <NA>  500
#> 209      1   1  7   312 Inf          span.ratio 2.057066e+01    <NA>  500
#> 210      1   1  7   312 Inf          aucinf.obs 1.899722e+04    <NA>  500
#> 211      1   0  8     0  24             auclast 2.721779e+04    <NA>  500
#> 212      1   0  8     0 Inf                cmax 6.994537e+03    <NA>  500
#> 213      1   0  8     0 Inf                tmax 1.500000e+00    <NA>  500
#> 214      1   0  8     0 Inf               tlast 1.680000e+02    <NA>  500
#> 215      1   0  8     0 Inf           clast.obs 7.224617e-05    <NA>  500
#> 216      1   0  8     0 Inf            lambda.z 9.219047e-02    <NA>  500
#> 217      1   0  8     0 Inf           r.squared 9.999442e-01    <NA>  500
#> 218      1   0  8     0 Inf       adj.r.squared 9.999330e-01    <NA>  500
#> 219      1   0  8     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 220      1   0  8     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 221      1   0  8     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 222      1   0  8     0 Inf          clast.pred 7.169308e-05    <NA>  500
#> 223      1   0  8     0 Inf           half.life 7.518643e+00    <NA>  500
#> 224      1   0  8     0 Inf          span.ratio 2.074843e+01    <NA>  500
#> 225      1   0  8     0 Inf          aucinf.obs 2.765162e+04    <NA>  500
#> 226      0   0  8   312 336             auclast 3.788023e+04    <NA>  500
#> 227      0   0  8   312 Inf                cmax 5.483093e+03    <NA>  500
#> 228      0   0  8   312 Inf                tmax 2.000000e+00    <NA>  500
#> 229      0   0  8   312 Inf               tlast 1.680000e+02    <NA>  500
#> 230      0   0  8   312 Inf           clast.obs 1.472322e-04    <NA>  500
#> 231      0   0  8   312 Inf            lambda.z 9.124757e-02    <NA>  500
#> 232      0   0  8   312 Inf           r.squared 9.999874e-01    <NA>  500
#> 233      0   0  8   312 Inf       adj.r.squared 9.999832e-01    <NA>  500
#> 234      0   0  8   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 235      0   0  8   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 236      0   0  8   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 237      0   0  8   312 Inf          clast.pred 1.452053e-04    <NA>  500
#> 238      0   0  8   312 Inf           half.life 7.596336e+00    <NA>  500
#> 239      0   0  8   312 Inf          span.ratio 1.579709e+01    <NA>  500
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
#> 260      1   0  9   312 Inf           clast.obs 1.519292e-04    <NA>  500
#> 261      1   0  9   312 Inf            lambda.z 8.781657e-02    <NA>  500
#> 262      1   0  9   312 Inf           r.squared 9.999436e-01    <NA>  500
#> 263      1   0  9   312 Inf       adj.r.squared 9.999295e-01    <NA>  500
#> 264      1   0  9   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 265      1   0  9   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 266      1   0  9   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 267      1   0  9   312 Inf          clast.pred 1.437644e-04    <NA>  500
#> 268      1   0  9   312 Inf           half.life 7.893125e+00    <NA>  500
#> 269      1   0  9   312 Inf          span.ratio 1.824372e+01    <NA>  500
#> 270      1   0  9   312 Inf          aucinf.obs 2.955187e+04    <NA>  500
#> 271      0   0 10     0  24             auclast 2.608545e+04    <NA>  500
#> 272      0   0 10     0 Inf                cmax 3.842711e+03    <NA>  500
#> 273      0   0 10     0 Inf                tmax 2.000000e+00    <NA>  500
#> 274      0   0 10     0 Inf               tlast 1.680000e+02    <NA>  500
#> 275      0   0 10     0 Inf           clast.obs 2.539816e-04    <NA>  500
#> 276      0   0 10     0 Inf            lambda.z 8.367579e-02    <NA>  500
#> 277      0   0 10     0 Inf           r.squared 9.999242e-01    <NA>  500
#> 278      0   0 10     0 Inf       adj.r.squared 9.998989e-01    <NA>  500
#> 279      0   0 10     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 280      0   0 10     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 281      0   0 10     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 282      0   0 10     0 Inf          clast.pred 2.446395e-04    <NA>  500
#> 283      0   0 10     0 Inf           half.life 8.283725e+00    <NA>  500
#> 284      0   0 10     0 Inf          span.ratio 1.448624e+01    <NA>  500
#> 285      0   0 10     0 Inf          aucinf.obs 2.675187e+04    <NA>  500
#> 286      1   0 10   312 336             auclast 1.772037e+04    <NA>  500
#> 287      1   0 10   312 Inf                cmax 4.871447e+03    <NA>  500
#> 288      1   0 10   312 Inf                tmax 1.000000e+00    <NA>  500
#> 289      1   0 10   312 Inf               tlast 1.680000e+02    <NA>  500
#> 290      1   0 10   312 Inf           clast.obs 1.236499e-04    <NA>  500
#> 291      1   0 10   312 Inf            lambda.z 8.359194e-02    <NA>  500
#> 292      1   0 10   312 Inf           r.squared 9.999874e-01    <NA>  500
#> 293      1   0 10   312 Inf       adj.r.squared 9.999842e-01    <NA>  500
#> 294      1   0 10   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 295      1   0 10   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 296      1   0 10   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 297      1   0 10   312 Inf          clast.pred 1.237296e-04    <NA>  500
#> 298      1   0 10   312 Inf           half.life 8.292034e+00    <NA>  500
#> 299      1   0 10   312 Inf          span.ratio 1.736607e+01    <NA>  500
#> 300      1   0 10   312 Inf          aucinf.obs 1.797195e+04    <NA>  500
#> 301      0   0 11     0  24             auclast 2.142685e+04    <NA>  500
#> 302      0   0 11     0 Inf                cmax 3.166345e+03    <NA>  500
#> 303      0   0 11     0 Inf                tmax 2.000000e+00    <NA>  500
#> 304      0   0 11     0 Inf               tlast 1.680000e+02    <NA>  500
#> 305      0   0 11     0 Inf           clast.obs 3.574064e-05    <NA>  500
#> 306      0   0 11     0 Inf            lambda.z 9.556086e-02    <NA>  500
#> 307      0   0 11     0 Inf           r.squared 9.999791e-01    <NA>  500
#> 308      0   0 11     0 Inf       adj.r.squared 9.999722e-01    <NA>  500
#> 309      0   0 11     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 310      0   0 11     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 311      0   0 11     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 312      0   0 11     0 Inf          clast.pred 3.653020e-05    <NA>  500
#> 313      0   0 11     0 Inf           half.life 7.253464e+00    <NA>  500
#> 314      0   0 11     0 Inf          span.ratio 1.654382e+01    <NA>  500
#> 315      0   0 11     0 Inf          aucinf.obs 2.192913e+04    <NA>  500
#> 316      1   0 11   312 336             auclast 1.520481e+04    <NA>  500
#> 317      1   0 11   312 Inf                cmax 4.223442e+03    <NA>  500
#> 318      1   0 11   312 Inf                tmax 1.000000e+00    <NA>  500
#> 319      1   0 11   312 Inf               tlast 1.680000e+02    <NA>  500
#> 320      1   0 11   312 Inf           clast.obs 1.656390e-05    <NA>  500
#> 321      1   0 11   312 Inf            lambda.z 9.590828e-02    <NA>  500
#> 322      1   0 11   312 Inf           r.squared 9.999847e-01    <NA>  500
#> 323      1   0 11   312 Inf       adj.r.squared 9.999808e-01    <NA>  500
#> 324      1   0 11   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 325      1   0 11   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 326      1   0 11   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 327      1   0 11   312 Inf          clast.pred 1.664784e-05    <NA>  500
#> 328      1   0 11   312 Inf           half.life 7.227188e+00    <NA>  500
#> 329      1   0 11   312 Inf          span.ratio 1.992476e+01    <NA>  500
#> 330      1   0 11   312 Inf          aucinf.obs 1.537818e+04    <NA>  500
#> 331      1   0 12     0  24             auclast 2.288086e+04    <NA>  500
#> 332      1   0 12     0 Inf                cmax 5.426283e+03    <NA>  500
#> 333      1   0 12     0 Inf                tmax 1.500000e+00    <NA>  500
#> 334      1   0 12     0 Inf               tlast 1.680000e+02    <NA>  500
#> 335      1   0 12     0 Inf           clast.obs 5.739691e-05    <NA>  500
#> 336      1   0 12     0 Inf            lambda.z 9.219572e-02    <NA>  500
#> 337      1   0 12     0 Inf           r.squared 9.999707e-01    <NA>  500
#> 338      1   0 12     0 Inf       adj.r.squared 9.999633e-01    <NA>  500
#> 339      1   0 12     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 340      1   0 12     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 341      1   0 12     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 342      1   0 12     0 Inf          clast.pred 5.638429e-05    <NA>  500
#> 343      1   0 12     0 Inf           half.life 7.518214e+00    <NA>  500
#> 344      1   0 12     0 Inf          span.ratio 1.915349e+01    <NA>  500
#> 345      1   0 12     0 Inf          aucinf.obs 2.324308e+04    <NA>  500
#> 346      0   0 12   312 336             auclast 3.274660e+04    <NA>  500
#> 347      0   0 12   312 Inf                cmax 4.500265e+03    <NA>  500
#> 348      0   0 12   312 Inf                tmax 2.000000e+00    <NA>  500
#> 349      0   0 12   312 Inf               tlast 1.680000e+02    <NA>  500
#> 350      0   0 12   312 Inf           clast.obs 1.192894e-04    <NA>  500
#> 351      0   0 12   312 Inf            lambda.z 9.264300e-02    <NA>  500
#> 352      0   0 12   312 Inf           r.squared 9.999919e-01    <NA>  500
#> 353      0   0 12   312 Inf       adj.r.squared 9.999892e-01    <NA>  500
#> 354      0   0 12   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 355      0   0 12   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 356      0   0 12   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 357      0   0 12   312 Inf          clast.pred 1.178747e-04    <NA>  500
#> 358      0   0 12   312 Inf           half.life 7.481916e+00    <NA>  500
#> 359      0   0 12   312 Inf          span.ratio 1.603867e+01    <NA>  500
#> 360      0   0 12   312 Inf          aucinf.obs 3.377363e+04    <NA>  500
#> 361      0   0 13     0  24             auclast 2.661068e+04    <NA>  500
#> 362      0   0 13     0 Inf                cmax 4.815076e+03    <NA>  500
#> 363      0   0 13     0 Inf                tmax 2.000000e+00    <NA>  500
#> 364      0   0 13     0 Inf               tlast 1.680000e+02    <NA>  500
#> 365      0   0 13     0 Inf           clast.obs 9.344742e-05    <NA>  500
#> 366      0   0 13     0 Inf            lambda.z 8.991307e-02    <NA>  500
#> 367      0   0 13     0 Inf           r.squared 9.999727e-01    <NA>  500
#> 368      0   0 13     0 Inf       adj.r.squared 9.999636e-01    <NA>  500
#> 369      0   0 13     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 370      0   0 13     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 371      0   0 13     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 372      0   0 13     0 Inf          clast.pred 9.330874e-05    <NA>  500
#> 373      0   0 13     0 Inf           half.life 7.709082e+00    <NA>  500
#> 374      0   0 13     0 Inf          span.ratio 1.556606e+01    <NA>  500
#> 375      0   0 13     0 Inf          aucinf.obs 2.709179e+04    <NA>  500
#> 376      1   0 13   312 336             auclast 1.839950e+04    <NA>  500
#> 377      1   0 13   312 Inf                cmax 5.777191e+03    <NA>  500
#> 378      1   0 13   312 Inf                tmax 1.000000e+00    <NA>  500
#> 379      1   0 13   312 Inf               tlast 1.680000e+02    <NA>  500
#> 380      1   0 13   312 Inf           clast.obs 5.534056e-05    <NA>  500
#> 381      1   0 13   312 Inf            lambda.z 9.011663e-02    <NA>  500
#> 382      1   0 13   312 Inf           r.squared 9.999682e-01    <NA>  500
#> 383      1   0 13   312 Inf       adj.r.squared 9.999618e-01    <NA>  500
#> 384      1   0 13   312 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 385      1   0 13   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 386      1   0 13   312 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 387      1   0 13   312 Inf          clast.pred 5.460419e-05    <NA>  500
#> 388      1   0 13   312 Inf           half.life 7.691668e+00    <NA>  500
#> 389      1   0 13   312 Inf          span.ratio 2.028169e+01    <NA>  500
#> 390      1   0 13   312 Inf          aucinf.obs 1.865533e+04    <NA>  500
#> 391      1   0 14     0  24             auclast 1.358547e+04    <NA>  500
#> 392      1   0 14     0 Inf                cmax 4.489737e+03    <NA>  500
#> 393      1   0 14     0 Inf                tmax 1.000000e+00    <NA>  500
#> 394      1   0 14     0 Inf               tlast 1.680000e+02    <NA>  500
#> 395      1   0 14     0 Inf           clast.obs 4.392014e-05    <NA>  500
#> 396      1   0 14     0 Inf            lambda.z 8.757461e-02    <NA>  500
#> 397      1   0 14     0 Inf           r.squared 9.999867e-01    <NA>  500
#> 398      1   0 14     0 Inf       adj.r.squared 9.999833e-01    <NA>  500
#> 399      1   0 14     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 400      1   0 14     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 401      1   0 14     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 402      1   0 14     0 Inf          clast.pred 4.446672e-05    <NA>  500
#> 403      1   0 14     0 Inf           half.life 7.914933e+00    <NA>  500
#> 404      1   0 14     0 Inf          span.ratio 1.819346e+01    <NA>  500
#> 405      1   0 14     0 Inf          aucinf.obs 1.373685e+04    <NA>  500
#> 406      0   0 14   312 336             auclast 2.069564e+04    <NA>  500
#> 407      0   0 14   312 Inf                cmax 3.629774e+03    <NA>  500
#> 408      0   0 14   312 Inf                tmax 2.000000e+00    <NA>  500
#> 409      0   0 14   312 Inf               tlast 1.680000e+02    <NA>  500
#> 410      0   0 14   312 Inf           clast.obs 8.699047e-05    <NA>  500
#> 411      0   0 14   312 Inf            lambda.z 8.736426e-02    <NA>  500
#> 412      0   0 14   312 Inf           r.squared 9.999752e-01    <NA>  500
#> 413      0   0 14   312 Inf       adj.r.squared 9.999669e-01    <NA>  500
#> 414      0   0 14   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 415      0   0 14   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 416      0   0 14   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 417      0   0 14   312 Inf          clast.pred 8.923103e-05    <NA>  500
#> 418      0   0 14   312 Inf           half.life 7.933990e+00    <NA>  500
#> 419      0   0 14   312 Inf          span.ratio 1.512480e+01    <NA>  500
#> 420      0   0 14   312 Inf          aucinf.obs 2.106485e+04    <NA>  500
#> 421      0   0 15     0  24             auclast 4.705248e+04    <NA>  500
#> 422      0   0 15     0 Inf                cmax 6.801036e+03    <NA>  500
#> 423      0   0 15     0 Inf                tmax 3.000000e+00    <NA>  500
#> 424      0   0 15     0 Inf               tlast 1.680000e+02    <NA>  500
#> 425      0   0 15     0 Inf           clast.obs 1.637464e-04    <NA>  500
#> 426      0   0 15     0 Inf            lambda.z 9.299096e-02    <NA>  500
#> 427      0   0 15     0 Inf           r.squared 9.999320e-01    <NA>  500
#> 428      0   0 15     0 Inf       adj.r.squared 9.999093e-01    <NA>  500
#> 429      0   0 15     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 430      0   0 15     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 431      0   0 15     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 432      0   0 15     0 Inf          clast.pred 1.585577e-04    <NA>  500
#> 433      0   0 15     0 Inf           half.life 7.453920e+00    <NA>  500
#> 434      0   0 15     0 Inf          span.ratio 1.609891e+01    <NA>  500
#> 435      0   0 15     0 Inf          aucinf.obs 4.832842e+04    <NA>  500
#> 436      1   0 15   312 336             auclast 3.544204e+04    <NA>  500
#> 437      1   0 15   312 Inf                cmax 9.339374e+03    <NA>  500
#> 438      1   0 15   312 Inf                tmax 1.500000e+00    <NA>  500
#> 439      1   0 15   312 Inf               tlast 1.680000e+02    <NA>  500
#> 440      1   0 15   312 Inf           clast.obs 8.556535e-05    <NA>  500
#> 441      1   0 15   312 Inf            lambda.z 9.361431e-02    <NA>  500
#> 442      1   0 15   312 Inf           r.squared 9.999499e-01    <NA>  500
#> 443      1   0 15   312 Inf       adj.r.squared 9.999399e-01    <NA>  500
#> 444      1   0 15   312 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 445      1   0 15   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 446      1   0 15   312 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 447      1   0 15   312 Inf          clast.pred 8.300402e-05    <NA>  500
#> 448      1   0 15   312 Inf           half.life 7.404287e+00    <NA>  500
#> 449      1   0 15   312 Inf          span.ratio 2.106888e+01    <NA>  500
#> 450      1   0 15   312 Inf          aucinf.obs 3.605800e+04    <NA>  500
#> 451      1   0 16     0  24             auclast 1.964559e+04    <NA>  500
#> 452      1   0 16     0 Inf                cmax 5.547706e+03    <NA>  500
#> 453      1   0 16     0 Inf                tmax 1.500000e+00    <NA>  500
#> 454      1   0 16     0 Inf               tlast 1.680000e+02    <NA>  500
#> 455      1   0 16     0 Inf           clast.obs 6.348328e-05    <NA>  500
#> 456      1   0 16     0 Inf            lambda.z 8.924301e-02    <NA>  500
#> 457      1   0 16     0 Inf           r.squared 9.999895e-01    <NA>  500
#> 458      1   0 16     0 Inf       adj.r.squared 9.999868e-01    <NA>  500
#> 459      1   0 16     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 460      1   0 16     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 461      1   0 16     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 462      1   0 16     0 Inf          clast.pred 6.299725e-05    <NA>  500
#> 463      1   0 16     0 Inf           half.life 7.766963e+00    <NA>  500
#> 464      1   0 16     0 Inf          span.ratio 1.854006e+01    <NA>  500
#> 465      1   0 16     0 Inf          aucinf.obs 1.991570e+04    <NA>  500
#> 466      0   0 16   312 336             auclast 2.697415e+04    <NA>  500
#> 467      0   0 16   312 Inf                cmax 4.161112e+03    <NA>  500
#> 468      0   0 16   312 Inf                tmax 3.000000e+00    <NA>  500
#> 469      0   0 16   312 Inf               tlast 1.680000e+02    <NA>  500
#> 470      0   0 16   312 Inf           clast.obs 1.123415e-04    <NA>  500
#> 471      0   0 16   312 Inf            lambda.z 8.996109e-02    <NA>  500
#> 472      0   0 16   312 Inf           r.squared 9.999979e-01    <NA>  500
#> 473      0   0 16   312 Inf       adj.r.squared 9.999972e-01    <NA>  500
#> 474      0   0 16   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 475      0   0 16   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 476      0   0 16   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 477      0   0 16   312 Inf          clast.pred 1.130966e-04    <NA>  500
#> 478      0   0 16   312 Inf           half.life 7.704966e+00    <NA>  500
#> 479      0   0 16   312 Inf          span.ratio 1.557437e+01    <NA>  500
#> 480      0   0 16   312 Inf          aucinf.obs 2.762222e+04    <NA>  500
#> 481      1   0 17     0  24             auclast 1.982688e+04    <NA>  500
#> 482      1   0 17     0 Inf                cmax 5.447778e+03    <NA>  500
#> 483      1   0 17     0 Inf                tmax 1.500000e+00    <NA>  500
#> 484      1   0 17     0 Inf               tlast 1.680000e+02    <NA>  500
#> 485      1   0 17     0 Inf           clast.obs 4.083561e-05    <NA>  500
#> 486      1   0 17     0 Inf            lambda.z 9.207758e-02    <NA>  500
#> 487      1   0 17     0 Inf           r.squared 9.999516e-01    <NA>  500
#> 488      1   0 17     0 Inf       adj.r.squared 9.999395e-01    <NA>  500
#> 489      1   0 17     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 490      1   0 17     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 491      1   0 17     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 492      1   0 17     0 Inf          clast.pred 4.030935e-05    <NA>  500
#> 493      1   0 17     0 Inf           half.life 7.527860e+00    <NA>  500
#> 494      1   0 17     0 Inf          span.ratio 1.912894e+01    <NA>  500
#> 495      1   0 17     0 Inf          aucinf.obs 2.007515e+04    <NA>  500
#> 496      0   0 17   312 336             auclast 2.838540e+04    <NA>  500
#> 497      0   0 17   312 Inf                cmax 4.650235e+03    <NA>  500
#> 498      0   0 17   312 Inf                tmax 2.000000e+00    <NA>  500
#> 499      0   0 17   312 Inf               tlast 1.680000e+02    <NA>  500
#> 500      0   0 17   312 Inf           clast.obs 7.862104e-05    <NA>  500
#> 501      0   0 17   312 Inf            lambda.z 9.176957e-02    <NA>  500
#> 502      0   0 17   312 Inf           r.squared 9.999844e-01    <NA>  500
#> 503      0   0 17   312 Inf       adj.r.squared 9.999793e-01    <NA>  500
#> 504      0   0 17   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 505      0   0 17   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 506      0   0 17   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 507      0   0 17   312 Inf          clast.pred 7.926174e-05    <NA>  500
#> 508      0   0 17   312 Inf           half.life 7.553127e+00    <NA>  500
#> 509      0   0 17   312 Inf          span.ratio 1.588746e+01    <NA>  500
#> 510      0   0 17   312 Inf          aucinf.obs 2.897641e+04    <NA>  500
#> 511      1   0 18     0  24             auclast 2.631496e+04    <NA>  500
#> 512      1   0 18     0 Inf                cmax 6.407331e+03    <NA>  500
#> 513      1   0 18     0 Inf                tmax 1.500000e+00    <NA>  500
#> 514      1   0 18     0 Inf               tlast 1.680000e+02    <NA>  500
#> 515      1   0 18     0 Inf           clast.obs 6.838679e-06    <NA>  500
#> 516      1   0 18     0 Inf            lambda.z 1.073402e-01    <NA>  500
#> 517      1   0 18     0 Inf           r.squared 9.999576e-01    <NA>  500
#> 518      1   0 18     0 Inf       adj.r.squared 9.999470e-01    <NA>  500
#> 519      1   0 18     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 520      1   0 18     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 521      1   0 18     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 522      1   0 18     0 Inf          clast.pred 6.510515e-06    <NA>  500
#> 523      1   0 18     0 Inf           half.life 6.457482e+00    <NA>  500
#> 524      1   0 18     0 Inf          span.ratio 2.229971e+01    <NA>  500
#> 525      1   0 18     0 Inf          aucinf.obs 2.663572e+04    <NA>  500
#> 526      0   0 18   312 336             auclast 3.539986e+04    <NA>  500
#> 527      0   0 18   312 Inf                cmax 4.499100e+03    <NA>  500
#> 528      0   0 18   312 Inf                tmax 2.000000e+00    <NA>  500
#> 529      0   0 18   312 Inf               tlast 1.680000e+02    <NA>  500
#> 530      0   0 18   312 Inf           clast.obs 1.581906e-05    <NA>  500
#> 531      0   0 18   312 Inf            lambda.z 1.078428e-01    <NA>  500
#> 532      0   0 18   312 Inf           r.squared 9.999480e-01    <NA>  500
#> 533      0   0 18   312 Inf       adj.r.squared 9.999307e-01    <NA>  500
#> 534      0   0 18   312 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 535      0   0 18   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 536      0   0 18   312 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 537      0   0 18   312 Inf          clast.pred 1.580885e-05    <NA>  500
#> 538      0   0 18   312 Inf           half.life 6.427385e+00    <NA>  500
#> 539      0   0 18   312 Inf          span.ratio 1.867011e+01    <NA>  500
#> 540      0   0 18   312 Inf          aucinf.obs 3.657208e+04    <NA>  500
#> 541      0   1 19     0  24             auclast 2.461443e+04    <NA>  500
#> 542      0   1 19     0 Inf                cmax 3.516646e+03    <NA>  500
#> 543      0   1 19     0 Inf                tmax 3.000000e+00    <NA>  500
#> 544      0   1 19     0 Inf               tlast 1.680000e+02    <NA>  500
#> 545      0   1 19     0 Inf           clast.obs 1.120752e-03    <NA>  500
#> 546      0   1 19     0 Inf            lambda.z 7.252218e-02    <NA>  500
#> 547      0   1 19     0 Inf           r.squared 9.999962e-01    <NA>  500
#> 548      0   1 19     0 Inf       adj.r.squared 9.999924e-01    <NA>  500
#> 549      0   1 19     0 Inf lambda.z.time.first 9.600000e+01    <NA>  500
#> 550      0   1 19     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 551      0   1 19     0 Inf   lambda.z.n.points 3.000000e+00    <NA>  500
#> 552      0   1 19     0 Inf          clast.pred 1.125150e-03    <NA>  500
#> 553      0   1 19     0 Inf           half.life 9.557727e+00    <NA>  500
#> 554      0   1 19     0 Inf          span.ratio 7.533172e+00    <NA>  500
#> 555      0   1 19     0 Inf          aucinf.obs 2.532563e+04    <NA>  500
#> 556      1   1 19   312 336             auclast 1.697932e+04    <NA>  500
#> 557      1   1 19   312 Inf                cmax 4.588825e+03    <NA>  500
#> 558      1   1 19   312 Inf                tmax 1.500000e+00    <NA>  500
#> 559      1   1 19   312 Inf               tlast 1.680000e+02    <NA>  500
#> 560      1   1 19   312 Inf           clast.obs 6.157276e-04    <NA>  500
#> 561      1   1 19   312 Inf            lambda.z 7.320385e-02    <NA>  500
#> 562      1   1 19   312 Inf           r.squared 9.999732e-01    <NA>  500
#> 563      1   1 19   312 Inf       adj.r.squared 9.999664e-01    <NA>  500
#> 564      1   1 19   312 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 565      1   1 19   312 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 566      1   1 19   312 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 567      1   1 19   312 Inf          clast.pred 6.078007e-04    <NA>  500
#> 568      1   1 19   312 Inf           half.life 9.468725e+00    <NA>  500
#> 569      1   1 19   312 Inf          span.ratio 1.520796e+01    <NA>  500
#> 570      1   1 19   312 Inf          aucinf.obs 1.729700e+04    <NA>  500
#> 571      1   0 20     0  24             auclast 1.365089e+04    <NA>  500
#> 572      1   0 20     0 Inf                cmax 4.126813e+03    <NA>  500
#> 573      1   0 20     0 Inf                tmax 1.000000e+00    <NA>  500
#> 574      1   0 20     0 Inf               tlast 1.680000e+02    <NA>  500
#> 575      1   0 20     0 Inf           clast.obs 3.322188e-06    <NA>  500
#> 576      1   0 20     0 Inf            lambda.z 1.064484e-01    <NA>  500
#> 577      1   0 20     0 Inf           r.squared 9.999136e-01    <NA>  500
#> 578      1   0 20     0 Inf       adj.r.squared 9.998963e-01    <NA>  500
#> 579      1   0 20     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 580      1   0 20     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 581      1   0 20     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 582      1   0 20     0 Inf          clast.pred 3.193435e-06    <NA>  500
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
