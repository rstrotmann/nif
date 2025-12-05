# Non-compartmental analysis of NIF data

**\[experimental\]**

## Usage

``` r
nca1(
  nif,
  analyte = NULL,
  parent = NULL,
  keep = NULL,
  group = NULL,
  time = "TIME",
  average_duplicates = TRUE
)
```

## Arguments

- nif:

  A nif object.

- analyte:

  The analyte as character.

- parent:

  The parent as character.

- keep:

  Columns to keep, as character.

- group:

  Columns to group by, as character.

- time:

  The time field as character.

- average_duplicates:

  Average duplicate concentration values, as logical.

## Value

A data frame.

## Details

This function is a wrapper around the NCA functions provided by the
[PKNCA](https://CRAN.R-project.org/package=PKNCA) package.

## Examples

``` r
nca1(examplinib_sad_nif, time = "TAD")
#> NCA: No analyte specified. Selected RS2023 as the most likely.
#>     ID DI start end            PPTESTCD      PPORRES exclude DOSE
#> 1    1  1     0  24             auclast 1.358359e+02    <NA>    5
#> 2    1  1     0 Inf                cmax 4.855300e+01    <NA>    5
#> 3    1  1     0 Inf                tmax 1.000000e+00    <NA>    5
#> 4    1  1     0 Inf               tlast 9.600000e+01    <NA>    5
#> 5    1  1     0 Inf           clast.obs 2.000000e-04    <NA>    5
#> 6    1  1     0 Inf            lambda.z 8.606345e-02    <NA>    5
#> 7    1  1     0 Inf           r.squared 9.995549e-01    <NA>    5
#> 8    1  1     0 Inf       adj.r.squared 9.993323e-01    <NA>    5
#> 9    1  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>    5
#> 10   1  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>    5
#> 11   1  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>    5
#> 12   1  1     0 Inf          clast.pred 1.896607e-04    <NA>    5
#> 13   1  1     0 Inf           half.life 8.053909e+00    <NA>    5
#> 14   1  1     0 Inf          span.ratio 8.939758e+00    <NA>    5
#> 15   1  1     0 Inf          aucinf.obs 1.369298e+02    <NA>    5
#> 16   2  1     0  24             auclast 1.272326e+02    <NA>    5
#> 17   2  1     0 Inf                cmax 4.242000e+01    <NA>    5
#> 18   2  1     0 Inf                tmax 1.000000e+00    <NA>    5
#> 19   2  1     0 Inf               tlast 9.600000e+01    <NA>    5
#> 20   2  1     0 Inf           clast.obs 1.000000e-04    <NA>    5
#> 21   2  1     0 Inf            lambda.z 9.577401e-02    <NA>    5
#> 22   2  1     0 Inf           r.squared 9.997482e-01    <NA>    5
#> 23   2  1     0 Inf       adj.r.squared 9.996223e-01    <NA>    5
#> 24   2  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>    5
#> 25   2  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>    5
#> 26   2  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>    5
#> 27   2  1     0 Inf          clast.pred 9.564347e-05    <NA>    5
#> 28   2  1     0 Inf           half.life 7.237320e+00    <NA>    5
#> 29   2  1     0 Inf          span.ratio 9.948434e+00    <NA>    5
#> 30   2  1     0 Inf          aucinf.obs 1.282302e+02    <NA>    5
#> 31   3  1     0  24             auclast 1.468552e+02    <NA>    5
#> 32   3  1     0 Inf                cmax 4.359650e+01    <NA>    5
#> 33   3  1     0 Inf                tmax 1.000000e+00    <NA>    5
#> 34   3  1     0 Inf               tlast 9.600000e+01    <NA>    5
#> 35   3  1     0 Inf           clast.obs 2.000000e-04    <NA>    5
#> 36   3  1     0 Inf            lambda.z 8.992019e-02    <NA>    5
#> 37   3  1     0 Inf           r.squared 9.997885e-01    <NA>    5
#> 38   3  1     0 Inf       adj.r.squared 9.996828e-01    <NA>    5
#> 39   3  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>    5
#> 40   3  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>    5
#> 41   3  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>    5
#> 42   3  1     0 Inf          clast.pred 2.070389e-04    <NA>    5
#> 43   3  1     0 Inf           half.life 7.708471e+00    <NA>    5
#> 44   3  1     0 Inf          span.ratio 9.340374e+00    <NA>    5
#> 45   3  1     0 Inf          aucinf.obs 1.483347e+02    <NA>    5
#> 46   4  1     0  24             auclast 2.351229e+02    <NA>   10
#> 47   4  1     0 Inf                cmax 7.640650e+01    <NA>   10
#> 48   4  1     0 Inf                tmax 1.000000e+00    <NA>   10
#> 49   4  1     0 Inf               tlast 9.600000e+01    <NA>   10
#> 50   4  1     0 Inf           clast.obs 2.000000e-04    <NA>   10
#> 51   4  1     0 Inf            lambda.z 9.370438e-02    <NA>   10
#> 52   4  1     0 Inf           r.squared 9.993040e-01    <NA>   10
#> 53   4  1     0 Inf       adj.r.squared 9.989560e-01    <NA>   10
#> 54   4  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>   10
#> 55   4  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>   10
#> 56   4  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>   10
#> 57   4  1     0 Inf          clast.pred 1.859780e-04    <NA>   10
#> 58   4  1     0 Inf           half.life 7.397170e+00    <NA>   10
#> 59   4  1     0 Inf          span.ratio 9.733453e+00    <NA>   10
#> 60   4  1     0 Inf          aucinf.obs 2.368426e+02    <NA>   10
#> 61   5  1     0  24             auclast 2.552607e+02    <NA>   10
#> 62   5  1     0 Inf                cmax 8.028810e+01    <NA>   10
#> 63   5  1     0 Inf                tmax 1.000000e+00    <NA>   10
#> 64   5  1     0 Inf               tlast 9.600000e+01    <NA>   10
#> 65   5  1     0 Inf           clast.obs 5.000000e-04    <NA>   10
#> 66   5  1     0 Inf            lambda.z 8.231758e-02    <NA>   10
#> 67   5  1     0 Inf           r.squared 9.999246e-01    <NA>   10
#> 68   5  1     0 Inf       adj.r.squared 9.998492e-01    <NA>   10
#> 69   5  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>   10
#> 70   5  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>   10
#> 71   5  1     0 Inf   lambda.z.n.points 3.000000e+00    <NA>   10
#> 72   5  1     0 Inf          clast.pred 4.950725e-04    <NA>   10
#> 73   5  1     0 Inf           half.life 8.420403e+00    <NA>   10
#> 74   5  1     0 Inf          span.ratio 5.700440e+00    <NA>   10
#> 75   5  1     0 Inf          aucinf.obs 2.576642e+02    <NA>   10
#> 76   6  1     0  24             auclast 2.037377e+02    <NA>   10
#> 77   6  1     0 Inf                cmax 6.989260e+01    <NA>   10
#> 78   6  1     0 Inf                tmax 1.000000e+00    <NA>   10
#> 79   6  1     0 Inf               tlast 9.600000e+01    <NA>   10
#> 80   6  1     0 Inf           clast.obs 1.000000e-04    <NA>   10
#> 81   6  1     0 Inf            lambda.z 1.006709e-01    <NA>   10
#> 82   6  1     0 Inf           r.squared 9.995714e-01    <NA>   10
#> 83   6  1     0 Inf       adj.r.squared 9.994285e-01    <NA>   10
#> 84   6  1     0 Inf lambda.z.time.first 1.200000e+01    <NA>   10
#> 85   6  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>   10
#> 86   6  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>   10
#> 87   6  1     0 Inf          clast.pred 1.060309e-04    <NA>   10
#> 88   6  1     0 Inf           half.life 6.885276e+00    <NA>   10
#> 89   6  1     0 Inf          span.ratio 1.219995e+01    <NA>   10
#> 90   6  1     0 Inf          aucinf.obs 2.051535e+02    <NA>   10
#> 91   7  1     0  24             auclast 5.178897e+02    <NA>   20
#> 92   7  1     0 Inf                cmax 1.724369e+02    <NA>   20
#> 93   7  1     0 Inf                tmax 1.000000e+00    <NA>   20
#> 94   7  1     0 Inf               tlast 9.600000e+01    <NA>   20
#> 95   7  1     0 Inf           clast.obs 6.000000e-04    <NA>   20
#> 96   7  1     0 Inf            lambda.z 9.033266e-02    <NA>   20
#> 97   7  1     0 Inf           r.squared 9.999737e-01    <NA>   20
#> 98   7  1     0 Inf       adj.r.squared 9.999605e-01    <NA>   20
#> 99   7  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>   20
#> 100  7  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>   20
#> 101  7  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>   20
#> 102  7  1     0 Inf          clast.pred 5.921746e-04    <NA>   20
#> 103  7  1     0 Inf           half.life 7.673273e+00    <NA>   20
#> 104  7  1     0 Inf          span.ratio 9.383218e+00    <NA>   20
#> 105  7  1     0 Inf          aucinf.obs 5.222793e+02    <NA>   20
#> 106  8  1     0  24             auclast 6.075938e+02    <NA>   20
#> 107  8  1     0 Inf                cmax 1.831545e+02    <NA>   20
#> 108  8  1     0 Inf                tmax 1.000000e+00    <NA>   20
#> 109  8  1     0 Inf               tlast 9.600000e+01    <NA>   20
#> 110  8  1     0 Inf           clast.obs 3.000000e-04    <NA>   20
#> 111  8  1     0 Inf            lambda.z 1.034840e-01    <NA>   20
#> 112  8  1     0 Inf           r.squared 9.999430e-01    <NA>   20
#> 113  8  1     0 Inf       adj.r.squared 9.999145e-01    <NA>   20
#> 114  8  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>   20
#> 115  8  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>   20
#> 116  8  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>   20
#> 117  8  1     0 Inf          clast.pred 2.937904e-04    <NA>   20
#> 118  8  1     0 Inf           half.life 6.698107e+00    <NA>   20
#> 119  8  1     0 Inf          span.ratio 1.074931e+01    <NA>   20
#> 120  8  1     0 Inf          aucinf.obs 6.125093e+02    <NA>   20
#> 121  9  1     0  24             auclast 4.797358e+02    <NA>   20
#> 122  9  1     0 Inf                cmax 1.529454e+02    <NA>   20
#> 123  9  1     0 Inf                tmax 1.000000e+00    <NA>   20
#> 124  9  1     0 Inf               tlast 9.600000e+01    <NA>   20
#> 125  9  1     0 Inf           clast.obs 3.000000e-04    <NA>   20
#> 126  9  1     0 Inf            lambda.z 9.909291e-02    <NA>   20
#> 127  9  1     0 Inf           r.squared 9.999927e-01    <NA>   20
#> 128  9  1     0 Inf       adj.r.squared 9.999855e-01    <NA>   20
#> 129  9  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>   20
#> 130  9  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>   20
#> 131  9  1     0 Inf   lambda.z.n.points 3.000000e+00    <NA>   20
#> 132  9  1     0 Inf          clast.pred 2.988914e-04    <NA>   20
#> 133  9  1     0 Inf           half.life 6.994922e+00    <NA>   20
#> 134  9  1     0 Inf          span.ratio 6.862121e+00    <NA>   20
#> 135  9  1     0 Inf          aucinf.obs 4.837786e+02    <NA>   20
#> 136 10  1     0  24             auclast 1.304964e+03    <NA>   50
#> 137 10  1     0 Inf                cmax 3.929893e+02    <NA>   50
#> 138 10  1     0 Inf                tmax 1.000000e+00    <NA>   50
#> 139 10  1     0 Inf               tlast 1.440000e+02    <NA>   50
#> 140 10  1     0 Inf           clast.obs 1.000000e-04    <NA>   50
#> 141 10  1     0 Inf            lambda.z 7.704916e-02    <NA>   50
#> 142 10  1     0 Inf           r.squared 9.999629e-01    <NA>   50
#> 143 10  1     0 Inf       adj.r.squared 9.999506e-01    <NA>   50
#> 144 10  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>   50
#> 145 10  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>   50
#> 146 10  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>   50
#> 147 10  1     0 Inf          clast.pred 1.020973e-04    <NA>   50
#> 148 10  1     0 Inf           half.life 8.996167e+00    <NA>   50
#> 149 10  1     0 Inf          span.ratio 1.333901e+01    <NA>   50
#> 150 10  1     0 Inf          aucinf.obs 1.318549e+03    <NA>   50
#> 151 11  1     0  24             auclast 2.517417e+03    <NA>   50
#> 152 11  1     0 Inf                cmax 7.695240e+02    <NA>   50
#> 153 11  1     0 Inf                tmax 1.000000e+00    <NA>   50
#> 154 11  1     0 Inf               tlast 1.440000e+02    <NA>   50
#> 155 11  1     0 Inf           clast.obs 1.000000e-04    <NA>   50
#> 156 11  1     0 Inf            lambda.z 8.471310e-02    <NA>   50
#> 157 11  1     0 Inf           r.squared 9.999778e-01    <NA>   50
#> 158 11  1     0 Inf       adj.r.squared 9.999704e-01    <NA>   50
#> 159 11  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>   50
#> 160 11  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>   50
#> 161 11  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>   50
#> 162 11  1     0 Inf          clast.pred 9.813760e-05    <NA>   50
#> 163 11  1     0 Inf           half.life 8.182291e+00    <NA>   50
#> 164 11  1     0 Inf          span.ratio 1.466582e+01    <NA>   50
#> 165 11  1     0 Inf          aucinf.obs 2.547796e+03    <NA>   50
#> 166 12  1     0  24             auclast 1.463739e+03    <NA>   50
#> 167 12  1     0 Inf                cmax 4.477305e+02    <NA>   50
#> 168 12  1     0 Inf                tmax 1.000000e+00    <NA>   50
#> 169 12  1     0 Inf               tlast 9.600000e+01    <NA>   50
#> 170 12  1     0 Inf           clast.obs 6.000000e-04    <NA>   50
#> 171 12  1     0 Inf            lambda.z 1.051391e-01    <NA>   50
#> 172 12  1     0 Inf           r.squared 9.999930e-01    <NA>   50
#> 173 12  1     0 Inf       adj.r.squared 9.999894e-01    <NA>   50
#> 174 12  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>   50
#> 175 12  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>   50
#> 176 12  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>   50
#> 177 12  1     0 Inf          clast.pred 5.990725e-04    <NA>   50
#> 178 12  1     0 Inf           half.life 6.592668e+00    <NA>   50
#> 179 12  1     0 Inf          span.ratio 1.092122e+01    <NA>   50
#> 180 12  1     0 Inf          aucinf.obs 1.474792e+03    <NA>   50
#> 181 13  1     0  24             auclast 4.774852e+03    <NA>  100
#> 182 13  1     0 Inf                cmax 1.189801e+03    <NA>  100
#> 183 13  1     0 Inf                tmax 1.500000e+00    <NA>  100
#> 184 13  1     0 Inf               tlast 9.600000e+01    <NA>  100
#> 185 13  1     0 Inf           clast.obs 4.800000e-03    <NA>  100
#> 186 13  1     0 Inf            lambda.z 9.949830e-02    <NA>  100
#> 187 13  1     0 Inf           r.squared 9.999706e-01    <NA>  100
#> 188 13  1     0 Inf       adj.r.squared 9.999559e-01    <NA>  100
#> 189 13  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  100
#> 190 13  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  100
#> 191 13  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  100
#> 192 13  1     0 Inf          clast.pred 4.820410e-03    <NA>  100
#> 193 13  1     0 Inf           half.life 6.966422e+00    <NA>  100
#> 194 13  1     0 Inf          span.ratio 1.033529e+01    <NA>  100
#> 195 13  1     0 Inf          aucinf.obs 4.837404e+03    <NA>  100
#> 196 14  1     0  24             auclast 1.785177e+03    <NA>  100
#> 197 14  1     0 Inf                cmax 6.188954e+02    <NA>  100
#> 198 14  1     0 Inf                tmax 1.000000e+00    <NA>  100
#> 199 14  1     0 Inf               tlast 9.600000e+01    <NA>  100
#> 200 14  1     0 Inf           clast.obs 1.000000e-03    <NA>  100
#> 201 14  1     0 Inf            lambda.z 9.816547e-02    <NA>  100
#> 202 14  1     0 Inf           r.squared 9.999391e-01    <NA>  100
#> 203 14  1     0 Inf       adj.r.squared 9.999087e-01    <NA>  100
#> 204 14  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  100
#> 205 14  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  100
#> 206 14  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  100
#> 207 14  1     0 Inf          clast.pred 9.816758e-04    <NA>  100
#> 208 14  1     0 Inf           half.life 7.061008e+00    <NA>  100
#> 209 14  1     0 Inf          span.ratio 1.019684e+01    <NA>  100
#> 210 14  1     0 Inf          aucinf.obs 1.796966e+03    <NA>  100
#> 211 15  1     0  24             auclast 3.239935e+03    <NA>  100
#> 212 15  1     0 Inf                cmax 1.063624e+03    <NA>  100
#> 213 15  1     0 Inf                tmax 1.000000e+00    <NA>  100
#> 214 15  1     0 Inf               tlast 1.440000e+02    <NA>  100
#> 215 15  1     0 Inf           clast.obs 1.000000e-04    <NA>  100
#> 216 15  1     0 Inf            lambda.z 8.576395e-02    <NA>  100
#> 217 15  1     0 Inf           r.squared 9.992598e-01    <NA>  100
#> 218 15  1     0 Inf       adj.r.squared 9.990131e-01    <NA>  100
#> 219 15  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  100
#> 220 15  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  100
#> 221 15  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  100
#> 222 15  1     0 Inf          clast.pred 8.945041e-05    <NA>  100
#> 223 15  1     0 Inf           half.life 8.082034e+00    <NA>  100
#> 224 15  1     0 Inf          span.ratio 1.484775e+01    <NA>  100
#> 225 15  1     0 Inf          aucinf.obs 3.272423e+03    <NA>  100
#> 226 16  1     0  24             auclast 1.715794e+03    <NA>  100
#> 227 16  1     0 Inf                cmax 6.403108e+02    <NA>  100
#> 228 16  1     0 Inf                tmax 1.000000e+00    <NA>  100
#> 229 16  1     0 Inf               tlast 9.600000e+01    <NA>  100
#> 230 16  1     0 Inf           clast.obs 2.200000e-03    <NA>  100
#> 231 16  1     0 Inf            lambda.z 8.693226e-02    <NA>  100
#> 232 16  1     0 Inf           r.squared 9.999720e-01    <NA>  100
#> 233 16  1     0 Inf       adj.r.squared 9.999580e-01    <NA>  100
#> 234 16  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  100
#> 235 16  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  100
#> 236 16  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  100
#> 237 16  1     0 Inf          clast.pred 2.229918e-03    <NA>  100
#> 238 16  1     0 Inf           half.life 7.973417e+00    <NA>  100
#> 239 16  1     0 Inf          span.ratio 9.030005e+00    <NA>  100
#> 240 16  1     0 Inf          aucinf.obs 1.729164e+03    <NA>  100
#> 241 17  1     0  24             auclast 2.804696e+03    <NA>  100
#> 242 17  1     0 Inf                cmax 9.528560e+02    <NA>  100
#> 243 17  1     0 Inf                tmax 1.000000e+00    <NA>  100
#> 244 17  1     0 Inf               tlast 9.600000e+01    <NA>  100
#> 245 17  1     0 Inf           clast.obs 7.000000e-04    <NA>  100
#> 246 17  1     0 Inf            lambda.z 1.126758e-01    <NA>  100
#> 247 17  1     0 Inf           r.squared 9.999370e-01    <NA>  100
#> 248 17  1     0 Inf       adj.r.squared 9.999161e-01    <NA>  100
#> 249 17  1     0 Inf lambda.z.time.first 1.200000e+01    <NA>  100
#> 250 17  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  100
#> 251 17  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  100
#> 252 17  1     0 Inf          clast.pred 7.113547e-04    <NA>  100
#> 253 17  1     0 Inf           half.life 6.151693e+00    <NA>  100
#> 254 17  1     0 Inf          span.ratio 1.365478e+01    <NA>  100
#> 255 17  1     0 Inf          aucinf.obs 2.825315e+03    <NA>  100
#> 256 18  1     0  24             auclast 3.802874e+03    <NA>  100
#> 257 18  1     0 Inf                cmax 1.155409e+03    <NA>  100
#> 258 18  1     0 Inf                tmax 1.000000e+00    <NA>  100
#> 259 18  1     0 Inf               tlast 1.440000e+02    <NA>  100
#> 260 18  1     0 Inf           clast.obs 2.000000e-04    <NA>  100
#> 261 18  1     0 Inf            lambda.z 8.235036e-02    <NA>  100
#> 262 18  1     0 Inf           r.squared 9.999629e-01    <NA>  100
#> 263 18  1     0 Inf       adj.r.squared 9.999505e-01    <NA>  100
#> 264 18  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  100
#> 265 18  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  100
#> 266 18  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  100
#> 267 18  1     0 Inf          clast.pred 2.005679e-04    <NA>  100
#> 268 18  1     0 Inf           half.life 8.417051e+00    <NA>  100
#> 269 18  1     0 Inf          span.ratio 1.425677e+01    <NA>  100
#> 270 18  1     0 Inf          aucinf.obs 3.850425e+03    <NA>  100
#> 271 19  1     0  24             auclast 7.000260e+03    <NA>  200
#> 272 19  1     0 Inf                cmax 2.324836e+03    <NA>  200
#> 273 19  1     0 Inf                tmax 1.000000e+00    <NA>  200
#> 274 19  1     0 Inf               tlast 9.600000e+01    <NA>  200
#> 275 19  1     0 Inf           clast.obs 3.000000e-03    <NA>  200
#> 276 19  1     0 Inf            lambda.z 1.058481e-01    <NA>  200
#> 277 19  1     0 Inf           r.squared 9.998804e-01    <NA>  200
#> 278 19  1     0 Inf       adj.r.squared 9.998206e-01    <NA>  200
#> 279 19  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  200
#> 280 19  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  200
#> 281 19  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  200
#> 282 19  1     0 Inf          clast.pred 2.899822e-03    <NA>  200
#> 283 19  1     0 Inf           half.life 6.548506e+00    <NA>  200
#> 284 19  1     0 Inf          span.ratio 1.099487e+01    <NA>  200
#> 285 19  1     0 Inf          aucinf.obs 7.056708e+03    <NA>  200
#> 286 20  1     0  24             auclast 6.776600e+03    <NA>  200
#> 287 20  1     0 Inf                cmax 2.091633e+03    <NA>  200
#> 288 20  1     0 Inf                tmax 1.000000e+00    <NA>  200
#> 289 20  1     0 Inf               tlast 1.440000e+02    <NA>  200
#> 290 20  1     0 Inf           clast.obs 2.000000e-04    <NA>  200
#> 291 20  1     0 Inf            lambda.z 8.659533e-02    <NA>  200
#> 292 20  1     0 Inf           r.squared 9.999406e-01    <NA>  200
#> 293 20  1     0 Inf       adj.r.squared 9.999208e-01    <NA>  200
#> 294 20  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  200
#> 295 20  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  200
#> 296 20  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  200
#> 297 20  1     0 Inf          clast.pred 2.046620e-04    <NA>  200
#> 298 20  1     0 Inf           half.life 8.004441e+00    <NA>  200
#> 299 20  1     0 Inf          span.ratio 1.499168e+01    <NA>  200
#> 300 20  1     0 Inf          aucinf.obs 6.852957e+03    <NA>  200
#> 301 21  1     0  24             auclast 5.445269e+03    <NA>  200
#> 302 21  1     0 Inf                cmax 1.785409e+03    <NA>  200
#> 303 21  1     0 Inf                tmax 1.000000e+00    <NA>  200
#> 304 21  1     0 Inf               tlast 1.440000e+02    <NA>  200
#> 305 21  1     0 Inf           clast.obs 2.000000e-04    <NA>  200
#> 306 21  1     0 Inf            lambda.z 8.339245e-02    <NA>  200
#> 307 21  1     0 Inf           r.squared 9.996346e-01    <NA>  200
#> 308 21  1     0 Inf       adj.r.squared 9.995128e-01    <NA>  200
#> 309 21  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  200
#> 310 21  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  200
#> 311 21  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  200
#> 312 21  1     0 Inf          clast.pred 1.861553e-04    <NA>  200
#> 313 21  1     0 Inf           half.life 8.311870e+00    <NA>  200
#> 314 21  1     0 Inf          span.ratio 1.443718e+01    <NA>  200
#> 315 21  1     0 Inf          aucinf.obs 5.496143e+03    <NA>  200
#> 316 22  1     0  24             auclast 1.583959e+04    <NA>  500
#> 317 22  1     0 Inf                cmax 5.501999e+03    <NA>  500
#> 318 22  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 319 22  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 320 22  1     0 Inf           clast.obs 1.000000e-04    <NA>  500
#> 321 22  1     0 Inf            lambda.z 9.853742e-02    <NA>  500
#> 322 22  1     0 Inf           r.squared 9.998231e-01    <NA>  500
#> 323 22  1     0 Inf       adj.r.squared 9.997642e-01    <NA>  500
#> 324 22  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 325 22  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 326 22  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 327 22  1     0 Inf          clast.pred 9.413641e-05    <NA>  500
#> 328 22  1     0 Inf           half.life 7.034355e+00    <NA>  500
#> 329 22  1     0 Inf          span.ratio 1.705913e+01    <NA>  500
#> 330 22  1     0 Inf          aucinf.obs 1.597458e+04    <NA>  500
#> 331 23  1     0  24             auclast 1.234033e+04    <NA>  500
#> 332 23  1     0 Inf                cmax 4.137234e+03    <NA>  500
#> 333 23  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 334 23  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 335 23  1     0 Inf           clast.obs 1.000000e-04    <NA>  500
#> 336 23  1     0 Inf            lambda.z 9.462305e-02    <NA>  500
#> 337 23  1     0 Inf           r.squared 9.999486e-01    <NA>  500
#> 338 23  1     0 Inf       adj.r.squared 9.999315e-01    <NA>  500
#> 339 23  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 340 23  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 341 23  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 342 23  1     0 Inf          clast.pred 1.014497e-04    <NA>  500
#> 343 23  1     0 Inf           half.life 7.325352e+00    <NA>  500
#> 344 23  1     0 Inf          span.ratio 1.638147e+01    <NA>  500
#> 345 23  1     0 Inf          aucinf.obs 1.243185e+04    <NA>  500
#> 346 24  1     0  24             auclast 2.704707e+04    <NA>  500
#> 347 24  1     0 Inf                cmax 8.426502e+03    <NA>  500
#> 348 24  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 349 24  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 350 24  1     0 Inf           clast.obs 4.000000e-04    <NA>  500
#> 351 24  1     0 Inf            lambda.z 9.501933e-02    <NA>  500
#> 352 24  1     0 Inf           r.squared 9.998974e-01    <NA>  500
#> 353 24  1     0 Inf       adj.r.squared 9.998769e-01    <NA>  500
#> 354 24  1     0 Inf lambda.z.time.first 1.000000e+01    <NA>  500
#> 355 24  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 356 24  1     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 357 24  1     0 Inf          clast.pred 4.038177e-04    <NA>  500
#> 358 24  1     0 Inf           half.life 7.294802e+00    <NA>  500
#> 359 24  1     0 Inf          span.ratio 1.836924e+01    <NA>  500
#> 360 24  1     0 Inf          aucinf.obs 2.740676e+04    <NA>  500
#> 361 25  1     0  24             auclast 1.446619e+04    <NA>  500
#> 362 25  1     0 Inf                cmax 4.607995e+03    <NA>  500
#> 363 25  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 364 25  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 365 25  1     0 Inf           clast.obs 1.000000e-04    <NA>  500
#> 366 25  1     0 Inf            lambda.z 9.628048e-02    <NA>  500
#> 367 25  1     0 Inf           r.squared 9.995134e-01    <NA>  500
#> 368 25  1     0 Inf       adj.r.squared 9.993512e-01    <NA>  500
#> 369 25  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 370 25  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 371 25  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 372 25  1     0 Inf          clast.pred 1.105801e-04    <NA>  500
#> 373 25  1     0 Inf           half.life 7.199249e+00    <NA>  500
#> 374 25  1     0 Inf          span.ratio 1.666841e+01    <NA>  500
#> 375 25  1     0 Inf          aucinf.obs 1.458032e+04    <NA>  500
#> 376 26  1     0  24             auclast 1.354819e+04    <NA>  500
#> 377 26  1     0 Inf                cmax 4.390954e+03    <NA>  500
#> 378 26  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 379 26  1     0 Inf               tlast 9.600000e+01    <NA>  500
#> 380 26  1     0 Inf           clast.obs 4.500000e-03    <NA>  500
#> 381 26  1     0 Inf            lambda.z 1.056342e-01    <NA>  500
#> 382 26  1     0 Inf           r.squared 9.999958e-01    <NA>  500
#> 383 26  1     0 Inf       adj.r.squared 9.999916e-01    <NA>  500
#> 384 26  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 385 26  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  500
#> 386 26  1     0 Inf   lambda.z.n.points 3.000000e+00    <NA>  500
#> 387 26  1     0 Inf          clast.pred 4.513525e-03    <NA>  500
#> 388 26  1     0 Inf           half.life 6.561770e+00    <NA>  500
#> 389 26  1     0 Inf          span.ratio 7.315099e+00    <NA>  500
#> 390 26  1     0 Inf          aucinf.obs 1.364245e+04    <NA>  500
#> 391 27  1     0  24             auclast 1.061977e+04    <NA>  500
#> 392 27  1     0 Inf                cmax 3.752019e+03    <NA>  500
#> 393 27  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 394 27  1     0 Inf               tlast 9.600000e+01    <NA>  500
#> 395 27  1     0 Inf           clast.obs 4.500000e-03    <NA>  500
#> 396 27  1     0 Inf            lambda.z 1.033160e-01    <NA>  500
#> 397 27  1     0 Inf           r.squared 9.999966e-01    <NA>  500
#> 398 27  1     0 Inf       adj.r.squared 9.999948e-01    <NA>  500
#> 399 27  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 400 27  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  500
#> 401 27  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  500
#> 402 27  1     0 Inf          clast.pred 4.516459e-03    <NA>  500
#> 403 27  1     0 Inf           half.life 6.708999e+00    <NA>  500
#> 404 27  1     0 Inf          span.ratio 1.073185e+01    <NA>  500
#> 405 27  1     0 Inf          aucinf.obs 1.069405e+04    <NA>  500
#> 406 28  1     0  24             auclast 2.250367e+04    <NA>  800
#> 407 28  1     0 Inf                cmax 7.479920e+03    <NA>  800
#> 408 28  1     0 Inf                tmax 1.000000e+00    <NA>  800
#> 409 28  1     0 Inf               tlast 1.680000e+02    <NA>  800
#> 410 28  1     0 Inf           clast.obs 1.000000e-04    <NA>  800
#> 411 28  1     0 Inf            lambda.z 8.305962e-02    <NA>  800
#> 412 28  1     0 Inf           r.squared 9.996882e-01    <NA>  800
#> 413 28  1     0 Inf       adj.r.squared 9.996102e-01    <NA>  800
#> 414 28  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  800
#> 415 28  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  800
#> 416 28  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  800
#> 417 28  1     0 Inf          clast.pred 8.894987e-05    <NA>  800
#> 418 28  1     0 Inf           half.life 8.345176e+00    <NA>  800
#> 419 28  1     0 Inf          span.ratio 1.725548e+01    <NA>  800
#> 420 28  1     0 Inf          aucinf.obs 2.267750e+04    <NA>  800
#> 421 29  1     0  24             auclast 1.863645e+04    <NA>  800
#> 422 29  1     0 Inf                cmax 6.486925e+03    <NA>  800
#> 423 29  1     0 Inf                tmax 1.000000e+00    <NA>  800
#> 424 29  1     0 Inf               tlast 1.440000e+02    <NA>  800
#> 425 29  1     0 Inf           clast.obs 1.000000e-04    <NA>  800
#> 426 29  1     0 Inf            lambda.z 9.846170e-02    <NA>  800
#> 427 29  1     0 Inf           r.squared 9.996907e-01    <NA>  800
#> 428 29  1     0 Inf       adj.r.squared 9.995876e-01    <NA>  800
#> 429 29  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  800
#> 430 29  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  800
#> 431 29  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  800
#> 432 29  1     0 Inf          clast.pred 9.210014e-05    <NA>  800
#> 433 29  1     0 Inf           half.life 7.039764e+00    <NA>  800
#> 434 29  1     0 Inf          span.ratio 1.704603e+01    <NA>  800
#> 435 29  1     0 Inf          aucinf.obs 1.876833e+04    <NA>  800
#> 436 30  1     0  24             auclast 3.002218e+04    <NA>  800
#> 437 30  1     0 Inf                cmax 8.334664e+03    <NA>  800
#> 438 30  1     0 Inf                tmax 1.500000e+00    <NA>  800
#> 439 30  1     0 Inf               tlast 1.440000e+02    <NA>  800
#> 440 30  1     0 Inf           clast.obs 3.000000e-04    <NA>  800
#> 441 30  1     0 Inf            lambda.z 9.567977e-02    <NA>  800
#> 442 30  1     0 Inf           r.squared 9.999637e-01    <NA>  800
#> 443 30  1     0 Inf       adj.r.squared 9.999517e-01    <NA>  800
#> 444 30  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  800
#> 445 30  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  800
#> 446 30  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  800
#> 447 30  1     0 Inf          clast.pred 2.930591e-04    <NA>  800
#> 448 30  1     0 Inf           half.life 7.244449e+00    <NA>  800
#> 449 30  1     0 Inf          span.ratio 1.656441e+01    <NA>  800
#> 450 30  1     0 Inf          aucinf.obs 3.032327e+04    <NA>  800
#> 451 31  1     0  24             auclast 3.392579e+04    <NA>  800
#> 452 31  1     0 Inf                cmax 9.071352e+03    <NA>  800
#> 453 31  1     0 Inf                tmax 1.000000e+00    <NA>  800
#> 454 31  1     0 Inf               tlast 1.440000e+02    <NA>  800
#> 455 31  1     0 Inf           clast.obs 1.000000e-04    <NA>  800
#> 456 31  1     0 Inf            lambda.z 1.066895e-01    <NA>  800
#> 457 31  1     0 Inf           r.squared 9.999219e-01    <NA>  800
#> 458 31  1     0 Inf       adj.r.squared 9.998959e-01    <NA>  800
#> 459 31  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  800
#> 460 31  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  800
#> 461 31  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  800
#> 462 31  1     0 Inf          clast.pred 1.045572e-04    <NA>  800
#> 463 31  1     0 Inf           half.life 6.496867e+00    <NA>  800
#> 464 31  1     0 Inf          span.ratio 1.847044e+01    <NA>  800
#> 465 31  1     0 Inf          aucinf.obs 3.427425e+04    <NA>  800
#> 466 32  1     0  24             auclast 1.871379e+04    <NA>  800
#> 467 32  1     0 Inf                cmax 6.288157e+03    <NA>  800
#> 468 32  1     0 Inf                tmax 1.000000e+00    <NA>  800
#> 469 32  1     0 Inf               tlast 1.440000e+02    <NA>  800
#> 470 32  1     0 Inf           clast.obs 2.000000e-04    <NA>  800
#> 471 32  1     0 Inf            lambda.z 9.384124e-02    <NA>  800
#> 472 32  1     0 Inf           r.squared 9.999033e-01    <NA>  800
#> 473 32  1     0 Inf       adj.r.squared 9.998791e-01    <NA>  800
#> 474 32  1     0 Inf lambda.z.time.first 1.200000e+01    <NA>  800
#> 475 32  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  800
#> 476 32  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  800
#> 477 32  1     0 Inf          clast.pred 2.045114e-04    <NA>  800
#> 478 32  1     0 Inf           half.life 7.386381e+00    <NA>  800
#> 479 32  1     0 Inf          span.ratio 1.787073e+01    <NA>  800
#> 480 32  1     0 Inf          aucinf.obs 1.887601e+04    <NA>  800
#> 481 33  1     0  24             auclast 1.973778e+04    <NA>  800
#> 482 33  1     0 Inf                cmax 6.011806e+03    <NA>  800
#> 483 33  1     0 Inf                tmax 1.000000e+00    <NA>  800
#> 484 33  1     0 Inf               tlast 9.600000e+01    <NA>  800
#> 485 33  1     0 Inf           clast.obs 4.000000e-03    <NA>  800
#> 486 33  1     0 Inf            lambda.z 1.133840e-01    <NA>  800
#> 487 33  1     0 Inf           r.squared 9.999439e-01    <NA>  800
#> 488 33  1     0 Inf       adj.r.squared 9.999158e-01    <NA>  800
#> 489 33  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  800
#> 490 33  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  800
#> 491 33  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  800
#> 492 33  1     0 Inf          clast.pred 4.091777e-03    <NA>  800
#> 493 33  1     0 Inf           half.life 6.113271e+00    <NA>  800
#> 494 33  1     0 Inf          span.ratio 1.177766e+01    <NA>  800
#> 495 33  1     0 Inf          aucinf.obs 1.986365e+04    <NA>  800
#> 496 34  1     0  24             auclast 2.232290e+04    <NA> 1000
#> 497 34  1     0 Inf                cmax 7.332282e+03    <NA> 1000
#> 498 34  1     0 Inf                tmax 1.000000e+00    <NA> 1000
#> 499 34  1     0 Inf               tlast 1.440000e+02    <NA> 1000
#> 500 34  1     0 Inf           clast.obs 1.000000e-04    <NA> 1000
#> 501 34  1     0 Inf            lambda.z 9.914251e-02    <NA> 1000
#> 502 34  1     0 Inf           r.squared 9.999461e-01    <NA> 1000
#> 503 34  1     0 Inf       adj.r.squared 9.998922e-01    <NA> 1000
#> 504 34  1     0 Inf lambda.z.time.first 7.200000e+01    <NA> 1000
#> 505 34  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA> 1000
#> 506 34  1     0 Inf   lambda.z.n.points 3.000000e+00    <NA> 1000
#> 507 34  1     0 Inf          clast.pred 9.899653e-05    <NA> 1000
#> 508 34  1     0 Inf           half.life 6.991423e+00    <NA> 1000
#> 509 34  1     0 Inf          span.ratio 1.029833e+01    <NA> 1000
#> 510 34  1     0 Inf          aucinf.obs 2.250714e+04    <NA> 1000
#> 511 35  1     0  24             auclast 3.252422e+04    <NA> 1000
#> 512 35  1     0 Inf                cmax 1.073462e+04    <NA> 1000
#> 513 35  1     0 Inf                tmax 1.000000e+00    <NA> 1000
#> 514 35  1     0 Inf               tlast 1.440000e+02    <NA> 1000
#> 515 35  1     0 Inf           clast.obs 3.000000e-04    <NA> 1000
#> 516 35  1     0 Inf            lambda.z 9.555373e-02    <NA> 1000
#> 517 35  1     0 Inf           r.squared 9.999367e-01    <NA> 1000
#> 518 35  1     0 Inf       adj.r.squared 9.999209e-01    <NA> 1000
#> 519 35  1     0 Inf lambda.z.time.first 1.200000e+01    <NA> 1000
#> 520 35  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA> 1000
#> 521 35  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA> 1000
#> 522 35  1     0 Inf          clast.pred 3.004602e-04    <NA> 1000
#> 523 35  1     0 Inf           half.life 7.254004e+00    <NA> 1000
#> 524 35  1     0 Inf          span.ratio 1.819685e+01    <NA> 1000
#> 525 35  1     0 Inf          aucinf.obs 3.281589e+04    <NA> 1000
#> 526 36  1     0  24             auclast 3.787009e+04    <NA> 1000
#> 527 36  1     0 Inf                cmax 1.177701e+04    <NA> 1000
#> 528 36  1     0 Inf                tmax 1.000000e+00    <NA> 1000
#> 529 36  1     0 Inf               tlast 1.680000e+02    <NA> 1000
#> 530 36  1     0 Inf           clast.obs 1.000000e-04    <NA> 1000
#> 531 36  1     0 Inf            lambda.z 8.923816e-02    <NA> 1000
#> 532 36  1     0 Inf           r.squared 9.987622e-01    <NA> 1000
#> 533 36  1     0 Inf       adj.r.squared 9.984528e-01    <NA> 1000
#> 534 36  1     0 Inf lambda.z.time.first 2.400000e+01    <NA> 1000
#> 535 36  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA> 1000
#> 536 36  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA> 1000
#> 537 36  1     0 Inf          clast.pred 7.708138e-05    <NA> 1000
#> 538 36  1     0 Inf           half.life 7.767385e+00    <NA> 1000
#> 539 36  1     0 Inf          span.ratio 1.853906e+01    <NA> 1000
#> 540 36  1     0 Inf          aucinf.obs 3.821574e+04    <NA> 1000
#> 541 37  1     0  24             auclast 1.842380e+04    <NA>  500
#> 542 37  1     0 Inf                cmax 6.388314e+03    <NA>  500
#> 543 37  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 544 37  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 545 37  1     0 Inf           clast.obs 4.000000e-04    <NA>  500
#> 546 37  1     0 Inf            lambda.z 8.832400e-02    <NA>  500
#> 547 37  1     0 Inf           r.squared 9.999527e-01    <NA>  500
#> 548 37  1     0 Inf       adj.r.squared 9.999369e-01    <NA>  500
#> 549 37  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 550 37  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 551 37  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 552 37  1     0 Inf          clast.pred 3.909153e-04    <NA>  500
#> 553 37  1     0 Inf           half.life 7.847779e+00    <NA>  500
#> 554 37  1     0 Inf          span.ratio 1.529095e+01    <NA>  500
#> 555 37  1     0 Inf          aucinf.obs 1.860268e+04    <NA>  500
#> 556 38  1     0  24             auclast 1.401704e+04    <NA>  500
#> 557 38  1     0 Inf                cmax 4.814671e+03    <NA>  500
#> 558 38  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 559 38  1     0 Inf               tlast 9.600000e+01    <NA>  500
#> 560 38  1     0 Inf           clast.obs 5.600000e-03    <NA>  500
#> 561 38  1     0 Inf            lambda.z 1.036302e-01    <NA>  500
#> 562 38  1     0 Inf           r.squared 9.999967e-01    <NA>  500
#> 563 38  1     0 Inf       adj.r.squared 9.999951e-01    <NA>  500
#> 564 38  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 565 38  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  500
#> 566 38  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  500
#> 567 38  1     0 Inf          clast.pred 5.622718e-03    <NA>  500
#> 568 38  1     0 Inf           half.life 6.688663e+00    <NA>  500
#> 569 38  1     0 Inf          span.ratio 1.076448e+01    <NA>  500
#> 570 38  1     0 Inf          aucinf.obs 1.411132e+04    <NA>  500
#> 571 39  1     0  24             auclast 9.911626e+03    <NA>  500
#> 572 39  1     0 Inf                cmax 3.553992e+03    <NA>  500
#> 573 39  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 574 39  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 575 39  1     0 Inf           clast.obs 2.000000e-04    <NA>  500
#> 576 39  1     0 Inf            lambda.z 8.642586e-02    <NA>  500
#> 577 39  1     0 Inf           r.squared 9.999689e-01    <NA>  500
#> 578 39  1     0 Inf       adj.r.squared 9.999585e-01    <NA>  500
#> 579 39  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 580 39  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 581 39  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 582 39  1     0 Inf          clast.pred 1.965897e-04    <NA>  500
#> 583 39  1     0 Inf           half.life 8.020136e+00    <NA>  500
#> 584 39  1     0 Inf          span.ratio 1.496234e+01    <NA>  500
#> 585 39  1     0 Inf          aucinf.obs 9.984762e+03    <NA>  500
#> 586 40  1     0  24             auclast 2.110296e+04    <NA>  500
#> 587 40  1     0 Inf                cmax 6.406481e+03    <NA>  500
#> 588 40  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 589 40  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 590 40  1     0 Inf           clast.obs 4.000000e-04    <NA>  500
#> 591 40  1     0 Inf            lambda.z 9.135775e-02    <NA>  500
#> 592 40  1     0 Inf           r.squared 9.999753e-01    <NA>  500
#> 593 40  1     0 Inf       adj.r.squared 9.999670e-01    <NA>  500
#> 594 40  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 595 40  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 596 40  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 597 40  1     0 Inf          clast.pred 3.988927e-04    <NA>  500
#> 598 40  1     0 Inf           half.life 7.587174e+00    <NA>  500
#> 599 40  1     0 Inf          span.ratio 1.581616e+01    <NA>  500
#> 600 40  1     0 Inf          aucinf.obs 2.135506e+04    <NA>  500
#> 601 41  1     0  24             auclast 9.606489e+03    <NA>  500
#> 602 41  1     0 Inf                cmax 3.378997e+03    <NA>  500
#> 603 41  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 604 41  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 605 41  1     0 Inf           clast.obs 2.000000e-04    <NA>  500
#> 606 41  1     0 Inf            lambda.z 8.596185e-02    <NA>  500
#> 607 41  1     0 Inf           r.squared 9.998521e-01    <NA>  500
#> 608 41  1     0 Inf       adj.r.squared 9.998029e-01    <NA>  500
#> 609 41  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 610 41  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 611 41  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 612 41  1     0 Inf          clast.pred 2.099408e-04    <NA>  500
#> 613 41  1     0 Inf           half.life 8.063428e+00    <NA>  500
#> 614 41  1     0 Inf          span.ratio 1.488201e+01    <NA>  500
#> 615 41  1     0 Inf          aucinf.obs 9.678709e+03    <NA>  500
#> 616 42  1     0  24             auclast 1.080272e+04    <NA>  500
#> 617 42  1     0 Inf                cmax 3.596331e+03    <NA>  500
#> 618 42  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 619 42  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 620 42  1     0 Inf           clast.obs 1.000000e-04    <NA>  500
#> 621 42  1     0 Inf            lambda.z 7.966875e-02    <NA>  500
#> 622 42  1     0 Inf           r.squared 9.983950e-01    <NA>  500
#> 623 42  1     0 Inf       adj.r.squared 9.979938e-01    <NA>  500
#> 624 42  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 625 42  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 626 42  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 627 42  1     0 Inf          clast.pred 7.642016e-05    <NA>  500
#> 628 42  1     0 Inf           half.life 8.700365e+00    <NA>  500
#> 629 42  1     0 Inf          span.ratio 1.655103e+01    <NA>  500
#> 630 42  1     0 Inf          aucinf.obs 1.090034e+04    <NA>  500
#> 631 43  1     0  24             auclast 2.159418e+04    <NA>  500
#> 632 43  1     0 Inf                cmax 5.899466e+03    <NA>  500
#> 633 43  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 634 43  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 635 43  1     0 Inf           clast.obs 1.000000e-04    <NA>  500
#> 636 43  1     0 Inf            lambda.z 1.038920e-01    <NA>  500
#> 637 43  1     0 Inf           r.squared 9.992929e-01    <NA>  500
#> 638 43  1     0 Inf       adj.r.squared 9.990571e-01    <NA>  500
#> 639 43  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 640 43  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 641 43  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 642 43  1     0 Inf          clast.pred 8.760502e-05    <NA>  500
#> 643 43  1     0 Inf           half.life 6.671806e+00    <NA>  500
#> 644 43  1     0 Inf          span.ratio 1.798614e+01    <NA>  500
#> 645 43  1     0 Inf          aucinf.obs 2.182821e+04    <NA>  500
#> 646 44  1     0  24             auclast 2.414661e+04    <NA>  500
#> 647 44  1     0 Inf                cmax 7.981871e+03    <NA>  500
#> 648 44  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 649 44  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 650 44  1     0 Inf           clast.obs 2.000000e-04    <NA>  500
#> 651 44  1     0 Inf            lambda.z 8.066645e-02    <NA>  500
#> 652 44  1     0 Inf           r.squared 9.998778e-01    <NA>  500
#> 653 44  1     0 Inf       adj.r.squared 9.998534e-01    <NA>  500
#> 654 44  1     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 655 44  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 656 44  1     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 657 44  1     0 Inf          clast.pred 2.105656e-04    <NA>  500
#> 658 44  1     0 Inf           half.life 8.592756e+00    <NA>  500
#> 659 44  1     0 Inf          span.ratio 1.815483e+01    <NA>  500
#> 660 44  1     0 Inf          aucinf.obs 2.442868e+04    <NA>  500
#> 661 45  1     0  24             auclast 1.709256e+04    <NA>  500
#> 662 45  1     0 Inf                cmax 5.031265e+03    <NA>  500
#> 663 45  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 664 45  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 665 45  1     0 Inf           clast.obs 2.000000e-04    <NA>  500
#> 666 45  1     0 Inf            lambda.z 9.431686e-02    <NA>  500
#> 667 45  1     0 Inf           r.squared 9.999652e-01    <NA>  500
#> 668 45  1     0 Inf       adj.r.squared 9.999536e-01    <NA>  500
#> 669 45  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 670 45  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 671 45  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 672 45  1     0 Inf          clast.pred 1.950136e-04    <NA>  500
#> 673 45  1     0 Inf           half.life 7.349133e+00    <NA>  500
#> 674 45  1     0 Inf          span.ratio 1.632846e+01    <NA>  500
#> 675 45  1     0 Inf          aucinf.obs 1.726502e+04    <NA>  500
#> 676 46  1     0  24             auclast 1.174430e+04    <NA>  500
#> 677 46  1     0 Inf                cmax 3.940205e+03    <NA>  500
#> 678 46  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 679 46  1     0 Inf               tlast 9.600000e+01    <NA>  500
#> 680 46  1     0 Inf           clast.obs 6.000000e-04    <NA>  500
#> 681 46  1     0 Inf            lambda.z 1.308467e-01    <NA>  500
#> 682 46  1     0 Inf           r.squared 9.999813e-01    <NA>  500
#> 683 46  1     0 Inf       adj.r.squared 9.999719e-01    <NA>  500
#> 684 46  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 685 46  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  500
#> 686 46  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  500
#> 687 46  1     0 Inf          clast.pred 5.936901e-04    <NA>  500
#> 688 46  1     0 Inf           half.life 5.297400e+00    <NA>  500
#> 689 46  1     0 Inf          span.ratio 1.359157e+01    <NA>  500
#> 690 46  1     0 Inf          aucinf.obs 1.180045e+04    <NA>  500
#> 691 47  1     0  24             auclast 1.781765e+04    <NA>  500
#> 692 47  1     0 Inf                cmax 6.210117e+03    <NA>  500
#> 693 47  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 694 47  1     0 Inf               tlast 1.440000e+02    <NA>  500
#> 695 47  1     0 Inf           clast.obs 2.000000e-04    <NA>  500
#> 696 47  1     0 Inf            lambda.z 9.309795e-02    <NA>  500
#> 697 47  1     0 Inf           r.squared 9.999411e-01    <NA>  500
#> 698 47  1     0 Inf       adj.r.squared 9.999214e-01    <NA>  500
#> 699 47  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 700 47  1     0 Inf  lambda.z.time.last 1.440000e+02    <NA>  500
#> 701 47  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 702 47  1     0 Inf          clast.pred 2.056943e-04    <NA>  500
#> 703 47  1     0 Inf           half.life 7.445354e+00    <NA>  500
#> 704 47  1     0 Inf          span.ratio 1.611743e+01    <NA>  500
#> 705 47  1     0 Inf          aucinf.obs 1.797211e+04    <NA>  500
#> 706 48  1     0  24             auclast 1.199247e+04    <NA>  500
#> 707 48  1     0 Inf                cmax 3.793325e+03    <NA>  500
#> 708 48  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 709 48  1     0 Inf               tlast 9.600000e+01    <NA>  500
#> 710 48  1     0 Inf           clast.obs 3.200000e-03    <NA>  500
#> 711 48  1     0 Inf            lambda.z 1.103228e-01    <NA>  500
#> 712 48  1     0 Inf           r.squared 9.998781e-01    <NA>  500
#> 713 48  1     0 Inf       adj.r.squared 9.998171e-01    <NA>  500
#> 714 48  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 715 48  1     0 Inf  lambda.z.time.last 9.600000e+01    <NA>  500
#> 716 48  1     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  500
#> 717 48  1     0 Inf          clast.pred 3.233494e-03    <NA>  500
#> 718 48  1     0 Inf           half.life 6.282898e+00    <NA>  500
#> 719 48  1     0 Inf          span.ratio 1.145968e+01    <NA>  500
#> 720 48  1     0 Inf          aucinf.obs 1.207482e+04    <NA>  500
nca1(examplinib_fe_nif, time = "TAD", group = "FASTED")
#> NCA: No analyte specified. Selected RS2023 as the most likely.
#> NCA: Group by FASTED
#> Warning: Negative concentrations found
#>     FASTED ID DI start end            PPTESTCD      PPORRES exclude DOSE
#> 1        1  1  1     0  24             auclast 1.952250e+04    <NA>  500
#> 2        1  1  1     0 Inf                cmax 5.605797e+03    <NA>  500
#> 3        1  1  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 4        1  1  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 5        1  1  1     0 Inf           clast.obs 4.150914e-05    <NA>  500
#> 6        1  1  1     0 Inf            lambda.z 9.115619e-02    <NA>  500
#> 7        1  1  1     0 Inf           r.squared 9.999821e-01    <NA>  500
#> 8        1  1  1     0 Inf       adj.r.squared 9.999776e-01    <NA>  500
#> 9        1  1  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 10       1  1  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 11       1  1  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 12       1  1  1     0 Inf          clast.pred 4.129542e-05    <NA>  500
#> 13       1  1  1     0 Inf           half.life 7.603951e+00    <NA>  500
#> 14       1  1  1     0 Inf          span.ratio 1.893752e+01    <NA>  500
#> 15       1  1  1     0 Inf          aucinf.obs 1.974931e+04    <NA>  500
#> 16       0  1  2     0  24             auclast 2.692168e+04    <NA>  500
#> 17       0  1  2     0 Inf                cmax 4.207640e+03    <NA>  500
#> 18       0  1  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 19       0  1  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 20       0  1  2     0 Inf           clast.obs 7.955180e-05    <NA>  500
#> 21       0  1  2     0 Inf            lambda.z 9.108573e-02    <NA>  500
#> 22       0  1  2     0 Inf           r.squared 9.999968e-01    <NA>  500
#> 23       0  1  2     0 Inf       adj.r.squared 9.999958e-01    <NA>  500
#> 24       0  1  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 25       0  1  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 26       0  1  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 27       0  1  2     0 Inf          clast.pred 7.996105e-05    <NA>  500
#> 28       0  1  2     0 Inf           half.life 7.609833e+00    <NA>  500
#> 29       0  1  2     0 Inf          span.ratio 1.576907e+01    <NA>  500
#> 30       0  1  2     0 Inf          aucinf.obs 2.747138e+04    <NA>  500
#> 31       0  2  1     0  24             auclast 1.296987e+04    <NA>  500
#> 32       0  2  1     0 Inf                cmax 2.168668e+03    <NA>  500
#> 33       0  2  1     0 Inf                tmax 2.000000e+00    <NA>  500
#> 34       0  2  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 35       0  2  1     0 Inf           clast.obs 1.751290e-05    <NA>  500
#> 36       0  2  1     0 Inf            lambda.z 9.392942e-02    <NA>  500
#> 37       0  2  1     0 Inf           r.squared 9.999678e-01    <NA>  500
#> 38       0  2  1     0 Inf       adj.r.squared 9.999570e-01    <NA>  500
#> 39       0  2  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 40       0  2  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 41       0  2  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 42       0  2  1     0 Inf          clast.pred 1.735746e-05    <NA>  500
#> 43       0  2  1     0 Inf           half.life 7.379447e+00    <NA>  500
#> 44       0  2  1     0 Inf          span.ratio 1.626138e+01    <NA>  500
#> 45       0  2  1     0 Inf          aucinf.obs 1.318205e+04    <NA>  500
#> 46       1  2  2     0  24             auclast 8.499976e+03    <NA>  500
#> 47       1  2  2     0 Inf                cmax 2.783724e+03    <NA>  500
#> 48       1  2  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 49       1  2  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 50       1  2  2     0 Inf           clast.obs 8.307843e-06    <NA>  500
#> 51       1  2  2     0 Inf            lambda.z 9.363952e-02    <NA>  500
#> 52       1  2  2     0 Inf           r.squared 9.999818e-01    <NA>  500
#> 53       1  2  2     0 Inf       adj.r.squared 9.999772e-01    <NA>  500
#> 54       1  2  2     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 55       1  2  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 56       1  2  2     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 57       1  2  2     0 Inf          clast.pred 8.131810e-06    <NA>  500
#> 58       1  2  2     0 Inf           half.life 7.402293e+00    <NA>  500
#> 59       1  2  2     0 Inf          span.ratio 1.945343e+01    <NA>  500
#> 60       1  2  2     0 Inf          aucinf.obs 8.562265e+03    <NA>  500
#> 61       1  3  1     0  24             auclast 2.127273e+04    <NA>  500
#> 62       1  3  1     0 Inf                cmax 6.439872e+03    <NA>  500
#> 63       1  3  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 64       1  3  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 65       1  3  1     0 Inf           clast.obs 7.390936e-06    <NA>  500
#> 66       1  3  1     0 Inf            lambda.z 1.047673e-01    <NA>  500
#> 67       1  3  1     0 Inf           r.squared 9.999660e-01    <NA>  500
#> 68       1  3  1     0 Inf       adj.r.squared 9.999592e-01    <NA>  500
#> 69       1  3  1     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 70       1  3  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 71       1  3  1     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 72       1  3  1     0 Inf          clast.pred 7.106165e-06    <NA>  500
#> 73       1  3  1     0 Inf           half.life 6.616064e+00    <NA>  500
#> 74       1  3  1     0 Inf          span.ratio 2.357897e+01    <NA>  500
#> 75       1  3  1     0 Inf          aucinf.obs 2.150811e+04    <NA>  500
#> 76       0  3  2     0  24             auclast 3.058559e+04    <NA>  500
#> 77       0  3  2     0 Inf                cmax 4.767907e+03    <NA>  500
#> 78       0  3  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 79       0  3  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 80       0  3  2     0 Inf           clast.obs 1.462743e-05    <NA>  500
#> 81       0  3  2     0 Inf            lambda.z 1.045455e-01    <NA>  500
#> 82       0  3  2     0 Inf           r.squared 9.999830e-01    <NA>  500
#> 83       0  3  2     0 Inf       adj.r.squared 9.999773e-01    <NA>  500
#> 84       0  3  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 85       0  3  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 86       0  3  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 87       0  3  2     0 Inf          clast.pred 1.458709e-05    <NA>  500
#> 88       0  3  2     0 Inf           half.life 6.630102e+00    <NA>  500
#> 89       0  3  2     0 Inf          span.ratio 1.809927e+01    <NA>  500
#> 90       0  3  2     0 Inf          aucinf.obs 3.116406e+04    <NA>  500
#> 91       1  4  1     0  24             auclast 2.443897e+04    <NA>  500
#> 92       1  4  1     0 Inf                cmax 6.043145e+03    <NA>  500
#> 93       1  4  1     0 Inf                tmax 1.500000e+00    <NA>  500
#> 94       1  4  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 95       1  4  1     0 Inf           clast.obs 1.984748e-04    <NA>  500
#> 96       1  4  1     0 Inf            lambda.z 8.484051e-02    <NA>  500
#> 97       1  4  1     0 Inf           r.squared 9.999722e-01    <NA>  500
#> 98       1  4  1     0 Inf       adj.r.squared 9.999652e-01    <NA>  500
#> 99       1  4  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 100      1  4  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 101      1  4  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 102      1  4  1     0 Inf          clast.pred 1.988844e-04    <NA>  500
#> 103      1  4  1     0 Inf           half.life 8.170002e+00    <NA>  500
#> 104      1  4  1     0 Inf          span.ratio 1.762545e+01    <NA>  500
#> 105      1  4  1     0 Inf          aucinf.obs 2.491278e+04    <NA>  500
#> 106      0  4  2     0  24             auclast 3.817864e+04    <NA>  500
#> 107      0  4  2     0 Inf                cmax 5.333942e+03    <NA>  500
#> 108      0  4  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 109      0  4  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 110      0  4  2     0 Inf           clast.obs 3.870134e-04    <NA>  500
#> 111      0  4  2     0 Inf            lambda.z 8.522916e-02    <NA>  500
#> 112      0  4  2     0 Inf           r.squared 9.999328e-01    <NA>  500
#> 113      0  4  2     0 Inf       adj.r.squared 9.999104e-01    <NA>  500
#> 114      0  4  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 115      0  4  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 116      0  4  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 117      0  4  2     0 Inf          clast.pred 4.005540e-04    <NA>  500
#> 118      0  4  2     0 Inf           half.life 8.132747e+00    <NA>  500
#> 119      0  4  2     0 Inf          span.ratio 1.475516e+01    <NA>  500
#> 120      0  4  2     0 Inf          aucinf.obs 3.934480e+04    <NA>  500
#> 121      0  5  1     0  24             auclast 3.202317e+04    <NA>  500
#> 122      0  5  1     0 Inf                cmax 5.283736e+03    <NA>  500
#> 123      0  5  1     0 Inf                tmax 2.000000e+00    <NA>  500
#> 124      0  5  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 125      0  5  1     0 Inf           clast.obs 3.525085e-05    <NA>  500
#> 126      0  5  1     0 Inf            lambda.z 9.840913e-02    <NA>  500
#> 127      0  5  1     0 Inf           r.squared 9.999789e-01    <NA>  500
#> 128      0  5  1     0 Inf       adj.r.squared 9.999718e-01    <NA>  500
#> 129      0  5  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 130      0  5  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 131      0  5  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 132      0  5  1     0 Inf          clast.pred 3.514853e-05    <NA>  500
#> 133      0  5  1     0 Inf           half.life 7.043525e+00    <NA>  500
#> 134      0  5  1     0 Inf          span.ratio 1.703692e+01    <NA>  500
#> 135      0  5  1     0 Inf          aucinf.obs 3.262419e+04    <NA>  500
#> 136      1  5  2     0  24             auclast 2.316675e+04    <NA>  500
#> 137      1  5  2     0 Inf                cmax 7.048011e+03    <NA>  500
#> 138      1  5  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 139      1  5  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 140      1  5  2     0 Inf           clast.obs 1.856992e-05    <NA>  500
#> 141      1  5  2     0 Inf            lambda.z 9.916635e-02    <NA>  500
#> 142      1  5  2     0 Inf           r.squared 9.999721e-01    <NA>  500
#> 143      1  5  2     0 Inf       adj.r.squared 9.999665e-01    <NA>  500
#> 144      1  5  2     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 145      1  5  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 146      1  5  2     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 147      1  5  2     0 Inf          clast.pred 1.843892e-05    <NA>  500
#> 148      1  5  2     0 Inf           half.life 6.989742e+00    <NA>  500
#> 149      1  5  2     0 Inf          span.ratio 2.231842e+01    <NA>  500
#> 150      1  5  2     0 Inf          aucinf.obs 2.345301e+04    <NA>  500
#> 151      0  6  1     0  24             auclast 1.866451e+04    <NA>  500
#> 152      0  6  1     0 Inf                cmax 3.236202e+03    <NA>  500
#> 153      0  6  1     0 Inf                tmax 2.000000e+00    <NA>  500
#> 154      0  6  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 155      0  6  1     0 Inf           clast.obs 8.552855e-05    <NA>  500
#> 156      0  6  1     0 Inf            lambda.z 8.536023e-02    <NA>  500
#> 157      0  6  1     0 Inf           r.squared 9.999375e-01    <NA>  500
#> 158      0  6  1     0 Inf       adj.r.squared 9.999167e-01    <NA>  500
#> 159      0  6  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 160      0  6  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 161      0  6  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 162      0  6  1     0 Inf          clast.pred 8.550542e-05    <NA>  500
#> 163      0  6  1     0 Inf           half.life 8.120259e+00    <NA>  500
#> 164      0  6  1     0 Inf          span.ratio 1.477785e+01    <NA>  500
#> 165      0  6  1     0 Inf          aucinf.obs 1.895641e+04    <NA>  500
#> 166      1  6  2     0  24             auclast 1.373147e+04    <NA>  500
#> 167      1  6  2     0 Inf                cmax 4.724722e+03    <NA>  500
#> 168      1  6  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 169      1  6  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 170      1  6  2     0 Inf           clast.obs 4.544319e-05    <NA>  500
#> 171      1  6  2     0 Inf            lambda.z 8.501096e-02    <NA>  500
#> 172      1  6  2     0 Inf           r.squared 9.999110e-01    <NA>  500
#> 173      1  6  2     0 Inf       adj.r.squared 9.998888e-01    <NA>  500
#> 174      1  6  2     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 175      1  6  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 176      1  6  2     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 177      1  6  2     0 Inf          clast.pred 4.775939e-05    <NA>  500
#> 178      1  6  2     0 Inf           half.life 8.153621e+00    <NA>  500
#> 179      1  6  2     0 Inf          span.ratio 1.766086e+01    <NA>  500
#> 180      1  6  2     0 Inf          aucinf.obs 1.384603e+04    <NA>  500
#> 181      0  7  1     0  24             auclast 1.516648e+04    <NA>  500
#> 182      0  7  1     0 Inf                cmax 2.479996e+03    <NA>  500
#> 183      0  7  1     0 Inf                tmax 1.500000e+00    <NA>  500
#> 184      0  7  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 185      0  7  1     0 Inf           clast.obs 5.028866e-06    <NA>  500
#> 186      0  7  1     0 Inf            lambda.z 1.033940e-01    <NA>  500
#> 187      0  7  1     0 Inf           r.squared 9.999443e-01    <NA>  500
#> 188      0  7  1     0 Inf       adj.r.squared 9.999258e-01    <NA>  500
#> 189      0  7  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 190      0  7  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 191      0  7  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 192      0  7  1     0 Inf          clast.pred 5.064049e-06    <NA>  500
#> 193      0  7  1     0 Inf           half.life 6.703939e+00    <NA>  500
#> 194      0  7  1     0 Inf          span.ratio 1.789992e+01    <NA>  500
#> 195      0  7  1     0 Inf          aucinf.obs 1.540089e+04    <NA>  500
#> 196      1  7  2     0  24             auclast 1.137142e+04    <NA>  500
#> 197      1  7  2     0 Inf                cmax 3.963992e+03    <NA>  500
#> 198      1  7  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 199      1  7  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 200      1  7  2     0 Inf           clast.obs 2.704002e-06    <NA>  500
#> 201      1  7  2     0 Inf            lambda.z 1.027174e-01    <NA>  500
#> 202      1  7  2     0 Inf           r.squared 9.999537e-01    <NA>  500
#> 203      1  7  2     0 Inf       adj.r.squared 9.999422e-01    <NA>  500
#> 204      1  7  2     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 205      1  7  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 206      1  7  2     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 207      1  7  2     0 Inf          clast.pred 2.636875e-06    <NA>  500
#> 208      1  7  2     0 Inf           half.life 6.748101e+00    <NA>  500
#> 209      1  7  2     0 Inf          span.ratio 2.133934e+01    <NA>  500
#> 210      1  7  2     0 Inf          aucinf.obs 1.144124e+04    <NA>  500
#> 211      1  8  1     0  24             auclast 2.721779e+04    <NA>  500
#> 212      1  8  1     0 Inf                cmax 6.994537e+03    <NA>  500
#> 213      1  8  1     0 Inf                tmax 1.500000e+00    <NA>  500
#> 214      1  8  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 215      1  8  1     0 Inf           clast.obs 7.224614e-05    <NA>  500
#> 216      1  8  1     0 Inf            lambda.z 9.219047e-02    <NA>  500
#> 217      1  8  1     0 Inf           r.squared 9.999442e-01    <NA>  500
#> 218      1  8  1     0 Inf       adj.r.squared 9.999330e-01    <NA>  500
#> 219      1  8  1     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 220      1  8  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 221      1  8  1     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 222      1  8  1     0 Inf          clast.pred 7.169306e-05    <NA>  500
#> 223      1  8  1     0 Inf           half.life 7.518643e+00    <NA>  500
#> 224      1  8  1     0 Inf          span.ratio 2.074843e+01    <NA>  500
#> 225      1  8  1     0 Inf          aucinf.obs 2.765162e+04    <NA>  500
#> 226      0  8  2     0  24             auclast 3.788023e+04    <NA>  500
#> 227      0  8  2     0 Inf                cmax 5.483093e+03    <NA>  500
#> 228      0  8  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 229      0  8  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 230      0  8  2     0 Inf           clast.obs 1.471476e-04    <NA>  500
#> 231      0  8  2     0 Inf            lambda.z 9.125122e-02    <NA>  500
#> 232      0  8  2     0 Inf           r.squared 9.999876e-01    <NA>  500
#> 233      0  8  2     0 Inf       adj.r.squared 9.999835e-01    <NA>  500
#> 234      0  8  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 235      0  8  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 236      0  8  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 237      0  8  2     0 Inf          clast.pred 1.451554e-04    <NA>  500
#> 238      0  8  2     0 Inf           half.life 7.596032e+00    <NA>  500
#> 239      0  8  2     0 Inf          span.ratio 1.579772e+01    <NA>  500
#> 240      0  8  2     0 Inf          aucinf.obs 3.886029e+04    <NA>  500
#> 241      0  9  1     0  24             auclast 3.965673e+04    <NA>  500
#> 242      0  9  1     0 Inf                cmax 5.496735e+03    <NA>  500
#> 243      0  9  1     0 Inf                tmax 3.000000e+00    <NA>  500
#> 244      0  9  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 245      0  9  1     0 Inf           clast.obs 2.767436e-04    <NA>  500
#> 246      0  9  1     0 Inf            lambda.z 8.765911e-02    <NA>  500
#> 247      0  9  1     0 Inf           r.squared 9.999889e-01    <NA>  500
#> 248      0  9  1     0 Inf       adj.r.squared 9.999852e-01    <NA>  500
#> 249      0  9  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 250      0  9  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 251      0  9  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 252      0  9  1     0 Inf          clast.pred 2.815239e-04    <NA>  500
#> 253      0  9  1     0 Inf           half.life 7.907304e+00    <NA>  500
#> 254      0  9  1     0 Inf          span.ratio 1.517584e+01    <NA>  500
#> 255      0  9  1     0 Inf          aucinf.obs 4.087872e+04    <NA>  500
#> 256      1  9  2     0  24             auclast 2.903697e+04    <NA>  500
#> 257      1  9  2     0 Inf                cmax 7.358267e+03    <NA>  500
#> 258      1  9  2     0 Inf                tmax 1.500000e+00    <NA>  500
#> 259      1  9  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 260      1  9  2     0 Inf           clast.obs 1.519074e-04    <NA>  500
#> 261      1  9  2     0 Inf            lambda.z 8.781736e-02    <NA>  500
#> 262      1  9  2     0 Inf           r.squared 9.999437e-01    <NA>  500
#> 263      1  9  2     0 Inf       adj.r.squared 9.999297e-01    <NA>  500
#> 264      1  9  2     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 265      1  9  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 266      1  9  2     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 267      1  9  2     0 Inf          clast.pred 1.437517e-04    <NA>  500
#> 268      1  9  2     0 Inf           half.life 7.893054e+00    <NA>  500
#> 269      1  9  2     0 Inf          span.ratio 1.824389e+01    <NA>  500
#> 270      1  9  2     0 Inf          aucinf.obs 2.955187e+04    <NA>  500
#> 271      0 10  1     0  24             auclast 2.072545e+04    <NA>  500
#> 272      0 10  1     0 Inf                cmax 3.262716e+03    <NA>  500
#> 273      0 10  1     0 Inf                tmax 2.000000e+00    <NA>  500
#> 274      0 10  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 275      0 10  1     0 Inf           clast.obs 1.266630e-04    <NA>  500
#> 276      0 10  1     0 Inf            lambda.z 8.516665e-02    <NA>  500
#> 277      0 10  1     0 Inf           r.squared 9.999193e-01    <NA>  500
#> 278      0 10  1     0 Inf       adj.r.squared 9.998923e-01    <NA>  500
#> 279      0 10  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 280      0 10  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 281      0 10  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 282      0 10  1     0 Inf          clast.pred 1.218694e-04    <NA>  500
#> 283      0 10  1     0 Inf           half.life 8.138716e+00    <NA>  500
#> 284      0 10  1     0 Inf          span.ratio 1.474434e+01    <NA>  500
#> 285      0 10  1     0 Inf          aucinf.obs 2.115739e+04    <NA>  500
#> 286      1 10  2     0  24             auclast 1.400201e+04    <NA>  500
#> 287      1 10  2     0 Inf                cmax 4.273190e+03    <NA>  500
#> 288      1 10  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 289      1 10  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 290      1 10  2     0 Inf           clast.obs 6.121180e-05    <NA>  500
#> 291      1 10  2     0 Inf            lambda.z 8.504572e-02    <NA>  500
#> 292      1 10  2     0 Inf           r.squared 9.999877e-01    <NA>  500
#> 293      1 10  2     0 Inf       adj.r.squared 9.999847e-01    <NA>  500
#> 294      1 10  2     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 295      1 10  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 296      1 10  2     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 297      1 10  2     0 Inf          clast.pred 6.124034e-05    <NA>  500
#> 298      1 10  2     0 Inf           half.life 8.150289e+00    <NA>  500
#> 299      1 10  2     0 Inf          span.ratio 1.766809e+01    <NA>  500
#> 300      1 10  2     0 Inf          aucinf.obs 1.415292e+04    <NA>  500
#> 301      0 11  1     0  24             auclast 1.412949e+04    <NA>  500
#> 302      0 11  1     0 Inf                cmax 2.301752e+03    <NA>  500
#> 303      0 11  1     0 Inf                tmax 2.000000e+00    <NA>  500
#> 304      0 11  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 305      0 11  1     0 Inf           clast.obs 9.495220e-06    <NA>  500
#> 306      0 11  1     0 Inf            lambda.z 9.872524e-02    <NA>  500
#> 307      0 11  1     0 Inf           r.squared 9.999859e-01    <NA>  500
#> 308      0 11  1     0 Inf       adj.r.squared 9.999813e-01    <NA>  500
#> 309      0 11  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 310      0 11  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 311      0 11  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 312      0 11  1     0 Inf          clast.pred 9.693159e-06    <NA>  500
#> 313      0 11  1     0 Inf           half.life 7.020973e+00    <NA>  500
#> 314      0 11  1     0 Inf          span.ratio 1.709165e+01    <NA>  500
#> 315      0 11  1     0 Inf          aucinf.obs 1.436188e+04    <NA>  500
#> 316      1 11  2     0  24             auclast 9.913804e+03    <NA>  500
#> 317      1 11  2     0 Inf                cmax 3.246430e+03    <NA>  500
#> 318      1 11  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 319      1 11  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 320      1 11  2     0 Inf           clast.obs 4.354529e-06    <NA>  500
#> 321      1 11  2     0 Inf            lambda.z 9.894250e-02    <NA>  500
#> 322      1 11  2     0 Inf           r.squared 9.999855e-01    <NA>  500
#> 323      1 11  2     0 Inf       adj.r.squared 9.999819e-01    <NA>  500
#> 324      1 11  2     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 325      1 11  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 326      1 11  2     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 327      1 11  2     0 Inf          clast.pred 4.369671e-06    <NA>  500
#> 328      1 11  2     0 Inf           half.life 7.005555e+00    <NA>  500
#> 329      1 11  2     0 Inf          span.ratio 2.055512e+01    <NA>  500
#> 330      1 11  2     0 Inf          aucinf.obs 9.982154e+03    <NA>  500
#> 331      1 12  1     0  24             auclast 1.750481e+04    <NA>  500
#> 332      1 12  1     0 Inf                cmax 4.611494e+03    <NA>  500
#> 333      1 12  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 334      1 12  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 335      1 12  1     0 Inf           clast.obs 2.251078e-05    <NA>  500
#> 336      1 12  1     0 Inf            lambda.z 9.469446e-02    <NA>  500
#> 337      1 12  1     0 Inf           r.squared 9.999719e-01    <NA>  500
#> 338      1 12  1     0 Inf       adj.r.squared 9.999648e-01    <NA>  500
#> 339      1 12  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 340      1 12  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 341      1 12  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 342      1 12  1     0 Inf          clast.pred 2.210769e-05    <NA>  500
#> 343      1 12  1     0 Inf           half.life 7.319828e+00    <NA>  500
#> 344      1 12  1     0 Inf          span.ratio 1.967259e+01    <NA>  500
#> 345      1 12  1     0 Inf          aucinf.obs 1.770308e+04    <NA>  500
#> 346      0 12  2     0  24             auclast 2.522414e+04    <NA>  500
#> 347      0 12  2     0 Inf                cmax 3.802789e+03    <NA>  500
#> 348      0 12  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 349      0 12  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 350      0 12  2     0 Inf           clast.obs 4.748016e-05    <NA>  500
#> 351      0 12  2     0 Inf            lambda.z 9.521710e-02    <NA>  500
#> 352      0 12  2     0 Inf           r.squared 9.999906e-01    <NA>  500
#> 353      0 12  2     0 Inf       adj.r.squared 9.999875e-01    <NA>  500
#> 354      0 12  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 355      0 12  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 356      0 12  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 357      0 12  2     0 Inf          clast.pred 4.682720e-05    <NA>  500
#> 358      0 12  2     0 Inf           half.life 7.279650e+00    <NA>  500
#> 359      0 12  2     0 Inf          span.ratio 1.648431e+01    <NA>  500
#> 360      0 12  2     0 Inf          aucinf.obs 2.584586e+04    <NA>  500
#> 361      0 13  1     0  24             auclast 1.729230e+04    <NA>  500
#> 362      0 13  1     0 Inf                cmax 3.521432e+03    <NA>  500
#> 363      0 13  1     0 Inf                tmax 2.000000e+00    <NA>  500
#> 364      0 13  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 365      0 13  1     0 Inf           clast.obs 2.320590e-05    <NA>  500
#> 366      0 13  1     0 Inf            lambda.z 9.318105e-02    <NA>  500
#> 367      0 13  1     0 Inf           r.squared 9.999748e-01    <NA>  500
#> 368      0 13  1     0 Inf       adj.r.squared 9.999663e-01    <NA>  500
#> 369      0 13  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 370      0 13  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 371      0 13  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 372      0 13  1     0 Inf          clast.pred 2.317447e-05    <NA>  500
#> 373      0 13  1     0 Inf           half.life 7.438714e+00    <NA>  500
#> 374      0 13  1     0 Inf          span.ratio 1.613182e+01    <NA>  500
#> 375      0 13  1     0 Inf          aucinf.obs 1.748411e+04    <NA>  500
#> 376      1 13  2     0  24             auclast 1.181238e+04    <NA>  500
#> 377      1 13  2     0 Inf                cmax 4.475683e+03    <NA>  500
#> 378      1 13  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 379      1 13  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 380      1 13  2     0 Inf           clast.obs 1.361345e-05    <NA>  500
#> 381      1 13  2     0 Inf            lambda.z 9.338642e-02    <NA>  500
#> 382      1 13  2     0 Inf           r.squared 9.999705e-01    <NA>  500
#> 383      1 13  2     0 Inf       adj.r.squared 9.999646e-01    <NA>  500
#> 384      1 13  2     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 385      1 13  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 386      1 13  2     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 387      1 13  2     0 Inf          clast.pred 1.340461e-05    <NA>  500
#> 388      1 13  2     0 Inf           half.life 7.422355e+00    <NA>  500
#> 389      1 13  2     0 Inf          span.ratio 2.101759e+01    <NA>  500
#> 390      1 13  2     0 Inf          aucinf.obs 1.190945e+04    <NA>  500
#> 391      1 14  1     0  24             auclast 1.126597e+04    <NA>  500
#> 392      1 14  1     0 Inf                cmax 4.004570e+03    <NA>  500
#> 393      1 14  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 394      1 14  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 395      1 14  1     0 Inf           clast.obs 2.505832e-05    <NA>  500
#> 396      1 14  1     0 Inf            lambda.z 8.872611e-02    <NA>  500
#> 397      1 14  1     0 Inf           r.squared 9.999870e-01    <NA>  500
#> 398      1 14  1     0 Inf       adj.r.squared 9.999837e-01    <NA>  500
#> 399      1 14  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 400      1 14  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 401      1 14  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 402      1 14  1     0 Inf          clast.pred 2.537010e-05    <NA>  500
#> 403      1 14  1     0 Inf           half.life 7.812212e+00    <NA>  500
#> 404      1 14  1     0 Inf          span.ratio 1.843268e+01    <NA>  500
#> 405      1 14  1     0 Inf          aucinf.obs 1.136659e+04    <NA>  500
#> 406      0 14  2     0  24             auclast 1.721037e+04    <NA>  500
#> 407      0 14  2     0 Inf                cmax 3.157314e+03    <NA>  500
#> 408      0 14  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 409      0 14  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 410      0 14  2     0 Inf           clast.obs 4.999301e-05    <NA>  500
#> 411      0 14  2     0 Inf            lambda.z 8.850608e-02    <NA>  500
#> 412      0 14  2     0 Inf           r.squared 9.999777e-01    <NA>  500
#> 413      0 14  2     0 Inf       adj.r.squared 9.999703e-01    <NA>  500
#> 414      0 14  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 415      0 14  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 416      0 14  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 417      0 14  2     0 Inf          clast.pred 5.122493e-05    <NA>  500
#> 418      0 14  2     0 Inf           half.life 7.831634e+00    <NA>  500
#> 419      0 14  2     0 Inf          span.ratio 1.532247e+01    <NA>  500
#> 420      0 14  2     0 Inf          aucinf.obs 1.746625e+04    <NA>  500
#> 421      0 15  1     0  24             auclast 2.874750e+04    <NA>  500
#> 422      0 15  1     0 Inf                cmax 4.926105e+03    <NA>  500
#> 423      0 15  1     0 Inf                tmax 2.000000e+00    <NA>  500
#> 424      0 15  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 425      0 15  1     0 Inf           clast.obs 2.789319e-05    <NA>  500
#> 426      0 15  1     0 Inf            lambda.z 9.788170e-02    <NA>  500
#> 427      0 15  1     0 Inf           r.squared 9.999226e-01    <NA>  500
#> 428      0 15  1     0 Inf       adj.r.squared 9.998968e-01    <NA>  500
#> 429      0 15  1     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 430      0 15  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 431      0 15  1     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 432      0 15  1     0 Inf          clast.pred 2.680777e-05    <NA>  500
#> 433      0 15  1     0 Inf           half.life 7.081479e+00    <NA>  500
#> 434      0 15  1     0 Inf          span.ratio 1.694561e+01    <NA>  500
#> 435      0 15  1     0 Inf          aucinf.obs 2.920060e+04    <NA>  500
#> 436      1 15  2     0  24             auclast 2.129608e+04    <NA>  500
#> 437      1 15  2     0 Inf                cmax 6.733367e+03    <NA>  500
#> 438      1 15  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 439      1 15  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 440      1 15  2     0 Inf           clast.obs 1.402076e-05    <NA>  500
#> 441      1 15  2     0 Inf            lambda.z 9.862580e-02    <NA>  500
#> 442      1 15  2     0 Inf           r.squared 9.999469e-01    <NA>  500
#> 443      1 15  2     0 Inf       adj.r.squared 9.999363e-01    <NA>  500
#> 444      1 15  2     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 445      1 15  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 446      1 15  2     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 447      1 15  2     0 Inf          clast.pred 1.357619e-05    <NA>  500
#> 448      1 15  2     0 Inf           half.life 7.028052e+00    <NA>  500
#> 449      1 15  2     0 Inf          span.ratio 2.219676e+01    <NA>  500
#> 450      1 15  2     0 Inf          aucinf.obs 2.149229e+04    <NA>  500
#> 451      1 16  1     0  24             auclast 1.388796e+04    <NA>  500
#> 452      1 16  1     0 Inf                cmax 4.537822e+03    <NA>  500
#> 453      1 16  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 454      1 16  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 455      1 16  1     0 Inf           clast.obs 2.099104e-05    <NA>  500
#> 456      1 16  1     0 Inf            lambda.z 9.182541e-02    <NA>  500
#> 457      1 16  1     0 Inf           r.squared 9.999899e-01    <NA>  500
#> 458      1 16  1     0 Inf       adj.r.squared 9.999873e-01    <NA>  500
#> 459      1 16  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 460      1 16  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 461      1 16  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 462      1 16  1     0 Inf          clast.pred 2.081782e-05    <NA>  500
#> 463      1 16  1     0 Inf           half.life 7.548534e+00    <NA>  500
#> 464      1 16  1     0 Inf          span.ratio 1.907655e+01    <NA>  500
#> 465      1 16  1     0 Inf          aucinf.obs 1.401384e+04    <NA>  500
#> 466      0 16  2     0  24             auclast 1.920053e+04    <NA>  500
#> 467      0 16  2     0 Inf                cmax 3.222528e+03    <NA>  500
#> 468      0 16  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 469      0 16  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 470      0 16  2     0 Inf           clast.obs 3.758053e-05    <NA>  500
#> 471      0 16  2     0 Inf            lambda.z 9.256004e-02    <NA>  500
#> 472      0 16  2     0 Inf           r.squared 9.999986e-01    <NA>  500
#> 473      0 16  2     0 Inf       adj.r.squared 9.999982e-01    <NA>  500
#> 474      0 16  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 475      0 16  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 476      0 16  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 477      0 16  2     0 Inf          clast.pred 3.779789e-05    <NA>  500
#> 478      0 16  2     0 Inf           half.life 7.488622e+00    <NA>  500
#> 479      0 16  2     0 Inf          span.ratio 1.602431e+01    <NA>  500
#> 480      0 16  2     0 Inf          aucinf.obs 1.952937e+04    <NA>  500
#> 481      1 17  1     0  24             auclast 1.469949e+04    <NA>  500
#> 482      1 17  1     0 Inf                cmax 4.572924e+03    <NA>  500
#> 483      1 17  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 484      1 17  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 485      1 17  1     0 Inf           clast.obs 1.582716e-05    <NA>  500
#> 486      1 17  1     0 Inf            lambda.z 9.426986e-02    <NA>  500
#> 487      1 17  1     0 Inf           r.squared 9.999544e-01    <NA>  500
#> 488      1 17  1     0 Inf       adj.r.squared 9.999430e-01    <NA>  500
#> 489      1 17  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 490      1 17  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 491      1 17  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 492      1 17  1     0 Inf          clast.pred 1.564340e-05    <NA>  500
#> 493      1 17  1     0 Inf           half.life 7.352797e+00    <NA>  500
#> 494      1 17  1     0 Inf          span.ratio 1.958438e+01    <NA>  500
#> 495      1 17  1     0 Inf          aucinf.obs 1.482847e+04    <NA>  500
#> 496      0 17  2     0  24             auclast 2.124316e+04    <NA>  500
#> 497      0 17  2     0 Inf                cmax 3.759509e+03    <NA>  500
#> 498      0 17  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 499      0 17  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 500      0 17  2     0 Inf           clast.obs 3.137722e-05    <NA>  500
#> 501      0 17  2     0 Inf            lambda.z 9.387799e-02    <NA>  500
#> 502      0 17  2     0 Inf           r.squared 9.999892e-01    <NA>  500
#> 503      0 17  2     0 Inf       adj.r.squared 9.999856e-01    <NA>  500
#> 504      0 17  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 505      0 17  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 506      0 17  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 507      0 17  2     0 Inf          clast.pred 3.140842e-05    <NA>  500
#> 508      0 17  2     0 Inf           half.life 7.383490e+00    <NA>  500
#> 509      0 17  2     0 Inf          span.ratio 1.625248e+01    <NA>  500
#> 510      0 17  2     0 Inf          aucinf.obs 2.157841e+04    <NA>  500
#> 511      1 18  1     0  24             auclast 1.963267e+04    <NA>  500
#> 512      1 18  1     0 Inf                cmax 5.212429e+03    <NA>  500
#> 513      1 18  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 514      1 18  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 515      1 18  1     0 Inf           clast.obs 2.234632e-06    <NA>  500
#> 516      1 18  1     0 Inf            lambda.z 1.106557e-01    <NA>  500
#> 517      1 18  1     0 Inf           r.squared 9.999557e-01    <NA>  500
#> 518      1 18  1     0 Inf       adj.r.squared 9.999446e-01    <NA>  500
#> 519      1 18  1     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 520      1 18  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 521      1 18  1     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 522      1 18  1     0 Inf          clast.pred 2.119822e-06    <NA>  500
#> 523      1 18  1     0 Inf           half.life 6.263998e+00    <NA>  500
#> 524      1 18  1     0 Inf          span.ratio 2.298851e+01    <NA>  500
#> 525      1 18  1     0 Inf          aucinf.obs 1.979621e+04    <NA>  500
#> 526      0 18  2     0  24             auclast 2.652888e+04    <NA>  500
#> 527      0 18  2     0 Inf                cmax 3.693272e+03    <NA>  500
#> 528      0 18  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 529      0 18  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 530      0 18  2     0 Inf           clast.obs 5.333505e-06    <NA>  500
#> 531      0 18  2     0 Inf            lambda.z 1.105909e-01    <NA>  500
#> 532      0 18  2     0 Inf           r.squared 9.999868e-01    <NA>  500
#> 533      0 18  2     0 Inf       adj.r.squared 9.999803e-01    <NA>  500
#> 534      0 18  2     0 Inf lambda.z.time.first 7.200000e+01    <NA>  500
#> 535      0 18  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 536      0 18  2     0 Inf   lambda.z.n.points 4.000000e+00    <NA>  500
#> 537      0 18  2     0 Inf          clast.pred 5.395323e-06    <NA>  500
#> 538      0 18  2     0 Inf           half.life 6.267668e+00    <NA>  500
#> 539      0 18  2     0 Inf          span.ratio 1.531670e+01    <NA>  500
#> 540      0 18  2     0 Inf          aucinf.obs 2.722821e+04    <NA>  500
#> 541      0 19  1     0  24             auclast 1.759191e+04    <NA>  500
#> 542      0 19  1     0 Inf                cmax 2.815318e+03    <NA>  500
#> 543      0 19  1     0 Inf                tmax 1.500000e+00    <NA>  500
#> 544      0 19  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 545      0 19  1     0 Inf           clast.obs 4.124970e-04    <NA>  500
#> 546      0 19  1     0 Inf            lambda.z 7.452964e-02    <NA>  500
#> 547      0 19  1     0 Inf           r.squared 9.999964e-01    <NA>  500
#> 548      0 19  1     0 Inf       adj.r.squared 9.999928e-01    <NA>  500
#> 549      0 19  1     0 Inf lambda.z.time.first 9.600000e+01    <NA>  500
#> 550      0 19  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 551      0 19  1     0 Inf   lambda.z.n.points 3.000000e+00    <NA>  500
#> 552      0 19  1     0 Inf          clast.pred 4.141164e-04    <NA>  500
#> 553      0 19  1     0 Inf           half.life 9.300289e+00    <NA>  500
#> 554      0 19  1     0 Inf          span.ratio 7.741695e+00    <NA>  500
#> 555      0 19  1     0 Inf          aucinf.obs 1.795650e+04    <NA>  500
#> 556      1 19  2     0  24             auclast 1.210305e+04    <NA>  500
#> 557      1 19  2     0 Inf                cmax 3.838087e+03    <NA>  500
#> 558      1 19  2     0 Inf                tmax 1.000000e+00    <NA>  500
#> 559      1 19  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 560      1 19  2     0 Inf           clast.obs 2.247090e-04    <NA>  500
#> 561      1 19  2     0 Inf            lambda.z 7.521028e-02    <NA>  500
#> 562      1 19  2     0 Inf           r.squared 9.999746e-01    <NA>  500
#> 563      1 19  2     0 Inf       adj.r.squared 9.999682e-01    <NA>  500
#> 564      1 19  2     0 Inf lambda.z.time.first 2.400000e+01    <NA>  500
#> 565      1 19  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 566      1 19  2     0 Inf   lambda.z.n.points 6.000000e+00    <NA>  500
#> 567      1 19  2     0 Inf          clast.pred 2.218219e-04    <NA>  500
#> 568      1 19  2     0 Inf           half.life 9.216123e+00    <NA>  500
#> 569      1 19  2     0 Inf          span.ratio 1.562479e+01    <NA>  500
#> 570      1 19  2     0 Inf          aucinf.obs 1.225372e+04    <NA>  500
#> 571      1 20  1     0  24             auclast 1.365089e+04    <NA>  500
#> 572      1 20  1     0 Inf                cmax 4.126813e+03    <NA>  500
#> 573      1 20  1     0 Inf                tmax 1.000000e+00    <NA>  500
#> 574      1 20  1     0 Inf               tlast 1.680000e+02    <NA>  500
#> 575      1 20  1     0 Inf           clast.obs 3.322179e-06    <NA>  500
#> 576      1 20  1     0 Inf            lambda.z 1.064484e-01    <NA>  500
#> 577      1 20  1     0 Inf           r.squared 9.999136e-01    <NA>  500
#> 578      1 20  1     0 Inf       adj.r.squared 9.998963e-01    <NA>  500
#> 579      1 20  1     0 Inf lambda.z.time.first 1.200000e+01    <NA>  500
#> 580      1 20  1     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 581      1 20  1     0 Inf   lambda.z.n.points 7.000000e+00    <NA>  500
#> 582      1 20  1     0 Inf          clast.pred 3.193431e-06    <NA>  500
#> 583      1 20  1     0 Inf           half.life 6.511579e+00    <NA>  500
#> 584      1 20  1     0 Inf          span.ratio 2.395732e+01    <NA>  500
#> 585      1 20  1     0 Inf          aucinf.obs 1.378046e+04    <NA>  500
#> 586      0 20  2     0  24             auclast 2.050570e+04    <NA>  500
#> 587      0 20  2     0 Inf                cmax 3.358273e+03    <NA>  500
#> 588      0 20  2     0 Inf                tmax 2.000000e+00    <NA>  500
#> 589      0 20  2     0 Inf               tlast 1.680000e+02    <NA>  500
#> 590      0 20  2     0 Inf           clast.obs 7.391557e-06    <NA>  500
#> 591      0 20  2     0 Inf            lambda.z 1.061880e-01    <NA>  500
#> 592      0 20  2     0 Inf           r.squared 9.999752e-01    <NA>  500
#> 593      0 20  2     0 Inf       adj.r.squared 9.999670e-01    <NA>  500
#> 594      0 20  2     0 Inf lambda.z.time.first 4.800000e+01    <NA>  500
#> 595      0 20  2     0 Inf  lambda.z.time.last 1.680000e+02    <NA>  500
#> 596      0 20  2     0 Inf   lambda.z.n.points 5.000000e+00    <NA>  500
#> 597      0 20  2     0 Inf          clast.pred 7.244380e-06    <NA>  500
#> 598      0 20  2     0 Inf           half.life 6.527547e+00    <NA>  500
#> 599      0 20  2     0 Inf          span.ratio 1.838363e+01    <NA>  500
#> 600      0 20  2     0 Inf          aucinf.obs 2.087764e+04    <NA>  500
```
