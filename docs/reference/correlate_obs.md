# Correlate observations

**\[experimental\]**

## Usage

``` r
correlate_obs(
  obj,
  indep_analyte,
  dep_analyte,
  window = 10/60,
  time_field = "TIME",
  duplicate_function = mean
)
```

## Arguments

- obj:

  A nif object.

- indep_analyte:

  The independent analyte as character.

- dep_analyte:

  The dependent analyte(s) as character.

- window:

  The allowed time window between the independent and dependent analyte
  observations in hours.

- time_field:

  Time variable to use for the correlation of observations, can be 'DTC'
  or 'TIME' (default).

- duplicate_function:

  A function to resolve duplicate values for observations of the
  dependent analyte. Defaults to `mean`.

## Value

A data frame.

## Details

Identify observations of the dependent variables that are timely
correlated to the independent variable, i.e., are within a specified
time window to the observations of the independent variable. If multiple
dependent observations of a given analyte fall into the window, they are
summarized using the 'duplicate_function'. The output data frame is a
wide table based on the independent observations, with the (summarized)
dependent observations and the respective summarized observation times
for the dependent observations as additional columns.
