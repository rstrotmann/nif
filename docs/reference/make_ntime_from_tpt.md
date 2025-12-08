# Convert time point text to numeric time values

Extracts numeric time values (in hours) from time point text
descriptions. This function handles various formats including pre-dose
notations, hour and minute specifications, time ranges, and day
information.

## Usage

``` r
make_ntime_from_tpt(obj, domain = NULL)
```

## Arguments

- obj:

  A data frame containing time point text descriptions.

- domain:

  The domain code as character (default: "PC" for pharmacokinetic). Used
  to determine the column name containing time point descriptions.

## Value

A data frame with a column representing the unique values of the xxTPT
variable and a NTIME column with the time in hours.

## Details

The function recognizes and processes the following patterns:

- Pre-dose notations (e.g., "PRE-DOSE", "PREDOSE") are converted to 0

- Hour specifications (e.g., "1H POST-DOSE", "2.5 HOURS POST DOSE")

- Minute specifications (e.g., "30 MIN POST DOSE") are converted to
  hours

- Time ranges (e.g., "0.5H TO 2H POST-DOSE") - the later time is used

- Day information (e.g., "DAY1 - 2 HOURS POST ADMINISTRATION")

## Examples

``` r
df <- data.frame(PCTPT = c("PRE-DOSE", "1H POST-DOSE", "2.5 HRS POST-DOSE"))
make_ntime_from_tpt(df)  # Returns c(0, 1, 2.5)
#> NULL
```
