# Check for incomplete date format as per ISO 8601 in columns ending with 'DTC'

Check for incomplete date format as per ISO 8601 in columns ending with
'DTC'

## Usage

``` r
check_date_format(obj, verbose = TRUE)
```

## Arguments

- obj:

  The SDTM domain as data frame.

- verbose:

  Boolean to indicate whether to issue message output.

## Value

The (unchanged) SDTM domain.

## Examples

``` r
ex <- check_date_format(domain(examplinib_poc, "ex"))
```
