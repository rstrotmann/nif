# Trial title

Extract trial title from the TS domain, if available. Non-ASCII
characters are removed from the output to ensure compatibility.

## Usage

``` r
trial_title(obj)
```

## Arguments

- obj:

  A sdtm object.

## Value

The title as ASCII character, or NULL.

## Examples

``` r
trial_title(examplinib_sad)
#> [1] "An open label dose escalation study of RS2023 in healthy subjects"
```
