# Nice enumeration of multiple strings

Nice enumeration of multiple strings

## Usage

``` r
nice_enumeration(items, conjunction = "and")
```

## Arguments

- items:

  Items to enumerate as character.

- conjunction:

  The conjunction between the last and penultimate items.

## Value

Enumeration as character.

## Examples

``` r
nice_enumeration("A")
#> [1] "A"
nice_enumeration(c("A", "B"))
#> [1] "A and B"
nice_enumeration(c("A", "B", "C"))
#> [1] "A, B and C"
nice_enumeration(c("A", "B", "C"), conjunction = "or")
#> [1] "A, B or C"
```
