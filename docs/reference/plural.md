# Return singular or plural form of word

Return singular or plural form of word

## Usage

``` r
plural(word, plural)
```

## Arguments

- word:

  Source word in singular form, as character.

- plural:

  Return plural form, as character.

## Value

Character.

## Examples

``` r
plural("subject", FALSE)
#> [1] "subject"
plural("subject", TRUE)
#> [1] "subjects"
plural("study", FALSE)
#> [1] "study"
plural("study", TRUE)
#> [1] "studies"
```
