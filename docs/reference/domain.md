# Return a specific domain from a sdtm object

Return a specific domain from a sdtm object

## Usage

``` r
domain(obj, name)
```

## Arguments

- obj:

  The sdtm object.

- name:

  The domain to return as a single character string.

## Value

The specified domain as data frame. Stops if the domain is not
available.

## Examples

``` r
head(domain(examplinib_fe, "dm"), 3)
#> ──────── SDTM domain ──────── 
#> Domain DM, study 2023000400
#> An open-label 2-period crossover study in healthy subjects to investigate the effect of food on the pharmacokinetics of examplinib
#> 
#> # A tibble: 3 × 19
#>   SITEID SUBJID  ACTARM       ACTARMCD RFICDTC          RFSTDTC         
#>   <chr>  <chr>   <chr>        <chr>    <chr>            <chr>           
#> 1 102    1020001 Fasted - Fed AB       2000-12-22T09:49 2001-01-01T09:49
#> 2 104    1040001 Fed - Fasted BA       2000-12-24T11:25 2001-01-03T11:25
#> 3 104    1040002 Fed - Fasted BA       2000-12-27T10:17 2001-01-07T10:17
#>   RFXSTDTC         STUDYID    USUBJID           SEX     AGE AGEU  COUNTRY DOMAIN
#>   <chr>            <chr>      <chr>             <chr> <dbl> <chr> <chr>   <chr> 
#> 1 2001-01-01T09:49 2023000400 20230004001020001 M        23 YEARS DEU     DM    
#> 2 2001-01-03T11:25 2023000400 20230004001040001 F        47 YEARS DEU     DM    
#> 3 2001-01-07T10:17 2023000400 20230004001040002 F        53 YEARS DEU     DM    
#>   ARM          ARMCD RACE  ETHNIC RFENDTC         
#>   <chr>        <chr> <fct> <chr>  <chr>           
#> 1 Fasted - Fed AB    WHITE ""     2001-01-14T09:49
#> 2 Fed - Fasted BA    WHITE ""     2001-01-16T11:25
#> 3 Fed - Fasted BA    WHITE ""     2001-01-20T10:17
```
