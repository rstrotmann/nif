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
#> 1 104    1040001 Fed - Fasted BA       2000-12-21T09:10 2001-01-02T09:10
#> 2 102    1020001 Fed - Fasted BA       2000-12-23T10:22 2000-12-31T10:22
#> 3 104    1040002 Fasted - Fed AB       2000-12-24T10:26 2001-01-02T10:26
#>   RFXSTDTC         STUDYID    USUBJID           SEX     AGE AGEU  COUNTRY DOMAIN
#>   <chr>            <chr>      <chr>             <chr> <dbl> <chr> <chr>   <chr> 
#> 1 2001-01-02T09:10 2023000400 20230004001040001 M        22 YEARS DEU     DM    
#> 2 2000-12-31T10:22 2023000400 20230004001020001 F        38 YEARS DEU     DM    
#> 3 2001-01-02T10:26 2023000400 20230004001040002 M        34 YEARS DEU     DM    
#>   ARM          ARMCD RACE  ETHNIC RFENDTC         
#>   <chr>        <chr> <fct> <chr>  <chr>           
#> 1 Fed - Fasted BA    WHITE ""     2001-01-15T09:10
#> 2 Fed - Fasted BA    WHITE ""     2001-01-13T10:22
#> 3 Fasted - Fed AB    WHITE ""     2001-01-15T10:26
```
