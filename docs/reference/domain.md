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

The specified domain as data frame. Issues a warning if the domain does
not exist and returns NULL.

## Examples

``` r
head(domain(examplinib_fe, "dm"), 3)
#>   SITEID  SUBJID         ACTARM ACTARMCD          RFICDTC          RFSTDTC
#> 1    102 1020001 Screen Failure SCRNFAIL 2000-12-21T10:30             <NA>
#> 2    104 1040001   Fed - Fasted       BA 2000-12-25T09:47 2001-01-02T09:47
#> 3    105 1050001   Fasted - Fed       AB 2000-12-26T10:05 2001-01-05T10:05
#>           RFXSTDTC    STUDYID           USUBJID SEX AGE  AGEU COUNTRY DOMAIN
#> 1             <NA> 2023000400 20230004001020001   F  42 YEARS     DEU     DM
#> 2 2001-01-02T09:47 2023000400 20230004001040001   M  28 YEARS     DEU     DM
#> 3 2001-01-05T10:05 2023000400 20230004001050001   M  34 YEARS     DEU     DM
#>              ARM    ARMCD                      RACE ETHNIC          RFENDTC
#> 1 Screen Failure SCRNFAIL                     WHITE                    <NA>
#> 2   Fed - Fasted       BA                     WHITE        2001-01-15T09:47
#> 3   Fasted - Fed       AB BLACK OR AFRICAN AMERICAN        2001-01-18T10:05
```
