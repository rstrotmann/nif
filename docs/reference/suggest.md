# Suggest data programming steps to generate a nif object from an sdtm object

Suggest data programming steps to generate a nif object from an sdtm
object

## Usage

``` r
suggest(obj, consider_nif_auto = FALSE)
```

## Arguments

- obj:

  A sdtm object

- consider_nif_auto:

  Include suggestions regarding parent or metabolite mappings to the
  sdtm object, as logical.

## Value

Nothing.

## Examples

``` r
suggest(examplinib_poc)
#> 
#> ── 1. Treatments ───────────────────────────────────────────────────────────────
#> There are 1 treatments (EXTRT) in EX: EXAMPLINIB. Consider adding them to the
#> nif object using `add_administration()`, see the code snippet below (replace
#> 'sdtm' with the name of your sdtm object):
#> 
#>   add_administration(sdtm, 'EXAMPLINIB') %>%
#> 
#> ── 2. Pharmacokinetic observations ─────────────────────────────────────────────
#> There are 2 pharmacokinetic analytes:
#> 
#> PCTEST       PCTESTCD     
#> RS2023       RS2023       
#> RS2023487A   RS2023487A   
#> 
#> Consider adding them to the nif object using `add_observation()`, see the code
#> snippet below (replace 'sdtm' with the name of your sdtm object):
#> 
#>   add_observation(sdtm, 'pc', RS2023') %>%
#>   add_observation(sdtm, 'pc', RS2023487A') %>%
#> 
#> 
#> ── 3. Study arms ───────────────────────────────────────────────────────────────
#> There are 2 study arms defined in DM:
#> 
#> ACTARMCD    ACTARM                 
#> SCRNFAIL    Screen Faillure        
#> TREATMENT   Single Arm Treatment   
#> 
#> Consider defining a PART or ARM variable, filtering for a particular arm, or
#> defining a covariate based on ACTARMCD.
```
