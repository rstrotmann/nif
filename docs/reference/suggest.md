# Suggest data programming steps to generate a nif object from an sdtm object

Suggest data programming steps to generate a nif object from an sdtm
object

## Usage

``` r
suggest(obj, show_all = FALSE)
```

## Arguments

- obj:

  A sdtm object

- show_all:

  Show all lines in longer data frames, as logical.

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
#>   add_administration(sdtm, 'EXAMPLINIB')
#> 
#> ── 2. Pharmacokinetic observations ─────────────────────────────────────────────
#> There are 2 pharmacokinetic analytes:
#> 
#>   PCTEST       PCTESTCD     
#>   RS2023       RS2023       
#>   RS2023487A   RS2023487A    
#> 
#> Consider adding them to the nif object using `add_observation()`, see the code
#> snippet below (replace 'sdtm' with the name of your sdtm object):
#> 
#>   add_observation(sdtm, 'pc', 'RS2023')
#>   add_observation(sdtm, 'pc', 'RS2023487A')
#> 
#> ── NTIME definition ──
#> 
#> The PC domain contains multiple fields that the nominal sampling time can be
#> derived from:
#> 
#>   PCTPT            PCTPTNUM   PCELTM   
#>   PREDOSE          0          PT0H     
#>   POSTDOSE 0.5 H   0.5        PT0.5H   
#>   POSTDOSE 1 H     1          PT1H     
#>   POSTDOSE 1.5 H   1.5        PT1.5H   
#>   POSTDOSE 2 H     2          PT2H      
#>   (6 more rows)
#> 
#> Consider specifying a suitabe 'ntime_method' argument to 'add_observation()'.
#> By default, the function will attempt to extract time information from the
#> PCTPT field.
#> 
#> ── 3. Study arms ───────────────────────────────────────────────────────────────
#> There are 2 study arms defined in DM:
#> 
#>   ACTARMCD    ACTARM                 
#>   SCRNFAIL    Screen Faillure        
#>   TREATMENT   Single Arm Treatment    
#> 
#> Consider defining a PART or ARM variable, filtering for a particular arm, or
#> defining a covariate based on ACTARMCD.
#> 
#> ── 4. Baseline covariates ──────────────────────────────────────────────────────
#> The LB domains contains creatinine (CREAT) observations. Consider adding a
#> baseline creatinine covariate, baseline creatinine clearance (BL_CRCL) and
#> baseline renal function category:
#> 
#>   add_observation(sdtm, 'lb', 'CREAT')
#>   add_bl_crcl()
#>   add_bl_renal()
```
