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
#> 1. There are 1 different treatments in 'EX' (see below).
#>       EXTRT        
#>       ----------
#>       EXAMPLINIB   
#>    Consider adding them to the nif object using `add_administration()`, see the
#>    code snippet below (replace 'sdtm' with the name of your sdtm object):
#>    ---
#>      %>%
#>        add_administration(sdtm, 'EXAMPLINIB')
#>    ---
#> 2. There are 2 different pharmacokinetic analytes in 'PC':
#>       PCTEST       PCTESTCD     
#>       ----------   ----------
#>       RS2023       RS2023       
#>       RS2023487A   RS2023487A   
#>    Consider adding them to the nif object using `add_observation()`, see the
#>    code snippet below (replace 'sdtm' with the name of your sdtm object):
#>    ---
#>      %>%
#>        add_observation(sdtm, 'pc', 'RS2023', parent = 'x') %>%
#>        add_observation(sdtm, 'pc', 'RS2023487A', parent = 'x')
#>    ---
#> 3. There are 2 study arms defined in DM (see below). Consider defining a PART or
#>    ARM variable in the nif dataset, filtering for a particular arm, or defining
#>    a covariate based on ACTARMCD.
#>       ACTARM                 ACTARMCD    
#>       --------------------   ---------
#>       Single Arm Treatment   TREATMENT   
#>       Screen Faillure        SCRNFAIL    
```
