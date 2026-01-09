# Set all NA values in DV to zero

Set all NA values in DV to zero

## Usage

``` r
dv_na_to_zero(obj)
```

## Arguments

- obj:

  A nif object.

## Value

A nif object.

## Examples

``` r
dv_na_to_zero(examplinib_sad_min_nif)
#> ----- NONMEM Input Format (NIF) data -----
#> 816 observations from 48 subjects  
#> 1 compartment with observations: '2' 
#> 
#> Columns:
#>   ID, TIME, AMT, RATE, EVID, DV, CMT, MDV 
#> 
#> Hash: 82487a361969b8ed15b890508d49c285
#> 
#> Data (selected columns):
#>   ID   TIME   EVID   CMT   AMT   DV      
#>   1    0      1      1     5     0       
#>   1    0      0      2     0     0       
#>   1    0.5    0      2     0     0.002   
#>   1    1      0      2     0     0.007   
#>   1    1.5    0      2     0     0.016   
#>   1    2      0      2     0     0.021   
#>   1    3      0      2     0     0.015   
#>   1    4      0      2     0     0.013   
#>   1    6      0      2     0     0.005   
#>   1    8      0      2     0     0.002    
#> 854 more rows
```
