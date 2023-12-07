
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NIF

<!-- badges: start -->
<!-- badges: end -->

This is a package to create NONMEM input files (NIF) from SDTM data.

## Installation

You can install the development version of nif like so:

``` r
devtools::install_github("rstrotmann/nif", build_vignettes=TRUE)
```

## Example

### NIF data set creation

This is a basic example using a sample SDTM object to create a NIF data
set:

``` r
library(nif)

sdtm <- examplinib_sad

nif <-make_nif(sdtm) %>% 
  compress_nif()
```

In most cases, you may want to add further covariates after `make_nif()`
and before `ìndex_nif()`, consider a more complex example:

``` r
nif <-make_nif(sdtm) %>% 
  compress_nif() %>% 
  mutate(COHORT=ACTARMCD) %>% 
  mutate(FOOD=0)
```

### Data exploration

``` r
summary(nif)
plot(summary(nif))
```

# Further information

For further guidance see the help for individual functions and the
included vignettes (see `browseVignettes("nif")`):
