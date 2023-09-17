
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nif

<!-- badges: start -->
<!-- badges: end -->

This is a package to create NONMEM input files (NIF) from SDTM data.

## Installation

You can install the development version of nif like so:

``` r
devtools::install_github("rstrotmann/nif", build_vignettes=TRUE)
```

## Example

This is a basic example to load SDTM data from SAS files and create a
NIF. The path `/path/to/sdtm/data` needs to point to a location where at
least the following SAS export files from a study needs to be present:

- dm.sas7bdat
- ex.sas7bdat
- vs.sas7bdat
- pc.sas7bdat

``` r
library(nif)

sdtm.data <- nif::read_sdtm_sas("/path/to/sdtm/data")

nif.data <- nif::make_nif(sdtm.data) %>%
  index_nif() %>%
  compress_nif(standard_nif_fields)
```

In most cases, you may want to add further covariates after `make_nif()`
and before `ìndex_nif()`, consider a more complex example:

``` r
nif.data <- nif::make_nif(sdtm.data) %>%
  filter(!ACTARMCD %in% c("SCRNFAIL", "NOTTRT")) %>% 
  add_lab_observation(sdtm.data$domains[["lb"]], "BILI", 5) %>% 
  add_bl_lab(sdtm.data$domains[["lb"]], "HGB") %>% 
  mutate(PART=str_sub(ACTARMCD,1,1), COHORT=as.numeric(str_sub(ACTARMCD, 2, 2))) %>%
  index_nif() %>%
  compress_nif(standard_nif_fields)
```

# Further information

For further guidance see the help for individual functions and the
included vignettes (see `browseVignettes("nif")`):

- “Creating NIF files from SDTM data”
- “Imputations and assumptions used by make_nif()”
