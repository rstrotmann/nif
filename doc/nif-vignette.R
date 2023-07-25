## ----echo=F, message=F, warning=F---------------------------------------------
library(tidyverse)
library(knitr)

## ---- eval=FALSE, echo=T------------------------------------------------------
#  nif::read_sdtm_sas("path/to/sdtm/data")

## ---- eval=FALSE, echo=T------------------------------------------------------
#  read_sdtm_sas("path/to/sdtm/data", "dm", "vs", "ex", "pc", "lb", "ae")

## -----------------------------------------------------------------------------
sdtm.expl <- nif::sdtm(nif::examplinib)
sdtm.expl

## -----------------------------------------------------------------------------
nif::suggest(sdtm.expl)

## -----------------------------------------------------------------------------
sdtm.expl <- nif::sdtm(nif::examplinib) %>% 
  nif::add_mapping("EXAMPLINIB", "RS2023")

## -----------------------------------------------------------------------------
nif.expl <- nif::make_nif(sdtm.expl)

## -----------------------------------------------------------------------------
nif.expl %>% 
  as.data.frame() %>% 
  distinct(EPOCH, ACTARMCD, ACTARM)

## -----------------------------------------------------------------------------
nif.expl <- nif::make_nif(sdtm.expl, silent=T) %>%
  mutate(PERIOD=str_sub(EPOCH, -1, -1)) %>% 
  mutate(TREATMENT=str_sub(ACTARMCD, PERIOD, PERIOD)) %>% 
  mutate(FASTED=case_when(TREATMENT=="A" ~ 1, .default=0))

## -----------------------------------------------------------------------------
nif.expl %>%
  dplyr::select(USUBJID, PERIOD, TREATMENT, FASTED, TIME, EVID, AMT, DV) %>% 
  as.data.frame() %>% 
  head() %>% 
  kable()

## ---- warning=F, fig.width=4, fig.height=3------------------------------------
plot(nif.expl)

## ---- warning=F, fig.width=4, fig.height=3------------------------------------
plot(nif.expl, analyte="RS2023", group="FASTED", max_x=24, mean=F, points=T, nominal_time=T)

## ---- warning=F, fig.width=4, fig.height=3------------------------------------
plot(nif.expl, analyte="RS2023", group="FASTED", max_x=24, mean=T)

## ---- eval=FALSE, echo=T------------------------------------------------------
#  nif_viewer(nif.expl)

