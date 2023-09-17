## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=F, warning=F----------------------------------------------
library(tidyr)
library(dplyr)
library(stringr)
library(knitr)
library(nif)

## ---- eval=FALSE, echo=T------------------------------------------------------
#  read_sdtm_sas("path/to/sdtm/data")

## ---- eval=FALSE, echo=T------------------------------------------------------
#  read_sdtm_sas("path/to/sdtm/data", "dm", "vs", "ex", "pc", "lb", "ae")

## -----------------------------------------------------------------------------
examplinib$domains[["dm"]] %>% head()

## -----------------------------------------------------------------------------
dm <- examplinib$dm

## -----------------------------------------------------------------------------
examplinib

## -----------------------------------------------------------------------------
suggest(examplinib)

## -----------------------------------------------------------------------------
sdtm_expl <- examplinib %>% 
  add_analyte_mapping("EXAMPLINIB", "RS2023") %>% 
  add_metabolite_mapping("RS2023", "RS2023487A")

## -----------------------------------------------------------------------------
nif_expl <- make_nif(sdtm_expl)

## -----------------------------------------------------------------------------
nif_expl %>% 
  as.data.frame() %>% 
  distinct(EPOCH, ACTARMCD, ACTARM)

## -----------------------------------------------------------------------------
nif_expl <- make_nif(sdtm_expl, silent=T) %>%
  mutate(PERIOD=str_sub(EPOCH, -1, -1)) %>% 
  mutate(TREATMENT=str_sub(ACTARMCD, PERIOD, PERIOD)) %>% 
  mutate(FASTED=case_when(TREATMENT=="A" ~ 1, .default=0))

## -----------------------------------------------------------------------------
nif_expl %>%
  select(USUBJID, PERIOD, TREATMENT, FASTED, TIME, EVID, AMT, ANALYTE, DV) %>% 
  arrange(USUBJID, PERIOD, TREATMENT, FASTED, TIME, EVID, ANALYTE) %>% 
  as.data.frame() %>% 
  head() %>% 
  kable()

## ---- warning=F, fig.width=4, fig.height=3------------------------------------
plot(nif_expl)

## ---- warning=F, fig.width=4, fig.height=3------------------------------------
plot(nif_expl, analyte="RS2023", group="FASTED", max_x=24, mean=F, points=T, nominal_time=T)

## ---- warning=F, fig.width=4, fig.height=3------------------------------------
plot(nif_expl, analyte="RS2023", group="FASTED", max_x=24, mean=T)

## ---- eval=FALSE, echo=T------------------------------------------------------
#  nif_viewer(nif_expl)

