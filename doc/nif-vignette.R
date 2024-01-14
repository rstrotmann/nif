## ----include = FALSE----------------------------------------------------------
library(knitr)
knitr::opts_chunk$set(
  collapse = TRUE#,
  #comment = "> "
)

## ----setup, message=F, warning=F----------------------------------------------
library(tidyr)
library(dplyr)
library(stringr)
library(nif)

## ----eval=FALSE, echo=T-------------------------------------------------------
#  read_sdtm_sas("path/to/sdtm/data")

## ----eval=FALSE, echo=T-------------------------------------------------------
#  read_sdtm_sas("path/to/sdtm/data", "dm", "vs", "ex", "pc", "lb", "ae")

## -----------------------------------------------------------------------------
examplinib_fe %>% domain("dm") %>% head(3)

## -----------------------------------------------------------------------------
dm <- examplinib_fe$dm

## -----------------------------------------------------------------------------
examplinib_fe

## -----------------------------------------------------------------------------
examplinib_fe %>% subject_info("20230004001050001")

## -----------------------------------------------------------------------------
suggest(examplinib_fe)

## -----------------------------------------------------------------------------
sdtm_expl <- examplinib_fe %>% 
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
  mutate(FASTED=case_when(TREATMENT=="A" ~ 1, .default=0)) #%>% 
  # compress()

## -----------------------------------------------------------------------------
nif_expl %>%
  select(USUBJID, PERIOD, TREATMENT, FASTED, TIME, EVID, AMT, ANALYTE, DV) %>% 
  head()

## -----------------------------------------------------------------------------
summary(nif_expl)

## ----results='hide', fig.width=3, fig.height=3--------------------------------
invisible(capture.output(
   nif_expl %>% 
  summary() %>% 
  plot()
))

## ----warning=F, fig.width=5, fig.height=4-------------------------------------
plot(nif_expl)

## -----------------------------------------------------------------------------
plot(nif_expl %>% add_tad(), tad=T, analyte="RS2023")

## ----warning=F, fig.width=5, fig.height=4-------------------------------------
plot(nif_expl,
     analyte="RS2023",
     group="FASTED",
     max_x=24,
     mean=FALSE,
     points=TRUE,
     nominal_time=TRUE)

## ----warning=F, fig.width=5, fig.height=4-------------------------------------
plot(nif_expl,
     analyte="RS2023",
     group="FASTED",
     max_x=24,
     mean=T)

## ----eval=FALSE, echo=T-------------------------------------------------------
#  nif_viewer(nif_expl)

## -----------------------------------------------------------------------------
nca <- nif_expl %>% 
  nca(analyte="RS2023",
      group="FASTED",
      nominal_time=T)

nca %>% 
  nca_summary_table(group="FASTED") %>% 
  kable()

