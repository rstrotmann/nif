## This files contains code that is only used during package development to
## generate the fictional `examplinib` SDTM data. It is included only for
## documentation. If, during development, the dataset needs to be recreated,
## please see the instructions at the end of the file.



# PC
#
# Required fields as per SDTM domain model 3.2 are:
# STUDYID, DOMAIN, USUBJID, PCSEQ, PCTESTCD, PCTEST
#
# Expected fields:
# PCORRES, PCORRESU, PCSTRESC, PCSTRESN, PCSTRESU, PCNAM, PCSPEC, PCLLOQ,
# VISITNUM, PCDTC

#' Simulate PK data for fictional subjects, using fictional popPK model
#'
#' Based on fictional two-compartment model with sequential zero-order and
#'   first-order absorption, elimination from central compartment.
#'
#' @param sbs The subject baseline information as data frame. Needs at least
#'   ID, PERIOD, SEX, AGE, HEIGHT, WEIGHT, and FOOD fields.
#' @param sampling_scheme The PK sampling scheme as data frame. The field `time`
#'   is to provide the sampling times in numerical form.
#' @return The simulated PK data as data frame.
#' @import rxode2
pk_sim <- function(sbs, sampling_scheme) {
  mod <- rxode2::rxode2({
    c_centr = centr / v_centr * (1+centr.err);
    c_peri = peri / v_peri;
    c_metab = metab / v_metab;

    ke = t.ke * exp(eta.ke)    # renal elimination constant
    ka = t.ka * exp(eta.ka) + FOOD * t.ka1
    d1 = t.d1 * exp(eta.d1)
    fm = t.fm * exp(eta.fm)   # fraction metabolized

    cl = t.cl * exp(eta.cl)    # metabolic clearance
    kem = t.kem * exp(eta.kem) # elimination constant for metabolite
    fpar = 1 * exp(eta.fpar) + FOOD * t.fpar1
    q = t.q * exp(eta.q)

    d/dt(depot) = -ka * depot * fpar;
    dur(depot) = d1
    d/dt(centr) = ka * depot * fpar - ke * c_centr - q * c_centr + q * c_peri;
    d/dt(peri) = q * c_centr - q * c_peri;
    d/dt(renal) = ke * c_centr * (1 - fm)

    d/dt(metab) = cl * c_centr * fm - kem * c_metab
    d/dt(metab_excr) = kem * c_metab
  })

  theta <- c(
    t.ka = 0.8,
    t.ka1 = 0.8, # food effect on Ka
    t.d1 = 1.8,
    t.fpar1 = 2, # food effect on F
    t.ke = 30,
    t.q = 5,
    t.cl = 20,
    t.kem = 10,
    t.fm = 0.8,

    v_centr = 100,
    v_peri = 100,
    v_metab = 10
  )

  omega <- rxode2::lotri(
    eta.ke + eta.ka + eta.d1 + eta.fpar + eta.q + eta.cl + eta.kem + eta.fm ~ c(
    0.3^2,
    0,      0.1^2,
    0,      0,      0.2^2,
    0,      0,      0,      0.3^2,
    0,      0,      0,      0,      0.3^2,
    0,      0,      0,      0,      0,      0.4^2,
    0,      0,      0,      0,      0,      0,      0.3^2,
    0,      0,      0,      0,      0,      0,      0,      0.03^2))

  sigma <- rxode2::lotri(centr.err ~ .1^2)

  # reference: https://nlmixr2.github.io/rxode2/articles/rxode2-event-table.html
  ev <- rxode2::et(amountUnits="mg", timeUnits="hours") %>%
    rxode2::add.dosing(dose=500, dosing.to="depot", rate=-2, start.time=0) %>%
    rxode2::add.sampling(sampling_scheme$time) %>%
    rxode2::et(id=unique(sbs$ID)) %>%
    as.data.frame() %>%
    left_join(
      sbs %>%
        dplyr::select(id=ID, PERIOD, SEX, AGE, HEIGHT, WEIGHT, FOOD),
      by="id")

  sim <- mod$solve(theta, ev, omega=omega, sigma=sigma,
                   keep=c("FOOD", "PERIOD")) %>%
    as.data.frame()
  return(sim)
}




#' Simulate fictional subject disposition data
#'
#' This function generates the prespecified number of subjects across different
#'   clinical sites with fictional dates for signing the informed consent and
#'   treatment start.
#'
#' @param studyid The study identifyer as string.
#' @param nsubs The number of subjects to be simulated.
#' @param nsites The number of clinical sites to be simulated.
#' @param screenfailure_rate The fraction of subjects to be screeing failures.
#' @param start_date The fictional study start date.
#'
#' @import lubridate
#'
#' @return The disposition data for the simulated subjects as data frame.
make_subs <- function(studyid="1", nsubs=10, nsites=4, screenfailure_rate=0.25,
                      start_date="2000-12-20 10:00") {
  current_date <- lubridate::as_datetime(start_date, format="%Y-%m-%d %H:%M")
  site_names <- 100 + seq(1:nsites)
  sbs <- data.frame(SITEID="", SUBJID="", ACTARM="", ACTARMCD="", RFICDTC=NA,
                    RFSTDTC=NA)
  sbs_by_site <- rep(0, nsites)

  while(nrow(sbs %>% filter(ACTARMCD!="SCRNFAIL")) < nsubs+1) {
    current_site <- round(runif(1, 1, nsites), 0)
    current_sb_no <- sbs_by_site[current_site] + 1
    current_date <- current_date + floor(abs(rnorm(1, 0.5, 2))) * 60*60*24
    siteid <- as.character(site_names[current_site])
    enrolled = runif(1) > screenfailure_rate
    # if(enrolled){
      sbs_by_site[current_site] <- sbs_by_site[current_site] + 1
    # }
    sbs <- sbs %>%
      add_row(
        SITEID=siteid,
        SUBJID=paste0(siteid, as.character(formatC(current_sb_no, width=4, flag="0"))),
        ACTARMCD=case_when(!enrolled ~ "SCRNFAIL", .default=""),
        ACTARM=case_when(!enrolled ~ "Screen Failure", .default=""),
        RFICDTC=current_date + rnorm(1, 0, 1) * 60 * 60,
        RFSTDTC=case_when(enrolled ~ RFICDTC + floor(rnorm(1, 10, 2)) * 60*60*24, .default=NA)
      )
  }
 return(sbs[-1,])
}



# required: STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, SEX, ARMCD, ARM,
#   ACTARMCD, ACTARM, COUNTRY
# expected: RFSTDTC, RFENDTC, RFXSTDTC, RFXSTDTC, RFICDTC, RFPENDTC,
#   DTHDTC, DTHFL, RACE

#' Synthesize a fictional DM domain
#'
#' Currently geared toward a food effect study, but can be adapted easily to
#'   generate other study types.
#'
#' @param studyid The study ID.
#' @param nsubs The number of subjects to synthesize.
#' @param nsites The number of clinical sites.
#' @param duration The duration of the study.
#'
#' @return The DM data as data frame.
make_dm <- function(studyid="2000950044", nsubs=10, nsites=5, duration=7) {
  sbs <- make_subs(studyid, nsubs, nsites)
  dm <- sbs %>%
    group_by_all() %>%
    mutate(
      STUDYID=as.character(studyid),
      USUBJID=paste0(STUDYID, SUBJID),
      SEX=case_when(runif(1)>0.5 ~ "F", .default="M"),
      AGE=round(runif(1, 18, 55), 0),
      AGEU="YEARS",
      COUNTRY="DEU",
      DOMAIN="DM",
      ARM=ACTARM,
      ARMCD=ACTARMCD,
      RACE=cut(runif(1),
               breaks=c(0, .05, .2, 1),
               labels=c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")),
      ETHNIC=""
    ) %>%
    as.data.frame()

  seq1.sbs <- dm %>%
    filter(ACTARMCD!="SCRNFAIL") %>%
    as.data.frame() %>%
    slice_sample(prop=0.5) %>%
    pull(SUBJID)

  seq2.sbs <- dm %>%
    filter(ACTARMCD!="SCRNFAIL") %>%
    filter(!USUBJID %in% seq1.sbs) %>%
    pull(SUBJID)

  dm <- dm %>%
    mutate(ACTARMCD=case_when(
      SUBJID %in% seq1.sbs ~ "AB",
      SUBJID %in% seq2.sbs ~ "BA",
      .default="SCRNFAIL")) %>%
    mutate(ACTARM=case_when(
      SUBJID %in% seq1.sbs ~ "Fasted - Fed",
      SUBJID %in% seq2.sbs ~ "Fed - Fasted",
      .default="Screen Failure"))

  return(dm)
}

# Required: STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST
# Expected: VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VSBLFL, VISITNUM, VSDTC

# height 179, 6.81
# weigth 78.5 10.2

#' Synthesize fictional baseline vital sign data (VS domain)
#'
#' @param dm The DM domain to provide the subject data to generate baseline
#'   vital sign data for.
#'
#' @return The VS domain data as data frame.
make_vs <- function(dm) {
  vs <- dm %>%
    # filter(ACTARMCD!="SCRNFAIL") %>%
    dplyr::select(STUDYID, USUBJID, RFSTDTC) %>%
    group_by_all() %>%
    expand(VSTESTCD=c("HEIGHT", "WEIGHT")) %>%
    mutate(VSORRES=case_when(VSTESTCD=="HEIGHT" ~ rnorm(1, 179, 6.81),
                             VSTESTCD=="WEIGHT" ~ rnorm(1, 78.5, 10.2))) %>%
    mutate(VSTEST=case_when(VSTESTCD=="HEIGHT" ~ "Height",
                            VSTESTCD=="WEIGHT" ~ "Weight")) %>%
    mutate(VSORRESU=case_when(VSTESTCD=="HEIGHT" ~ "cm",
                            VSTESTCD=="WEIGHT" ~ "kg")) %>%
    mutate(VSSTRESN=round(VSORRES, digits=1)) %>%
    mutate(VSSTRESU=VSORRESU, EPOCH="SCREENING", DOMAIN="VS", VSBLFL="") %>%
    ungroup() %>%
    as.data.frame()
}


# Required: USUBJID, STUDYID, DOMAIN, EXSEEQ, EXTRT
# expected: EXDOSE, EXDOSEU, EXDOSEFRM, EXSTDTC, EXENDTC


#' Synthesize a fictional EX domain for single dose administration
#'
#' @param dm The DM including the subject info for whom to synthesize EX.
#' @param admindays The treatment days as vector.
#' @param drug The name of the drug to be administered.
#'
#' @return EX data as data frame.
make_sd_ex <- function(dm, admindays=c(1, 14), drug="RS2023") {
  ex <- dm %>%
    filter(ACTARMCD!="SCRNFAIL") %>%
    dplyr::select(STUDYID, USUBJID, RFSTDTC) %>%
    mutate(DOMAIN="EX") %>%
    group_by_all() %>%
    expand(EXDY=admindays) %>%
    ungroup() %>%
    mutate(EXSTDTC=RFSTDTC + (EXDY - 1) * 60*60*24) %>%
    mutate(EXSTDY=EXDY, EXENDY=EXDY) %>%
    mutate(EXENDTC=EXSTDTC, EXTRT=drug, EXDOSE=500) %>%
    mutate(EXROUTE="ORAL", EXDOSFRM="TABLET") %>%
    arrange(USUBJID, EXSTDTC) %>%
    group_by(USUBJID) %>%
    mutate(EXSEQ=row_number()) %>%
    ungroup() %>%
    mutate(EPOCH=case_when(EXSEQ==1 ~ "OPEN LABEL TREATMENT 1",
                           EXSEQ==2 ~ "OPEN LABEL TREATMENT 2")) %>%
    as.data.frame()

  return(ex)
}

# required: STUDYID, DOMAIN, USUBJID, PCSEQ, PCTESTCD, PCTEST
# expected: PCORRES, PCORRESU, PCSTRESC, PCSTRESN, PCSTRESU, PCNAM,
#     PCSPEC, PCLLOQ, VISITNUM, PCDTC

# make PCRFTDTC:  reference time
# make PCELTM:    NTIME!
# make PCTPT:
# make PCSEQ:

#' Synthesize a fictional PC domain.
#'
#' @param ex The EX domain to syntesize PC data for.
#' @param dm The DM domain to syntesize PC data for.
#' @param vs The VS domain to syntesize PC data for.
#' @param sampling_scheme The sampling scheme to be used.
#'
#' @return PC data as data frame.
make_pc <- function(ex, dm, vs, sampling_scheme) {
  abs_vs <- vs %>%
    filter(EPOCH=="SCREENING") %>%
    dplyr::select("USUBJID", "VSTESTCD", "VSSTRESN") %>%
    pivot_wider(names_from="VSTESTCD", values_from="VSSTRESN") %>%
    as.data.frame()

  sbs <- dm %>%
    dplyr::select(USUBJID, SEX, AGE, ACTARMCD) %>%
    left_join(abs_vs, by="USUBJID") %>%
    filter(ACTARMCD!="SCRNFAIL") %>%
    group_by(USUBJID) %>%
    mutate(ID=cur_group_id()) %>%
    ungroup() %>%
    arrange(ID)

  temp <- sbs %>%
    group_by_all() %>%
    expand(PERIOD=c(1, 2)) %>%
    ungroup() %>%
    mutate(EXDY=case_when(PERIOD==1 ~ 1, PERIOD==2 ~ 14)) %>%
    mutate(TREATMENT=str_sub(ACTARMCD, PERIOD, PERIOD)) %>%
    mutate(FOOD=case_when(TREATMENT=="B" ~ 1, .default=0)) %>%
    left_join(dm %>% select(USUBJID, RFSTDTC), by="USUBJID") %>%
    as.data.frame()

  sim <- rbind(
    pk_sim((temp %>% filter(PERIOD==1)), sampling_scheme),
    pk_sim((temp %>% filter(PERIOD==2)), sampling_scheme))

  pc <- sim %>%
    dplyr::select(id, time, c_centr, c_metab, PERIOD) %>%
    mutate(RS2023=c_centr, RS2023487A=c_metab) %>%
    pivot_longer(c("RS2023", "RS2023487A"), names_to="PCTESTCD", values_to="PCSTRESN") %>%
    mutate(PCTEST=case_when(PCTESTCD=="RS2023" ~ "RS2023",
                             PCTESTCD=="RS2023487A" ~"RS2023487A")) %>%
    left_join(temp %>% distinct(ID, USUBJID, RFSTDTC), by=c("id"="ID")) %>%
    mutate(STUDYID=unique(dm$STUDYID), DOMAIN="PC") %>%
    mutate(PCELTM=paste0("PT", as.character(time), "H")) %>%
    mutate(PCTPTNUM=time) %>%
    left_join(sampling_scheme, by="time") %>%
    mutate(PCDTC=RFSTDTC + (PERIOD-1)*13*24*60*60 + time*60*60) %>%
    arrange(id, PCDTC, PCTESTCD) %>%
    group_by(id) %>%
    mutate(PCSEQ=row_number()) %>%
    ungroup() %>%
    mutate(PCSPEC="PLASMA") %>%
    mutate(PCRFTDTC=RFSTDTC) %>%
    mutate(EPOCH=case_match(PERIOD, 1 ~ "OPEN LABEL TREATMENT 1",
                            2 ~ "OPEN LABEL TREATMENT 2")) %>%
    dplyr::select(-id, -time, -c_centr, - c_metab, -RFSTDTC) %>%
    as.data.frame()
  return(pc)
}

reformat_date <- function(x) {
  return(format(x, "%Y-%m-%dT%H:%M"))
}


#' Synthesize SDTM data for a fictional study
#'
#' @return The SDTM data as sdtm object.
synthesize_sdtm <- function() {
  basic_sampling_scheme <- data.frame(
    time = c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 24, 48, 72, 96, 144, 168),
    PCTPT = c("PREDOSE", "HOUR 0.5", "HOUR 1", "HOUR 1.5", "HOUR 2", "HOUR 3",
              "HOUR 4", "HOUR 6", "HOUR 8", "HOUR 10", "HOUR 12", "HOUR 24",
              "HOUR 48", "HOUR 72", "HOUR 96", "HOUR 144", "HOUR 168"))

  dm <- make_dm(studyid="2023000400", nsubs=20)
  vs <- make_vs(dm)
  ex <- make_sd_ex(dm, drug="RS2023")
  pc <- make_pc(ex, dm, vs, basic_sampling_scheme)

  out <- list()
  out[["dm"]] <- dm %>%
    mutate(RFICDTC=reformat_date(RFICDTC)) %>%
    mutate(RFSTDTC=reformat_date(RFSTDTC))

  out[["vs"]] <- vs %>%
    mutate(RFSTDTC=reformat_date(RFSTDTC))

  out[["ex"]] <- ex %>%
    mutate(RFSTDTC=reformat_date(RFSTDTC)) %>%
    mutate(EXSTDTC=reformat_date(EXSTDTC)) %>%
    mutate(EXENDTC=reformat_date(EXENDTC)) %>%
    mutate(EXTRT="EXAMPLINIB")

  out[["pc"]] <- pc %>%
    mutate(PCRFTDTC=reformat_date(PCRFTDTC)) %>%
    mutate(PCDTC=reformat_date(PCDTC))

  return(new_sdtm(out))
}

#' Synthesize the package data for `examplinib`
#'
#' @return The sdtm object.
make_examplinib_sdtm <- function() {
  examplinib <- synthesize_sdtm() %>%
    add_analyte_mapping("EXAMPLINIB", "RS2023") #%>%
    #add_analyte_mapping("EXAMPLINIB", "RS2023487A")
  return(examplinib)
}

#' Synthesize the package data for `examplinib`
#'
#' @return The nif object.
make_examplinib_nif <- function() {
  out <- make_examplinib_sdtm() %>%
    add_analyte_mapping("EXAMPLINIB", "RS2023") %>%
    #add_analyte_mapping("EXAMPLINIB", "RS2023487A") %>%
    make_nif() %>%
    mutate(PERIOD=str_sub(EPOCH, -1, -1)) %>%
    mutate(TREATMENT=str_sub(ACTARMCD, PERIOD, PERIOD)) %>%
    mutate(FASTED=case_when(TREATMENT=="A" ~ 1, .default=0))
  return(out)
}



#### Development note: To update the package data `examplinib` during the
#### package development, run the following code:
####
#### `examplinib <- make_examplinib_sdtm()`
#### `use_data(examplinib, overwrite=T)`
#### `examplinib_nif <- make_examplinib_nif()`
#### `use_data(examplinib_nif, overwrite=T)`






