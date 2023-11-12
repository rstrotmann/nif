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
# pk_sim <- function(sbs, sampling_scheme) {
#   mod <- rxode2::rxode2({
#     c_centr = centr / v_centr * (1+centr.err);
#     c_peri = peri / v_peri;
#     c_metab = metab / v_metab;
#
#     ke = t.ke * exp(eta.ke)    # renal elimination constant
#     ka = t.ka * exp(eta.ka) + FOOD * t.ka1
#     d1 = t.d1 * exp(eta.d1)
#     fm = t.fm * exp(eta.fm)   # fraction metabolized
#
#     cl = t.cl * exp(eta.cl)    # metabolic clearance
#     kem = t.kem * exp(eta.kem) # elimination constant for metabolite
#     fpar = 1 * exp(eta.fpar) + FOOD * t.fpar1
#     q = t.q * exp(eta.q)
#
#     d/dt(depot) = -ka * depot * fpar;
#     dur(depot) = d1
#     d/dt(centr) = ka * depot * fpar - ke * c_centr - q * c_centr + q * c_peri;
#     d/dt(peri) = q * c_centr - q * c_peri;
#     d/dt(renal) = ke * c_centr * (1 - fm)
#
#     d/dt(metab) = cl * c_centr * fm - kem * c_metab
#     d/dt(metab_excr) = kem * c_metab
#   })
#
#   theta <- c(
#     t.ka = 0.8,
#     t.ka1 = 0.8, # food effect on Ka
#     t.d1 = 1.8,
#     t.fpar1 = 2, # food effect on F
#     t.ke = 30,
#     t.q = 5,
#     t.cl = 20,
#     t.kem = 10,
#     t.fm = 0.8,
#
#     v_centr = 100,
#     v_peri = 100,
#     v_metab = 10
#   )
#
#   omega <- rxode2::lotri(
#     eta.ke + eta.ka + eta.d1 + eta.fpar + eta.q + eta.cl + eta.kem + eta.fm ~ c(
#     0.3^2,
#     0,      0.1^2,
#     0,      0,      0.2^2,
#     0,      0,      0,      0.3^2,
#     0,      0,      0,      0,      0.3^2,
#     0,      0,      0,      0,      0,      0.4^2,
#     0,      0,      0,      0,      0,      0,      0.3^2,
#     0,      0,      0,      0,      0,      0,      0,      0.03^2))
#
#   sigma <- rxode2::lotri(centr.err ~ .1^2)
#
#   # reference: https://nlmixr2.github.io/rxode2/articles/rxode2-event-table.html
#   ev <- rxode2::et(amountUnits="mg", timeUnits="hours") %>%
#     rxode2::add.dosing(dose=500, dosing.to="depot", rate=-2, start.time=0) %>%
#     rxode2::add.sampling(sampling_scheme$time) %>%
#     rxode2::et(id=unique(sbs$ID)) %>%
#     as.data.frame() %>%
#     left_join(
#       sbs %>%
#         dplyr::select(id=ID, PERIOD, SEX, AGE, HEIGHT, WEIGHT, FOOD),
#       by="id")
#
#   sim <- mod$solve(theta, ev, omega=omega, sigma=sigma,
#                    keep=c("FOOD", "PERIOD")) %>%
#     as.data.frame()
#   return(sim)
# }


pk_sim1 <- function(sbs, sampling_scheme) {
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

  temp <- sbs %>%
    select(USUBJID, EXDOSE) %>%
    mutate(id=row_number())

  # reference: https://nlmixr2.github.io/rxode2/articles/rxode2-event-table.html
  ev <- rxode2::et(amountUnits="mg", timeUnits="hours") %>%
    rxode2::add.dosing(dose=500, dosing.to="depot", rate=-2, start.time=0) %>%
    rxode2::add.sampling(sampling_scheme$time) %>%
    # rxode2::et(id=unique(sbs$ID)) %>%
    rxode2::et(id=sbs$ID) %>%
    as.data.frame() %>%
    left_join(
      sbs %>%
        dplyr::select(id=ID, SEX, AGE, HEIGHT, WEIGHT, FOOD, PERIOD, EXDOSE),
      by="id") %>%
    mutate(amt=case_when(!is.na(amt)~EXDOSE, .default=NA)) %>%
    select(-EXDOSE)

  sim <- mod$solve(theta, ev, omega=omega, sigma=sigma,
                   keep=c("FOOD", "PERIOD")) %>%
    as.data.frame()
  return(sim)
}



pk_sim <- function(event_table) {
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

  # temp <- sbs %>%
  #   select(USUBJID, EXDOSE) %>%
  #   mutate(id=row_number())
  #
  # # reference: https://nlmixr2.github.io/rxode2/articles/rxode2-event-table.html
  # ev <- rxode2::et(amountUnits="mg", timeUnits="hours") %>%
  #   rxode2::add.dosing(dose=500, dosing.to="depot", rate=-2, start.time=0) %>%
  #   rxode2::add.sampling(sampling_scheme$time) %>%
  #   # rxode2::et(id=unique(sbs$ID)) %>%
  #   rxode2::et(id=sbs$ID) %>%
  #   as.data.frame() %>%
  #   left_join(
  #     sbs %>%
  #       dplyr::select(id=ID, SEX, AGE, HEIGHT, WEIGHT, FOOD, PERIOD, EXDOSE),
  #     by="id") %>%
  #   mutate(amt=case_when(!is.na(amt)~EXDOSE, .default=NA)) %>%
  #   select(-EXDOSE)

  # sim <- mod$solve(theta, event_table, omega=omega, sigma=sigma,
  #                  keep=c("FOOD", "PERIOD")) %>%
  sim <- mod$solve(theta, event_table, omega=omega, sigma=sigma,
                   keep="NTIME") %>%
    as.data.frame()
  return(sim)
}



#' Simulate fictional subject disposition data
#'
#' This function generates a pre-specified number of subjects across different
#'   clinical sites with fictional dates for signing the informed consent and
#'   treatment start.
#'
#' @param studyid The study identifier as string.
#' @param nsubs The number of subjects to be simulated.
#' @param nsites The number of clinical sites to be simulated.
#' @param screenfailure_rate The fraction of subjects to be screeing failures.
#' @param start_date The fictional study start date.
#'
#' @import lubridate
#'
#' @return The disposition data for the simulated subjects as data frame.
make_subs <- function(studyid="2023001", nsubs=10, nsites=4, screenfailure_rate=0.25,
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
#' @param nsubs The number of subjects.
#' @param nsites The number of clinical sites.
#' @param duration The duration of the study in days.
#' @param female_fraction The fraction of female subjects (between 0 and 1).
#' @param min_age The minimum age.
#' @param max_age The maximum age.
#'
#' @return The DM data as data frame.
make_dm <- function(studyid="2023001", nsubs=10, nsites=5, duration=7,
                    female_fraction=0.5, min_age=18, max_age=55) {
  sbs <- make_subs(studyid, nsubs, nsites)
  dm <- sbs %>%
    group_by_all() %>%
    mutate(
      STUDYID=as.character(studyid),
      USUBJID=paste0(STUDYID, SUBJID),
      SEX=case_when(runif(1)>female_fraction ~ "M", .default="F"),
      AGE=round(runif(1, min_age, max_age), 0),
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

  # seq1.sbs <- dm %>%
  #   filter(ACTARMCD!="SCRNFAIL") %>%
  #   as.data.frame() %>%
  #   slice_sample(prop=0.5) %>%
  #   pull(SUBJID)
  #
  # seq2.sbs <- dm %>%
  #   filter(ACTARMCD!="SCRNFAIL") %>%
  #   filter(!USUBJID %in% seq1.sbs) %>%
  #   pull(SUBJID)
  #
  # dm <- dm %>%
  #   mutate(ACTARMCD=case_when(
  #     SUBJID %in% seq1.sbs ~ "AB",
  #     SUBJID %in% seq2.sbs ~ "BA",
  #     .default="SCRNFAIL")) %>%
  #   mutate(ACTARM=case_when(
  #     SUBJID %in% seq1.sbs ~ "Fasted - Fed",
  #     SUBJID %in% seq2.sbs ~ "Fed - Fasted",
  #     .default="Screen Failure"))

  return(dm)
}


# Required: STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST
# Expected: VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VSBLFL, VISITNUM, VSDTC

# height 179, 6.81
# weigth 78.5 10.2

#' Synthesize fictional baseline vital sign data (VS domain)
#'
#' @param dm The DM domain to provide the subject data.
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
#' @param dose The dose.
#'
#' @return EX data as data frame.
make_sd_ex <- function(dm, admindays=c(1, 14), drug="RS2023", dose=500) {
  ex <- dm %>%
    filter(ACTARMCD!="SCRNFAIL") %>%
    dplyr::select(STUDYID, USUBJID, RFSTDTC) %>%
    mutate(DOMAIN="EX") %>%
    group_by_all() %>%
    expand(EXDY=admindays) %>%
    ungroup() %>%
    mutate(EXSTDTC=RFSTDTC + (EXDY - 1) * 60*60*24) %>%
    mutate(EXSTDY=EXDY, EXENDY=EXDY) %>%
    mutate(EXENDTC=EXSTDTC, EXTRT=drug, EXDOSE=dose) %>%
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




#
# expand_missed <- function(start_dtc, treatment_duration=50, missed_prob=0.15) {
#   end_dtc <- start_dtc + 60*60*24 * treatment_duration
#   n <- floor(treatment_duration * runif(1, 0, missed_prob))
#   # temp <- sort(as.POSIXct(runif(n, start_dtc, end_dtc)))
#   temp <- sort(floor(runif(n, 0, treatment_duration)))
#   # print(paste("start_dtc", start_dtc))
#   # print(temp)
#
#   if(length(temp)>0){
#     df <- data.frame(
#       # EXSTDTC = c(start_dtc, temp[1:length(temp)] + 60*60*24),
#       EXSTDTC = c(start_dtc, temp + 60*60*24),
#       EXENDTC = c(temp - 60*60*24, end_dtc)#,
#       # temp=temp
#     )
#   } else {
#     df <- data.frame(
#       EXSTDTC = start_dtc,
#       EXENDTC = end_dtc,
#       temp=0
#     )
#   }
#   return(df)
# }
#



#' Create administration schedule with randomly missed doses
#'
#' @param start_dtc Starting DTC.
#' @param missed_prob Probability of missing administration.
#' @param end_dtc End DTC.
#' @param dose The normal dose.
#' @param dose_red The reduced dose.
#' @param red_prob The probability that a subject has a dose reduction. Dose
#'   reductions, if any, occur at a random day after day 7.
#'
#' @return EXSTDTC and EXENDTC as data frame.
miss_admins <- function(start_dtc, end_dtc, dose=500, dose_red=250,
                        missed_prob=0.15, red_prob=0.2) {
  # create missed administration days
  #end_dtc <- start_dtc + 60*60*24 * (treatment_duration-1)
  dose_reduction <- red_prob != 0
  treatment_duration = as.numeric(end_dtc - start_dtc) +1
  n <- floor(treatment_duration * runif(1, 0, missed_prob))
  admins <- data.frame(
    day=seq(1, treatment_duration),
    dtc=seq(start_dtc, end_dtc, by="1 day"),
    dose=dose
  )
  missed_days <- sort(unique(floor(runif(n, 2, treatment_duration))))
  admins[missed_days, "dtc"] <- NA

  if(dose_reduction) {
    if(runif(1, 0, 1) < red_prob) {
      red_start_dy <- floor(runif(1, 7, treatment_duration))
      red_days <- seq(red_start_dy, treatment_duration)
      admins[red_days, "dose"] <- dose_red
    }
  }

  # to do:
  # change times slightly
  # omit times occasionally

  # compress
  admins %>%
    # mutate(block=lead(case_when(is.na(dtc)~1, .default=NA))) %>%
    mutate(prev_dose=lag(dose)) %>%
    mutate(dose_red_start=dose!=prev_dose) %>%
    mutate(dose_restart=lag(is.na(dtc))) %>%
    # mutate(chg=case_when(is.na(dtc)~1, dose!=prev_dose~1, .default=NA)) %>%
    # mutate(block=lag(chg)) %>%
    # mutate(block=ifelse(row_number()==treatment_duration, 1, block)) %>%
    filter(!is.na(dtc)) %>%
    mutate(block=dose_red_start==T | dose_restart==T | row_number()==1) %>%
    group_by(block) %>%
    mutate(block_id=case_when(block==1 ~row_number(),
                              .default=NA)) %>%
    ungroup() %>%
    as.data.frame() %>%
    fill(block_id, .direction="down") %>%
    group_by(block_id) %>%
    mutate(EXSTDTC=dtc[1], EXENDTC=dtc[n()]) %>%
    ungroup() %>%
    as.data.frame() %>%
    distinct(EXSTDTC, EXENDTC, DOSE=dose)
}



#' Synthesize a fictional EX domain for single dose administration
#'
#' @param dm The DM including the subject info for whom to synthesize EX.
#' @param drug The name of the drug to be administered.
#' @param dose The dose.
#' @param treatment_duration The treatment duration in days.
#' @param missed_prob Probability to miss doses.
#' @param missed_doses Switch whether to include randomly missed doses as boolean.
#'
#' @return The EX domain as data frame.
make_md_ex <- function(dm, drug="RS2023", dose=500, treatment_duration=50,
                       missed_prob=0.15, missed_doses=T, dose_reductions=T) {
  ex <- dm %>%
    filter(ACTARMCD!="SCRNFAIL")

  ex <- ex %>%
    select(STUDYID, USUBJID, RFSTDTC) %>%
    mutate(DOMAIN="EX") %>%
    mutate(trtdur=floor(runif(nrow(ex), treatment_duration*.6,
                              treatment_duration+1))) %>%
    mutate(EXSTDTC=RFSTDTC,
           # EXENDTC=RFSTDTC+treatment_duration*3600*24)
           EXENDTC=RFSTDTC+trtdur*3600*24)

  if(missed_doses==TRUE) {
    ex <- ex %>%
      group_by(DOMAIN, STUDYID, USUBJID) %>%
      expand(miss_admins(EXSTDTC, EXENDTC)) %>%
      ungroup() %>%
      as.data.frame()
  }


  ex <- ex %>%
    mutate(EXTRT=drug, EXDOSE=DOSE, EPOCH="TREATMENT") %>%
    mutate(EXROUTE="ORAL", EXDOSFRM="TABLET") %>%
    arrange(USUBJID, EXSTDTC) %>%
    group_by(USUBJID) %>%
    mutate(EXSEQ=row_number()) %>%
    ungroup() %>%
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

#' Synthesize the PC domain for a food effect study.
#'
#' @param ex The EX domain to syntesize PC data for.
#' @param dm The DM domain to syntesize PC data for.
#' @param vs The VS domain to syntesize PC data for.
#' @param sampling_scheme The sampling scheme to be used.
#'
#' @return PC data as data frame.
make_fe_pc <- function(ex, dm, vs, sampling_scheme) {
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
    mutate(EXDOSE=500) %>%
    left_join(dm %>% select(USUBJID, RFSTDTC), by="USUBJID") %>%
    as.data.frame()

  sim <- rbind(
    pk_sim1((temp %>% filter(PERIOD==1)), sampling_scheme),
    pk_sim1((temp %>% filter(PERIOD==2)), sampling_scheme))

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


#' Title
#'
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @param vs The VS domain as data frame.
#' @param sampling_scheme The PK sampling scheme as data frame.
#'
#' @return The PC domain as data frame.
#' @export
make_sd_pc <- function(ex, dm, vs, sampling_scheme) {
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
    mutate(PERIOD=1) %>%
    mutate(EXDY=1) %>%
    mutate(FOOD=0) %>%
    left_join(dm %>% select(USUBJID, RFSTDTC), by="USUBJID") %>%
    left_join(ex %>% distinct(USUBJID, EXDOSE), by="USUBJID") %>%
    as.data.frame()

  sim <- pk_sim1(temp, sampling_scheme)

  pc <- sim %>%
    dplyr::select(id, time, c_centr, c_metab, PERIOD) %>%
    mutate(RS2023=c_centr, RS2023487A=c_metab) %>%
    pivot_longer(c("RS2023", "RS2023487A"), names_to="PCTESTCD",
                 values_to="PCSTRESN") %>%
    mutate(PCSTRESN=round(PCSTRESN, 3)) %>%
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
    mutate(EPOCH="OPEN LABEL TREATMENT") %>%
    dplyr::select(-id, -time, -c_centr, - c_metab, -RFSTDTC) %>%
    as.data.frame()
  return(pc)
}





#' Reformat date to SDTM conventions
#'
#' @param x The date in POSIX format
#'
#' @return Date-time formatted as xxxx-xx-xxTxx:xx:xx
#' @export
reformat_date <- function(x) {
  return(format(x, "%Y-%m-%dT%H:%M"))
}


#' Synthesize SDTM data for a fictional food effect study
#'
#' @return The SDTM data as sdtm object.
synthesize_sdtm_food_effect_study <- function() {
  basic_sampling_scheme <- data.frame(
    time = c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 24, 48, 72, 96, 144, 168),
    PCTPT = c("PREDOSE", "HOUR 0.5", "HOUR 1", "HOUR 1.5", "HOUR 2", "HOUR 3",
              "HOUR 4", "HOUR 6", "HOUR 8", "HOUR 10", "HOUR 12", "HOUR 24",
              "HOUR 48", "HOUR 72", "HOUR 96", "HOUR 144", "HOUR 168"))

  dm <- make_dm(studyid="2023000400", nsubs=20)

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

  vs <- make_vs(dm)
  ex <- make_sd_ex(dm, drug="RS2023")
  # pc <- make_pc(ex, dm, vs, basic_sampling_scheme)
  pc <- make_fe_pc(ex, dm, vs, basic_sampling_scheme)

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

  temp <- new_sdtm(out) %>%
    add_analyte_mapping("EXAMPLINIB", "RS2023")
  return(temp)
}

#' #' Synthesize the package data for `examplinib_fe_sdtm`
#' #'
#' #' @return The sdtm object.
#' make_examplinib_food_effect_sdtm <- function() {
#'   examplinib <- synthesize_sdtm() %>%
#'     add_analyte_mapping("EXAMPLINIB", "RS2023")
#'   return(examplinib)
#' }

#' Synthesize the package data for `examplinib_fe_nif`
#'
#' @return The nif object.
make_examplinib_food_effect_nif <- function() {
  out <- synthesize_sdtm_food_effect_study() %>%
    add_analyte_mapping("EXAMPLINIB", "RS2023") %>%
    make_nif() %>%
    mutate(PERIOD=str_sub(EPOCH, -1, -1)) %>%
    mutate(TREATMENT=str_sub(ACTARMCD, PERIOD, PERIOD)) %>%
    mutate(FASTED=case_when(TREATMENT=="A" ~ 1, .default=0))
  return(out)
}



#### Development note: To update the package data `examplinib` during the
#### package development, run the following code:
####
#### `examplinib_fe_sdtm <- synthesize_sdtm_food_effect_study()`
#### `use_data(examplinib_fe_sdtm, overwrite=T)`
#### `examplinib_fe_nif <- make_examplinib_food_effect_nif()`
#### `use_data(make_examplinib_fe_nif, overwrite=T)`



#' 3 + 3 dose escalation
#'
#' @return A sdtm object.
#' @export
synthesize_sdtm_sad_study <- function() {
  rich_sampling_scheme <- data.frame(
    time = c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 24, 48, 72, 96, 144, 168),
    PCTPT = c("PREDOSE", "HOUR 0.5", "HOUR 1", "HOUR 1.5", "HOUR 2", "HOUR 3",
              "HOUR 4", "HOUR 6", "HOUR 8", "HOUR 10", "HOUR 12", "HOUR 24",
              "HOUR 48", "HOUR 72", "HOUR 96", "HOUR 144", "HOUR 168"))

  dose_levels <- data.frame(
    dose = c(5, 10, 20, 50, 100, 200, 500, 800, 1000, 500),
    n = c(3, 3, 3, 3, 6, 3, 6, 6, 3, 12)) %>%
    mutate(cohort = row_number()) %>%
    group_by(cohort, dose) %>%
    expand(i=seq(n)) %>%
    select(-i) %>%
    ungroup()

  dm <- make_dm(studyid="2023000001", nsubs=nrow(dose_levels), nsites=1,
                female_fraction=0, duration=10)

  sb_assignment <- dose_levels %>%
    mutate(USUBJID=dm %>%
             filter(ACTARMCD!="SCRNFAIL") %>%
             arrange(USUBJID) %>%
             pull(USUBJID))

  dm <- dm %>%
    left_join(sb_assignment, by="USUBJID") %>%
    mutate(ACTARMCD=case_match(ACTARMCD, ""~ paste0("C", cohort),
                               .default="SCRNFAIL")) %>%
    mutate(ACTARM=case_match(ACTARMCD, "SCRNFAIL"~"Screen Failure",
                             .default=paste0("Treatment cohort ", cohort, ", ",
                                             dose, " mg examplinib"))) %>%
    mutate(ARM=ACTARM, ARMCD=ACTARMCD)

  vs <- make_vs(dm)

  ex <- make_sd_ex(dm, drug="RS2023", admindays=1, dose=5) %>%
    select(-EXDOSE) %>%
    left_join(sb_assignment %>% distinct(USUBJID, EXDOSE=dose), by="USUBJID")
  pc <- make_sd_pc(ex, dm, vs, rich_sampling_scheme)

  out <- list()
  out[["dm"]] <- dm %>%
    mutate(RFICDTC=reformat_date(RFICDTC)) %>%
    mutate(RFSTDTC=reformat_date(RFSTDTC)) %>%
    select(-c("cohort", "dose"))

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

  temp <- new_sdtm(out) %>%
    add_analyte_mapping("EXAMPLINIB", "RS2023")
  return(temp)
}


make_examplinib_sad_nif <- function() {
  out <- synthesize_sdtm_sad_study() %>%
    add_analyte_mapping("EXAMPLINIB", "RS2023") %>%
    make_nif()
  return(out)
}





#' MD study
#'
#' @return A stdm object.
#' @export
synthesize_sdtm_poc_study <- function() {
  dose <- 500
  nrich <- 12
  nsubs <- 80
  nsites <- 6
  rich_sampling_scheme <- data.frame(
    NTIME = c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12),
    PCTPT = c("PREDOSE", "HOUR 0.5", "HOUR 1", "HOUR 1.5", "HOUR 2", "HOUR 3",
              "HOUR 4", "HOUR 6", "HOUR 8", "HOUR 10", "HOUR 12"))

  sparse_sampling_scheme <- data.frame(
    NTIME = c(0, 1.5, 4),
    PCTPT = c("PRE", "1.5 H POST", "4 H POST")
  )

  dm <- make_dm(studyid="2023000022", nsubs=nsubs, nsites=nsites,
                female_fraction=0.4, duration=30, min_age=52, max_age=84) %>%
    mutate(ACTARMCD=case_match(ACTARMCD, ""~"TREATMENT",
                               .default=ACTARMCD)) %>%
    mutate(ACTARM=case_match(ACTARMCD,
                             "SCRNFAIL"~"Screen Faillure",
                             "TREATMENT"~"Single Arm Treatment")) %>%
    mutate(ARM=ACTARM, ARMCD=ACTARMCD)
  vs <- make_vs(dm)
  ex <- make_md_ex(dm, drug="RS2023", dose=500, missed_doses = T) %>%
    as.data.frame()


  #######################
  # to do
  #
  # omit time information randomly?
  # make md PC based on sparse sampling (run in with rich?)
  #

  sbs <- dm %>%
    filter(ACTARMCD!="SCRNFAIL") %>%
    left_join(vs %>%
                select(USUBJID, VSTESTCD, VSSTRESN) %>%
                group_by(USUBJID) %>%
                pivot_wider(names_from="VSTESTCD", values_from="VSSTRESN"),
              by="USUBJID") %>%
    select(USUBJID, SEX, AGE, HEIGHT, WEIGHT) %>%
    mutate(ID=row_number()) %>%
    mutate(rich=ID<=nrich)

  admin <- ex %>%
    group_by(USUBJID) %>%
    mutate(first_dtc=EXSTDTC[EXSEQ==1]) %>%
    ungroup() %>%
    group_by(USUBJID, EXSEQ, first_dtc) %>%
    expand(dtc=seq(EXSTDTC, EXENDTC, by="1 day")) %>%
    ungroup() %>%
    mutate(TIME=as.numeric(dtc-first_dtc)/3600) %>%
    left_join(sbs, by="USUBJID") %>%
    select(id=ID, USUBJID, time=TIME, SEX, AGE, HEIGHT, WEIGHT) %>%
    mutate(cmt=0, amt=dose, rate=-2, evid=1, NTIME=0)

  temp <- admin %>%
    # day 8
    group_by(USUBJID) %>%
    filter(time>=8*24) %>%
    summarize(ref_time=min(time), .groups="drop") %>%
    # day 1
    add_row(admin %>%
              distinct(USUBJID) %>%
              mutate(ref_time=0)) %>%
    left_join(sbs %>% distinct(USUBJID, rich), by="USUBJID")

  sampling <- rbind(
    temp %>%
      filter(rich==T) %>%
      group_by_all() %>%
      expand(rich_sampling_scheme) %>%
      ungroup(),
    temp %>%
      filter(rich==F) %>%
      group_by_all() %>%
      expand(sparse_sampling_scheme) %>%
      ungroup()
  ) %>%
    mutate(time=ref_time + NTIME) %>%
    arrange(USUBJID, time) %>%

    # time1: time since first dose with random variations in the sampling
    #   time points:
    group_by(NTIME==0) %>%
    mutate(delta=case_when(NTIME==0~runif(n(), -1, -0.1),
                           .default=rnorm(n(), 0, .02)*NTIME)) %>%
    ungroup() %>%
    mutate(time1=time+delta) %>%
    left_join(sbs, by="USUBJID") %>%
    select(id=ID, USUBJID, time=time1, SEX, AGE, HEIGHT, WEIGHT, NTIME) %>%
    mutate(cmt=1, amt=0, rate=0, evid=0)

  ev <- rbind(admin, sampling) %>%
    arrange(id, time) %>%
    mutate(FOOD=1) %>%
    as.data.frame()

  pc <- pk_sim(ev) %>%
    select(id, time, RS2023=c_centr, RS2023487A=c_metab, NTIME) %>%
    pivot_longer(c("RS2023", "RS2023487A"), names_to="PCTESTCD", values_to="PCSTRESN") %>%
    mutate(PCTEST=case_when(PCTESTCD=="RS2023" ~ "RS2023",
                            PCTESTCD=="RS2023487A" ~"RS2023487A")) %>%
    left_join(sbs %>% distinct(ID, USUBJID), by=c("id"="ID")) %>%
    arrange(USUBJID, time) %>%
    group_by(USUBJID) %>%
    mutate(delta_time=time-time[row_number()==1]) %>%
    ungroup() %>%
    left_join(dm %>%
                select(USUBJID, RFSTDTC, STUDYID),
              by="USUBJID") %>%
    mutate(PCDTC=RFSTDTC + delta_time*3600) %>%
    mutate(DOMAIN="PC", PCSPEC="PLASMA", EPOCH="TREATMENT") %>%
    mutate(PCTPT=case_when(NTIME==0~"PREDOSE",
                           .default=paste0("POSTDOSE ", NTIME, " H"))) %>%
    mutate(PCTPTNUM=NTIME) %>%
    mutate(PCRFTDTC=RFSTDTC) %>%
    select(-c(time, NTIME, delta_time, id)) %>%
    arrange(USUBJID, PCDTC) %>%
    group_by(USUBJID) %>%
    mutate(PCSEQ=row_number()) %>%
    ungroup() %>%
    as.data.frame()

  out <- list()
  out[["dm"]] <- dm %>%
    mutate(RFICDTC=reformat_date(RFICDTC)) %>%
    mutate(RFSTDTC=reformat_date(RFSTDTC))

  out[["vs"]] <- vs %>%
    mutate(RFSTDTC=reformat_date(RFSTDTC))

  out[["ex"]] <- ex %>%
    mutate(EXSTDTC=reformat_date(EXSTDTC)) %>%
    mutate(EXENDTC=reformat_date(EXENDTC)) %>%
    mutate(EXTRT="EXAMPLINIB")

  out[["pc"]] <- pc %>%
    mutate(PCDTC=reformat_date(PCDTC)) %>%
    mutate(PCRFTDTC=reformat_date(PCRFTDTC))

  out <- new_sdtm(out) %>%
    add_analyte_mapping("EXAMPLINIB", "RS2023") %>%
    add_metabolite_mapping("RS2023", "RS2023487A")

  return(out)
}





synthesize_examplinib <- function() {
  examplinib_sad <- synthesize_sdtm_sad_study()
  examplinib_poc <- synthesize_sdtm_poc_study()
  examplinib_fe <- synthesize_sdtm_food_effect_study()
  use_data(examplinib_sad, overwrite=T)
  use_data(examplinib_poc, overwrite=T)
  use_data(examplinib_fe, overwrite=T)
}







