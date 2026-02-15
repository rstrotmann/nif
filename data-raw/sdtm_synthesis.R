#' Simulate PK based on the examplinib PopPK model
#'
#' @param event_table The event table as required by RxODE.
#' @return PK simulation as data frame
#' @keywords internal
pk_sim <- function(event_table) {
  if (!("EGFR" %in% colnames(event_table))) {
    event_table <- event_table %>%
      mutate(EGFR = 120)
  }

  keep_columns <- event_table %>%
    mutate(time = as.numeric(time)) %>%
    mutate(NTIME = as.numeric(NTIME)) %>%
    select(any_of(c("id", "time", "NTIME", "PERIOD", "pcrft"))) %>%
    distinct()

  mod <- rxode2::rxode2({
    v_centr <- t_v_centr * (WEIGHT / 70)^1

    c_centr <- centr / v_centr * (1 + centr.err)
    c_peri <- peri / v_peri
    c_metab <- metab / v_centr

    ka <- (t_ka + FOOD * t_ka_f) * exp(eta_ka)
    d1 <- t_d1 + FOOD * t_d1_f
    f <- t_f * exp(eta_f) + FOOD * t_f_f * exp(eta_f_f)
    ke <- t_ke * exp(eta_ke) * (EGFR / 120)^1.2 # renal clearance
    cl_par <- t_cl_par * (WEIGHT / 70)^0.75 * exp(eta_cl_par) # clearance of parent to metabolite
    cl_met <- t_cl_met * (WEIGHT / 70)^0.75 * exp(eta_cl_met) # clearance of metabolite
    q <- t_q * exp(eta_q)

    d / dt(depot) <- -ka * depot
    dur(depot) <- d1
    d / dt(centr) <- ka * depot * f - ke * centr - q * centr + q * peri - cl_par * centr
    d / dt(peri) <- q * centr - q * peri
    d / dt(metab) <- cl_par * centr - cl_met * metab
    d / dt(renal) <- ke * centr
    d / dt(auc) <- centr
    d / dt(auc_metab) <- metab
  })

  theta <- c(
    t_d1 = 0.01,
    t_d1_f = 1, # food effect on D1
    t_f = 0.5,
    t_f_f = 0.2, # food effect on F
    t_ka = 0.6,
    t_ka_f = -0.35, # food effect on Ka
    t_ke = 5 / 10,
    t_cl_par = 10 / 10,
    t_q = 10 / 100,
    t_cl_met = 10 / 10,
    t_v_centr = 10,

    # v_centr = 10,
    v_peri = 100
  )

  omega <- rxode2::lotri(
    eta_ka + eta_ke + eta_f + eta_f_f + eta_q + eta_cl_par + eta_cl_met ~ c(
      0.1^2,
      0, 0.1^2,
      0, 0, 0.1^2,
      0, 0, 0, 0.1^2,
      0, 0, 0, 0, 0.1^2,
      0, 0, 0, 0, 0, 0.1^2,
      0, 0, 0, 0, 0, 0, 0.1^2
    )
  )

  sigma <- rxode2::lotri(centr.err ~ .03^2)

  mod$solve(theta, event_table,
    omega = omega, sigma = sigma,
    keep = c("NTIME", "EGFR", "WEIGHT", "AGE")
  ) %>%
    as.data.frame()

  mod$solve(theta, event_table,
    omega = omega, sigma = sigma # ,
    # keep = c("NTIME", "EGFR", "WEIGHT", "AGE", "PERIOD")
  ) %>%
    as.data.frame() %>%
    left_join(keep_columns, by = c("id", "time"))
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
#' @return The disposition data for the simulated subjects as data frame.
#' @keywords internal
synthesize_subjects <- function(studyid = "2023001", nsubs = 10, nsites = 4,
                                screenfailure_rate = 0.25,
                                start_date = "2000-12-20 10:00") {
  current_date <- lubridate::as_datetime(start_date, format = "%Y-%m-%d %H:%M")
  site_names <- 100 + seq(1:nsites)
  sbs <- data.frame(
    SITEID = "", SUBJID = "", ACTARM = "", ACTARMCD = "", RFICDTC = NA,
    RFSTDTC = NA, RFXSTDTC = NA
  )
  sbs_by_site <- rep(0, nsites)

  while (nrow(sbs %>% filter(ACTARMCD != "SCRNFAIL")) < nsubs + 1) {
    current_site <- round(runif(1, 1, nsites), 0)
    current_sb_no <- sbs_by_site[current_site] + 1
    current_date <- current_date + floor(abs(rnorm(1, 0.5, 2))) * 60 * 60 * 24
    siteid <- as.character(site_names[current_site])
    enrolled <- runif(1) > screenfailure_rate
    sbs_by_site[current_site] <- sbs_by_site[current_site] + 1
    sbs <- sbs %>%
      add_row(
        SITEID = siteid,
        SUBJID = paste0(
          siteid,
          as.character(formatC(current_sb_no, width = 4, flag = "0"))
        ),
        ACTARMCD = case_when(!enrolled ~ "SCRNFAIL", .default = ""),
        ACTARM = case_when(!enrolled ~ "Screen Failure", .default = ""),
        RFICDTC = current_date + rnorm(1, 0, 1) * 60 * 60,
        RFSTDTC = case_when(
          enrolled ~ RFICDTC + floor(rnorm(1, 10, 2)) * 60 * 60 * 24,
          .default = NA
        ),
        RFXSTDTC = RFSTDTC
      )
  }
  return(sbs[-1, ])
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
#' @return The DM data as data frame.
#' @import dplyr
#' @keywords internal
synthesize_dm <- function(studyid = "2023001", nsubs = 10, nsites = 5,
                          duration = 7, female_fraction = 0.5, min_age = 18,
                          max_age = 55) {
  sbs <- synthesize_subjects(studyid, nsubs, nsites)
  dm <- sbs %>%
    group_by_all() %>%
    mutate(
      STUDYID = as.character(studyid),
      USUBJID = paste0(.data$STUDYID, .data$SUBJID),
      SEX = case_when(runif(1) > female_fraction ~ "M", .default = "F"),
      AGE = round(runif(1, min_age, max_age), 0),
      AGEU = "YEARS",
      COUNTRY = "DEU",
      DOMAIN = "DM",
      ARM = .data$ACTARM,
      ARMCD = .data$ACTARMCD,
      RACE = cut(runif(1),
        breaks = c(0, .05, .2, 1),
        labels = c("ASIAN", "BLACK OR AFRICAN AMERICAN", "WHITE")
      ),
      ETHNIC = ""
    ) %>%
    as.data.frame()
  return(dm)
}


# Required: STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST
# Expected: VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU, VSBLFL, VISITNUM, VSDTC

# height 179, 6.81
# weigth 78.5 10.2

#' Synthesize fictional baseline vital sign data (VS domain)
#'
#' @param dm The DM domain to provide the subject data.
#' @return The VS domain data as data frame.
#' @keywords internal
synthesize_vs <- function(dm) {
  vs <- dm %>%
    dplyr::select(c("STUDYID", "USUBJID", "RFSTDTC")) %>%
    group_by_all() %>%
    tidyr::expand(VSTESTCD = c("HEIGHT", "WEIGHT")) %>%
    mutate(VSORRES = case_when(
      .data$VSTESTCD == "HEIGHT" ~ rnorm(1, 179, 6.81),
      .data$VSTESTCD == "WEIGHT" ~ rnorm(1, 78.5, 10.2)
    )) %>%
    mutate(VSTEST = case_when(
      .data$VSTESTCD == "HEIGHT" ~ "Height",
      .data$VSTESTCD == "WEIGHT" ~ "Weight"
    )) %>%
    mutate(VSORRESU = case_when(
      .data$VSTESTCD == "HEIGHT" ~ "cm",
      .data$VSTESTCD == "WEIGHT" ~ "kg"
    )) %>%
    mutate(VSSTRESN = round(.data$VSORRES, digits = 1)) %>%
    mutate(
      VSSTRESU = .data$VSORRESU, EPOCH = "SCREENING", DOMAIN = "VS",
      VSBLFL = ""
    ) %>%
    ungroup() %>%
    mutate(VISIT = "SCREENING") %>%
    mutate(VSBLFL = "Y") %>%
    mutate(VSDTC = .data$RFSTDTC) %>%
    select(-c("RFSTDTC")) %>%
    group_by(.data$USUBJID) %>%
    mutate(VSSEQ = row_number()) %>%
    as.data.frame()
  return(vs)
}


#' Syntesize TS domain for fictional study
#'
#' @param studyid The studyid as character.
#' @param title The study title as character.
#' @param type The study type as character. Multiple values allowed.
#' @param phase The study phase as character.
#' @param hv Healthy Volunteer flag as logical.
#' @param startdate Study start date as character.
#' @param enddate Study end date as character.
#'
#' @returns A data frame.
#' @keywords internal
synthesize_ts <- function(
  studyid, title, type, phase,
  startdate = "", enddate = "",
  hv = TRUE
) {
  ttype <- tibble::tribble(
    ~TSVALCD, ~TSVAL, ~typeval,
    "C98729", "FOOD EFFECT", "fe",
    "C49666", "EFFICACY", "eff",
    "C49664", "BIO-AVAILABILITY", "ba",
    "C49665", "BIO-EQUIVALENCE", "be",
    "C158289", "DOSE FINDING", "df",
    "C158286", "DRUG-DRUG INTERACTION", "ddi",
    "C178057", "ECG", "ecg",
    "C49663", "PHARMACOKINETIC", "pk",
    "C49667", "SAFETY", "saf",
    "C98791", "TOLERABILITY", "tol"
  )

  tphase <- tibble::tribble(
    ~NCI, ~TPHASE, ~phase,
    "C15600", "PHASE I TRIAL", "1",
    "C49686", "PHASE IIA TRIAL", "2a"
  )

  yesno_valcd <- function(val) {
    ifelse(val == TRUE, "C49488", "C49487")
  }

  yesno_val <- function(val) {
    ifelse(val == TRUE, "Y", "N")
  }

  out <- tibble::tribble(
    ~TSPARMCD, ~TSPARM, ~TSVAL, ~TSVALCD, ~TSVCDREF,
    "TITLE", "Trial Title",
    title, "C49802", "CDISC",
    "SDTMVER", "SDTM Version",
    "1.4", "C161428", "CDISC",
    "SSTDTC", "Study Start Date",
    startdate, "C69208", "ISO 8601",
    "SENDTC", "Study End Date",
    enddate, "C90462", "ISO 8601",
    "TPHASE", "Trial Phase Classification",
    as.character(tphase[tphase$phase == phase, "TPHASE"]),
    as.character(tphase[tphase$phase == phase, "NCI"]), "CDISC",
    "HLTSUBJI", "Healthy Subject Indicator",
    yesno_val(hv), yesno_valcd(hv), "CDISC"
  ) %>%
    rbind(
      ttype %>% filter(typeval %in% type) %>%
        mutate(TSPARMCD = "TTYPE", TSPARM = "Trial Type") %>%
        mutate(TSVCDREF = "CDISC") %>%
        select(-typeval)
    ) %>%
    mutate(TSVCDVER = case_when(
      TSVCDREF == "CDISC" ~ "2019-06-28",
      .default = ""
    )) %>%
    mutate(STUDYID = studyid) %>%
    mutate(DOMAIN = "TS") %>%
    mutate(TSSEQ = row_number(), .by = TSPARMCD) %>%
    select(STUDYID, DOMAIN, TSSEQ, TSPARMCD, TSPARM, TSVAL, TSVALCD, TSVCDREF, TSVCDVER)

  return(out)
}


#' Synthesize a fictional EX domain for single dose administration
#'
#' @param dm The DM including the subject info for whom to synthesize EX.
#' @param admindays The treatment days as vector.
#' @param drug The name of the drug to be administered.
#' @param dose The dose.
#' @return EX data as data frame.
#' @keywords internal
make_sd_ex <- function(dm, admindays = c(1, 14), drug = "RS2023", dose = 500) {
  ex <- dm %>%
    filter(.data$ACTARMCD != "SCRNFAIL") %>%
    dplyr::select(c("STUDYID", "USUBJID", "RFSTDTC")) %>%
    mutate(DOMAIN = "EX") %>%
    group_by_all() %>%
    tidyr::expand(EXDY = admindays) %>%
    ungroup() %>%
    mutate(EXSTDTC = .data$RFSTDTC + (.data$EXDY - 1) * 60 * 60 * 24) %>%
    mutate(EXSTDY = .data$EXDY, EXENDY = .data$EXDY) %>%
    mutate(EXENDTC = .data$EXSTDTC, EXTRT = drug, EXDOSE = dose) %>%
    mutate(EXROUTE = "ORAL", EXDOSFRM = "TABLET") %>%
    arrange(.data$USUBJID, .data$EXSTDTC) %>%
    group_by(.data$USUBJID) %>%
    mutate(EXSEQ = row_number()) %>%
    ungroup() %>%
    mutate(EPOCH = case_when(
      EXSEQ == 1 ~ "OPEN LABEL TREATMENT 1",
      EXSEQ == 2 ~ "OPEN LABEL TREATMENT 2"
    )) %>%
    select(-"RFSTDTC") %>%
    as.data.frame()

  return(ex)
}


#' Create administration schedule with randomly missed doses
#'
#' @param start_dtc Starting DTC.
#' @param missed_prob Probability of missing administration.
#' @param end_dtc End DTC.
#' @param dose The normal dose.
#' @param dose_red The reduced dose.
#' @param red_prob The probability that a subject has a dose reduction. Dose
#'   reductions, if any, occur at a random day after day 7.
#' @return EXSTDTC and EXENDTC as data frame.
#' @keywords internal
miss_admins <- function(start_dtc, end_dtc, dose = 500, dose_red = 250,
                        missed_prob = 0.15, red_prob = 0.3) {
  # create missed administration days
  dose_reduction <- red_prob != 0
  treatment_duration <- as.numeric(end_dtc - start_dtc) + 1
  n <- floor(treatment_duration * runif(1, 0, missed_prob))
  admins <- data.frame(
    day = seq(1, treatment_duration),
    dtc = seq(start_dtc, end_dtc, by = "1 day"),
    dose = dose
  )
  missed_days <- sort(unique(floor(runif(n, 2, treatment_duration))))
  admins[missed_days, "dtc"] <- NA

  if (dose_reduction) {
    if (runif(1, 0, 1) < red_prob) {
      red_start_dy <- floor(runif(1, 7, treatment_duration))
      red_days <- seq(red_start_dy, treatment_duration)
      admins[red_days, "dose"] <- dose_red
    }
  }

  # to do:
  # change times slightly
  # omit times occasionally

  admins %>%
    mutate(prev_dose = lag(dose)) %>%
    mutate(dose_red_start = dose != prev_dose) %>%
    mutate(dose_restart = lag(is.na(dtc))) %>%
    filter(!is.na(dtc)) %>%
    mutate(block = dose_red_start == T | dose_restart == T | row_number() == 1) %>%
    group_by(block) %>%
    mutate(block_id = case_when(block == 1 ~ row_number(),
      .default = NA
    )) %>%
    ungroup() %>%
    as.data.frame() %>%
    tidyr::fill("block_id", .direction = "down") %>%
    group_by(block_id) %>%
    mutate(EXSTDTC = dtc[1], EXENDTC = dtc[n()]) %>%
    ungroup() %>%
    as.data.frame() %>%
    distinct(EXSTDTC, EXENDTC, DOSE = dose)
}


#' Synthesize a fictional EX domain for single dose administration
#'
#' @param dm The DM including the subject info for whom to synthesize EX.
#' @param drug The name of the drug to be administered.
#' @param dose The dose.
#' @param treatment_duration The treatment duration in days.
#' @param missed_prob Probability to miss doses.
#' @param red_prob The dose reduction probability.
#' @param missed_doses Switch whether to include randomly missed doses as boolean.
#'
#' @return The EX domain as data frame.
#' @keywords internal
make_md_ex <- function(dm,
                       drug = "RS2023",
                       dose = 500,
                       treatment_duration = 50,
                       missed_prob = 0.15,
                       missed_doses = T,
                       red_prob = 0.3) {
  ex <- dm %>%
    filter(.data$ACTARMCD != "SCRNFAIL") %>%
    select(c("STUDYID", "USUBJID", "RFSTDTC")) %>%
    mutate(DOMAIN = "EX") %>%
    # make random treatment duration between 60% and 100% of the specified
    mutate(trtdur = floor(runif(
      nrow(.), treatment_duration * .6,
      treatment_duration + 1
    ))) %>%
    mutate(
      EXSTDTC = .data$RFSTDTC,
      EXENDTC = .data$RFSTDTC + .data$trtdur * 3600 * 24
    )

  # randomly introduce missed doses and dose reductions
  if (missed_doses == TRUE) {
    ex <- ex %>%
      group_by(.data$DOMAIN, .data$STUDYID, .data$USUBJID) %>%
      tidyr::expand(miss_admins(EXSTDTC, EXENDTC, red_prob = red_prob)) %>%
      ungroup() %>%
      as.data.frame()
  }

  ex <- ex %>%
    mutate(
      EXTRT = drug,
      EXDOSE = .data$DOSE,
      EPOCH = "TREATMENT",
      EXROUTE = "ORAL",
      EXDOSFRM = "TABLET"
    ) %>%
    arrange(.data$USUBJID, .data$EXSTDTC) %>%
    group_by(.data$USUBJID) %>%
    mutate(EXSEQ = row_number()) %>%
    ungroup()

  return(ex)
}


#' Create PC based on single-dose administration
#'
#' @param ex The EX domain as data frame.
#' @param dm The DM domain as data frame.
#' @param vs The VS domain as data frame.
#' @param lb The LB domain as data frame.
#' @param sampling_scheme The PK sampling scheme as data frame.
#' @return The PC domain as data frame.
#' @keywords internal
make_sd_pc <- function(dm, ex, vs, lb, sampling_scheme) {
  sbs <- subject_baseline_data(dm, vs, lb) %>%
    left_join(ex %>%
      distinct(.data$USUBJID, .data$EXDOSE) %>%
      select(c("USUBJID", "EXDOSE")), by = "USUBJID") %>%
    mutate(FOOD = 0, PERIOD = 1)

  ev <- rxode2::et(amountUnits = "mg", timeUnits = "hours") %>%
    rxode2::add.dosing(
      dose = 500, dosing.to = "depot",
      rate = -2, start.time = 0
    ) %>%
    rxode2::add.sampling(sampling_scheme$time) %>%
    rxode2::et(id = sbs$ID) %>%
    mutate(NTIME = .data$time) %>%
    left_join(
      sbs %>%
        dplyr::select(c(
          "id" = "ID", "USUBJID", "SEX", "AGE", "HEIGHT",
          "WEIGHT", "FOOD", "PERIOD", "EGFR", "EXDOSE"
        )),
      by = "id"
    ) %>%
    mutate(amt = case_when(!is.na(.data$amt) ~ .data$EXDOSE, .default = NA)) %>%
    mutate(NTIME = .data$time)

  sim <- pk_sim(ev) %>%
    left_join(sbs %>% select(c("id" = "ID", "USUBJID")), by = "id")

  pc <- sim %>%
    dplyr::select(c("USUBJID", "id", "time", "c_centr", "c_metab")) %>%
    mutate(RS2023 = .data$c_centr * 1000, RS2023487A = .data$c_metab * 1000) %>%
    tidyr::pivot_longer(c("RS2023", "RS2023487A"),
      names_to = "PCTESTCD",
      values_to = "PCSTRESN"
    ) %>%
    mutate(PCTEST = case_when(
      .data$PCTESTCD == "RS2023" ~ "RS2023",
      .data$PCTESTCD == "RS2023487A" ~ "RS2023487A"
    )) %>%
    mutate(PCSTRESN = round(.data$PCSTRESN, 4)) %>%
    left_join(
      dm %>%
        distinct(.data$USUBJID, .data$RFSTDTC),
      by = "USUBJID"
    ) %>%
    mutate(STUDYID = unique(dm$STUDYID), DOMAIN = "PC") %>%
    mutate(PCELTM = paste0("PT", as.character(time), "H")) %>%
    mutate(PCTPTNUM = .data$time) %>%
    left_join(sampling_scheme, by = "time") %>%
    mutate(PCDTC = .data$RFSTDTC + time * 60 * 60) %>%
    arrange(.data$id, .data$PCDTC, .data$PCTESTCD) %>%
    group_by(.data$id) %>%
    mutate(PCSEQ = row_number()) %>%
    ungroup() %>%
    mutate(PCSPEC = "PLASMA") %>%
    mutate(PCRFTDTC = .data$RFSTDTC) %>%
    mutate(EPOCH = "OPEN LABEL TREATMENT") %>%
    dplyr::select(-c("id", "time", "c_centr", "c_metab", "RFSTDTC"))
  return(pc)
}


#' Synthesize the PC domain for a food effect study.
#'
#' @param ex The EX domain to syntesize PC data for.
#' @param dm The DM domain to syntesize PC data for.
#' @param vs The VS domain to syntesize PC data for.
#' @param sampling_scheme The sampling scheme to be used.
#' @return PC data as data frame.
#' @keywords internal
make_fe_pc <- function(ex, dm, vs, sampling_scheme) {
  sbs <- subject_baseline_data(dm, vs, lb)

  temp <- sbs %>%
    group_by_all() %>%
    tidyr::expand(PERIOD = c(1, 2)) %>%
    ungroup() %>%
    mutate(EXDY = case_when(.data$PERIOD == 1 ~ 1, .data$PERIOD == 2 ~ 14)) %>%
    mutate(TREATMENT = str_sub(.data$ACTARMCD, .data$PERIOD, .data$PERIOD)) %>%
    mutate(FOOD = case_when(.data$TREATMENT == "B" ~ 1, .default = 0)) %>%
    mutate(EXDOSE = 500) %>%
    as.data.frame()

  ev <- rxode2::et() %>%
    rxode2::add.dosing(
      dose = 500, dosing.to = "depot",
      rate = -2, start.time = 0
    ) %>%
    rxode2::add.sampling(sampling_scheme$time) %>%
    rxode2::et(id = sbs$ID) %>%
    as.data.frame() %>%
    mutate(NTIME = .data$time) %>%
    group_by_all() %>%
    tidyr::expand(PERIOD = c(1, 2)) %>%
    mutate(time = .data$time + 13 * 24 * (.data$PERIOD - 1)) %>%
    left_join(
      temp %>%
        select(id = ID, PERIOD, FOOD, EXDOSE, AGE, WEIGHT, SEX, EGFR),
      by = c("id", "PERIOD")
    ) %>%
    as.data.frame() %>%
    mutate(amt = case_when(!is.na(.data$amt) ~ .data$EXDOSE, .default = NA)) %>%
    arrange(.data$id, .data$time, .data$evid)

  sim <- pk_sim(ev)

  pc <- sim %>%
    dplyr::select(id, time, c_centr, c_metab, PERIOD, NTIME) %>%
    mutate(RS2023 = c_centr * 1000, RS2023487A = c_metab * 1000) %>%
    tidyr::pivot_longer(c("RS2023", "RS2023487A"),
      names_to = "PCTESTCD",
      values_to = "PCSTRESN"
    ) %>%
    mutate(PCTEST = case_when(
      PCTESTCD == "RS2023" ~ "RS2023",
      PCTESTCD == "RS2023487A" ~ "RS2023487A"
    )) %>%
    left_join(temp %>% distinct(.data$ID, .data$USUBJID, .data$RFSTDTC),
      by = c("id" = "ID")
    ) %>%
    mutate(STUDYID = unique(dm$STUDYID), DOMAIN = "PC") %>%
    mutate(PCELTM = paste0("PT", as.character(.data$NTIME), "H")) %>%
    mutate(PCTPTNUM = .data$NTIME) %>%
    left_join(sampling_scheme, by = c("NTIME" = "time")) %>%
    mutate(PCDTC = .data$RFSTDTC + time * 60 * 60) %>%
    arrange(.data$id, .data$PCDTC, .data$PCTESTCD) %>%
    group_by(id) %>%
    mutate(PCSEQ = row_number()) %>%
    ungroup() %>%
    mutate(PCSPEC = "PLASMA") %>%
    mutate(PCRFTDTC = .data$RFSTDTC) %>%

    # mutate(EPOCH = case_match(
    #   .data$PERIOD, 1 ~ "OPEN LABEL TREATMENT 1",
    #   2 ~ "OPEN LABEL TREATMENT 2"
    # )) %>%

    # mutate(EPOCH = recode_values(
    #   .data$PERIOD, 1 ~ "OPEN LABEL TREATMENT 1",
    #   2 ~ "OPEN LABEL TREATMENT 2"
    # )) %>%

    mutate(EPOCH = case_when(
      .data$PERIOD == 1 ~ "OPEN LABEL TREATMENT 1",
      .data$PERIOD == 2 ~ "OPEN LABEL TREATMENT 2"
    )) %>%

    dplyr::select(-c(
      "id", "time", "c_centr", "c_metab", "RFSTDTC",
      "NTIME"
    )) %>%
    as.data.frame()
  return(pc)
}


#' Reformat date to SDTM conventions
#'
#' @param x The date in POSIX format
#' @return Date-time formatted as xxxx-xx-xxTxx:xx:xx
#' @keywords internal
reformat_date <- function(x) {
  return(format(x, "%Y-%m-%dT%H:%M"))
}


#' Title
#'
#' @param dm The DM domain as data frame.
#' @param ex The EX domain as data frame.
#' @return The updated DM domain as data frame.
#' @keywords internal
add_RFENDTC <- function(dm, ex) {
  dm %>%
    left_join(
      ex %>%
        group_by(.data$USUBJID) %>%
        mutate(RFENDTC = max(.data$EXENDTC, na.rm = T)) %>%
        distinct(.data$USUBJID, .data$RFENDTC),
      by = "USUBJID"
    )
}


#' Generate baseline subject data
#'
#' @param dm The DM domain as data.frame.
#' @param vs The VS domain as data.frame.
#' @param lb The LB domain as data.frame.
#' @return A data frame.
#' @keywords internal
subject_baseline_data <- function(dm, vs, lb) {
  baseline_vs <- vs %>%
    filter(EPOCH == "SCREENING") %>%
    dplyr::select(c("USUBJID", "VSTESTCD", "VSSTRESN")) %>%
    tidyr::pivot_wider(names_from = "VSTESTCD", values_from = "VSSTRESN")

  baseline_lb <- lb %>%
    filter(.data$LBBLFL == "Y", .data$LBTESTCD == "CREAT") %>%
    select(c("USUBJID", "BL_CREAT" = "LBSTRESN"))

  sbs <- dm %>%
    filter(.data$ACTARMCD != "SCRNFAIL") %>%
    left_join(baseline_vs, by = "USUBJID") %>%
    left_join(baseline_lb, by = "USUBJID") %>%
    mutate(EGFR = egfr_cg(.data$BL_CREAT, .data$AGE, .data$SEX, .data$RACE,
      .data$WEIGHT,
      molar = T
    )) %>%
    select(c(
      "USUBJID", "RFSTDTC", "SEX", "AGE", "HEIGHT", "WEIGHT", "EGFR",
      "ACTARMCD"
    )) %>%
    mutate(ID = row_number())
}


#' Synthesize SDTM data for a 3 + 3 dose escalation study
#'
#' @return A sdtm object.
#' @keywords internal
synthesize_sdtm_sad_study <- function() {
  studyid <- "2023000001"
  studytitle <- "An open label dose escalation study of RS2023 in healthy subjects"

  rich_sampling_scheme <- rich_sampling_scheme <- tibble::tribble(
    ~time, ~PCTPT,
    0, "PREDOSE",
    0.5, "0.5 HOURS POST-DOSE",
    1, "1 HOURS POST-DOSE",
    1.5, "1.5 HOURS POST-DOSE",
    2, "2 HOURS POST-DOSE",
    3, "3 HOURS POST-DOSE",
    4, "4 HOURS POST-DOSE",
    6, "6 HOURS POST-DOSE",
    8, "8 HOURS POST-DOSE",
    10, "10 HOURS POST-DOSE",
    12, "12 HOURS POST-DOSE",
    24, "24 HOURS POST-DOSE",
    48, "48 HOURS POST-DOSE",
    72, "72 HOURS POST-DOSE",
    96, "96 HOURS POST-DOSE",
    144, "144 HOURS POST-DOSE",
    168, "168 HOURS POST-DOSE",
  )

  dose_levels <- data.frame(
    dose = c(5, 10, 20, 50, 100, 200, 500, 800, 1000, 500),
    n = c(3, 3, 3, 3, 6, 3, 6, 6, 3, 12)
  ) %>%
    mutate(cohort = row_number()) %>%
    group_by(.data$cohort, .data$dose) %>%
    tidyr::expand(i = seq(n)) %>%
    select(-i) %>%
    ungroup()

  dm <- synthesize_dm(
    studyid = studyid, nsubs = nrow(dose_levels), nsites = 1,
    female_fraction = 0, duration = 10, min_age = 18, max_age = 55
  )

  sb_assignment <- dose_levels %>%
    mutate(USUBJID = dm %>%
      filter(.data$ACTARMCD != "SCRNFAIL") %>%
      arrange(.data$USUBJID) %>%
      pull(.data$USUBJID))

  dm <- dm %>%
    left_join(sb_assignment, by = "USUBJID") %>%

    # mutate(ACTARMCD = case_match(.data$ACTARMCD, "" ~ paste0("C", cohort),
    #   .default = "SCRNFAIL"
    # )) %>%

    # mutate(ACTARMCD = recode_values(.data$ACTARMCD, "" ~ paste0("C", cohort),
    #                              default = "SCRNFAIL"
    # )) %>%

    mutate(ACTARMCD = case_when(.data$ACTARMCD == "" ~ paste0("C", cohort),
                                    .default = "SCRNFAIL"
    )) %>%

    # mutate(ACTARM = case_match(.data$ACTARMCD, "SCRNFAIL" ~ "Screen Failure",
    #   .default = paste0(
    #     "Treatment cohort ", cohort, ", ",
    #     dose, " mg examplinib"
    #   )
    # )) %>%

    # mutate(ACTARM = recode_values(.data$ACTARMCD, "SCRNFAIL" ~ "Screen Failure",
    #                            default = paste0(
    #                              "Treatment cohort ", cohort, ", ",
    #                              dose, " mg examplinib"
    #                            )
    # )) %>%

    mutate(ACTARM = case_when(
      .data$ACTARMCD == "SCRNFAIL" ~ "Screen Failure",
      .default = paste0("Treatment cohort ", cohort, ", ", dose,
                        " mg examplinib")
    )) %>%

    mutate(ARM = .data$ACTARM, ARMCD = .data$ACTARMCD)

  vs <- synthesize_vs(dm)
  lb <- synthesize_lb(dm)

  ex <- make_sd_ex(dm, drug = "RS2023", admindays = 1, dose = NA) %>%
    select(-EXDOSE) %>%
    left_join(
      sb_assignment %>%
        distinct(.data$USUBJID, EXDOSE = .data$dose),
      by = "USUBJID"
    )

  dm <- dm %>%
    add_RFENDTC(ex)

  pc <- make_sd_pc(dm, ex, vs, lb, rich_sampling_scheme)

  ts <- synthesize_ts(studyid, studytitle, c("saf", "tol", "pk"), "1",
    startdate = format(min(dm$RFICDTC, na.rm = T)),
    enddate = format(last_dtc_data_frame(pc)),
    hv = TRUE
  )

  out <- list(
    dm = select(dm, -c("cohort", "dose")),
    vs = vs,
    ex = mutate(ex, EXTRT = "EXAMPLINIB"),
    pc = pc,
    lb = lb,
    ts = ts
  )

  sdtm(lapply(out, isofy_dates))
}


#' MD study
#'
#' @param dose The dose to be administered. Defauts to 500.
#' @param nrich The number of subjects with rich PK sampling.
#' @param nsubs The number of subjects.
#' @param nsites The number of clinical sites.
#' @param studyid The study identifyer.
#' @param red_prob The dose reduction probability.
#' @param duration The study duration in days.
#'
#' @return A stdm object.
#' @keywords internal
synthesize_sdtm_poc_study <- function(
  studyid = "2023000022",
  dose = 500,
  nrich = 12,
  nsubs = 80,
  nsites = 8,
  duration = 100,
  red_prob = 0.3
) {
  studytitle <- "An open-label single-arm Phase 2 study of examplinib in patients"

  rich_sampling_scheme <- tibble::tribble(
    ~NTIME, ~PCTPT,
    0, "PREDOSE",
    0.5, "0.5 HOURS POST-DOSE",
    1, "1 HOURS POST-DOSE",
    1.5, "1.5 HOURS POST-DOSE",
    2, "2 HOURS POST-DOSE",
    3, "3 HOURS POST-DOSE",
    4, "4 HOURS POST-DOSE",
    6, "6 HOURS POST-DOSE",
    8, "8 HOURS POST-DOSE",
    10, "10 HOURS POST-DOSE",
    12, "12 HOURS POST-DOSE",
  )

  sparse_sampling_scheme <- data.frame(
    NTIME = c(0, 1.5, 4),
    PCTPT = c("PRE", "1.5 H POST-DOSE", "4 H POST-DOSE")
  )

  dm <- synthesize_dm(
    studyid = studyid, nsubs = nsubs, nsites = nsites,
    female_fraction = 0.4, duration = duration, min_age = 47, max_age = 86
  ) %>%

    # mutate(ACTARMCD = case_match(.data$ACTARMCD, "" ~ "TREATMENT",
    #   .default = .data$ACTARMCD
    # )) %>%

    # mutate(ACTARMCD = recode_values(.data$ACTARMCD, "" ~ "TREATMENT",
    #                              default = .data$ACTARMCD
    # )) %>%

    mutate(ACTARMCD = case_when(.data$ACTARMCD == "" ~ "TREATMENT",
                               .default = .data$ACTARMCD
    )) %>%

    # mutate(ACTARM = case_match(
    #   .data$ACTARMCD,
    #   "SCRNFAIL" ~ "Screen Faillure",
    #   "TREATMENT" ~ "Single Arm Treatment"
    # )) %>%

    # mutate(ACTARM = recode_values(
    #   .data$ACTARMCD,
    #   "SCRNFAIL" ~ "Screen Faillure",
    #   "TREATMENT" ~ "Single Arm Treatment"
    # )) %>%

    mutate(ACTARM = case_when(
      .data$ACTARMCD == "SCRNFAIL" ~ "Screen Faillure",
      .data$ACTARMCD == "TREATMENT" ~ "Single Arm Treatment"
    )) %>%

    mutate(ARM = .data$ACTARM, ARMCD = .data$ACTARMCD)

  vs <- synthesize_vs(dm)
  lb <- synthesize_lb(dm)
  ex <- make_md_ex(dm,
    drug = "RS2023", dose = 500, missed_doses = T,
    treatment_duration = duration, red_prob = red_prob
  )

  dm <- dm %>%
    add_RFENDTC(ex)

  sbs <- subject_baseline_data(dm, vs, lb) %>%
    mutate(rich = .data$ID <= nrich)

  admin <- ex %>%
    group_by(.data$USUBJID) %>%
    mutate(first_dtc = .data$EXSTDTC[.data$EXSEQ == 1]) %>%
    ungroup() %>%
    group_by(.data$USUBJID, .data$EXSEQ, .data$first_dtc) %>%
    tidyr::expand(dtc = seq(.data$EXSTDTC, .data$EXENDTC, by = "1 day")) %>%
    ungroup() %>%
    mutate(TIME = as.numeric(.data$dtc - .data$first_dtc) / 3600) %>%
    left_join(sbs, by = "USUBJID") %>%
    select(c(
      "id" = "ID", "USUBJID", "time" = "TIME", "SEX", "AGE", "HEIGHT",
      "WEIGHT", "EGFR"
    )) %>%
    mutate(cmt = 0, amt = dose, rate = -2, evid = 1, NTIME = 0)

  temp <- admin %>%
    # day 8
    group_by(.data$USUBJID) %>%
    filter(.data$time >= 8 * 24) %>%
    summarize(ref_time = min(.data$time), .groups = "drop") %>%
    # day 1
    add_row(admin %>%
      distinct(.data$USUBJID) %>%
      mutate(ref_time = 0)) %>%
    left_join(sbs %>% distinct(.data$USUBJID, rich), by = "USUBJID")

  sampling <- rbind(
    temp %>%
      filter(rich == T) %>%
      group_by_all() %>%
      tidyr::expand(rich_sampling_scheme) %>%
      ungroup(),
    temp %>%
      filter(rich == F) %>%
      group_by_all() %>%
      tidyr::expand(sparse_sampling_scheme) %>%
      ungroup()
  ) %>%
    mutate(time = .data$ref_time + .data$NTIME) %>%
    arrange(.data$USUBJID, .data$time) %>%
    # time1: time since first dose with random variations in the sampling
    #   time points:
    group_by(NTIME == 0) %>%
    mutate(delta = case_when(.data$NTIME == 0 ~ runif(n(), -1, -0.1),
      .default = rnorm(n(), 0, .02) * .data$NTIME
    )) %>%
    ungroup() %>%
    mutate(time1 = .data$time + .data$delta) %>%
    left_join(sbs, by = "USUBJID") %>%
    select(c(
      "id" = "ID", "USUBJID", "time" = "time1", "SEX", "AGE", "HEIGHT",
      "WEIGHT", "EGFR", "NTIME"
    )) %>%
    mutate(cmt = 1, amt = 0, rate = 0, evid = 0)

  ev <- rbind(admin, sampling) %>%
    arrange(.data$id, .data$time) %>%
    mutate(FOOD = 1) %>%
    as.data.frame()

  pc <- pk_sim(ev) %>%
    select(c("id", "time",
      "RS2023" = "c_centr", "RS2023487A" = "c_metab",
      "NTIME"
    )) %>%
    mutate(pcrft = case_when(
      time >= 0 & time < 192 ~ 0,
      time >= 192 ~ 192,
      .default = NA
    )) |>
    mutate(
      RS2023 = .data$RS2023 * 1000,
      RS2023487A = .data$RS2023487A * 1000
    ) %>%
    tidyr::pivot_longer(c("RS2023", "RS2023487A"),
      names_to = "PCTESTCD",
      values_to = "PCSTRESN"
    ) %>%
    mutate(PCTEST = case_when(
      .data$PCTESTCD == "RS2023" ~ "RS2023",
      .data$PCTESTCD == "RS2023487A" ~ "RS2023487A"
    )) %>%
    left_join(sbs %>% distinct(.data$ID, .data$USUBJID),
      by = c("id" = "ID")
    ) %>%
    arrange(.data$USUBJID, .data$time) %>%
    group_by(.data$USUBJID) %>%
    mutate(delta_time = .data$time - .data$time[row_number() == 1]) %>%
    ungroup() %>%
    left_join(
      dm %>%
        select(c("USUBJID", "RFSTDTC", "STUDYID")),
      by = "USUBJID"
    ) %>%
    mutate(PCDTC = .data$RFSTDTC + .data$delta_time * 3600) %>%
    mutate(PCRFTDTC = .data$RFSTDTC + .data$pcrft * 3600) |>
    select(-c("pcrft")) |>
    mutate(DOMAIN = "PC", PCSPEC = "PLASMA", EPOCH = "TREATMENT") %>%
    mutate(PCTPT = case_when(.data$NTIME == 0 ~ "PREDOSE",
      .default = paste0("POSTDOSE ", .data$NTIME, " H")
    )) %>%
    mutate(PCTPTNUM = .data$NTIME) %>%
    # mutate(PCRFTDTC = .data$RFSTDTC) %>%
    select(-RFSTDTC) %>%
    mutate(PCELTM = paste0("PT", as.character(.data$NTIME), "H")) %>%
    select(-c("time", "NTIME", "delta_time", "id")) %>%
    arrange(.data$USUBJID, .data$PCDTC) %>%
    group_by(.data$USUBJID) %>%
    mutate(PCSEQ = row_number()) %>%
    ungroup() %>%
    as.data.frame()

  ts <- synthesize_ts(
    studyid, studytitle, c("saf", "tol", "pk", "eff"), "2a",
    startdate = format(min(dm$RFICDTC, na.rm = T)),
    enddate = format(last_dtc_data_frame(pc)),
    hv = FALSE
  )

  out <- list(
    dm = dm,
    vs = vs,
    ex = mutate(ex, EXTRT = "EXAMPLINIB"),
    pc = pc,
    lb = lb,
    ts = ts
  )

  sdtm(lapply(out, isofy_dates))
}


#' Synthesize SDTM data for a fictional food effect study
#'
#' @return The SDTM data as sdtm object.
#' @keywords internal
synthesize_sdtm_food_effect_study <- function() {
  studyid <- "2023000400"
  studytitle <- "An open-label 2-period crossover study in healthy subjects to investigate the effect of food on the pharmacokinetics of examplinib"

  sampling_scheme <- rich_sampling_scheme <- tibble::tribble(
    ~time, ~PCTPT,
    0, "PREDOSE",
    0.5, "0.5 HOURS POST-DOSE",
    1, "1 HOURS POST-DOSE",
    1.5, "1.5 HOURS POST-DOSE",
    2, "2 HOURS POST-DOSE",
    3, "3 HOURS POST-DOSE",
    4, "4 HOURS POST-DOSE",
    6, "6 HOURS POST-DOSE",
    8, "8 HOURS POST-DOSE",
    10, "10 HOURS POST-DOSE",
    12, "12 HOURS POST-DOSE",
    24, "24 HOURS POST-DOSE",
    48, "48 HOURS POST-DOSE",
    72, "72 HOURS POST-DOSE",
    96, "96 HOURS POST-DOSE",
    144, "144 HOURS POST-DOSE",
    168, "168 HOURS POST-DOSE",
  )

  dm <- synthesize_dm(studyid = studyid, nsubs = 20)
  i_treated <- which(dm$ACTARMCD != "SCRNFAIL")
  i_seq1 <- sample(i_treated, length(i_treated) / 2)

  dm <- dm %>%
    mutate(ACTARMCD = case_when(
      row_number() %in% i_seq1 ~ "AB",
      row_number() %in% i_treated & !(row_number() %in% i_seq1) ~ "BA",
      .default = "SCRNFAIL"
    )) %>%
    mutate(ACTARM = case_match(ACTARMCD,
      "AB" ~ "Fasted - Fed", "BA" ~ "Fed - Fasted",
      .default = "Screen Failure"
    )) %>%
    mutate(ARM = ACTARM, ARMCD = ACTARMCD)

  vs <- synthesize_vs(dm)
  lb <- synthesize_lb(dm)
  ex <- make_sd_ex(dm, drug = "RS2023")

  dm <- dm %>%
    add_RFENDTC(ex)

  pc <- make_fe_pc(ex, dm, vs, sampling_scheme)

  ts <- synthesize_ts(studyid, studytitle, c("fe", "pk"), "1",
    startdate = format(min(dm$RFICDTC, na.rm = T)),
    enddate = format(last_dtc_data_frame(pc)),
    hv = TRUE
  )

  out <- list(
    dm = dm,
    vs = vs,
    ex = mutate(ex, EXTRT = "EXAMPLINIB"),
    pc = pc,
    lb = lb,
    ts = ts
  )

  sdtm(lapply(out, isofy_dates))
}


#' Synthesize baseline serum creatinine for fictional subjects
#'
#' @details This function makes use of empirical eGFR data from a study in
#' non-diseased Caucasian subjects as published by Wetzels et
#' al. <https://doi.org/10.1038/sj.ki.5002374>, Table 1. In this study, eGFR
#' were calculated by the Modification of Diet in Renal Disease (MDRD) method.
#'
#' The tabulated data is modeled using a generalized linear model based on age
#' and sex. The model is used to predict the target eGFR (`target_egfr`) for the
#' subjects in DM. An actual eGFR (`egfr`) is then generated for each subjects
#' as a random sample from a normal distribution with the target eGFR as mean
#' and a standard deviation of 13 (i.e., the mean SD in the empirical data).
#'
#' A corresponding baseline creatinine concentration is then calculated from the
#' actual eGFR using the method specified in `crea_method`.
#'
#' @param dm The DM domain as data frame.
#' @param crea_method The creatinine calculation function as function reference.
#'   Can currently be `crea_mdrd` or `crea_raynaud`.
#' @importFrom stats glm
#' @importFrom stats predict
#' @return A DM domain with additional fields as data frame.
#' @keywords internal
synthesize_crea <- function(dm, crea_method = crea_mdrd) {
  empirical_egfr <- tribble(
    # Source: <https://doi.org/10.1038/sj.ki.5002374>, Table 1.
    ~female, ~age_lo, ~age_hi, ~N, ~Mean, ~SD, ~min, ~max, ~P5, ~P25, ~P50, ~P75, ~P95,
    0, 18, 24, 94, 100, 13, 72, 137, 77, 90, 99, 109, 121,
    0, 25, 29, 96, 93, 13, 67, 125, 74, 82, 90, 102, 117,
    0, 30, 34, 118, 86, 13, 63, 133, 68, 77, 85, 93, 107,
    0, 35, 39, 125, 85, 14, 61, 118, 65, 74, 85, 95, 110,
    0, 40, 44, 143, 84, 13, 54, 124, 66, 76, 83, 92, 106,
    0, 45, 49, 160, 83, 13, 50, 123, 63, 73, 82, 91, 105,
    0, 50, 54, 143, 79, 12, 46, 120, 60, 71, 78, 87, 97,
    0, 55, 59, 158, 76, 13, 27, 118, 58, 68, 75, 84, 98,
    0, 60, 64, 149, 75, 15, 48, 199, 59, 67, 73, 83, 95,
    0, 65, 69, 154, 75, 14, 51, 165, 56, 66, 74, 82, 97,
    0, 70, 74, 102, 71, 12, 38, 102, 54, 64, 70, 79, 92,
    0, 75, 79, 112, 70, 13, 41, 110, 45, 62, 70, 79, 91,
    0, 80, 84, 73, 67, 15, 41, 129, 43, 58, 69, 77, 87,
    0, 85, NA, 33, 62, 16, 34, 101, 35, 47, 65, 72, 92,
    1, 18, 24, 187, 91, 15, 58, 186, 72, 80, 90, 99, 112,
    1, 25, 29, 159, 85, 13, 55, 140, 63, 76, 83, 93, 107,
    1, 30, 34, 171, 85, 15, 53, 153, 63, 74, 83, 93, 113,
    1, 35, 39, 193, 79, 13, 55, 165, 63, 72, 76, 85, 102,
    1, 40, 44, 195, 77, 12, 48, 117, 58, 67, 77, 84, 100,
    1, 45, 49, 227, 74, 10, 47, 109, 56, 67, 74, 81, 91,
    1, 50, 54, 191, 73, 13, 51, 152, 56, 64, 71, 79, 93,
    1, 55, 59, 174, 70, 12, 48, 149, 53, 63, 69, 76, 89,
    1, 60, 64, 180, 68, 12, 41, 148, 50, 61, 68, 75, 84,
    1, 65, 69, 156, 66, 10, 44, 102, 52, 60, 65, 71, 85,
    1, 70, 74, 95, 66, 11, 40, 96, 49, 58, 64, 73, 85,
    1, 75, 79, 77, 62, 11, 37, 100, 45, 54, 61, 69, 82,
    1, 80, 84, 40, 64, 14, 46, 114, 46, 56, 62, 73, 88,
    1, 85, NA, 27, 59, 14, 30, 87, 36, 48, 61, 69, 78
  ) %>%
    mutate(AGE = age_lo + (age_hi - age_lo) / 2) %>%
    mutate(EGFR = Mean)

  m <- stats::glm(EGFR ~ AGE + female,
    family = "gaussian",
    data = empirical_egfr
  )
  dm <- dm %>%
    mutate(target_egfr = stats::predict(m, dm %>%
      mutate(female = case_when(SEX == "F" ~ 1, .default = 0))))
  renal <- rnorm(nrow(dm), dm$target_egfr, 13)
  dm %>%
    mutate(EGFR = renal) %>%
    mutate(CREA = crea_method(EGFR, AGE, SEX, RACE))
}


#' Synthesize baseline LB data
#'
#' @param dm The DM domain as data frame.
#' @return The LB domain as data frame.
#' @keywords internal
synthesize_lb <- function(dm) {
  dm %>%
    synthesize_crea() %>%
    select(c("STUDYID", "USUBJID", "DOMAIN", "RFSTDTC", "CREA")) %>%
    mutate(
      DOMAIN = "LB",
      LBSEQ = 1,
      LBCAT = "BIOCHEMISTRY",
      LBSPEC = "SERUM",
      VISITNUM = 1,
      LBBLFL = "Y",
      LBDTC = .data$RFSTDTC - 24 * 60 * 60,
      LBTESTCD = "CREAT",
      LBTEST = "Creatinine",
      LBORRES = .data$CREA,
      LBORRESU = "mg/dL",
      LBORNRLO = 0.67,
      LBORNRHI = 1.17,
      LBSTRESN = .data$LBORRES * 88.4,
      LBSTRESC = as.character(round(.data$LBSTRESN, 3)),
      LBSTRESU = "umol/L",
      LBSTNRLO = 59.2,
      LBSTNRHI = 103.4
    ) %>%
    select(-c("RFSTDTC", "CREA"))
}


#' Synthesize PP domain
#'
#' @param nif A nif object.
#'
#' @return The PP domain as data frame.
#' @keywords internal
synthesize_pp <- function(obj) {
  pp_translation <- tribble(
    ~PKNCAPARAM, ~PPTESTCD, ~PPTEST, ~PPORRESU, ~PPSTRESU,
    "aucinf.obs", "AUCIFP", "AUC Infinity Pred", "h*ng/mL", "h*ng/mL",
    "auclast", "AUCLST", "AUC to Last Nonzero Conc", "h*ng/mL", "h*ng/mL",
    "cmax", "CMAX", "Max Conc", "ng/mL", "ng/mL",
    "lambda.z", "LAMZ", "Lambda z", "1/h", "1/h",
    "half.life", "LAMZHL", "Half-Life Lambda z", "h", "h",
    "lambda.z.n.points", "LAMZNPT", "Number of Points for Lambda z", NA, NA,
    "adj.r.squared", "R2ADJ", "R Squared Adjusted", NA, NA,
    "tlast", "TLST", "Time of Last Nonzero Conc", "h", "h",
    "tmax", "TMAX", "Time of CMAX", "h", "h",
  )

  obj <- obj %>%
    index_rich_sampling_intervals() %>%
    # as.data.frame() %>%
    filter(!is.na(RICH_N)) # %>%
  # nif()

  analytes <- obj %>%
    as.data.frame() %>%
    distinct(ANALYTE, PARENT, METABOLITE)

  out <- data.frame()
  for (i in 1:nrow(analytes)) {
    times <- obj %>%
      filter(EVID == 1) %>%
      as.data.frame() %>%
      distinct(USUBJID, RICH_N, PPRFTDTC = DTC)

    temp <- nca(obj,
      analyte = analytes[i, "ANALYTE"],
      parent = analytes[i, "PARENT"],
      keep = c("USUBJID", "STUDYID", "ID", "DOSE"),
      group = "RICH_N"
    ) %>%
      mutate(RICH_N = as.numeric(as.character(RICH_N))) %>%
      left_join(times, by = c("USUBJID", "RICH_N"))

    out <- bind_rows(out, temp %>%
      mutate(DOMAIN = "PP", PPSTRESN = PPORRES, PPSPEC = "PLASMA") %>%
      mutate(PPSCAT = "NON-COMPARTMENTAL") %>%
      mutate(PPCAT = analytes[i, "ANALYTE"]) %>%
      rename(PKNCAPARAM = "PPTESTCD") %>%
      right_join(pp_translation, by = "PKNCAPARAM") %>%
      select(-c("PKNCAPARAM", "start", "end", "exclude", "ID")) %>%
      group_by(.data$USUBJID) %>%
      mutate(PPSEQ = row_number()) %>%
      ungroup() %>%
      as.data.frame() %>%
      select(c(
        "STUDYID", "DOMAIN", "USUBJID", "PPSEQ", "PPTESTCD", "PPTEST",
        "PPCAT", "PPORRES", "PPORRESU", "PPSTRESN", "PPSTRESU", "PPSPEC",
        "PPRFTDTC", "DOSE"
      )))
  }
  return(out)
}
