

pkpd_model <- rxode2::rxode2({
  # PK part
  c_centr <- centr / v_centr * (1 + centr.err)
  c_peri <- peri / v_peri
  c_metab <- metab / v_metab

  ke <- t.ke * exp(eta.ke) * (EGFR / 100)^0.9 # renal elimination constant
  ka <- t.ka * exp(eta.ka) + FOOD * t.ka1
  d1 <- t.d1 * exp(eta.d1)
  fm <- t.fm * exp(eta.fm) # fraction metabolized

  cl <- t.cl * exp(eta.cl) # metabolic clearance

  kem <- t.kem * exp(eta.kem) # elimination constant for metabolite
  fpar <- 1 * exp(eta.fpar) + FOOD * t.fpar1
  q <- t.q * exp(eta.q)

  d / dt(depot) <- -ka * depot * fpar
  dur(depot) <- d1
  d / dt(centr) <- ka * depot * fpar - ke * c_centr - q * c_centr + q * c_peri - cl * c_centr
  d / dt(peri) <- q * c_centr - q * c_peri
  d / dt(renal) <- ke * c_centr * (1 - fm)

  d / dt(metab) <- cl * c_centr * fm - kem * c_metab
  d / dt(metab_excr) <- kem * c_metab


  # PD part
  blood_loss = 0
  edrug = 0.9 #c_centr * 1.3
  RBC0 = RET0*ktr2/kcir
  PROL0 = RET0*ktr2/ktr1
  RBC0 = RET0*ktr2/kcir

  prol(0) = PROL0
  a1(0)   = PROL0
  a2(0)   = PROL0
  a3(0)   = PROL0
  a4(0)   = RET0
  a5(0)   = RET0
  a6(0)   = RET0
  a7(0)   = RET0
  a8(0)   = RBC0
  a9(0)   = RBC0
  a10(0)  = RBC0
  a11(0)  = RBC0

  RET = a4
  RBC = a8
  HGB = (a4+a5+a6+a7+a8+a9+a10+a11)*shb/1000/10  # in mg/dl
  fb = (RET0/RET)^gamma1 * (RBC0/RBC)^gamma2

  d/dt(prol) = ktr1*prol*(1-edrug)*fb - ktr1*prol

  d/dt(a1) = ktr1*prol - ktr1*a1*(1-edrug)
  d/dt(a2) = ktr1*a1*(1-edrug) - ktr1*a2*(1-edrug)
  d/dt(a3) = ktr1*a2*(1-edrug) - ktr1*a3*(1-edrug)

  d/dt(a4) = ktr1*a3*(1-edrug) - ktr2*a4
  d/dt(a5) = ktr2*a4 - ktr2*a5
  d/dt(a6) = ktr2*a5 - ktr2*a6
  d/dt(a7) = ktr2*a6 - ktr2*a7

  d/dt(a8) =  ktr2*a7 -  kcir*a8  - blood_loss/4
  d/dt(a9) =  kcir*a8 -  kcir*a9  - blood_loss/4
  d/dt(a10) = kcir*a9 -  kcir*a10 - blood_loss/4
  d/dt(a11) = kcir*a10 - kcir*a11 - blood_loss/4
})


make_md_study <- function(){
  duration <- 300
  dose <- 500
  nsubs <- 20
  sparse_sampling_scheme <- data.frame(
    NTIME = c(0, 1.5, 4),
    PCTPT = c("PRE", "1.5 H POST", "4 H POST")
  )

  theta <- c(
    # PK part
    t.ka = 0.8,
    t.ka1 = 0.8, # food effect on Ka
    t.d1 = 1.8,
    t.fpar1 = 2, # food effect on F
    t.ke = 30,
    t.q = 5,
    t.cl = 20,
    # t.kem = 10,
    t.kem = 2,
    t.fm = 0.8,
    v_centr = 100,
    v_peri = 100,
    v_metab = 10,

    # PD part
    ktr1 =   0.0418,
    ktr2 =   0.0975,
    kcir =   0.00139,
    gamma1 = 0.352,
    gamma2 = 0.925,
    shb    = 30.2,
    RET0   = 16.3
  )

  omega <- rxode2::lotri(
    eta.ke + eta.ka + eta.d1 + eta.fpar + eta.q + eta.cl + eta.kem + eta.fm ~ c(
      0.3^2,
      0, 0.1^2,
      0, 0, 0.2^2,
      0, 0, 0, 0.3^2,
      0, 0, 0, 0, 0.3^2,
      0, 0, 0, 0, 0, 0.4^2,
      0, 0, 0, 0, 0, 0, 0.3^2,
      0, 0, 0, 0, 0, 0, 0, 0.03^2
    )
  )

  sigma <- rxode2::lotri(centr.err ~ .1^2)
  sigma <- rxode2::lotri(centr.err ~ .0001^2)

  dm <- make_dm(
    studyid = "RS2023000020", nsubs = nsubs, nsites = 2,
    female_fraction = 0.5, duration = duration, min_age = 47, max_age = 86
  )

  vs <- make_vs(dm)
  lb <- make_lb(dm)
  ex <- make_md_ex(
    dm,
    drug = "RS2023",
    dose = dose,
    missed_doses = FALSE,
    treatment_duration = duration
  ) %>%
    as.data.frame()

  dm <- dm %>%
    add_RFENDTC(ex)

  sbs <- subject_baseline_data(dm, vs, lb)

  temp <- ex %>%
    mutate(RET0 = runif(nsubs, 14, 20)) %>%
    # mutate(RET0 = 16.8) %>%
    group_by(USUBJID) %>%
    mutate(first_dtc = EXSTDTC[EXSEQ == 1]) %>%
    ungroup() %>%
    group_by(USUBJID, EXSEQ, first_dtc) %>%
    expand(dtc = seq(EXSTDTC, EXENDTC, by = "1 day"), RET0) %>%
    ungroup() %>%
    mutate(TIME = as.numeric(dtc - first_dtc) / 3600) %>%
    left_join(sbs, by = "USUBJID") %>%
    select(id = ID, USUBJID, time = TIME, SEX, AGE, HEIGHT, WEIGHT, EGFR, RET0) %>%
    mutate(cmt = "depot", amt = dose, rate = -2, evid = 1, NTIME = 0, FOOD = 0) %>%
    # mutate(RET0 = 20) %>%
    as.data.frame()

  # administration is 14 days on, 7 days off
  admin <- temp %>%
    filter((time/24) %% 21 < 15)

  obs_pc_hb <- temp %>%
    # filter(time %in% c(0, 168)) %>%
    group_by_all() %>%
    # expand(sparse_sampling_scheme) %>%
    # ungroup() %>%
    # mutate(time = time + NTIME) %>%
    mutate(evid = 0, cmt = "(obs)") %>%
    # select(-PCTPT) %>%
    as.data.frame()

  ev <- rbind(admin, obs_pc_hb) %>%
    arrange(id, time, -evid)

  sim <- pkpd_model$solve(theta, ev, omega = omega, sigma = sigma) %>%
    as.data.frame()

  sim %>%
    ggplot(aes(x=time, y=c_centr, group=id)) +
    geom_line() +
    geom_line(aes(x=time, y=RBC/1000, group=id), color="red")
}
