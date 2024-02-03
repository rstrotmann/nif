

# pkpd_model <- rxode2::rxode2({
#   # PK part
#   c_centr <- centr / v_centr * (1 + centr.err)
#   c_peri <- peri / v_peri
#   c_metab <- metab / v_metab
#
#   ke <- t.ke * exp(eta.ke) * (EGFR / 100)^0.9 # renal elimination constant
#   ka <- t.ka * exp(eta.ka) + FOOD * t.ka1
#   d1 <- t.d1 * exp(eta.d1)
#   fm <- t.fm * exp(eta.fm) # fraction metabolized
#
#   cl <- t.cl * exp(eta.cl) # metabolic clearance
#
#   kem <- t.kem * exp(eta.kem) # elimination constant for metabolite
#   fpar <- 1 * exp(eta.fpar) + FOOD * t.fpar1
#   q <- t.q * exp(eta.q)
#
#
#
#   d / dt(depot) <- -ka * depot * fpar
#   dur(depot) <- d1
#   d / dt(centr) <- ka * depot * fpar - ke * c_centr - q * c_centr + q * c_peri - cl * c_centr
#   d / dt(peri) <- q * c_centr - q * c_peri
#   d / dt(renal) <- ke * c_centr * (1 - fm)
#
#   d / dt(metab) <- cl * c_centr * fm - kem * c_metab
#   d / dt(metab_excr) <- kem * c_metab
#
#   # PD part
#   RBC0 = RET0*ktr2/kcir
#   PROL0 = RET0*ktr2/ktr1
#   RBC0 = RET0*ktr2/kcir
#
#   prol(0) = PROL0
#   a1(0)   = PROL0
#   a2(0)   = PROL0
#   a3(0)   = PROL0
#   a4(0)   = RET0
#   a5(0)   = RET0
#   a6(0)   = RET0
#   a7(0)   = RET0
#   a8(0)   = RBC0
#   a9(0)   = RBC0
#   a10(0)  = RBC0
#   a11(0)  = RBC0
#
#   RET = a4
#   RBC = a8
#   HGB = (a4+a5+a6+a7+a8+a9+a10+a11)*shb/1000/10  # in mg/dl
#   fb = (RET0/RET)^gamma1 * (RBC0/RBC)^gamma2
#
#   d/dt(prol) = ktr1*prol*(1-edrug)*fb - ktr1*prol
#
#   d/dt(a1) = ktr1*prol - ktr1*a1*(1-edrug)
#   d/dt(a2) = ktr1*a1*(1-edrug) - ktr1*a2*(1-edrug)
#   d/dt(a3) = ktr1*a2*(1-edrug) - ktr1*a3*(1-edrug)
#
#   d/dt(a4) = ktr1*a3*(1-edrug) - ktr2*a4
#   d/dt(a5) = ktr2*a4 - ktr2*a5
#   d/dt(a6) = ktr2*a5 - ktr2*a6
#   d/dt(a7) = ktr2*a6 - ktr2*a7
#
#   d/dt(a8) =  ktr2*a7 -  kcir*a8  - blood_loss/4
#   d/dt(a9) =  kcir*a8 -  kcir*a9  - blood_loss/4
#   d/dt(a10) = kcir*a9 -  kcir*a10 - blood_loss/4
#   d/dt(a11) = kcir*a10 - kcir*a11 - blood_loss/4
# })
#
#
# theta <- c(
#   # PK part
#   t.ka = 0.8,
#   t.ka1 = 0.8, # food effect on Ka
#   t.d1 = 1.8,
#   t.fpar1 = 2, # food effect on F
#   t.ke = 30,
#   t.q = 5,
#   t.cl = 20,
#   # t.kem = 10,
#   t.kem = 2,
#   t.fm = 0.8,
#   v_centr = 100,
#   v_peri = 100,
#   v_metab = 10,
#
#   # PD part
#   ktr1 =   0.0418,
#   ktr2 =   0.0975,
#   kcir =   0.00139,
#   gamma1 = 0.352,
#   gamma2 = 0.925,
#   shb    = 30.2,
#   RET0   = 16.3
# )
#
# omega <- rxode2::lotri(
#   eta.ke + eta.ka + eta.d1 + eta.fpar + eta.q + eta.cl + eta.kem + eta.fm ~ c(
#     0.3^2,
#     0, 0.1^2,
#     0, 0, 0.2^2,
#     0, 0, 0, 0.3^2,
#     0, 0, 0, 0, 0.3^2,
#     0, 0, 0, 0, 0, 0.4^2,
#     0, 0, 0, 0, 0, 0, 0.3^2,
#     0, 0, 0, 0, 0, 0, 0, 0.03^2
#   )
# )
#
# sigma <- rxode2::lotri(centr.err ~ .1^2)


# ev <- et(amountUnits="10^9 cells/l", timeUnits="days") %>%
#   et(0:400) %>%
#   mutate(edrug=0) %>%
#   mutate(blood_loss=0)

# rich_sampling_scheme <- data.frame(
#   NTIME = c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12),
#   PCTPT = c(
#     "PREDOSE", "HOUR 0.5", "HOUR 1", "HOUR 1.5", "HOUR 2", "HOUR 3",
#     "HOUR 4", "HOUR 6", "HOUR 8", "HOUR 10", "HOUR 12"
#   )
# )

make_md_study <- function(){
  duration <- 10
  dose <- 500

  dm <- make_dm(
    studyid = "RS2023000020", nsubs = 3, nsites = 2,
    female_fraction = 0.5, duration = duration, min_age = 47, max_age = 86
  )

  vs <- make_vs(dm)
  lb <- make_lb(dm)
  ex <- make_md_ex(dm, drug = "RS2023", dose = dose, missed_doses = TRUE,
                   treatment_duration = duration)

  dm <- dm %>%
    add_RFENDTC(ex)

  sbs <- subject_baseline_data(dm, vs, lb)

  admin <- ex %>%
    group_by(USUBJID) %>%
    mutate(first_dtc = EXSTDTC[EXSEQ == 1]) %>%
    ungroup() %>%
    group_by(USUBJID, EXSEQ, first_dtc) %>%
    expand(dtc = seq(EXSTDTC, EXENDTC, by = "1 day")) %>%
    ungroup() %>%
    mutate(TIME = as.numeric(dtc - first_dtc) / 3600) %>%
    left_join(sbs, by = "USUBJID") %>%
    select(id = ID, USUBJID, time = TIME, SEX, AGE, HEIGHT, WEIGHT, EGFR) %>%
    mutate(cmt = 0, amt = dose, rate = -2, evid = 1, NTIME = 0)



  # sim <- mod$solve(theta, event_table, omega = omega, sigma = sigma) %>%
  #   as.data.frame() %>%
  #   left_join(keep_columns, by = c("id", "time"))
}
