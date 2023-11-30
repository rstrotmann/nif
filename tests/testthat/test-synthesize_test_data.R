
test_that("make_crea works", {
  make_dataset <- function(){
    dm <- make_dm(nsubs=100, min_age=18, max_age=95)
    vs <- make_vs(dm) %>%
      select(USUBJID, VSSTRESN, VSTESTCD) %>%
      pivot_wider(names_from="VSTESTCD", values_from="VSSTRESN")
    dm %>%
      make_crea() %>%
      left_join(vs, by="USUBJID") %>%
      dplyr::select(USUBJID, AGE, SEX, RACE, WEIGHT, HEIGHT, CREA) %>%
      dplyr::mutate(MDRD=egfr_mdrd(CREA, AGE, SEX, RACE)) %>%
      dplyr::mutate(RAYNAUD=egfr_raynaud(CREA, AGE, SEX, RACE)) %>%
      dplyr::mutate(CG=egfr_cg(CREA, AGE, SEX, RACE, WEIGHT))
  }

  expect_no_error(temp <- make_dataset())

  temp %>%
    ggplot(aes(x=AGE, y=MDRD, color=SEX)) +
    geom_point(size=3) +
    geom_hline(yintercept=c(30, 60, 90)) +
    theme_bw() +
    ggtitle("eGFR (MDRD) by age", subtitle="Synthetic data")

  temp %>%
    ggplot(aes(x=MDRD, y=RAYNAUD, color=RACE)) +
    geom_point(size=3) +
    geom_abline(slope=1, intercept=0) +
    theme_bw() +
    ggtitle("MDRD vs Raynoud method for eGFT by RACE", subtitle="Synthetic data")

  temp %>%
    ggplot(aes(x=MDRD, y=CG, color=SEX)) +
    geom_point(size=3) +
    geom_abline(slope=1, intercept=0) +
    theme_bw() +
    ggtitle("MDRD vs CG method for eGFT by RACE", subtitle="Synthetic data")

  temp %>%
    ggplot(aes(x=MDRD, y=RAYNAUD, color=SEX)) +
    geom_point(size=3) +
    geom_abline(slope=1, intercept=0) +
    theme_bw() +
    ggtitle("MDRD vs Raynoud method for eGFT by SEX", subtitle="Synthetic data")
})



create_subjects <- function(n=100) {
  dm <- make_dm(nsubs=n, min_age=18, max_age=90) %>%
    make_crea()
  vs <- make_vs(dm) %>%
    filter(EPOCH=="SCREENING") %>%
    dplyr::select("USUBJID", "VSTESTCD", "VSSTRESN") %>%
    pivot_wider(names_from="VSTESTCD", values_from="VSSTRESN") %>%
    as.data.frame()
  sbs <- dm %>%
    left_join(vs, by="USUBJID") %>%
    mutate(ID=row_number()) %>%
    dplyr::select(ID, SEX, AGE, HEIGHT, WEIGHT, EGFR)
  return(sbs)
}


test_that("EGFR is age-dependent", {
  expect_no_error(sbs <- create_subjects())
  sbs %>%
    ggplot(aes(x=AGE, y=EGFR)) +
    geom_point(size=3) +
    theme_bw()
})


simulate_pk <- function(n=20) {
  sbs <- create_subjects(n=n)

  sampling_scheme <- data.frame(
    time = c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 24, 48, 72, 96, 144, 168))

  ev <- rxode2::et(amountUnits="mg", timeUnits="hours") %>%
    rxode2::add.dosing(dose=500, dosing.to="depot", rate=-2, start.time=0) %>%
    rxode2::add.sampling(sampling_scheme$time) %>%
    rxode2::et(id=sbs$ID) %>%
    as.data.frame() %>%
    left_join(
      sbs %>% dplyr::select(id=ID, SEX, AGE, HEIGHT, WEIGHT, EGFR),
      by="id") %>%
    mutate(amt=500, FOOD=0, NTIME=time)

  pk_sim(ev)
}


test_that("PK model works", {
  expect_no_error(
    pk <- simulate_pk(n=20) %>%
      pivot_longer(cols=c("centr", "peri", "metab", "renal", "metab_excr"),
                   names_to="COMP", values_to="VALUE"))

  pk %>%
    group_by(time, COMP) %>%
    summarize(MEAN=mean(VALUE), SD=sd(VALUE), .groups="drop") %>%
    ggplot(aes(x=time, y=MEAN, color=COMP)) +
    geom_ribbon(aes(ymin=MEAN-SD, ymax=MEAN+SD, fill=COMP), color=NA, alpha=0.2) +
    geom_line() +
    xlim(0, 50) +
    theme_bw()
})



















