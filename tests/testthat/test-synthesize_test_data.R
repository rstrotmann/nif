
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


