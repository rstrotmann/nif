
test_that("make_crea works", {
  make_dataset <- function(){
    make_dm(nsubs=100, min_age=18, max_age=95) %>%
    make_crea() %>%
    dplyr::select(USUBJID, AGE, SEX, RACE, CREA) %>%
    dplyr::mutate(MDRD=mdrd_egfr(CREA, AGE, SEX, RACE)) %>%
    dplyr::mutate(RAYNAUD=raynaud_egfr(CREA, AGE, SEX, RACE))
  }

  expect_no_error(temp <- make_dataset())

  temp %>%
    ggplot(aes(x=AGE, y=MDRD, color=SEX)) +
    geom_point(size=3) +
    geom_hline(yintercept=c(30, 60, 90), color="red") +
    theme_bw() +
    ggtitle("eGFR (MDRD) by age", subtitle="Synthetic data")

  temp %>%
    ggplot(aes(x=MDRD, y=RAYNAUD, color=RACE)) +
    geom_point(size=3) +
    theme_bw() +
    ggtitle("MDRD vs Raynoud method for eGFT by RACE", subtitle="Synthetic data")

  temp %>%
    ggplot(aes(x=MDRD, y=RAYNAUD, color=SEX)) +
    geom_point(size=3) +
    theme_bw() +
    ggtitle("MDRD vs Raynoud method for eGFT by SEX", subtitle="Synthetic data")
})


