
test_that("synthesize_crea works", {
  make_dataset <- function(){
    dm <- synthesize_dm(nsubs = 100, min_age = 18, max_age = 95)
    vs <- synthesize_vs(dm) %>%
      select(USUBJID, VSSTRESN, VSTESTCD) %>%
      tidyr::pivot_wider(names_from="VSTESTCD", values_from="VSSTRESN")
    dm %>%
      synthesize_crea() %>%
      left_join(vs, by="USUBJID") %>%
      dplyr::select(USUBJID, AGE, SEX, RACE,
                    WEIGHT, HEIGHT, CREA) %>%
      dplyr::mutate(MDRD = egfr_mdrd(CREA, AGE, SEX,
                                     RACE)) %>%
      dplyr::mutate(RAYNAUD = egfr_raynaud(CREA, AGE, SEX,
                                           RACE)) %>%
      dplyr::mutate(CG = egfr_cg(CREA, AGE, SEX, RACE,
                                 WEIGHT))
  }

  expect_no_error(temp <- make_dataset())

  temp %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$AGE, y = .data$MDRD,
                                 color = .data$SEX)) +
    ggplot2::geom_point(size=3) +
    ggplot2::geom_hline(yintercept=c(30, 60, 90)) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("eGFR (MDRD) by age", subtitle="Synthetic data")

  temp %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$MDRD, y = .data$RAYNAUD,
                                 color = .data$RACE)) +
    ggplot2::geom_point(size=3) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("MDRD vs Raynoud method for eGFT by RACE",
                     subtitle="Synthetic data")

  temp %>%
    ggplot2::ggplot(ggplot2::aes(x = MDRD, y = CG, color = SEX)) +
    ggplot2::geom_point(size=3) +
    ggplot2::geom_abline(slope=1, intercept=0) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("MDRD vs CG method for eGFT by RACE",
                     subtitle="Synthetic data")

  temp %>%
    ggplot2::ggplot(ggplot2::aes(x=MDRD, y=RAYNAUD, color=SEX)) +
    ggplot2::geom_point(size=3) +
    ggplot2::geom_abline(slope=1, intercept=0) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle("MDRD vs Raynoud method for eGFT by SEX",
                     subtitle="Synthetic data")
})



create_subjects <- function(n=100) {
  dm <- synthesize_dm(nsubs = n, min_age = 18, max_age = 90) %>%
    synthesize_crea()
  vs <- synthesize_vs(dm) %>%
    filter(EPOCH == "SCREENING") %>%
    dplyr::select("USUBJID", "VSTESTCD", "VSSTRESN") %>%
    tidyr::pivot_wider(names_from = "VSTESTCD", values_from = "VSSTRESN") %>%
    as.data.frame()
  sbs <- dm %>%
    left_join(vs, by = "USUBJID") %>%
    mutate(ID = row_number()) %>%
    dplyr::select(ID, SEX, AGE, HEIGHT, WEIGHT, EGFR)
  return(sbs)
}


test_that("EGFR is age-dependent", {
  expect_no_error(sbs <- create_subjects())
  sbs %>%
    ggplot2::ggplot(ggplot2::aes(x = AGE, y = EGFR)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::theme_bw()
})


simulate_pk <- function(n = 20) {
  sbs <- create_subjects(n = n)

  sampling_scheme <- data.frame(
    time = c(0, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 10, 12, 24, 48, 72, 96, 144, 168))

  ev <- rxode2::et(amountUnits = "mg", timeUnits = "hours") %>%
    rxode2::add.dosing(dose = 500, dosing.to = "depot", rate = -2,
                       start.time = 0) %>%
    rxode2::add.sampling(sampling_scheme$time) %>%
    rxode2::et(id = sbs$ID) %>%
    as.data.frame() %>%
    left_join(
      sbs %>% dplyr::select(id = ID, SEX, AGE, HEIGHT, WEIGHT, EGFR),
      by = "id") %>%
    mutate(amt = 500, FOOD = 0, NTIME = time)

  pk_sim(ev)
}


# test_that("PK model works", {
#   expect_no_error(
#     pk <- simulate_pk(n = 20) %>%
#       tidyr::pivot_longer(cols = c("centr", "peri", "metab", "renal"),
#                    names_to = "COMP", values_to = "VALUE"))
#
#   pk %>%
#     group_by(time, COMP) %>%
#     summarize(MEAN = mean(VALUE), SD = sd(VALUE), .groups = "drop") %>%
#     ggplot2::ggplot(ggplot2::aes(x = time, y = MEAN, color = COMP)) +
#     ggplot2::geom_ribbon(ggplot2::aes(
#       ymin = MEAN-SD, ymax = MEAN+SD, fill = COMP), color = NA, alpha = 0.2) +
#     ggplot2::geom_line() +
#     ggplot2::xlim(0, 50) +
#     ggplot2::theme_bw()
# })



# test_that("EGFR is a covariate in PK model", {
#     pk <- simulate_pk(n = 100) %>%
#       tidyr::pivot_longer(cols = c("centr", "peri", "metab", "renal"),
#                    names_to = "COMP", values_to = "VALUE")
#
#   pk %>%
#     ggplot2::ggplot(ggplot2::aes(x = EGFR, y = ke)) +
#     ggplot2::geom_point(size = 3) +
#     ggplot2::theme_bw()
#
#   pk %>%
#     group_by(time, COMP) %>%
#     summarize(MEAN = mean(VALUE), SD = sd(VALUE), .groups = "drop") %>%
#     ggplot2::ggplot(ggplot2::aes(x = time, y = MEAN, color = COMP)) +
#     ggplot2::geom_ribbon(ggplot2::aes(ymin = MEAN-SD, ymax = MEAN+SD,
#                                       fill = COMP), color = NA, alpha = 0.2) +
#     ggplot2::geom_line() +
#     ggplot2::xlim(0, 24) +
#     ggplot2::theme_bw()
# })
















