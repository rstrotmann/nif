make_test_sdtm <- function() {
  dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~SEX,   ~ACTARMCD,          ~RFXSTDTC,
    "20230000221040001", "DM", "M", "TREATMENT", "2001-01-01T10:29",
    "20230000221040002", "DM", "M", "TREATMENT", "2001-01-02T09:09",
    "20230000221070001", "DM", "M", "TREATMENT", "2000-12-29T09:07",
    "20230000221060001", "DM", "F", "TREATMENT", "2001-01-06T11:18"
  ) %>%
    mutate(RFSTDTC = RFXSTDTC)
  vs <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~VSTESTCD, ~VSBLFL, ~VSSTRESN,
    "20230000221040001", "VS", "HEIGHT",     "Y",     190.8,
    "20230000221040001", "VS", "WEIGHT",     "Y",      79.3,
    "20230000221040002", "VS", "HEIGHT",     "Y",     199.5,
    "20230000221040002", "VS", "WEIGHT",     "Y",      81.6,
    "20230000221060001", "VS", "HEIGHT",     "Y",     185.4,
    "20230000221060001", "VS", "WEIGHT",     "Y",      92.8,
    "20230000221070001", "VS", "HEIGHT",     "Y",     177.8,
    "20230000221070001", "VS", "WEIGHT",     "Y",      83.3
  )
  lb <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~LBSPEC, ~LBBLFL, ~LBTESTCD,        ~LBSTRESN,
    "20230000221040001",    "DM", "SERUM",     "Y",   "CREAT", 89.2690855827183,
    "20230000221040002",    "DM", "SERUM",     "Y",   "CREAT", 73.3255705088018,
    "20230000221070001",    "DM", "SERUM",     "Y",   "CREAT", 77.8168976104201,
    "20230000221060001",    "DM", "SERUM",     "Y",   "CREAT", 66.8305453780658
  )
  temp <- list(
    dm = dm,
    vs = vs,
    lb = lb
  )
  return(list(domains = temp))
}


test_that("make subjects", {
  sdtm <- make_test_sdtm()$domains
  expect_no_error(
    test <- make_subjects(sdtm$dm, sdtm$vs)
  )
  expect_equal(dim(test), c(4, 9))
})


test_that("make_subject works with different age definitions", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~ACTARMCD,
    1, 0, "A",
    2, 1, "A",
    3, 0, "B",
    4, 1, "B"
  )

  make_subjects(dm)

  temp <- make_subjects(
    dm %>%
      mutate(RFICDTC = now()) %>%
      mutate(BRTHDTC = now() - years(50 - as.numeric(USUBJID)))
  )
  expect_equal(temp$AGE, 50 - temp$USUBJID)

  temp <- make_subjects(
    dm %>%
      mutate(RFICDTC = now()) %>%
      mutate(BRTHDTC = now() - years(50 - as.numeric(USUBJID))) %>%
      mutate(AGE = c(30, 20, NA, NA))
  )
  expect_equal(temp$AGE, c(30, 20, 47, 46))
})
