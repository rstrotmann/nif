## Complex integration tests for imputation_rules_minimal and
## imputation_rules_standard, exercising the full make_administration /
## add_administration / add_observation pipeline with realistic multi-subject,
## multi-episode scenarios.


# --- imputation_rules_minimal: complex admin scenarios -----------------------

test_that("minimal rules: multi-subject with missing EXENDTC, cutoff, and invalid episodes", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,          ~RFENDTC, ~ACTARMCD,
         "S01",    1, "2025-01-01T08:00", "2025-01-20T08:00",   "ARM A",
         "S02",    0, "2025-01-01T08:00", "2025-01-15T08:00",   "ARM A",
         "S03",    1, "2025-01-01T08:00", "2025-01-25T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "S01",      1,    "D", "2025-01-01T08:00", "2025-01-05T08:00",     100,
         "S01",      2,    "D", "2025-01-06T08:00",                 NA,     100,
         "S01",      3,    "D", "2025-01-10T08:00", "2025-01-14T08:00",     100,
         "S02",      1,    "D", "2025-01-01T09:00", "2025-01-03T09:00",     200,
         "S02",      2,    "D", "2025-01-05T09:00", "2025-01-02T09:00",     200,
         "S02",      3,    "D", "2025-01-08T09:00",                 NA,     200,
         "S03",      1,    "D", "2025-01-01T07:00", "2025-01-10T07:00",     300,
         "S03",      2,    "D", "2025-01-15T07:00", "2025-01-20T07:00",     300,
         "S03",      3,    "D", "2025-01-22T07:00",                 NA,     300
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,          ~PCRFTDTC,
         "S01",    "PC",       "D", "2025-01-01T08:15",
         "S01",    "PC",       "D", "2025-01-03T09:30",
         "S02",    "PC",       "D", "2025-01-01T09:20",
         "S03",    "PC",       "D", "2025-01-05T07:45"
    )
  ))

  cut_off <- as.POSIXct("2025-01-18 23:59:59", tz = "UTC")

  expect_no_message(
    result <- make_administration(
      sdtm, "D",
      imputation = imputation_rules_minimal,
      cut_off_date = cut_off,
      silent = TRUE
    )
  )

  res <- as.data.frame(result)

  # S02 episode 2 (EXENDTC before EXSTDTC) should be filtered out
  s02_dates <- as.Date(res$DTC[res$USUBJID == "S02"])
  expect_false(any(s02_dates == as.Date("2025-01-05")))

  # S03 episode 3 starts after cutoff -> should be removed
  s03_dates <- as.Date(res$DTC[res$USUBJID == "S03"])
  expect_false(any(s03_dates >= as.Date("2025-01-22")))

  # S01 middle missing EXENDTC (episode 2) should be imputed to day before
  # episode 3 start (2025-01-09)
  s01_dates <- as.Date(res$DTC[res$USUBJID == "S01"])
  expect_true(as.Date("2025-01-06") %in% s01_dates)
  expect_true(as.Date("2025-01-09") %in% s01_dates)

  # PCRFTDTC-based time imputation should appear for matching dates
  s01_imp <- res$IMPUTATION[res$USUBJID == "S01" &
                              as.Date(res$DTC) == as.Date("2025-01-01")]
  expect_true(any(grepl("PCRFTDTC", s01_imp)))

  s01_day3_imp <- res$IMPUTATION[res$USUBJID == "S01" &
                                   as.Date(res$DTC) == as.Date("2025-01-03")]
  expect_true(any(grepl("PCRFTDTC", s01_day3_imp)))
})


test_that("minimal rules: time carry-forward across multi-day episodes", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-02-01T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "P01",      1,    "X", "2025-02-01T06:30", "2025-02-07",          50
    )
  ))

  expect_no_message(
    result <- make_administration(
      sdtm, "X",
      imputation = imputation_rules_minimal,
      silent = TRUE
    )
  )

  res <- as.data.frame(result)

  # 7 days of dosing
  expect_equal(nrow(res), 7)

  # First day should have the time from EXSTDTC
  expect_equal(res$IMPUTATION[1], "time copied from EXSTDTC")

  # Remaining days should have carried-forward time
  expect_true(all(grepl("time carried forward", res$IMPUTATION[2:7])))

  # All DTC times should be "06:30" after carry-forward
  dtc_times <- format(res$DTC, "%H:%M")
  expect_true(all(dtc_times == "06:30"))
})


test_that("minimal rules: obs_raw and obs_final are identity (no LLOQ, no TAFD fix)", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-03-01T08:00",   "ARM A"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
         "VS",    "P01",  "WEIGHT",        70, "2025-03-01T08:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "P01",      1,    "X", "2025-03-01T08:00", "2025-03-01T08:00",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,             ~PCDTC, ~PCSTRESN, ~PCSTRESC,
                                                            ~PCLLOQ, ~PCRFTDTC,      ~PCTPT,
         "P01",    "PC",       "X", "2025-03-01T07:00",  NA_real_,     "BLQ",
                                                               0.5,        NA, "PRE-DOSE",
         "P01",    "PC",       "X", "2025-03-01T09:00",       5.2,     "5.2",
                                                               0.5,        NA, "1 H POSTDOSE",
         "P01",    "PC",       "X", "2025-03-01T10:00",      10.0,    "10.0",
                                                               0.5,        NA, "2H POST"
    )
  ))

  nif_result <- nif() |>
    add_administration(sdtm, "X", imputation = imputation_rules_minimal,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "X",
                    imputation = imputation_rules_minimal,
                    silent = TRUE)

  obs <- as.data.frame(nif_result) |>
    dplyr::filter(EVID == 0)

  # With minimal rules, BLQ should NOT be imputed -> DV stays NA for predose
  expect_true(is.na(obs$DV[1]))

  # Predose observation: TAFD should be negative (not zeroed) under minimal rules
  expect_true(obs$TAFD[1] <= 0)
})


test_that("minimal rules: PCRFTDTC overrides EXSTDTC time on matching days", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-04-01T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "P01",      1,    "D", "2025-04-01T08:00", "2025-04-05T08:00",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,          ~PCRFTDTC,
         "P01",    "PC",       "D", "2025-04-01T08:30",
         "P01",    "PC",       "D", "2025-04-03T09:15",
         "P01",    "PC",       "D", "2025-04-05T07:45"
    )
  ))

  expect_no_message(
    result <- make_administration(
      sdtm, "D",
      pctestcd = "D",
      imputation = imputation_rules_minimal,
      silent = TRUE
    )
  )

  res <- as.data.frame(result)
  expect_equal(nrow(res), 5)

  # Day 1: PCRFTDTC (08:30) should override EXSTDTC time (08:00)
  day1 <- res[as.Date(res$DTC) == as.Date("2025-04-01"), ]
  expect_equal(format(day1$DTC, "%H:%M"), "08:30")
  expect_true(grepl("PCRFTDTC", day1$IMPUTATION))

  # Day 3: PCRFTDTC (09:15) imputed
  day3 <- res[as.Date(res$DTC) == as.Date("2025-04-03"), ]
  expect_equal(format(day3$DTC, "%H:%M"), "09:15")
  expect_true(grepl("PCRFTDTC", day3$IMPUTATION))

  # Day 2: no PCRFTDTC -> carried forward from day 1 (08:30)
  day2 <- res[as.Date(res$DTC) == as.Date("2025-04-02"), ]
  expect_equal(format(day2$DTC, "%H:%M"), "08:30")
  expect_true(grepl("carried forward", day2$IMPUTATION))

  # Day 5: PCRFTDTC (07:45) overrides the EXENDTC time (08:00)
  day5 <- res[as.Date(res$DTC) == as.Date("2025-04-05"), ]
  expect_equal(format(day5$DTC, "%H:%M"), "07:45")
  expect_true(grepl("PCRFTDTC", day5$IMPUTATION))
})


test_that("minimal rules: last EXENDTC imputed to cutoff when missing", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-05-01T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC, ~EXENDTC, ~EXDOSE,
         "P01",      1,    "D", "2025-05-01T10:00",       NA,     100
    )
  ))

  cut_off <- as.POSIXct("2025-05-04 23:59:59", tz = "UTC")

  expect_no_message(
    result <- make_administration(
      sdtm, "D",
      imputation = imputation_rules_minimal,
      cut_off_date = cut_off,
      silent = TRUE
    )
  )

  res <- as.data.frame(result)

  # Should expand from May 1 to May 4 (cutoff date)
  dates <- as.Date(res$DTC)
  expect_true(as.Date("2025-05-01") %in% dates)
  expect_true(as.Date("2025-05-04") %in% dates)
  expect_equal(nrow(res), 4)
})


# --- imputation_rules_standard: complex admin scenarios ----------------------

test_that("standard rules: NTIME-based admin time estimation from PC", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-06-01T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,     ~EXENDTC, ~EXDOSE,
         "P01",      1,    "D", "2025-06-01T08:00", "2025-06-03",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,          ~PCRFTDTC,      ~PCTPT,
                                                                    ~PCDTC,
         "P01",    "PC",       "D", "2025-06-01T08:00", "PRE-DOSE",
                                                   "2025-06-01T07:00",
         "P01",    "PC",       "D", "2025-06-01T08:00", "1 H POSTDOSE",
                                                   "2025-06-01T09:05",
         "P01",    "PC",       "D", "2025-06-01T08:00", "2H POST",
                                                   "2025-06-01T10:05",
         "P01",    "PC",       "D", "2025-06-01T08:00", "4HRS",
                                                   "2025-06-01T12:05"
    )
  ))

  expect_no_message(
    result <- make_administration(
      sdtm, "D",
      pctestcd = "D",
      imputation = imputation_rules_standard,
      silent = TRUE
    )
  )

  res <- as.data.frame(result)

  # Day 1: should have time derived from PCRFTDTC (takes precedence over NTIME)
  day1 <- res[as.Date(res$DTC) == as.Date("2025-06-01"), ]
  expect_true(grepl("PCRFTDTC", day1$IMPUTATION))
  expect_equal(format(day1$DTC, "%H:%M"), "08:00")

  # Day 2: no PCRFTDTC or PC data -> carried forward
  day2 <- res[as.Date(res$DTC) == as.Date("2025-06-02"), ]
  expect_true(grepl("carried forward", day2$IMPUTATION))
})


test_that("standard rules: NTIME estimate used when PCRFTDTC absent", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-07-01T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,     ~EXENDTC, ~EXDOSE,
         "P01",      1,    "D", "2025-07-01",      "2025-07-03",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCRFTDTC,         ~PCTPT,
                                                                    ~PCDTC,
         "P01",    "PC",       "D",        NA, "1 H POSTDOSE",
                                                   "2025-07-01T09:00",
         "P01",    "PC",       "D",        NA, "2H POST",
                                                   "2025-07-01T10:00",
         "P01",    "PC",       "D",        NA, "4HRS",
                                                   "2025-07-01T12:00"
    )
  ))

  expect_no_message(
    result <- make_administration(
      sdtm, "D",
      pctestcd = "D",
      imputation = imputation_rules_standard,
      silent = TRUE
    )
  )

  res <- as.data.frame(result)

  # Day 1: should have NTIME-based time estimation (no PCRFTDTC available)
  day1 <- res[as.Date(res$DTC) == as.Date("2025-07-01"), ]
  expect_true(grepl("NTIME", day1$IMPUTATION))
  dtc_time <- format(day1$DTC, "%H:%M")
  expect_true(!is.na(dtc_time))
})


test_that("standard rules: LLOQ imputation on PC observations", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-08-01T08:00",   "ARM A",
         "P02",    0, "2025-08-01T08:00",   "ARM A"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
         "VS",    "P01",  "WEIGHT",        70, "2025-08-01T08:00",
         "VS",    "P02",  "WEIGHT",        65, "2025-08-01T08:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "P01",      1,    "D", "2025-08-01T08:00", "2025-08-01T08:00",     100,
         "P02",      1,    "D", "2025-08-01T08:00", "2025-08-01T08:00",     200
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCSTRESN, ~PCSTRESC, ~PCLLOQ,
                                                                   ~PCDTC,      ~PCTPT,
         "P01",    "PC",       "D",  NA_real_,     "BLQ",      0.5,
                                                    "2025-08-01T07:00", "PRE-DOSE",
         "P01",    "PC",       "D",       5.2,     "5.2",      0.5,
                                                    "2025-08-01T09:00", "1 H POSTDOSE",
         "P01",    "PC",       "D",      10.0,    "10.0",      0.5,
                                                    "2025-08-01T10:00", "2H POST",
         "P02",    "PC",       "D",  NA_real_,    "<0.5",      0.5,
                                                    "2025-08-01T07:30", "PRE-DOSE",
         "P02",    "PC",       "D",      12.0,    "12.0",      0.5,
                                                    "2025-08-01T09:30", "1 H POSTDOSE",
         "P02",    "PC",       "D",  NA_real_,     "BQL",      1.0,
                                                    "2025-08-01T12:00", "4HRS"
    )
  ))

  nif_result <- nif() |>
    add_administration(sdtm, "D",
                       imputation = imputation_rules_standard,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D",
                    imputation = imputation_rules_standard,
                    silent = TRUE)

  obs <- as.data.frame(nif_result) |>
    dplyr::filter(EVID == 0)

  # P01 predose BLQ: DV should be 0.5 / 2 = 0.25
  p01_predose <- obs[obs$USUBJID == "P01" &
                       as.Date(obs$DTC) == as.Date("2025-08-01") &
                       obs$NTIME == 0, ]
  expect_equal(p01_predose$DV, 0.25)

  # P02 predose "<0.5": DV should be 0.5 / 2 = 0.25
  p02_predose <- obs[obs$USUBJID == "P02" &
                       as.Date(obs$DTC) == as.Date("2025-08-01") &
                       obs$NTIME == 0, ]
  expect_equal(p02_predose$DV, 0.25)

  # P02 4hrs BQL with LLOQ=1.0: DV should be 1.0 / 2 = 0.5
  p02_4h <- obs[obs$USUBJID == "P02" &
                  as.Date(obs$DTC) == as.Date("2025-08-01") &
                  obs$NTIME == 4, ]
  expect_equal(p02_4h$DV, 0.5)

  # Non-BLQ values should be unchanged
  p01_1h <- obs[obs$USUBJID == "P01" &
                  as.Date(obs$DTC) == as.Date("2025-08-01") &
                  obs$NTIME == 1, ]
  expect_equal(p01_1h$DV, 5.2)
})


test_that("standard rules: predose TAFD set to zero", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "P01",    1, "2025-09-01T08:00",   "ARM A"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
         "VS",    "P01",  "WEIGHT",        70, "2025-09-01T08:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "P01",      1,    "D", "2025-09-01T08:00", "2025-09-03T08:00",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCSTRESN, ~PCSTRESC, ~PCLLOQ,
                                                                   ~PCDTC,
                                                                       ~PCTPT,  ~PCRFTDTC,
         "P01",    "PC",       "D",       2.0,     "2.0",      0.5,
                                                    "2025-09-01T07:00",
                                                    "PRE-DOSE",  "2025-09-01T08:00",
         "P01",    "PC",       "D",      10.0,    "10.0",      0.5,
                                                    "2025-09-01T09:00",
                                                    "1 H POSTDOSE",  "2025-09-01T08:00",
         "P01",    "PC",       "D",       5.0,     "5.0",      0.5,
                                                    "2025-09-02T07:30",
                                                    "PRE-DOSE",  "2025-09-02T08:00",
         "P01",    "PC",       "D",      12.0,    "12.0",      0.5,
                                                    "2025-09-02T09:00",
                                                    "1 H POSTDOSE",  "2025-09-02T08:00"
    )
  ))

  nif_result <- nif() |>
    add_administration(sdtm, "D",
                       imputation = imputation_rules_standard,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D",
                    imputation = imputation_rules_standard,
                    silent = TRUE)

  obs <- as.data.frame(nif_result) |>
    dplyr::filter(EVID == 0)

  predose_obs <- obs[obs$NTIME == 0, ]

  # Under standard rules, predose observations with negative TAFD are set to 0.
  # Day 1 predose (before first dose) should have TAFD == 0.
  # Day 2 predose has a positive TAFD (hours since first dose) and stays as-is.
  day1_predose <- predose_obs[as.Date(predose_obs$DTC) == as.Date("2025-09-01"), ]
  expect_equal(day1_predose$TAFD, 0)

  day2_predose <- predose_obs[as.Date(predose_obs$DTC) == as.Date("2025-09-02"), ]
  expect_true(day2_predose$TAFD > 0)
})


test_that("standard rules: multi-subject full pipeline with all imputation steps", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,          ~RFENDTC, ~ACTARMCD,
         "A01",    1, "2025-10-01T08:00", "2025-10-20T08:00",   "ARM A",
         "A02",    0, "2025-10-01T08:00", "2025-10-15T08:00",   "ARM A"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
         "VS",    "A01",  "WEIGHT",        75, "2025-10-01T08:00",
         "VS",    "A02",  "WEIGHT",        68, "2025-10-01T08:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "A01",      1,    "D", "2025-10-01T08:00", "2025-10-03T08:00",     100,
         "A01",      2,    "D", "2025-10-05T08:00",                 NA,     100,
         "A01",      3,    "D", "2025-10-08T08:00", "2025-10-10T08:00",     100,
         "A02",      1,    "D", "2025-10-01T09:00", "2025-10-02T09:00",     200,
         "A02",      2,    "D", "2025-10-03T09:00",                 NA,     200
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCSTRESN, ~PCSTRESC, ~PCLLOQ,
                                                                   ~PCDTC,
                                                            ~PCRFTDTC,        ~PCTPT,
         "A01",    "PC",       "D",  NA_real_,     "BLQ",      0.5,
                                                    "2025-10-01T07:30",
                                             "2025-10-01T08:15", "PRE-DOSE",
         "A01",    "PC",       "D",      25.0,    "25.0",      0.5,
                                                    "2025-10-01T09:15",
                                             "2025-10-01T08:15", "1 H POSTDOSE",
         "A01",    "PC",       "D",      50.0,    "50.0",      0.5,
                                                    "2025-10-01T10:15",
                                             "2025-10-01T08:15", "2H POST",
         "A02",    "PC",       "D",       3.0,     "3.0",      1.0,
                                                    "2025-10-01T08:30",
                                             "2025-10-01T09:30", "PRE-DOSE",
         "A02",    "PC",       "D",      15.0,    "15.0",      1.0,
                                                    "2025-10-01T10:30",
                                             "2025-10-01T09:30", "1 H POSTDOSE"
    )
  ))

  cut_off <- "2025-10-12 23:59:59"

  nif_result <- nif() |>
    add_administration(sdtm, "D",
                       imputation = imputation_rules_standard,
                       cut_off_date = cut_off,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D",
                    imputation = imputation_rules_standard,
                    silent = TRUE)

  res <- as.data.frame(nif_result)
  admin <- res[res$EVID == 1, ]
  obs <- res[res$EVID == 0, ]

  # A01 episode 2: missing EXENDTC imputed to day before episode 3 (2025-10-07)
  a01_admin <- admin[admin$USUBJID == "A01", ]
  a01_dates <- as.Date(a01_admin$DTC)
  expect_true(as.Date("2025-10-05") %in% a01_dates)
  expect_true(as.Date("2025-10-07") %in% a01_dates)

  # A02 last EXENDTC imputed to cutoff
  a02_admin <- admin[admin$USUBJID == "A02", ]
  a02_dates <- as.Date(a02_admin$DTC)
  expect_true(as.Date("2025-10-12") %in% a02_dates)

  # LLOQ: A01 predose BLQ should be 0.25
  a01_predose <- obs[obs$USUBJID == "A01" & obs$NTIME == 0, ]
  expect_equal(a01_predose$DV, 0.25)

  # Predose TAFD should be 0
  all_predose <- obs[obs$NTIME == 0, ]
  expect_true(all(all_predose$TAFD == 0))

  # A01 day1: admin time from PCRFTDTC
  a01_day1_admin <- a01_admin[as.Date(a01_admin$DTC) == as.Date("2025-10-01"), ]
  expect_equal(format(a01_day1_admin$DTC, "%H:%M"), "08:15")
})


# --- Comparative tests: minimal vs standard ---------------------------------

test_that("minimal vs standard: BLQ handling differs", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "C01",    1, "2025-11-01T08:00",   "ARM A"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
         "VS",    "C01",  "WEIGHT",        70, "2025-11-01T08:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "C01",      1,    "D", "2025-11-01T08:00", "2025-11-01T08:00",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCSTRESN, ~PCSTRESC, ~PCLLOQ,
                                                                   ~PCDTC,
                                                             ~PCTPT, ~PCRFTDTC,
         "C01",    "PC",       "D",  NA_real_,     "BLQ",      0.5,
                                                    "2025-11-01T07:30",
                                              "PRE-DOSE", "2025-11-01T08:00",
         "C01",    "PC",       "D",       5.0,     "5.0",      0.5,
                                                    "2025-11-01T09:00",
                                              "1 H POSTDOSE", "2025-11-01T08:00"
    )
  ))

  nif_minimal <- nif() |>
    add_administration(sdtm, "D", imputation = imputation_rules_minimal,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D", imputation = imputation_rules_minimal,
                    silent = TRUE)

  nif_standard <- nif() |>
    add_administration(sdtm, "D", imputation = imputation_rules_standard,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D", imputation = imputation_rules_standard,
                    silent = TRUE)

  obs_min <- as.data.frame(nif_minimal) |> dplyr::filter(EVID == 0)
  obs_std <- as.data.frame(nif_standard) |> dplyr::filter(EVID == 0)

  predose_min <- obs_min[obs_min$NTIME == 0, ]
  predose_std <- obs_std[obs_std$NTIME == 0, ]

  # Minimal: BLQ stays NA
  expect_true(is.na(predose_min$DV))

  # Standard: BLQ imputed to 0.25
  expect_equal(predose_std$DV, 0.25)
})


test_that("minimal vs standard: predose TAFD handling differs", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "C01",    1, "2025-12-01T10:00",   "ARM A"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
         "VS",    "C01",  "WEIGHT",        70, "2025-12-01T10:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "C01",      1,    "D", "2025-12-01T10:00", "2025-12-01T10:00",     100
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCSTRESN, ~PCSTRESC, ~PCLLOQ,
                                                                   ~PCDTC,
                                                             ~PCTPT, ~PCRFTDTC,
         "C01",    "PC",       "D",       1.0,     "1.0",      0.5,
                                                    "2025-12-01T09:00",
                                              "PRE-DOSE", "2025-12-01T10:00",
         "C01",    "PC",       "D",      20.0,    "20.0",      0.5,
                                                    "2025-12-01T11:00",
                                              "1 H POSTDOSE", "2025-12-01T10:00"
    )
  ))

  nif_minimal <- nif() |>
    add_administration(sdtm, "D", imputation = imputation_rules_minimal,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D", imputation = imputation_rules_minimal,
                    silent = TRUE)

  nif_standard <- nif() |>
    add_administration(sdtm, "D", imputation = imputation_rules_standard,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D", imputation = imputation_rules_standard,
                    silent = TRUE)

  obs_min <- as.data.frame(nif_minimal) |> dplyr::filter(EVID == 0)
  obs_std <- as.data.frame(nif_standard) |> dplyr::filter(EVID == 0)

  predose_min <- obs_min[obs_min$NTIME == 0, ]
  predose_std <- obs_std[obs_std$NTIME == 0, ]

  # Minimal: predose TAFD stays negative (observation is 1h before dose)
  expect_true(predose_min$TAFD < 0)

  # Standard: predose TAFD clamped to 0
  expect_equal(predose_std$TAFD, 0)
})


test_that("minimal vs standard: admin pre-expansion is identical", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,          ~RFENDTC, ~ACTARMCD,
         "C01",    1, "2025-11-15T08:00", "2025-11-25T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "C01",      1,    "D", "2025-11-15T08:00", "2025-11-17T08:00",     100,
         "C01",      2,    "D", "2025-11-18T08:00",                 NA,     100,
         "C01",      3,    "D", "2025-11-20T08:00", "2025-11-22T08:00",     100,
         "C01",      4,    "D", "2025-11-24T08:00",                 NA,     100
    )
  ))

  cut_off <- as.POSIXct("2025-11-26 23:59:59", tz = "UTC")

  result_min <- make_administration(
    sdtm, "D",
    imputation = imputation_rules_minimal,
    cut_off_date = cut_off,
    silent = TRUE
  )

  result_std <- make_administration(
    sdtm, "D",
    imputation = imputation_rules_standard,
    cut_off_date = cut_off,
    silent = TRUE
  )

  res_min <- as.data.frame(result_min)
  res_std <- as.data.frame(result_std)

  # Both should produce the same dates (pre-expansion is identical)
  expect_equal(
    sort(as.Date(res_min$DTC)),
    sort(as.Date(res_std$DTC))
  )

  # Both should have the same number of rows
  expect_equal(nrow(res_min), nrow(res_std))
})


test_that("standard rules: complex multi-episode multi-subject integration", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC,          ~RFENDTC, ~ACTARMCD,
         "M01",    1, "2026-01-01T08:00", "2026-01-20T08:00",   "ARM A",
         "M02",    0, "2026-01-01T08:00", "2026-01-25T08:00",   "ARM A",
         "M03",    1, "2026-01-01T08:00",                 NA,   "ARM A"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN,              ~VSDTC,
         "VS",    "M01",  "WEIGHT",        80, "2026-01-01T08:00",
         "VS",    "M02",  "WEIGHT",        65, "2026-01-01T08:00",
         "VS",    "M03",  "WEIGHT",        72, "2026-01-01T08:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "M01",      1,    "D", "2026-01-01T08:00", "2026-01-03T08:00",     100,
         "M01",      2,    "D", "2026-01-05T08:00", "2026-01-07T08:00",     150,
         "M02",      1,    "D", "2026-01-01T09:00", "2026-01-05T09:00",     200,
         "M02",      2,    "D", "2026-01-07T09:00",                 NA,     200,
         "M02",      3,    "D", "2026-01-12T09:00", "2026-01-15T09:00",     200,
         "M03",      1,    "D", "2026-01-01T07:00", "2026-01-10T07:00",     300,
         "M03",      2,    "D", "2026-01-15T07:00",                 NA,     300
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCSTRESN, ~PCSTRESC, ~PCLLOQ,
                                                                   ~PCDTC,
                                                            ~PCRFTDTC,        ~PCTPT,
         "M01",    "PC",       "D",  NA_real_,     "BLQ",      0.5,
                                                    "2026-01-01T07:30",
                                             "2026-01-01T08:15", "PRE-DOSE",
         "M01",    "PC",       "D",      20.0,    "20.0",      0.5,
                                                    "2026-01-01T09:15",
                                             "2026-01-01T08:15", "1 H POSTDOSE",
         "M01",    "PC",       "D",      40.0,    "40.0",      0.5,
                                                    "2026-01-01T10:15",
                                             "2026-01-01T08:15", "2H POST",
         "M01",    "PC",       "D",      30.0,    "30.0",      0.5,
                                                    "2026-01-01T12:15",
                                             "2026-01-01T08:15", "4HRS",
         "M02",    "PC",       "D",       1.0,     "1.0",      1.0,
                                                    "2026-01-01T08:30",
                                             "2026-01-01T09:30", "PRE-DOSE",
         "M02",    "PC",       "D",      50.0,    "50.0",      1.0,
                                                    "2026-01-01T10:30",
                                             "2026-01-01T09:30", "1 H POSTDOSE",
         "M02",    "PC",       "D",  NA_real_,     "BQL",      1.0,
                                                    "2026-01-01T13:30",
                                             "2026-01-01T09:30", "4HRS"
    )
  ))

  cut_off <- "2026-01-18 23:59:59"

  nif_result <- nif() |>
    add_administration(sdtm, "D",
                       imputation = imputation_rules_standard,
                       cut_off_date = cut_off,
                       silent = TRUE) |>
    add_observation(sdtm, "PC", "D",
                    imputation = imputation_rules_standard,
                    silent = TRUE)

  res <- as.data.frame(nif_result)
  admin <- res[res$EVID == 1, ]
  obs <- res[res$EVID == 0, ]

  # M01: 2 complete episodes, 3+3=6 days
  m01_admin <- admin[admin$USUBJID == "M01", ]
  expect_equal(nrow(m01_admin), 6)

  # M02: missing EXENDTC in episode 2 -> imputed to day before episode 3
  m02_admin <- admin[admin$USUBJID == "M02", ]
  m02_dates <- as.Date(m02_admin$DTC)
  expect_true(as.Date("2026-01-07") %in% m02_dates)
  expect_true(as.Date("2026-01-11") %in% m02_dates)

  # M03: missing last EXENDTC -> imputed to cutoff
  m03_admin <- admin[admin$USUBJID == "M03", ]
  m03_dates <- as.Date(m03_admin$DTC)
  expect_true(as.Date("2026-01-18") %in% m03_dates)
  expect_false(any(m03_dates > as.Date("2026-01-18")))

  # LLOQ imputations
  m01_predose <- obs[obs$USUBJID == "M01" & obs$NTIME == 0, ]
  expect_equal(m01_predose$DV, 0.25)

  m02_blq <- obs[obs$USUBJID == "M02" & obs$NTIME == 4, ]
  expect_equal(m02_blq$DV, 0.5)

  # Predose TAFD == 0 under standard rules
  all_predose <- obs[obs$NTIME == 0, ]
  expect_true(all(all_predose$TAFD == 0))

  # Admin times from PCRFTDTC
  m01_day1 <- m01_admin[as.Date(m01_admin$DTC) == as.Date("2026-01-01"), ]
  expect_equal(format(m01_day1$DTC, "%H:%M"), "08:15")
})


test_that("standard rules: dose escalation with multiple episodes", {
  sdtm <- sdtm(list(
    dm = tibble::tribble(
      ~USUBJID, ~SEX,          ~RFSTDTC, ~ACTARMCD,
         "E01",    1, "2026-02-01T08:00",   "ARM A"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,          ~EXENDTC, ~EXDOSE,
         "E01",      1,    "D", "2026-02-01T08:00", "2026-02-03T08:00",      50,
         "E01",      2,    "D", "2026-02-04T08:00", "2026-02-06T08:00",     100,
         "E01",      3,    "D", "2026-02-07T08:00", "2026-02-09T08:00",     200
    ),
    pc = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~PCTESTCD,          ~PCRFTDTC,             ~PCDTC,
         "E01",    "PC",       "D", "2026-02-01T08:30", "2026-02-01T09:30",
         "E01",    "PC",       "D", "2026-02-04T09:00", "2026-02-04T10:00",
         "E01",    "PC",       "D", "2026-02-07T08:45", "2026-02-07T09:45"
    )
  ))

  expect_no_message(
    result <- make_administration(
      sdtm, "D",
      pctestcd = "D",
      imputation = imputation_rules_standard,
      silent = TRUE
    )
  )

  res <- as.data.frame(result)

  # 9 days total
  expect_equal(nrow(res), 9)

  # Dose levels
  expect_equal(res$DOSE[1:3], c(50, 50, 50))
  expect_equal(res$DOSE[4:6], c(100, 100, 100))
  expect_equal(res$DOSE[7:9], c(200, 200, 200))

  # PCRFTDTC imputation on first day of each episode
  episode_starts <- res[c(1, 4, 7), ]
  expect_true(all(grepl("PCRFTDTC", episode_starts$IMPUTATION)))

  expect_equal(format(res$DTC[1], "%H:%M"), "08:30")
  expect_equal(format(res$DTC[4], "%H:%M"), "09:00")
  expect_equal(format(res$DTC[7], "%H:%M"), "08:45")
})
