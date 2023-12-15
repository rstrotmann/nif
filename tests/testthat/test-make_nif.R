ex <- tribble(
  ~STUDYID, ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC,               ~EXENDTC,              ~EXDOSE, ~EPOCH,
  "001",    1,        1,      "TEST",  "2020-02-26T10:00:00", "2020-03-17T11:15:00", 10,      "TREATMENT",
  "001",    1,        2,      "TEST",  "2020-03-18T11:35:00", "2020-04-07T10:55:00", 10,      "TREATMENT",
  "001",    1,        3,      "TEST",  "2020-04-08T11:54:00", "",                    10,      "TREATMENT",
  "001",    1,        4,      "TEST",  "2020-04-29T14:08:00", "",                    10,      "TREATMENT",
  "001",    2,        1,      "TEST",  "2020-04-30T09:25:00", "2020-05-20T05:53:00", 20,      "TREATMENT",
  "001",    2,        2,      "TEST",  "2020-05-21T13:10:00", "2020-06-10T05:41:00", 20,      "TREATMENT",
  "001",    2,        3,      "TEST",  "2020-06-11T12:21:00", "2020-07-01",          20,      "TREATMENT",
  "001",    2,        4,      "TEST",  "2020-07-02T13:12:00", "2020-07-22T05:53:00", 20,      "TREATMENT"
) %>%
  lubrify_dates()


obs <- tribble(
  ~USUBJID, ~PCTESTCD, ~DTC,                 ~PCRFTDTC,              ~DV,
  1,        "TEST",    "2020-02-26T09:50:00", "2020-02-26T10:00:00",       NA,
  1,        "TEST",    "2020-02-26T10:30:00", "2020-02-26T10:00:00", 0.023600,
  1,        "TEST",    "2020-02-26T11:00:00", "2020-02-26T10:00:00", 0.041100,
  1,        "TEST",    "2020-02-26T12:00:00", "2020-02-26T10:00:00", 0.022500,
  1,        "TEST",    "2020-02-26T13:00:00", "2020-02-26T10:00:00", 0.011100,
  1,        "TEST",    "2020-02-26T14:00:00", "2020-02-26T10:00:00", 0.006520,
  1,        "TEST",    "2020-02-26T16:00:00", "2020-02-26T10:00:00", 0.002020,
  1,        "TEST",    "2020-02-26T18:02:00", "2020-02-26T10:00:00", 0.000745,
  1,        "TEST",    "2020-02-26T22:00:00", "2020-02-26T10:00:00",       NA,
  1,        "TEST",    "2020-02-27T10:45:00", "2020-02-27T10:50:00",       NA,
  2,        "TEST",    "2020-04-30T09:19:00", "2020-04-30T09:25:00",       NA,
  2,        "TEST",    "2020-04-30T09:59:00", "2020-04-30T09:25:00",  0.08370,
  2,        "TEST",    "2020-04-30T10:25:00", "2020-04-30T09:25:00",  0.14700,
  2,        "TEST",    "2020-04-30T11:25:00", "2020-04-30T09:25:00",  0.14200,
  2,        "TEST",    "2020-04-30T12:26:00", "2020-04-30T09:25:00",  0.10200,
  2,        "TEST",    "2020-04-30T13:27:00", "2020-04-30T09:25:00",  0.05640,
  2,        "TEST",    "2020-04-30T15:23:00", "2020-04-30T09:25:00",  0.02060,
  2,        "TEST",    "2020-04-30T17:25:00", "2020-04-30T09:25:00",  0.00837,
  2,        "TEST",    "2020-04-30T21:25:00", "2020-04-30T09:25:00",  0.00231,
  2,        "TEST",    "2020-05-01T10:06:00", "2020-05-01T11:30:00",  0.00158,
  2,        "TEST",    "2020-05-07T10:20:00", "2020-05-07T10:24:00",       NA
) %>%
  lubrify_dates()


drug_mapping <- tribble(
  ~EXTRT, ~PCTESTCD, ~PARENT, ~METABOLITE,
  "TEST",   "TEST",      "TEST",    FALSE)


test_that("make_obs generally works", {
  sdtm <- examplinib_fe
  pc <- sdtm$pc

  obs <- make_obs(pc, time_mapping=sdtm$time_mapping,
           spec="PLASMA", silent=T, use_pctptnum=T)
  expect_gt(nrow(obs), 0)
})


test_that("date conversion works correctly", {
  test <- data.frame(
    RFICDTC=c("2000-12-20T11:11", "2000-12-22T10:09:53"),
    RFSTDTC=c("2001-01-01", "2000-12-30T09:31:19"),
    LABEL= c("A", "B")
  )
  expect_no_error(isofy_dates(lubrify_dates(test)))
})


test_that("make.admin works as intended", {
  cut.off.date <- last_ex_dtc(ex)
  impute.missing.end.time <- TRUE
  silent <- F

  test <- make_admin(ex, drug_mapping, cut.off.date, impute.missing.end.time)
  expect_equal(nrow(test), 232)
})


test_that("impute.administration.time works as intended", {
  cut.off.date <- last_ex_dtc(ex)
  admin <- make_admin(ex, drug_mapping=drug_mapping, cut.off.date=cut.off.date,
                      impute.missing.end.time=T, silent=T)

  test <- impute.administration.time(admin, obs)

})




