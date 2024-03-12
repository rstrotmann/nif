#' This ex test fixture has missing EXENDTC in rows 3 and 4.
ex <- tribble(
~STUDYID, ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EPOCH,
"1", 1, 1, "x", "2020-02-26T10:00:00", "2020-03-17T11:15:00", 10, "TREATMENT",
"1", 1, 2, "x", "2020-03-18T11:35:00", "2020-04-07T10:55:00", 10, "TREATMENT",
"1", 1, 3, "x", "2020-04-08T11:54:00", "",                    10, "TREATMENT",
"1", 1, 4, "x", "2020-04-29T14:08:00", "",                    10, "TREATMENT",
"1", 2, 1, "x", "2020-04-30T09:25:00", "2020-05-20T05:53:00", 20, "TREATMENT",
"1", 2, 2, "x", "2020-05-21T13:10:00", "2020-06-10T05:41:00", 20, "TREATMENT",
"1", 2, 3, "x", "2020-06-11T12:21:00", "2020-07-01",          20, "TREATMENT",
"1", 2, 4, "x", "2020-07-02T13:12:00", "2020-07-22T05:53:00", 20, "TREATMENT"
) %>% lubrify_dates()


obs <- tribble(
~USUBJID, ~PCTESTCD, ~DTC, ~PCRFTDTC, ~DV,
1, "y", "2020-02-26T09:50:00", "2020-02-26T10:00:00",       NA,
1, "y", "2020-02-26T10:30:00", "2020-02-26T10:00:00", 0.023600,
1, "y", "2020-02-26T11:00:00", "2020-02-26T10:00:00", 0.041100,
1, "y", "2020-02-26T12:00:00", "2020-02-26T10:00:00", 0.022500,
1, "y", "2020-02-26T13:00:00", "2020-02-26T10:00:00", 0.011100,
1, "y", "2020-02-26T14:00:00", "2020-02-26T10:00:00", 0.006520,
1, "y", "2020-02-26T16:00:00", "2020-02-26T10:00:00", 0.002020,
1, "y", "2020-02-26T18:02:00", "2020-02-26T10:00:00", 0.000745,
1, "y", "2020-02-26T22:00:00", "2020-02-26T10:00:00",       NA,
1, "y", "2020-02-27T10:45:00", "2020-02-27T10:50:00",       NA,
2, "y", "2020-04-30T09:19:00", "2020-04-30T09:25:00",       NA,
2, "y", "2020-04-30T09:59:00", "2020-04-30T09:25:00",  0.08370,
2, "y", "2020-04-30T10:25:00", "2020-04-30T09:25:00",  0.14700,
2, "y", "2020-04-30T11:25:00", "2020-04-30T09:25:00",  0.14200,
2, "y", "2020-04-30T12:26:00", "2020-04-30T09:25:00",  0.10200,
2, "y", "2020-04-30T13:27:00", "2020-04-30T09:25:00",  0.05640,
2, "y", "2020-04-30T15:23:00", "2020-04-30T09:25:00",  0.02060,
2, "y", "2020-04-30T17:25:00", "2020-04-30T09:25:00",  0.00837,
2, "y", "2020-04-30T21:25:00", "2020-04-30T09:25:00",  0.00231,
2, "y", "2020-05-01T10:06:00", "2020-05-01T11:30:00",  0.00158,
2, "y", "2020-05-07T10:20:00", "2020-05-07T10:24:00",       NA
) %>% lubrify_dates()


drug_mapping <- tribble(
  ~EXTRT, ~PCTESTCD, ~PARENT, ~METABOLITE,
  "x",   "y",      "TEST",    FALSE)



test_that("date conversion works correctly", {
  test <- data.frame(
    RFICDTC=c("2000-12-20T11:11", "2000-12-22T10:09:53"),
    RFSTDTC=c("2001-01-01", "2000-12-30T09:31:19"),
    LABEL= c("A", "B")
  )
  expect_no_error(isofy_dates(lubrify_dates(test)))
})



test_that("make.admin works as intended", {
  ex <- tribble(
    ~STUDYID, ~USUBJID,       ~EXSTDTC,           ~EXENDTC, ~EXTRT, ~EXDOSE,   ~EPOCH, ~EXSEQ,
    "1", "1010001", "2001-01-17T08:00", "2001-01-21T08:10", "x",     500, "TREATMENT",      1,
    "1", "1010001", "2001-02-01T08:20", "2001-02-08T08:30", "x",     250, "TREATMENT",      2,
    "1", "1010002", "2001-01-28T09:00", "2001-02-02T09:10", "x",     500, "TREATMENT",      1,
    "1", "1010002", "2001-02-04T09:20", "",                 "x",     500, "TREATMENT",      2
  ) %>% lubrify_dates()

  dm <- tribble(
    ~USUBJID,  ~RFSTDTC,           ~RFENDTC,
    "1010001", "2001-01-17T08:00", "2001-02-08T08:30",
    "1010002", "2001-01-28T09:00", "2001-02-08T09:30"
  ) %>% lubrify_dates()

  drug_mapping <- tribble(
    ~EXTRT, ~PCTESTCD,
    "x", "y"
  )

  cut_off_date <- last_ex_dtc(ex)
  ex <- ex %>%
    impute_missing_exendtc_time(silent = F) %>%
    exclude_exstdtc_after_rfendtc(dm, silent = F) %>%
    impute_exendtc_to_rfendtc(dm, silent = F) %>%
    impute_missing_exendtc(silent = F)

  test <- make_admin(ex, dm, drug_mapping, cut_off_date, silent=F)
  expect_equal(nrow(test), 24)
})





# This test confirms that `impute_missing_exendtc_time` completes missing
# time information in EXENDTC from the respective EXSTDTC field.
# The missing time information in rows 3, 7 and 10 in the test EX will be
# imputed. For row 6 where no date is available, too, the function will return
# 'NA'.
test_that("impute_missing_exendtc_time works as intended", {
  ex <- tribble(
    ~USUBJID, ~DOMAIN, ~EXTRT, ~EXSTDTC,           ~EXENDTC,          ~EXSEQ, ~imputation_expected,
    1,        "EX",    "TEST", "2022-07-11T13:50", "2022-07-24T09:00", 1,     FALSE,
    1,        "EX",    "TEST", "2022-08-02T13:45", "2022-08-15T11:10", 2,     FALSE,
    1,        "EX",    "TEST", "2022-08-23T13:30", "2022-09-05"      , 3,     TRUE,
    1,        "EX",    "TEST", "2022-09-13T13:48", "2022-09-26T11:05", 4,     FALSE,
    1,        "EX",    "TEST", "2022-10-04T13:32", "2022-10-17T11:00", 5,     FALSE,
    1,        "EX",    "TEST", "2022-11-15T14:20", ""                , 6,     FALSE,
    2,        "EX",    "TEST", "2022-07-18T13:23", "2022-07-31"      , 1,     TRUE,
    3,        "EX",    "TEST", "2022-07-18T17:03", "2022-07-31T11:50", 1,     FALSE,
    4,        "EX",    "TEST", "2022-07-18T13:54", "2022-07-31T12:30", 1,     FALSE,
    4,        "EX",    "TEST", "2022-08-08T14:35", "2022-08-21"      , 2,     TRUE
    ) %>%
    lubrify_dates() %>%
    mutate(IMPT_TIME = "")

  temp <- ex %>%
    impute_missing_exendtc_time(silent = T) %>%
    mutate(EXENDTC_has_time = has_time(EXENDTC))

  temp %>%
    summarize(sum = sum(EXENDTC_has_time == F, na.rm = T)) %>%
    as.numeric() %>%
    expect_equal(0)

    expect_equal(all((temp$IMPT_TIME != "") == temp$imputation_expected), TRUE)

})


# This test confirms that `impute_exendtc_to_rfendtc` completes missing
# EXENDTC information to RFENDTC for the last administration in a subject for a
# given EXTRT, provided it is contained in the DM domain.
# In the below test data, subjects 1 and 4 have missing EXENDTC information on
# their last administrations. For subject 1, DM provides a RFENDTC which is used
# as the EXENDT. This is not the case for subject 4. In addition, subject 1 has
# a missing EXENDTC on a non-last administration (row 3). This value is not
# imputed by this function, either.
test_that("impute_exendtc_to_rfendtc works as intended", {
  ex <- tribble(
    ~USUBJID, ~DOMAIN, ~EXTRT, ~EXSTDTC,           ~EXENDTC,          ~EXSEQ,
    1,        "EX",    "TEST", "2022-07-11T13:50", "2022-07-24T09:00", 1,
    1,        "EX",    "TEST", "2022-08-02T13:45", "2022-08-15T11:10", 2,
    1,        "EX",    "TEST", "2022-08-23T13:30", "",                 3,
    1,        "EX",    "TEST", "2022-09-13T13:48", "2022-09-26T11:05", 4,
    1,        "EX",    "TEST", "2022-10-04T13:32", "2022-10-17T11:00", 5,
    1,        "EX",    "TEST", "2022-11-15T14:20", "",                 6,
    2,        "EX",    "TEST", "2022-07-18T13:23", "2022-07-31T13:23", 1,
    3,        "EX",    "TEST", "2022-07-18T17:03", "2022-07-31T11:50", 1,
    4,        "EX",    "TEST", "2022-07-18T13:54", "2022-07-31T12:30", 1,
    4,        "EX",    "TEST", "2022-08-08T14:35", "",                 2) %>%
    lubrify_dates()

  # The reference end date (RFENDTC, i.e., the last administration time of study
  # drug) for subject 1 in the below DM is before the last EXSTDTC for this
  # subject in the above EX. The latter must be filtered out.
  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC,           ~RFENDTC,
    1,        "DM",    "2022-07-11T13:50", "2022-11-14T09:00",
    2,        "DM",    "2022-07-18T13:23", "2022-07-31T12:00",
    3,        "DM",    "2022-07-18T17:03", "2022-07-31T11:50",
    4,        "DM",    "2022-07-18T13:54", "") %>%
    lubrify_dates()

  ex %>%
    impute_exendtc_to_rfendtc(dm, silent=F) %>%
    summarize(sum=sum(is.na(EXENDTC))) %>%
    as.numeric() %>%
    expect_equal(2)
})


test_that("impute_admin_dtc_to_pcrftdtc works as intended", {
  admin <- tribble(
    ~USUBJID, ~PARENT, ~DTC,
    1,        "A",     "2022-07-11T13:50",
    2,        "A",     "2022-08-11",
    3,        "A",     "2022-09-11",
  ) %>%
    lubrify_dates() %>%
    mutate(date = as.Date(extract_date(DTC))) %>%
    mutate(time = extract_time(DTC)) %>%
    mutate(IMPT_TIME = "")

  obs <- tribble(
    ~USUBJID, ~PARENT, ~DTC,               ~PCRFTDTC,
    1,        "A",     "2022-07-11T14:50", "2022-07-11T13:50",
    2,        "A",     "2022-08-11T15:50", "2022-08-11T14:50",
    3,        "A",     "2022-09-11T15:50", ""
  ) %>% lubrify_dates() %>%
    mutate(IMPT_TIME = "")

  temp <- impute_admin_dtc_to_pcrftdtc(admin, obs, silent = TRUE)
  expect_equal(temp$IMPT_TIME != "", c(FALSE, TRUE, FALSE))
})


test_that("impute_missing_exendtc", {
  ex <- tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC,            ~EXENDTC,           ~imputation_expected,
    1,        1,      "A",    "2022-01-01T08:00",  "2022-01-03T08:00", FALSE,
    1,        2,      "A",    "2022-01-07T08:00",  "2022-01-09",       FALSE,
    1,        3,      "A",    "2022-01-14T08:00",  "",                 TRUE,
    1,        4,      "A",    "2022-01-21T08:00",  "2022-01-23T08:00", FALSE,
  ) %>% lubrify_dates() %>%
    mutate(IMPT_TIME = "")

  temp <- impute_missing_exendtc(ex, silent = F)
  expect_equal(temp$IMPT_TIME != "", temp$imputation_expected)
})


test_that("make_subjects works", {
  sdtm <- examplinib_poc
  temp <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"))
  expect_equal(nrow(temp), 80)
})


test_that("make_observation works", {
  sdtm <- examplinib_poc
  ntime_lu <- data.frame(
    PCELTM = c("PT0H", "PT1.5H", "PT_4H"),
    NTIME = c(0, 1.5, 4)
  )
  expect_no_error(
    make_observation(sdtm, "pc", "RS2023",
                     NTIME_lookup = ntime_lu)
  )
  expect_no_error(
    make_observation(sdtm, "pc", "RS2023")
  )
  expect_no_error(
    make_observation(sdtm, "vs", "WEIGHT")
  )

  expect_no_error(
    make_observation(sdtm, "vs", "WEIGHT", observation_filter = {USUBJID == "20230000011010001"})
  )
})


test_that("make_administration works", {
  expect_no_error(
    make_administration(examplinib_poc, "EXAMPLINIB", "RS2023")
  )
})


test_that("make_nif", {
  sdtm <- examplinib_sad
  nif <- new_nif() %>%
    add_administration(sdtm, extrt = "EXAMPLINIB", analyte = "RS2023") %>%
    add_observation(sdtm, domain = "pc", testcd = "RS2023", cmt = 2)

  expect_true(
    all.equal(obs_per_dose_level(nif), obs_per_dose_level(examplinib_sad_nif)))
})



