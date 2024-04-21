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
    mutate(IMPUTATION = "") %>%
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
    mutate(IMPUTATION = "")

  temp <- ex %>%
    impute_missing_exendtc_time(silent = T) %>%
    mutate(EXENDTC_has_time = has_time(EXENDTC))

  temp %>%
    summarize(sum = sum(EXENDTC_has_time == F, na.rm = T)) %>%
    as.numeric() %>%
    expect_equal(0)

    expect_equal(all((temp$IMPUTATION != "") == temp$imputation_expected), TRUE)

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
    mutate(IMPUTATION = "") %>%
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
    mutate(IMPUTATION = "")

  obs <- tribble(
    ~USUBJID, ~PARENT, ~DTC,               ~PCRFTDTC,
    1,        "A",     "2022-07-11T14:50", "2022-07-11T13:50",
    2,        "A",     "2022-08-11T15:50", "2022-08-11T14:50",
    3,        "A",     "2022-09-11T15:50", ""
  ) %>% lubrify_dates() %>%
    mutate(IMPUTATION = "")

  temp <- impute_admin_dtc_to_pcrftdtc(admin, obs, silent = TRUE)
  expect_equal(temp$IMPUTATION != "", c(FALSE, TRUE, FALSE))
})


test_that("impute_missing_exendtc", {
  ex <- tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~imputation_expected,
    1,        1,      "A",    "2022-01-01T08:00",  "2022-01-03T08:00", FALSE,
    1,        2,      "A",    "2022-01-07T08:00",  "2022-01-09",       FALSE,
    1,        3,      "A",    "2022-01-14T08:00",  "",                 TRUE,
    1,        4,      "A",    "2022-01-21T08:00",  "2022-01-23T08:00", FALSE,
  ) %>% lubrify_dates() %>%
    mutate(IMPUTATION = "")

  temp <- impute_missing_exendtc(ex, silent = F)
  expect_equal(temp$IMPUTATION != "", temp$imputation_expected)
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
    make_observation(sdtm, "vs", "WEIGHT",
                     observation_filter = "USUBJID == '20230000011010001'")
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


test_that("make_time", {
  test <- tribble(
    ~ID, ~ANALYTE, ~PARENT, ~DTC,               ~EVID,
    1,   "A",      "A",     "2024-03-18T08:00", 0,
    1,   "A",      "A",     "2024-03-18T08:15", 1,
    1,   "A",      "A",     "2024-03-18T08:45", 0,
    1,   "A",      "A",     "2024-03-18T09:00", 0,

    1,   "A1",     "A",     "2024-03-18T08:00", 0,
    1,   "A1",     "A",     "2024-03-18T08:45", 0,
    1,   "A1",     "A",     "2024-03-18T09:00", 0,

    1,   "B",      "B",     "2024-03-18T09:00", 0,
    1,   "B",      "B",     "2024-03-18T09:15", 1,
    1,   "B",      "B",     "2024-03-18T09:45", 0,
    1,   "B",      "B",     "2024-03-18T10:00", 0
  ) %>%
    lubrify_dates() %>%
    mutate(CMT = 1)

  expect_no_error(temp <- make_time(test) %>% as.data.frame())
})


test_that("multiple imputations", {
  ex <- tribble(
    ~USUBJID, ~DOMAIN, ~EXTRT, ~EXSEQ, ~EXSTDTC,           ~EXENDTC,
    "1",      "EX",    "A",    1,      "2021-09-10T10:10", "2021-09-13T22:00", #
    "1",      "EX",    "A",    2,      "2021-09-17T10:35", "2021-09-20T22:35", # on the last day the time is not the administration time!
    "1",      "EX",    "A",    3,      "2021-09-24T10:15", "2021-09-27T22:30", #
    "1",      "EX",    "A",    4,      "2021-10-07T11:00", "2021-10-10T23:00", #
    "1",      "EX",    "A",    5,      "2021-10-14T09:30", "2021-10-17T21:30", #
    "1",      "EX",    "A",    6,      "2021-10-21T10:00", "2021-10-24T22:00"  #
  ) %>% lubrify_dates()

  pc <- tribble(
    ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCRFTDTC,          ~PCDTC,              ~PCDY,       ~PCTPT,   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T09:59",  1,       "PRE-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T10:40",  1, "0.5H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T11:12",  1,   "1H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T12:12",  1,   "2H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T13:15",  1,   "3H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T14:15",  1,   "4H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T16:36",  1,   "6H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T18:10",  1,   "8H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T21:43",  1,  "12H POST-DOSE",   #
#   "1",      "PC",    "A",       "2021-09-11T09:50", "2021-09-11T10:00",  2,       "PRE-DOSE",   #
    "1",      "PC",    "A",       NA,                 "2021-09-11T10:00",  2,       "PRE-DOSE",   #
    "1",      "PC",    "A",       "2021-09-17T10:35", "2021-09-17T10:28",  8,       "PRE-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T10:35", 11,       "PRE-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T11:10", 11, "0.5H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T11:40", 11,   "1H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T12:35", 11,   "2H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T13:53", 11,   "3H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T14:50", 11,   "4H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T16:48", 11,   "6H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T18:48", 11,   "8H POST-DOSE",   #
    "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T22:10", 11,  "12H POST-DOSE",   #
    "1",      "PC",    "A",                       NA, "2021-09-21T10:55", 12,       "PRE-DOSE",   #
    "1",      "PC",    "A",       "2021-09-24T10:15", "2021-09-24T10:10", 15,       "PRE-DOSE",   #
    "1",      "PC",    "A",                       NA, "2021-10-28T09:15", 49,       "PRE-DOSE"    #
  ) %>% lubrify_dates()

  temp <- ex %>%
    mutate(ANALYTE = "A") %>%
    mutate(IMPUTATION = "") %>%
    decompose_dtc("EXSTDTC") %>%
    decompose_dtc("EXENDTC") %>%
    rowwise() %>%
    mutate(DTC_date = list(seq(
      as.Date(.data$EXSTDTC_date),
      as.Date(.data$EXENDTC_date),
      by = "days"))) %>%
    unnest(DTC_date) %>%
    # make time
    group_by(USUBJID, ANALYTE, EXENDTC_date) %>%
    mutate(DTC_time = case_when(
      row_number() == n() ~ .data$EXENDTC_time,
      .default = .data$EXSTDTC_time
    )) %>%
    ungroup() %>%
    select(-c("EXSTDTC_date", "EXSTDTC_time", "EXENDTC_date",
              "EXENDTC_time")) %>%
    mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
    # impute missing administration times from PCRFTDTC
    impute_admin_times_from_pcrftdtc(pc, "A", "A") %>%
    decompose_dtc("DTC") %>%
    as.data.frame()

  expect_equal(temp %>%
                 filter(as.character(DTC_date) == "2021-09-20") %>%
                 pull(DTC_time), "10:40")
})


make_test_sdtm <- function() {
  dm <- tribble(
    ~USUBJID, ~DOMAIN, ~SEX,   ~ACTARMCD,          ~RFXSTDTC,
    "20230000221040001", "DM", "M", "TREATMENT", "2001-01-01T10:29",
    "20230000221040002", "DM", "M", "TREATMENT", "2001-01-02T09:09",
    "20230000221070001", "DM", "M", "TREATMENT", "2000-12-29T09:07",
    "20230000221060001", "DM", "F", "TREATMENT", "2001-01-06T11:18"
  )
  vs <- tribble(
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
  lb <- tribble(
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


test_that("baseline covariates", {
  vs <- make_test_sdtm()$domains$vs
  expect_no_error(
    test <- baseline_covariates(vs, silent = T)
  )
  expect_equal(names(vs), c("USUBJID", "DOMAIN", "VSTESTCD", "VSBLFL", "VSSTRESN"))
  expect_equal(dim(test), c(4, 4))

})


test_that("make subjects", {
  sdtm <- make_test_sdtm()$domains
  expect_no_error(
    test <- make_subjects(sdtm$dm, sdtm$vs, silent = TRUE)
  )
  expect_equal(dim(test), c(4, 8))
})


# test_that("make baseline", {
#   sdtm <- make_test_sdtm()
#   expect_no_error(
#     test <- make_baseline(sdtm, "lb", "CREAT")
#   )
#   expect_equal(dim(test), c(4, 2))
# })


test_that("add_covariate", {
  nif <- tribble(
    ~USUBJID,                  ~DTC, ~NTIME, ~EVID, ~DV,
    1, "2001-01-15 10:36:00",      0,     1,  NA,
    1, "2001-01-15 10:36:00",      0,     0,   0,
    1, "2001-01-15 12:46:00",    1.5,     0, 3.4,
    1, "2001-01-15 15:14:00",      4,     0, 1.5,
    1, "2001-01-23 10:36:00",      0,     1,  NA,
    1, "2001-01-23 10:38:00",      0,     0, 0.1,
    1, "2001-01-23 12:46:00",    1.5,     0, 2.9,
    1, "2001-01-23 15:24:00",      4,     0, 1.5,
    2, "2001-04-07 10:27:00",      0,     1,  NA,
    2, "2001-04-07 10:27:00",      0,     0,   0,
    2, "2001-04-07 12:56:00",    1.5,     0, 3.2,
    2, "2001-04-07 15:32:00",      4,     0, 1.3,
    2, "2001-04-15 10:27:00",      0,     1,  NA,
    2, "2001-04-15 11:15:00",      0,     0,   0,
    2, "2001-04-15 12:56:00",    1.5,     0, 2.8,
    2, "2001-04-15 15:19:00",      4,     0, 1.6
  ) %>%
    mutate(DTC = as_datetime(DTC))

  lb <- tribble(
    ~USUBJID,                ~LBDTC, ~LBTESTCD, ~LBSTRESN,
    1, "2001-01-15T08:00:00",    "TEST",        1L,
    1, "2001-01-16T08:00:00",    "TEST",        1.1,
    # 1, "2001-01-23T08:00:00",    "TEST",        2L,
    2, "2001-04-07T08:00:00",    "TEST",        3L,
    2, "2001-04-15T08:00:00",    "TEST",        4L
  )

  sdtm = list(domains = list(lb = lb))
  expect_no_error(
    temp <- add_covariate(nif, sdtm, "lb", "TEST") %>% as.data.frame()
  )
  expect_equal(all(!is.na(temp$TEST)), TRUE)
})
