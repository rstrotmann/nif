#' This ex test fixture has missing EXENDTC in rows 3 and 4.
ex <- tibble::tribble(
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


obs <- tibble::tribble(
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


# This test confirms that `impute_exendtc_to_rfendtc` completes missing
# EXENDTC information to RFENDTC for the last administration in a subject for a
# given EXTRT, provided it is contained in the DM domain.
# In the below test data, subjects 1 and 4 have missing EXENDTC information on
# their last administrations. For subject 1, DM provides a RFENDTC which is used
# as the EXENDT. This is not the case for subject 4. In addition, subject 1 has
# a missing EXENDTC on a non-last administration (row 3). This value is not
# imputed by this function, either.
test_that("impute_exendtc_to_rfendtc works as intended", {
  ex <- tibble::tribble(
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
  dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC,           ~RFENDTC,
    1,        "DM",    "2022-07-11T13:50", "2022-11-14T09:00",
    2,        "DM",    "2022-07-18T13:23", "2022-07-31T12:00",
    3,        "DM",    "2022-07-18T17:03", "2022-07-31T11:50",
    4,        "DM",    "2022-07-18T13:54", "") %>%
    lubrify_dates()

  suppressMessages(
    ex %>%
      mutate(IMPUTATION = "") %>%
      impute_exendtc_to_rfendtc(dm) %>%
      summarize(sum = sum(is.na(EXENDTC))) %>%
      as.numeric() %>%
      expect_equal(2)
  )
})


test_that("impute_exendtc_to_rfendtc works correctly", {
  dm = tibble::tribble(
    ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
           1,    1, "2024-12-16T7:50", "2024-12-19",   "ARM A"
  ) %>%
    lubrify_dates()

  ex = tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC, ~EXENDTC, ~EXDOSE,
           1,      1,    "A", "2024-12-16T7:50",       "",     100
  ) %>%
    lubrify_dates()

  expect_message(
    temp <- ex %>%
      mutate(IMPUTATION = "") %>%
      impute_exendtc_to_rfendtc(dm, silent = FALSE)
  )

  expect_equal(
    temp$IMPUTATION[1],
    "missing EXENDTC set to RFENDTC")

  expect_equal(
    temp$EXENDTC[1],
    as.POSIXct("2024-12-19", tz = "UTC")
  )
})


# test_that("impute_exendtc_to_rfendtc", {
#   sdtm <- new_sdtm(list(
#     dm = tibble::tribble(
#       ~USUBJID, ~SEX,          ~RFSTDTC,     ~RFENDTC, ~ACTARMCD,
#       1,    1, "2024-12-16T7:50", "2024-12-19",   "ARM A"#,
#       # 2,    1, "2024-12-16T7:50", "2024-12-18",   "ARM A",
#       # 3,    1, "2024-12-16T7:50", "2024-12-17",   "ARM A"
#     ),
#     ex = tibble::tribble(
#       ~USUBJID, ~EXSEQ, ~EXTRT,          ~EXSTDTC,     ~EXENDTC, ~EXDOSE,
#       1,      1,    "A", "2024-12-16T7:50", "2024-12-19",     100,
#       # 2,      2,    "A", "2024-12-16T7:50", "2024-12-18",     100,
#       # 3,      3,    "A", "2024-12-16T7:50", "2024-12-17",     100,
#       # 3,      4,    "A", "2024-12-20",      "2024-12-22",     100
#     ),
#     pc = tibble::tribble(
#       ~USUBJID, ~PCTESTCD,          ~PCRFTDTC,
#       1,       "A",  "2024-12-19T8:10",
#       3,       "A", "2024-12-21T10:00",
#       3,       "A",       "2024-12-22"
#     )
#   ))
#
#
# })



test_that("impute_missing_exendtc", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT,           ~EXSTDTC,           ~EXENDTC, ~imputation_expected,
           1,      1,    "A", "2022-01-01T08:00", "2022-01-03T08:00",                FALSE,
           1,      2,    "A", "2022-01-07T08:00",       "2022-01-09",                FALSE,
           1,      3,    "A", "2022-01-14T08:00",                 "",                 TRUE,
           1,      4,    "A", "2022-01-21T08:00", "2022-01-23T08:00",                FALSE,
  ) %>% lubrify_dates() %>%
    mutate(IMPUTATION = "")

  suppressMessages(
    temp <- impute_missing_exendtc(ex)
  )

  expect_equal(temp$IMPUTATION != "", temp$imputation_expected)
})


test_that("impute_exendtc_to_cutoff works", {
  cutoff_date <- lubridate::as_datetime("2022-12-1", format = "%Y-%m-%d")

  ex <- tibble::tribble(
    ~USUBJID,           ~EXSTDTC,           ~EXENDTC, ~EXSEQ, ~replace, ~EXTRT,
    1, "2022-07-11T13:50", "2022-07-24T09:00",      1,    FALSE,    "A",
    1, "2022-08-02T13:45", "2022-08-15T11:10",      2,    FALSE,    "A",
    1, "2022-08-23T13:30",                 NA,      3,    FALSE,    "A",
    1, "2022-09-13T13:48", "2022-09-26T11:05",      4,    FALSE,    "A",
    1, "2022-10-04T13:32", "2022-10-17T11:00",      5,    FALSE,    "A",
    1, "2022-11-15T14:20",                 NA,      6,     TRUE,    "A",
    2, "2022-07-18T13:23", "2022-07-31T13:23",      1,    FALSE,    "A",
    3, "2022-07-18T17:03", "2022-07-31T11:50",      1,    FALSE,    "A",
    3, "2022-07-18T17:03",                 NA,      1,    TRUE,     "B",
    4, "2022-07-18T13:54", "2022-07-31T12:30",      1,    FALSE,    "A",
    4, "2022-08-08T14:35",                 NA,      2,     TRUE,    "A"
  ) %>%
    mutate(DOMAIN = "EX", IMPUTATION = NA) %>%
    lubrify_dates()
  expect_no_error(
    suppressMessages(
      temp <- impute_exendtc_to_cutoff(
      ex,
      cutoff_date)
    )
  )
  expect_true(all(filter(temp, replace == TRUE)$EXENDTC == cutoff_date))
})


test_that("filter_EXSTDTC_after_EXENDTC works", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    1, 1, "A", 1, 10,
    1, 2, "A", 15, 20,
    2, 1, "A", 1, NA,
    2, 2, "A", 20, 10
  )

  dm <- tibble::tribble(
    ~USUBJID, ~RFENDTC,
    1, 20,
    2, 20
  )

  expect_message(temp <- filter_EXSTDTC_after_EXENDTC(ex, dm))
  expect_equal(nrow(temp), 2)
})


# test_that("make_subjects works", {
#   sdtm <- examplinib_poc
#   expect_no_error(
#     temp <- make_subjects(domain(sdtm, "dm"), domain(sdtm, "vs"))
#   )
#   expect_equal(nrow(temp), 80)
#   expect_equal(
#     names(temp),
#     # c("ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "HEIGHT",
#     #   "WEIGHT", "BMI", "ACTARMCD", "RFXSTDTC"))
#     c("ID", "USUBJID", "SEX", "RACE", "ETHNIC", "COUNTRY", "AGE", "HEIGHT",
#       "WEIGHT", "BMI", "ACTARMCD", "RFXSTDTC", "RFSTDTC"))
# })


test_that("make_nif", {
  sdtm <- examplinib_sad
  nif <- new_nif() %>%
    add_administration(sdtm, extrt = "EXAMPLINIB", analyte = "RS2023") %>%
    add_observation(sdtm, domain = "pc", testcd = "RS2023", cmt = 2)

  expect_true(
    all.equal(obs_per_dose_level(nif), obs_per_dose_level(examplinib_sad_nif)))
})


test_that("make_time", {
  test <- tibble::tribble(
    ~ID, ~ANALYTE, ~PARENT,               ~DTC, ~EVID, ~CMT,
    1,      "A",     "A", "2024-03-18T08:00",     0,    1,
    1,      "A",     "A", "2024-03-18T08:15",     1,    1,
    1,      "A",     "A", "2024-03-18T08:45",     0,    1,
    1,      "A",     "A", "2024-03-18T09:00",     0,    1,
    1,     "A1",     "A", "2024-03-18T08:00",     0,    1,
    1,     "A1",     "A", "2024-03-18T08:45",     0,    1,
    1,     "A1",     "A", "2024-03-18T09:00",     0,    1,
    1,      "B",     "B", "2024-03-18T09:00",     0,    1,
    1,      "B",     "B", "2024-03-18T09:15",     1,    1,
    1,      "B",     "B", "2024-03-18T09:45",     0,    1,
    1,      "B",     "B", "2024-03-18T10:00",     0,    1
  ) %>%
    lubrify_dates() %>%
    new_nif()

  expect_no_error(temp <- make_time(test) %>% as.data.frame())
})


# test_that("multiple imputations", {
#   ex <- tibble::tribble(
#     ~USUBJID, ~DOMAIN, ~EXTRT, ~EXSEQ, ~EXSTDTC,           ~EXENDTC,
#     "1",      "EX",    "A",    1,      "2021-09-10T10:10", "2021-09-13T22:00", #
#     "1",      "EX",    "A",    2,      "2021-09-17T10:35", "2021-09-20T22:35", # on the last day the time is not the administration time!
#     "1",      "EX",    "A",    3,      "2021-09-24T10:15", "2021-09-27T22:30", #
#     "1",      "EX",    "A",    4,      "2021-10-07T11:00", "2021-10-10T23:00", #
#     "1",      "EX",    "A",    5,      "2021-10-14T09:30", "2021-10-17T21:30", #
#     "1",      "EX",    "A",    6,      "2021-10-21T10:00", "2021-10-24T22:00"  #
#   ) %>% lubrify_dates()
#
#   pc <- tibble::tribble(
#     ~USUBJID, ~DOMAIN, ~PCTESTCD, ~PCRFTDTC,          ~PCDTC,              ~PCDY,       ~PCTPT,   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T09:59",  1,       "PRE-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T10:40",  1, "0.5H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T11:12",  1,   "1H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T12:12",  1,   "2H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T13:15",  1,   "3H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T14:15",  1,   "4H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T16:36",  1,   "6H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T18:10",  1,   "8H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-10T10:10", "2021-09-10T21:43",  1,  "12H POST-DOSE",   #
# #   "1",      "PC",    "A",       "2021-09-11T09:50", "2021-09-11T10:00",  2,       "PRE-DOSE",   #
#     "1",      "PC",    "A",       NA,                 "2021-09-11T10:00",  2,       "PRE-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-17T10:35", "2021-09-17T10:28",  8,       "PRE-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T10:35", 11,       "PRE-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T11:10", 11, "0.5H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T11:40", 11,   "1H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T12:35", 11,   "2H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T13:53", 11,   "3H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T14:50", 11,   "4H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T16:48", 11,   "6H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T18:48", 11,   "8H POST-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-20T10:40", "2021-09-20T22:10", 11,  "12H POST-DOSE",   #
#     "1",      "PC",    "A",                       NA, "2021-09-21T10:55", 12,       "PRE-DOSE",   #
#     "1",      "PC",    "A",       "2021-09-24T10:15", "2021-09-24T10:10", 15,       "PRE-DOSE",   #
#     "1",      "PC",    "A",                       NA, "2021-10-28T09:15", 49,       "PRE-DOSE"    #
#   ) %>% lubrify_dates()
#
#   temp <- ex %>%
#     mutate(ANALYTE = "A") %>%
#     mutate(IMPUTATION = "") %>%
#     decompose_dtc("EXSTDTC") %>%
#     decompose_dtc("EXENDTC") %>%
#     rowwise() %>%
#     mutate(DTC_date = list(seq(
#       as.Date(.data$EXSTDTC_date),
#       as.Date(.data$EXENDTC_date),
#       by = "days"))) %>%
#     tidyr::unnest(DTC_date) %>%
#     # make time
#     group_by(USUBJID, ANALYTE, EXENDTC_date) %>%
#     mutate(DTC_time = case_when(
#       row_number() == n() ~ .data$EXENDTC_time,
#       .default = .data$EXSTDTC_time
#     )) %>%
#     ungroup() %>%
#     select(-c("EXSTDTC_date", "EXSTDTC_time", "EXENDTC_date",
#               "EXENDTC_time")) %>%
#     mutate(DTC = compose_dtc(.data$DTC_date, .data$DTC_time)) %>%
#     # impute missing administration times from PCRFTDTC
#     impute_admin_times_from_pcrftdtc(pc, "A", "A") %>%
#     decompose_dtc("DTC") %>%
#     as.data.frame()
#
#   expect_equal(temp %>%
#                  filter(as.character(DTC_date) == "2021-09-20") %>%
#                  pull(DTC_time), "10:40")
# })


# test_that("add_covariate works", {
#   nif <- tibble::tribble(
#     ~USUBJID,                  ~DTC, ~NTIME, ~EVID, ~DV,
#     1, "2001-01-15 10:36:00",      0,     1,  NA,
#     1, "2001-01-15 10:36:00",      0,     0,   0,
#     1, "2001-01-15 12:46:00",    1.5,     0, 3.4,
#     1, "2001-01-15 15:14:00",      4,     0, 1.5,
#     1, "2001-01-23 10:36:00",      0,     1,  NA,
#     1, "2001-01-23 10:38:00",      0,     0, 0.1,
#     1, "2001-01-23 12:46:00",    1.5,     0, 2.9,
#     1, "2001-01-23 15:24:00",      4,     0, 1.5,
#     2, "2001-04-07 10:27:00",      0,     1,  NA,
#     2, "2001-04-07 10:27:00",      0,     0,   0,
#     2, "2001-04-07 12:56:00",    1.5,     0, 3.2,
#     2, "2001-04-07 15:32:00",      4,     0, 1.3,
#     2, "2001-04-15 10:27:00",      0,     1,  NA,
#     2, "2001-04-15 11:15:00",      0,     0,   0,
#     2, "2001-04-15 12:56:00",    1.5,     0, 2.8,
#     2, "2001-04-15 15:19:00",      4,     0, 1.6
#   ) %>%
#     mutate(DTC = as_datetime(DTC)) %>%
#     new_nif()
#
#   lb <- tibble::tribble(
#     ~USUBJID,                ~LBDTC, ~LBTESTCD, ~LBSTRESN,
#     1, "2001-01-15T08:00:00",    "TEST",        1L,
#     1, "2001-01-16T08:00:00",    "TEST",        1.1,
#     # 1, "2001-01-23T08:00:00",    "TEST",        2L,
#     2, "2001-04-07T08:00:00",    "TEST",        3L,
#     2, "2001-04-15T08:00:00",    "TEST",        4L
#   )
#
#   sdtm = list(domains = list(lb = lb))
#
#   expect_no_error(
#     temp <- add_covariate(nif, sdtm, "lb", "TEST") %>%
#       as.data.frame()
#   )
#   expect_equal(all(!is.na(temp$TEST)), TRUE)
# })


test_that("add_administration, add_observation", {
  suppressMessages(
    expect_no_error(
      nif <- new_nif() %>%
      add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023") %>%
      add_observation(examplinib_sad, "pc", "RS2023", silent = TRUE)
    )
  )
  expect_equal(analytes(nif), "RS2023")

})


test_that("import_observation", {
  raw <- tibble::tribble(
    ~SUBJ,             ~TIME, ~VAL,
    "20230000011010001", 0,     1,
    "20230000011010001", 1,     2,
    "20230000011010001", 2,     3,
    "20230000011010001", 3,     4,
  ) #%>% mutate(COUNTRY = "x")

  nif <- examplinib_sad_nif %>%
    mutate(COUNTRY = "X")

  expect_no_error(
    test <- import_observation(
    nif = nif,
    raw = raw,
    analyte = "TEST",
    parent = NULL,
    cmt = NULL,
    observation_filter = "TRUE",
    DTC_field = NULL,
    USUBJID_field = "SUBJ",
    NTIME_field = "TIME",
    DV_field = "VAL",
    keep = "COUNTRY",
    debug = TRUE,
    silent = TRUE
    )
  )
})

test_that("make_nif integration works", {
  sdtm <- examplinib_poc
  suppressMessages(
    expect_no_error(
      nif <- new_nif() %>%
        add_administration(sdtm, "EXAMPLINIB", analyte = "RS2023") %>%
        add_observation(sdtm, "pc", "RS2023", cmt = 2) %>%
        add_observation(sdtm, "pc", "RS2023487A", analyte = "M1",
                        parent = "RS2023", cmt = 3) %>%
        add_baseline(sdtm, "lb", "CREAT") %>%
        add_bl_crcl() %>%
        add_bl_renal() %>%
        add_bl_lbm()
    )
  )
})


test_that("guess_pcspec works", {
  suppressMessages(expect_equal(guess_pcspec(
    data.frame(PCSPEC = c("Serum", "plasma", "SERUM"))), "plasma"))
  suppressMessages(expect_equal(guess_pcspec(
    data.frame(PCSPEC = c("Serum", "urine", "Serum"))), "Serum"))
  suppressMessages(expect_equal(guess_pcspec(
    data.frame(PCSPEC = c("Serum", "plasma"))), "plasma"))
})


test_that("guess_lbspec works", {
  suppressMessages(expect_equal(guess_lbspec(
    data.frame(LBSPEC = c("Serum", "plasma", "urine"))), "Serum"))
  suppressMessages(expect_equal(guess_lbspec(
    data.frame(LBSPEC = c("plasma", "urine"))), "urine"))
})


test_that("add_time works", {
  test <- data.frame(
    USUBJID = c(1, 2, 3, 4),
    DTC = now(),
    NTIME = c(0, 1, 2, 4)
  ) %>%
    tidyr::expand(USUBJID, NTIME, DTC) %>%
    mutate(DTC = DTC + hours(NTIME)) %>%
    add_time()
  expect_equal(test$NTIME, test$TIME)
})


test_that("limit works", {
  test <- tibble::tribble(
    ~ID, ~NTIME, ~EVID,
      1,      0,     1, # admin 1
      1,      0,     0,
      1,      1,     0,
      1,      2,     0,
      1,      4,     0,
      1,     24,     1, # admin 2
      1,     48,     1, # admin 3

      2,      0,     1, # admin 1
      2,      0,     0,
      2,      1,     0,
      2,      2,     0,
      2,     16,     0,
      2,     24,     0,

      3,      0,     1, # admin 1
      3,     24,     1  # admin 2
  ) %>%
    mutate(DTC = now() + hours(NTIME)) %>%
    arrange(ID, DTC, EVID) %>%
    as.data.frame()

  expect_equal(
    limit(test) %>%
      pull(NTIME),
    c(0, 0, 1, 2, 4, 0, 0, 1, 2, 16, 24)
  )
  expect_equal(
    limit(test, individual = FALSE) %>%
      pull(NTIME),
    c(0, 0, 1, 2, 4, 24, 0, 0, 1, 2, 16, 24)
  )
  expect_equal(
    limit(test, keep_no_obs_sbs = TRUE) %>%
      pull(NTIME),
    c(0, 0, 1, 2, 4, 0, 0, 1, 2, 16, 24, 0, 24)
  )
})









