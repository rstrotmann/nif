#' This ex test fixture has missing EXENDTC in rows 3 and 4.
ex <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE, ~EPOCH,
  "1", 1, 1, "x", "2020-02-26T10:00:00", "2020-03-17T11:15:00", 10, "TREATMENT",
  "1", 1, 2, "x", "2020-03-18T11:35:00", "2020-04-07T10:55:00", 10, "TREATMENT",
  "1", 1, 3, "x", "2020-04-08T11:54:00", "", 10, "TREATMENT",
  "1", 1, 4, "x", "2020-04-29T14:08:00", "", 10, "TREATMENT",
  "1", 2, 1, "x", "2020-04-30T09:25:00", "2020-05-20T05:53:00", 20, "TREATMENT",
  "1", 2, 2, "x", "2020-05-21T13:10:00", "2020-06-10T05:41:00", 20, "TREATMENT",
  "1", 2, 3, "x", "2020-06-11T12:21:00", "2020-07-01", 20, "TREATMENT",
  "1", 2, 4, "x", "2020-07-02T13:12:00", "2020-07-22T05:53:00", 20, "TREATMENT"
) %>% lubrify_dates()


obs <- tibble::tribble(
  ~USUBJID, ~PCTESTCD, ~DTC, ~PCRFTDTC, ~DV,
  1, "y", "2020-02-26T09:50:00", "2020-02-26T10:00:00", NA,
  1, "y", "2020-02-26T10:30:00", "2020-02-26T10:00:00", 0.023600,
  1, "y", "2020-02-26T11:00:00", "2020-02-26T10:00:00", 0.041100,
  1, "y", "2020-02-26T12:00:00", "2020-02-26T10:00:00", 0.022500,
  1, "y", "2020-02-26T13:00:00", "2020-02-26T10:00:00", 0.011100,
  1, "y", "2020-02-26T14:00:00", "2020-02-26T10:00:00", 0.006520,
  1, "y", "2020-02-26T16:00:00", "2020-02-26T10:00:00", 0.002020,
  1, "y", "2020-02-26T18:02:00", "2020-02-26T10:00:00", 0.000745,
  1, "y", "2020-02-26T22:00:00", "2020-02-26T10:00:00", NA,
  1, "y", "2020-02-27T10:45:00", "2020-02-27T10:50:00", NA,
  2, "y", "2020-04-30T09:19:00", "2020-04-30T09:25:00", NA,
  2, "y", "2020-04-30T09:59:00", "2020-04-30T09:25:00", 0.08370,
  2, "y", "2020-04-30T10:25:00", "2020-04-30T09:25:00", 0.14700,
  2, "y", "2020-04-30T11:25:00", "2020-04-30T09:25:00", 0.14200,
  2, "y", "2020-04-30T12:26:00", "2020-04-30T09:25:00", 0.10200,
  2, "y", "2020-04-30T13:27:00", "2020-04-30T09:25:00", 0.05640,
  2, "y", "2020-04-30T15:23:00", "2020-04-30T09:25:00", 0.02060,
  2, "y", "2020-04-30T17:25:00", "2020-04-30T09:25:00", 0.00837,
  2, "y", "2020-04-30T21:25:00", "2020-04-30T09:25:00", 0.00231,
  2, "y", "2020-05-01T10:06:00", "2020-05-01T11:30:00", 0.00158,
  2, "y", "2020-05-07T10:20:00", "2020-05-07T10:24:00", NA
) %>% lubrify_dates()


drug_mapping <- tribble(
  ~EXTRT, ~PCTESTCD, ~PARENT, ~METABOLITE,
  "x", "y", "TEST", FALSE
)


test_that("date conversion works correctly", {
  test <- data.frame(
    RFICDTC = c("2000-12-20T11:11", "2000-12-22T10:09:53"),
    RFSTDTC = c("2001-01-01", "2000-12-30T09:31:19"),
    LABEL = c("A", "B")
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
    ~USUBJID, ~DOMAIN, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXSEQ,
    1, "EX", "TEST", "2022-07-11T13:50", "2022-07-24T09:00", 1,
    1, "EX", "TEST", "2022-08-02T13:45", "2022-08-15T11:10", 2,
    1, "EX", "TEST", "2022-08-23T13:30", "", 3,
    1, "EX", "TEST", "2022-09-13T13:48", "2022-09-26T11:05", 4,
    1, "EX", "TEST", "2022-10-04T13:32", "2022-10-17T11:00", 5,
    1, "EX", "TEST", "2022-11-15T14:20", "", 6,
    2, "EX", "TEST", "2022-07-18T13:23", "2022-07-31T13:23", 1,
    3, "EX", "TEST", "2022-07-18T17:03", "2022-07-31T11:50", 1,
    4, "EX", "TEST", "2022-07-18T13:54", "2022-07-31T12:30", 1,
    4, "EX", "TEST", "2022-08-08T14:35", "", 2
  ) %>%
    lubrify_dates()

  # The reference end date (RFENDTC, i.e., the last administration time of study
  # drug) for subject 1 in the below DM is before the last EXSTDTC for this
  # subject in the above EX. The latter must be filtered out.
  dm <- tibble::tribble(
    ~USUBJID, ~DOMAIN, ~RFSTDTC,           ~RFENDTC,
    1,        "DM",    "2022-07-11T13:50", "2022-11-14T09:00",
    2,        "DM",    "2022-07-18T13:23", "2022-07-31T12:00",
    3,        "DM",    "2022-07-18T17:03", "2022-07-31T11:50",
    4,        "DM",    "2022-07-18T13:54", ""
  ) %>%
    lubrify_dates()

  ex %>%
    mutate(IMPUTATION = "") %>%
    impute_exendtc_to_rfendtc(dm, "TEST", silent = TRUE) %>%
    summarize(sum = sum(is.na(EXENDTC))) %>%
    as.numeric() %>%
    expect_equal(2)
})


test_that("impute_exendtc_to_rfendtc works correctly", {
  dm <- tibble::tribble(
    ~USUBJID, ~SEX, ~RFSTDTC, ~RFENDTC, ~ACTARMCD,
    1, 1, "2024-12-16T7:50", "2024-12-19", "ARM A"
  ) %>%
    lubrify_dates()

  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~EXDOSE,
    1, 1, "A", "2024-12-16T7:50", "", 100
  ) %>%
    lubrify_dates()

  expect_message(
    temp <- ex %>%
      mutate(IMPUTATION = "") %>%
      impute_exendtc_to_rfendtc(dm, "A", silent = FALSE)
  )

  expect_equal(
    temp$IMPUTATION[1],
    "missing EXENDTC set to RFENDTC"
  )

  expect_equal(
    temp$EXENDTC[1],
    as.POSIXct("2024-12-19", tz = "UTC")
  )
})


test_that("impute_missing_exendtc", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC, ~imputation_expected,
    1, 1, "A", "2022-01-01T08:00", "2022-01-03T08:00", FALSE,
    1, 2, "A", "2022-01-07T08:00", "2022-01-09", FALSE,
    1, 3, "A", "2022-01-14T08:00", "", TRUE,
    1, 4, "A", "2022-01-21T08:00", "2022-01-23T08:00", FALSE,
  ) %>%
    lubrify_dates() %>%
    mutate(IMPUTATION = "")

  suppressMessages(
    temp <- impute_missing_exendtc(ex)
  )

  expect_equal(temp$IMPUTATION != "", temp$imputation_expected)
})


test_that("impute_exendtc_to_cutoff works", {
  cutoff_date <- lubridate::as_datetime("2022-12-1", format = "%Y-%m-%d")

  ex <- tibble::tribble(
    ~USUBJID, ~EXSTDTC, ~EXENDTC, ~EXSEQ, ~replace, ~EXTRT,
    1, "2022-07-11T13:50", "2022-07-24T09:00", 1, FALSE, "A",
    1, "2022-08-02T13:45", "2022-08-15T11:10", 2, FALSE, "A",
    1, "2022-08-23T13:30", NA, 3, FALSE, "A",
    1, "2022-09-13T13:48", "2022-09-26T11:05", 4, FALSE, "A",
    1, "2022-10-04T13:32", "2022-10-17T11:00", 5, FALSE, "A",
    1, "2022-11-15T14:20", NA, 6, TRUE, "A",
    2, "2022-07-18T13:23", "2022-07-31T13:23", 1, FALSE, "A",
    3, "2022-07-18T17:03", "2022-07-31T11:50", 1, FALSE, "A",
    3, "2022-07-18T17:03", NA, 1, TRUE, "B",
    4, "2022-07-18T13:54", "2022-07-31T12:30", 1, FALSE, "A",
    4, "2022-08-08T14:35", NA, 2, TRUE, "A"
  ) %>%
    mutate(DOMAIN = "EX", IMPUTATION = NA) %>%
    lubrify_dates()

  expect_no_error(
    suppressMessages(
      temp <- impute_exendtc_to_cutoff(
        ex,
        cutoff_date
      )
    )
  )
  expect_true(all(filter(temp, replace == TRUE)$EXENDTC == cutoff_date))
})


test_that("filter_exendtc_after_exstdtc works", {
  ex <- tibble::tribble(
    ~USUBJID, ~EXSEQ, ~EXTRT, ~EXSTDTC, ~EXENDTC,
    1, 1, "A", "2025-01-01", "2025-01-10",
    1, 2, "A", "2025-01-15", "2025-01-20",
    2, 1, "A", "2025-01-01", NA,
    2, 2, "A", "2025-01-20", "2025-01-10"
  )

  dm <- tibble::tribble(
    ~USUBJID, ~RFENDTC,
    1, "2025-01-20",
    2, "2025-01-20"
  )

  expect_message(temp <- filter_exendtc_after_exstdtc(ex, dm, "A"))
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
  nif <- nif() %>%
    add_administration(
      sdtm,
      extrt = "EXAMPLINIB", analyte = "RS2023", silent = TRUE
    ) %>%
    add_observation(sdtm, domain = "pc", testcd = "RS2023", cmt = 2)

  expect_true(
    all.equal(obs_per_dose_level(nif), obs_per_dose_level(examplinib_sad_nif))
  )
})


test_that("make_time", {
  test <- tibble::tribble(
    ~ID, ~ANALYTE, ~PARENT, ~DTC, ~EVID, ~CMT,
    1, "A", "A", "2024-03-18T08:00", 0, 1,
    1, "A", "A", "2024-03-18T08:15", 1, 1,
    1, "A", "A", "2024-03-18T08:45", 0, 1,
    1, "A", "A", "2024-03-18T09:00", 0, 1,
    1, "A1", "A", "2024-03-18T08:00", 0, 1,
    1, "A1", "A", "2024-03-18T08:45", 0, 1,
    1, "A1", "A", "2024-03-18T09:00", 0, 1,
    1, "B", "B", "2024-03-18T09:00", 0, 1,
    1, "B", "B", "2024-03-18T09:15", 1, 1,
    1, "B", "B", "2024-03-18T09:45", 0, 1,
    1, "B", "B", "2024-03-18T10:00", 0, 1
  ) %>%
    lubrify_dates() %>%
    nif()

  expect_no_error(temp <- make_time(test) %>% as.data.frame())
})


test_that("add_administration, add_observation", {
  suppressMessages(
    expect_no_error(
      nif <- nif() %>%
        add_administration(examplinib_sad, "EXAMPLINIB", analyte = "RS2023") %>%
        add_observation(examplinib_sad, "pc", "RS2023", silent = TRUE)
    )
  )
  expect_equal(analytes(nif), "RS2023")
})


test_that("import_observation", {
  raw <- tibble::tribble(
    ~SUBJ, ~TIME, ~VAL,
    "20230000011010001", 0, 1,
    "20230000011010001", 1, 2,
    "20230000011010001", 2, 3,
    "20230000011010001", 3, 4,
  ) # %>% mutate(COUNTRY = "x")

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
      dtc_field = NULL,
      usubjid_field = "SUBJ",
      ntime_field = "TIME",
      dv_field = "VAL",
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
      nif <- nif() %>%
        add_administration(sdtm, "EXAMPLINIB", analyte = "RS2023") %>%
        add_observation(sdtm, "pc", "RS2023", cmt = 2) %>%
        add_observation(sdtm, "pc", "RS2023487A",
          analyte = "M1",
          parent = "RS2023", cmt = 3
        ) %>%
        add_baseline(sdtm, "lb", "CREAT") %>%
        add_bl_crcl() %>%
        add_bl_renal() %>%
        add_bl_lbm()
    )
  )
})


test_that("guess_pcspec works", {
  suppressMessages(expect_equal(guess_pcspec(
    data.frame(PCSPEC = c("Serum", "plasma", "SERUM"))
  ), "plasma"))
  suppressMessages(expect_equal(guess_pcspec(
    data.frame(PCSPEC = c("Serum", "urine", "Serum"))
  ), "Serum"))
  suppressMessages(expect_equal(guess_pcspec(
    data.frame(PCSPEC = c("Serum", "plasma"))
  ), "plasma"))
})


test_that("guess_lbspec works", {
  suppressMessages(expect_equal(guess_lbspec(
    data.frame(LBSPEC = c("Serum", "plasma", "urine"))
  ), "Serum"))
  suppressMessages(expect_equal(guess_lbspec(
    data.frame(LBSPEC = c("plasma", "urine"))
  ), "urine"))
})


test_that("limit works", {
  test <- tibble::tribble(
    ~ID, ~NTIME, ~EVID,
    1, 0, 1, # admin 1
    1, 0, 0,
    1, 1, 0,
    1, 2, 0,
    1, 4, 0,
    1, 24, 1, # admin 2
    1, 48, 1, # admin 3

    2, 0, 1, # admin 1
    2, 0, 0,
    2, 1, 0,
    2, 2, 0,
    2, 16, 0,
    2, 24, 0,
    3, 0, 1, # admin 1
    3, 24, 1 # admin 2
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
