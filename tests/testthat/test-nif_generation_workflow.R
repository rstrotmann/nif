test_that("full nif generation workflow works with imputations", {
  sdtm <- list(
    dm = tibble::tribble(
      ~DOMAIN, ~STUDYID, ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
      "DM", "test01", "1001", 0, "ACT", "2025-01-01T08:00:00",
      "DM", "test01", "1002", 0, "ACT", "2025-01-01T08:00:00",
      "DM", "test01", "1003", 1, "ACT", "2025-01-01T08:00:00",
      "DM", "test01", "1004", 1, "ACT", "2025-01-01T08:00:00"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
      "VS",    "1001",   "HEIGHT",  170,       "2025-01-01T08:00:00",
      "VS",    "1001",   "WEIGHT",  70,        "2025-01-01T08:00:00",
      "VS",    "1002",   "HEIGHT",  171,       "2025-01-01T08:00:00",
      "VS",    "1002",   "WEIGHT",  71,        "2025-01-01T08:00:00",
      "VS",    "1003",   "HEIGHT",  172,       "2025-01-01T08:00:00",
      "VS",    "1003",   "WEIGHT",  72,        "2025-01-01T08:00:00",
      "VS",    "1004",   "HEIGHT",  173,       "2025-01-01T08:00:00",
      "VS",    "1004",   "WEIGHT",  73,        "2025-01-01T08:00:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~EXDOSE, ~EXTRT, ~EXSTDTC, ~EXENDTC,
      "1001", "EX", 1, "A", "2025-01-01T08:00", "2025-01-01T08:00",
      "1002", "EX", 2, "A", "2025-01-01T08:00:00", "2025-01-03",
      "1002", "EX", 2, "A", "2025-01-04T08:30:00", "",
      "1003", "EX", 3, "A", "2025-01-03T08:00", "2025-01-01T09:00",
      "1003", "EX", 3, "A", "2025-01-07T08:00", "2025-01-10T09:00",
      "1004", "EX", 4, "A", "2025-01-01", "",
      "1004", "EX", 4, "A", "2025-01-04", "2025-01-06T08:00",
    ) %>%
      mutate(EXSEQ = row_number()),
    pc = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~PCTESTCD, ~PCDTC, ~PCSTRESN,
      "PC", "1001", "A", "2025-01-01T07:00:00", NA,
      "PC", "1001", "A", "2025-01-01T08:30", 10,
      "PC", "1001", "A", "2025-01-01T09:00:00", 100,
      "PC", "1001", "A", "2025-01-01T10:00:00", 200,
      "PC", "1001", "A", "2025-01-01T12:00:00", 100,
      "PC", "1001", "A", "2025-01-01T16:00:00", 80,
      "PC", "1002", "A", "2025-01-01T07:00:00", NA,
      "PC", "1002", "A", "2025-01-01T08:30", 11,
      "PC", "1002", "A", "2025-01-01T09:00:00", 101,
      "PC", "1002", "A", "2025-01-01T10:00:00", 201,
      "PC", "1002", "A", "2025-01-01T12:00:00", 101,
      "PC", "1002", "A", "2025-01-01T16:00:00", 81,
      "PC", "1003", "A", "2025-01-01T07:00:00", 5,
      "PC", "1003", "A", "2025-01-01T08:30", 12,
      "PC", "1003", "A", "2025-01-01T09:00:00", 102,
      "PC", "1003", "A", "2025-01-01T10:00:00", 202,
      "PC", "1003", "A", "2025-01-01T12:00:00", 102,
      "PC", "1003", "A", "2025-01-01T16:00:00", 82
    )
  ) %>%
    new_sdtm()

  expect_no_message(
    nif <- new_nif() %>%
      add_administration(sdtm, "A", silent = TRUE)
  )
})


test_that("add_administration imputes missing time to PCRFTDTC", {
  sdtm <- list(
    dm = tibble::tribble(
      ~DOMAIN, ~STUDYID, ~USUBJID, ~SEX, ~ACTARMCD, ~RFSTDTC,
      "DM", "test01", "1004", 1, "ACT", "2025-01-01T08:00:00"
    ),
    vs = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~VSTESTCD, ~VSSTRESN, ~VSDTC,
      "VS",    "1004",   "HEIGHT",  173,       "2025-01-01T08:00:00",
      "VS",    "1004",   "WEIGHT",  73,        "2025-01-01T08:00:00"
    ),
    ex = tibble::tribble(
      ~USUBJID, ~DOMAIN, ~EXDOSE, ~EXTRT, ~EXSTDTC, ~EXENDTC,
      "1004", "EX", 4, "A", "2025-01-01", "",
      "1004", "EX", 4, "A", "2025-01-04", "2025-01-06T08:00",
    ) %>%
      mutate(EXSEQ = row_number()),
    pc = tibble::tribble(
      ~DOMAIN, ~USUBJID, ~PCTESTCD, ~PCDTC, ~PCSTRESN, ~PCRFTDTC,
      "PC", "1004", "A", "2025-01-01T07:00:00", 5, "2025-01-01T08:00:00",
      "PC", "1004", "A", "2025-01-01T08:30", 12, "2025-01-01T08:00:00",
      "PC", "1004", "A", "2025-01-01T09:00:00", 102, "2025-01-01T08:00:00",
      "PC", "1004", "A", "2025-01-01T10:00:00", 202, "2025-01-01T08:00:00",
      "PC", "1004", "A", "2025-01-01T12:00:00", 102, "2025-01-01T08:00:00",
      "PC", "1004", "A", "2025-01-01T16:00:00", 82, "2025-01-01T08:00:00"
    )
  ) %>%
    new_sdtm()

  expect_no_message(
    result <- new_nif() %>%
      add_administration(sdtm, "A", silent = TRUE)
  )

  expect_equal(
    result[1, "IMPUTATION"], "admin time from PCRFTDTC"
  )
})
